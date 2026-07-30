#' Table summary of for MAIHDA analysis
#'
#' `r lifecycle::badge("experimental")`<br />
#' Helpers to generate formatted tables of a MAIHDA analysis as proposed by
#' Evans et al. (*SSM - Population Health* 2024, doi:
#' [10.1016/j.ssmph.2024.101664](https://doi.org/10.1016/j.ssmph.2024.101664)).
#' It relies on the [MAIHDA][MAIHDA::MAIHDA-package] package. This package
#' being under active development, the proposed functions here are experimental.
#'
#' `tbl_maihda()` is intended to replicate Table 3 of Evans et al. 2024, with
#' fixed effects, between-stratum variance and model summary statistics
#' including VPC (variance partition coefficient) and PCV (proportional change
#' in variance). It accepts a `maihda_analysis` object created with
#' [MAIHDA::maihda()], a single `maihda_model` created with
#' [MAIHDA::fit_maihda()] or a list of several `maihfda_model` objects. For this
#' last case, PCV should be manually added to the models to be displayed (see
#' examples).
#'
#' `tbl_strata_info()` is intended to replicate Table 2, showing the number of
#' strata having a certain sample size.
#'
#' `tbl_strata_predictions()` is intended to replicate Table, showing the strata
#' with the highest and the lowest predicted value. If a `maihda_analysis`
#' object is passed to `tbl_strata_predictions()`, the null model is
#' taken into account by default for computing the predicted values, following
#' the behavior of [MAIHDA::maihda_table()]. It should be noted that in Evans
#' et al. 2024, the authors used the adjusted model, which could be done with
#' the argument `which = "adjusted"`.
#'
#' To be noted, themes from the [gtsummary][gtsummary::theme_gtsummary] package
#' are taken into account for formatting the different values.
#' @param x a MAIHDA object
#' @param ... additional parameters passed to [gtsummary::tbl_regression()]
#' @param twomodels_labels for a two-model MAIHDA analysis, labels for the two
#' models
#' @param statistics_header string header of the summary statistics
#' @param statistics_labels name list of labels for the summary statistics
#' @param statistics_include <[`tidy-select`][dplyr::dplyr_tidy_select]>\cr
#' names of summary statistics to be included: must be column names of the
#' tibble returned by `glance_maihda_model()`
#' @param notes display some notes (number of strata, of observations, engine,
#' model family) about the analysis?
#' @param notes_labels name list of labels for the notes
#' @export
#' @keywords models
#' @examplesIf rlang::is_installed(c("gtsummary", "gt", "MAIHDA", "broom.mixed"))
#' \donttest{
#' theme_gtsummary_bold_labels()
#'
#' # gaussian model
#'
#' data("maihda_health_data", package = "MAIHDA")
#' a <- MAIHDA::maihda(
#'   BMI ~ Age + Gender + Race + (1 | Gender:Race),
#'   data = maihda_health_data
#' )
#'
#' a |> tbl_strata_info(breaks = c(50, 100, 150))
#' a |> tbl_maihda()
#' a |> tbl_strata_predictions()
#'
#' # a binomial example
#'
#' m <- MAIHDA::maihda(
#'   Survived ~ Age + Sex + Class + (1 | Age:Sex:Class),
#'   data = titanic,
#'   family = binomial
#' )
#'
#' m |> tbl_strata_info()
#' m |> tbl_maihda(exponentiate = TRUE)
#' m |> tbl_strata_predictions(n_strata = NULL)
#' m |> tbl_strata_predictions(which = "adjusted", n_strata = 3)
#'
#' # Partially adjusted models
#'
#' m0 <- MAIHDA::fit_maihda(
#'   Survived ~ 1 + (1 | Age:Sex:Class),
#'   data = titanic,
#'   family = binomial
#' )
#' m1 <- MAIHDA::fit_maihda(
#'   Survived ~ Age + (1 | Age:Sex:Class),
#'   data = titanic,
#'   family = binomial
#' )
#' m2 <- MAIHDA::fit_maihda(
#'   Survived ~ Sex + (1 | Age:Sex:Class),
#'   data = titanic,
#'   family = binomial
#' )
#' m3 <- MAIHDA::fit_maihda(
#'   Survived ~ Class + (1 | Age:Sex:Class),
#'   data = titanic,
#'   family = binomial
#' )
#'
#' # manually adding PCV
#' m1$pcv <- MAIHDA::calculate_pcv(m0, m1)
#' m2$pcv <- MAIHDA::calculate_pcv(m0, m2)
#' m3$pcv <- MAIHDA::calculate_pcv(m0, m3)
#'
#' list(Null = m0, Age = m1, Sex = m2, Class = m3) |>
#'   tbl_maihda(exponentiate = TRUE)
#' }
tbl_maihda <- function(
  x,
  ...,
  twomodels_labels = c("Null model", "Adjusted model"),
  statistics_header = "Summary statistics",
  statistics_labels = list(
    bsv = "Between-stratum variance",
    bssd = "Between-stratum standard deviation",
    vpc = "Variance Partition Coefficient (VPC)",
    pcv = "Proportional Change in Variance (PCV)",
    auc = "Area Under Receiver Operating Characteristic Curve (AUC)",
    mor = "Median Odds Ratio (MOR)"
  ),
  statistics_include = -dplyr::any_of("bssd"),
  notes = TRUE,
  notes_labels = list(
    n_strata = "Strata:",
    nobs = "Observations:",
    engine = "Engine:",
    family = "Family:"
  )
) {
  rlang::check_installed("gtsummary")
  rlang::check_installed("gt")
  rlang::check_installed("MAIHDA")
  rlang::check_installed("broom.mixed")

  if (inherits(x, "maihda_model")) {
    res <- x |>
      tbl_maihda_model(
        ...,
        statistics_labels = statistics_labels,
        statistics_include = {{ statistics_include }}
      ) |>
      add_glance_header(header = statistics_header) |>
      bold_variable_group_headers()
    if (notes) res <- res |> add_maihda_notes(x, notes_labels)
    return(res)
  }

  if (inherits(x, "maihda_analysis") && x$mode == "two-model") {
    model1 <- x$model
    model2 <- x$model_adjusted
    model2$pcv <- x$pcv
    x <- list(model1, model2)
    names(x) <- twomodels_labels
  }

  res <-
    x |>
    purrr::map(
      \(x) {
        tbl_maihda_model(
          x,
          ...,
          statistics_labels = statistics_labels,
          statistics_include = {{ statistics_include }}
        )
      }
    ) |>
    gtsummary::tbl_merge(
      tab_spanner = paste0("**", names(x), "**"),
      quiet = TRUE
    ) |>
    gtsummary::modify_table_body(
      \(x) dplyr::arrange(x, .data$row_type == "glance_statistic")
    ) |>
    pcv_after_vpc() |>
    add_glance_header(header = statistics_header) |>
    bold_variable_group_headers()

  if (notes)
    res <- res |> add_maihda_notes(x[[1]], notes_labels)

  res
}

tbl_maihda_model <- function(
  x,
  ...,
  statistics_labels = NULL,
  statistics_include = dplyr::everything()
) {
  if (!inherits(x, "maihda_model"))
    cli::cli_abort(
      "All elements of {.arg x} should be of class {.class maihda_model}."
    )
  stats <- glance_maihda_model(x)
  tbl <-
    x$model |>
    gtsummary::tbl_regression(intercept = TRUE, group_by = NULL, ...) |>
    gtsummary::add_glance_table(
      glance_fun = \(y) stats,
      label = statistics_labels,
      include = {{ statistics_include }},
      fmt_fun = list(
        everything() ~ gtsummary::label_style_sigfig(digits = 3),
        dplyr::any_of(c("vpc", "pcv")) ~
          gtsummary::label_style_percent(digits = 1, suffix = "%")
      )
    )
  # adding CI for PCV
  if (!is.null(x$pcv) && x$pcv$bootstrap) {
    tbl$table_body[tbl$table_body$variable == "pcv", "conf.low"] <-
      x$pcv$ci_lower * 100
    tbl$table_body[tbl$table_body$variable == "pcv", "conf.high"] <-
      x$pcv$ci_upper * 100
  }

  tbl
}

pcv_after_vpc <- function(tbl) {
  if (all(c("vpc", "pcv") %in% tbl$table_body$variable)) {
    tbl <-
      tbl |>
      gtsummary::modify_table_body(
        \(x) {
          x <-
            x |>
            dplyr::mutate(rank = as.numeric(dplyr::row_number()))
          x[!is.na(x$variable) & x$variable == "pcv", "rank"] <-
            x[!is.na(x$variable) & x$variable == "vpc", "rank"] + .5
          x |>
            dplyr::arrange(.data$rank) |>
            dplyr::select(-dplyr::any_of("rank"))
        }
      )
  }
  tbl
}

add_maihda_notes <- function(
  tbl,
  x,
  label = list(
    n_strata = "Strata:",
    nobs = "Observations:",
    engine = "Engine:",
    family = "Family:"
  )
) {
  rlang::check_installed("broom")
  g <- x |> broom::glance()
  note <- paste0(
    label$n_strata, " ", g$n_strata, ", ",
    label$nobs, " ", g$nobs, ", ",
    label$engine, " ", g$engine, ", ",
    label$family, " ", g$family, "."
  )
  tbl |>
    gtsummary::modify_source_note(note)
}

#' @rdname tbl_maihda
#' @param breaks breaks for sample size per stratum
#' @param column_labels named list of column labels
#' @param total_label string of the total label in the notes
#' @export
tbl_strata_info <- function(
  x,
  breaks = c(10, 20, 30, 50, 100),
  column_labels = list(
    size = "Sample size per stratum",
    n = "Number of strata",
    prop = "Proportion of strata"
  ),
  total_label = "Total number of strata:"
) {
  rlang::check_installed("gtsummary")
  rlang::check_installed("gt")
  rlang::check_installed("MAIHDA")
  if (inherits(x, "maihda_analysis"))
    x <- x$model
  if (!inherits(x, "maihda_model"))
    cli::cli_abort("{.arg x} should be of class {.class maihda_model} or {.class maihda_analysis}.") # no lint

  info <- x$strata_info
  breaks <- breaks |> sort(decreasing = TRUE)
  res <- dplyr::tibble()
  for (i in breaks) {
    res <-
      res |>
      dplyr::bind_rows(
        dplyr::tibble(
          size = paste("\u2265", i),
          n = sum(info$n >= i)
        )
      )
  }
  res <-
    res |>
    dplyr::bind_rows(
      dplyr::tibble(
        size = paste("<", min(breaks)),
        n = sum(info$n < min(breaks))
      )
    )
  res$prop <- res$n / nrow(info)

  res |>
    gt::gt() |>
    gt::cols_align("center", "n") |>
    gt::cols_label(.list = column_labels) |>
    gt::fmt_percent("prop", decimals = 1) |>
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = gt::cells_column_labels()
    ) |>
    gt::tab_source_note(
      paste(total_label, nrow(info))
    )
}

#' @rdname tbl_maihda
#' @param n_strata number of strata to show at each end (top and bottom),
#' use `Inf` or `NULL` to show all strata
#' @inheritParams MAIHDA::maihda_table scale which
#' @param group_labels labels for group names
#' @param digits number of decimals for predictions
#' @export
tbl_strata_predictions <- function(
  x,
  n_strata = 5L,
  scale = c("response", "link"),
  which = c("null", "adjusted"),
  column_labels = list(
    rank = "Rank",
    n = "n",
    predicted = "Predicted",
    ci = "95% CI"
  ),
  group_labels = list("highest", "lowest"),
  digits = 1L
) {
  rlang::check_installed("gtsummary")
  rlang::check_installed("gt")
  rlang::check_installed("MAIHDA")

  scale <- match.arg(scale)
  which <- match.arg(which)

  if (!inherits(x, "maihda_model") && !inherits(x, "maihda_analysis"))
    cli::cli_abort("{.arg x} should be of class {.class maihda_model} or {.class maihda_analysis}.") # no lint

  if (is.null(n_strata)) n_strata <- Inf
  n_strata |> rlang::check_number_whole(min = 1, allow_infinite = TRUE)

  res <-
    x |>
    MAIHDA::maihda_table(scale = scale, which = which) |>
    purrr::pluck("strata")

  if (inherits(x, "maihda_analysis")) x <- x$model

  if (n_strata < (nrow(res) / 2)) {
    res <-
      dplyr::bind_rows(
        res |>
          utils::head(n_strata) |>
          dplyr::mutate(group = paste(n_strata, group_labels[[1]])),
        res |>
          utils::tail(n_strata) |>
          dplyr::mutate(group = paste(n_strata, group_labels[[2]]))
      ) |>
      dplyr::group_by(.data$group)
  } else {
    n_strata <- Inf
  }

  res <-
    res |>
    dplyr::mutate(stratum = as.character(.data$stratum)) |>
    dplyr::left_join(
      x$strata_info |>
        dplyr::mutate(stratum = as.character(.data$stratum)) |>
        dplyr::select(dplyr::any_of(c("stratum", x$strata_vars))),
      by = "stratum"
    ) |>
    dplyr::select(dplyr::any_of(c(
      "group", "rank", x$strata_vars, "n",
      "predicted", "predicted_lower", "predicted_upper"
    )))

  if (x$family$family == "binomial" && scale == "response") {
    f <- gtsummary::label_style_percent(digits = digits, suffix = "%")
  } else {
    f <- gtsummary::label_style_number(digits = digits)
  }
  sep <- gtsummary::get_gtsummary_theme()$`pkgwide-str:ci.sep`
  if (is.null(sep)) sep <- ", "

  res <-
    res |>
    dplyr::mutate(
      predicted = f(.data$predicted),
      predicted_lower = f(.data$predicted_lower),
      predicted_upper = f(.data$predicted_upper),
      ci = paste0(.data$predicted_lower, sep, .data$predicted_upper)
    ) |>
    dplyr::select(-dplyr::any_of(c("predicted_lower", "predicted_upper")))

  strata_labels <-
    x$original_data |>
    dplyr::select(dplyr::any_of(x$strata_vars)) |>
    labelled::get_variable_labels(null_action = "fill")

  tbl <-
    res |>
    gt::gt() |>
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = gt::cells_column_labels()
    ) |>
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = gt::cells_row_groups()
    ) |>
    gt::cols_label(.list = column_labels) |>
    gt::cols_label(.list = strata_labels) |>
    gt::cols_align("center", "ci")

  tbl
}


#' @rdname tbl_maihda
#' @export
glance_maihda_model <- function(x) {
  rlang::check_installed("broom")
  rlang::check_installed("MAIHDA")

  if (inherits(x, "maihda_analysis"))
    x <- x$model

  res <-
    x |>
    MAIHDA::maihda_table() |>
    purrr::pluck("models") |>
    dplyr::filter(.data$statistic != "Intercept") |>
    dplyr::mutate(
      statistic = dplyr::case_when(
        .data$statistic == "Between-stratum variance" ~ "bsv",
        .data$statistic == "Between-stratum SD" ~ "bssd",
        .data$statistic == "VPC/ICC" ~ "vpc",
        TRUE ~ tolower(.data$statistic)
      )
    ) |>
    dplyr::select("statistic", "estimate") |>
    tidyr::pivot_wider(names_from = "statistic", values_from = "estimate")

  if (!is.null(x$pcv) && inherits(x$pcv, "pcv_result"))
    res$pcv <- x$pcv$pcv

  res
}
