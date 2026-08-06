#' Table summary of for MAIHDA analysis
#'
#' `r lifecycle::badge("experimental")`<br />
#' Helpers to generate formatted tables of a MAIHDA analysis as proposed by
#' Evans et al. (*SSM - Population Health* 2024,
#' \doi{10.1016/j.ssmph.2024.101664}).
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
#' `tbl_partially_adjusted_maihda()` is an helper allowing to compute and
#' display all partially adjusted models (see examples).
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
#' `plot_strata_predictions()` allows to visually compare predicted values
#' by strata according to one or several specific variable defining the strata.
#'
#' To be noted, themes from the [gtsummary][gtsummary::theme_gtsummary] package
#' are taken into account for formatting the different values.
#' @param x a MAIHDA object (`maihda_analysis` or `maihda_model`); for
#' `tbl_maihda()` it could also be a list of `maihda_model` objects; for
#' `tbl_partially_adjusted_maihda()`, only a `maihda_analysis` computed with
#' `MAIHDA::maihda(decomposition = "two-model")` is allowed; for
#' `tbl_strata_info()`, the result of [MAIHDA::make_strata()] is also accepted
#' @param ... additional parameters passed to [gtsummary::tbl_regression()]
#' @param global_p display global p-value instead of terms p-value (see
#' [gtsummary::add_global_p()]), not available if `engine = "wemix"`.
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
#' a |> plot_strata_predictions()
#' a |> plot_strata_predictions(by = Race)
#'
#' # a binomial example
#'
#' titanic |>
#'   MAIHDA::make_strata(c("Age", "Class")) |>
#'   tbl_strata_info()
#'
#' m <- MAIHDA::maihda(
#'   Survived ~ Age + Sex + Class + (1 | Age:Sex:Class),
#'   data = titanic,
#'   family = binomial
#' )
#'
#' m |> tbl_strata_info()
#' m |> tbl_strata_info(type = "exclusive")
#' m |> tbl_maihda(exponentiate = TRUE)
#' m |> tbl_strata_predictions(n_strata = NULL)
#' m |> tbl_strata_predictions(which = "adjusted", n_strata = 3)
#' m |> plot_strata_predictions()
#' m |> plot_strata_predictions(geom = "bar")
#' m |> plot_strata_predictions(n_strata = 3L)
#' m |> plot_strata_predictions(by = Sex)
#' m |> plot_strata_predictions(by = c(Sex, Age))
#' m |> plot_strata_predictions(highlight_n_below = 20)
#' m |> plot_strata_predictions(by = Age, highlight_n_below = 20)
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
#'   tbl_maihda(exponentiate = TRUE, global_p = TRUE)
#'
#' # in one call
#' m |> tbl_partially_adjusted_maihda(exponentiate = TRUE)
#'
#' # sample-weighted data
#' if (rlang::is_installed("WeMix")) {
#'
#' d <- titanic
#' d$weight <- 1
#' wm <- MAIHDA::maihda(
#'   Survived ~ Age + Sex + Class + (1 | Age:Sex:Class),
#'   data = d,
#'   family = binomial,
#'   sampling_weights = "weight",
#'   engine = "wemix"
#' )
#' wm |> tbl_maihda(exponentiate = TRUE)
#' }
#'
#' }
tbl_maihda <- function(
  x,
  ...,
  global_p = FALSE,
  twomodels_labels = c("Null model", "Adjusted model"),
  statistics_header = "Summary statistics",
  statistics_labels = list(
    bsv = "Between-stratum variance",
    bssd = "Between-stratum standard deviation",
    vpc = "Variance Partition Coefficient (VPC)",
    pcv = "Proportional Change in Variance (PCV)",
    auc = "Area Under Receiver Operating Characteristic Curve (AUC)",
    mor = "Median Odds Ratio (MOR)",
    csvpc = "Context share (VPC)"
  ),
  statistics_include = -dplyr::any_of("bssd"),
  notes = TRUE,
  notes_labels = list(
    n_strata = "Strata:",
    nobs = "Observations:",
    engine = "Engine:",
    family = "Family:",
    context = "Variable(s) in context:"
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
        global_p = global_p,
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
          global_p = global_p,
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
  global_p = FALSE,
  statistics_labels = NULL,
  statistics_include = dplyr::everything()
) {
  if (!inherits(x, "maihda_model"))
    cli::cli_abort(
      "All elements of {.arg x} should be of class {.class maihda_model}."
    )
  stats <- glance_maihda_model(x)

  if (x$engine == "wemix") {
    tbl <-
      x |>
      gtsummary::tbl_regression(
        intercept = TRUE,
        tidy_fun = tidy_maihda_model,
        ...
      )
  } else {
    tbl <-
      x$model |>
      gtsummary::tbl_regression(intercept = TRUE, group_by = NULL, ...)

    if (global_p && nrow(tbl$table_body) > 1) { # avoid if no fixed effects
      tbl <- tbl |> gtsummary::add_global_p()
    }
  }

  tbl <-
    tbl |>
    gtsummary::add_glance_table(
      glance_fun = \(y) stats,
      label = statistics_labels,
      include = {{ statistics_include }},
      fmt_fun = list(
        everything() ~ gtsummary::label_style_sigfig(digits = 3),
        dplyr::any_of(c("vpc", "pcv", "csvpc")) ~
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
    family = "Family:",
    context = "Variable(s) in context:"
  )
) {
  rlang::check_installed("broom")
  g <- x |> broom::glance()
  note <- paste0(
    label$n_strata, " ", g$n_strata, ", ",
    label$nobs, " ", g$nobs, ", ",
    label$engine, " ", g$engine, ", ",
    label$family, " ", g$family
  )

  if (!is.null(x$context_vars))
    note <- paste0(
      note, ", ",
      label$context, " ", paste(x$context_vars, collapse = ", ")
    )

  tbl |>
    gtsummary::modify_source_note(note)
}

#' @rdname tbl_maihda
#' @param return_data return a data frame instead of a table
#' @export
tbl_partially_adjusted_maihda <- function(
  x,
  ...,
  global_p = FALSE,
  twomodels_labels = c("Null model", "Fully adjusted model"),
  statistics_header = "Summary statistics",
  statistics_labels = list(
    bsv = "Between-stratum variance",
    bssd = "Between-stratum standard deviation",
    vpc = "Variance Partition Coefficient (VPC)",
    pcv = "Proportional Change in Variance (PCV)",
    auc = "Area Under Receiver Operating Characteristic Curve (AUC)",
    mor = "Median Odds Ratio (MOR)",
    csvpc = "Context share (VPC)"
  ),
  statistics_include = -dplyr::any_of("bssd"),
  notes = TRUE,
  notes_labels = list(
    n_strata = "Strata:",
    nobs = "Observations:",
    engine = "Engine:",
    family = "Family:",
    context = "Variable(s) in context:"
  ),
  return_data = FALSE
) {
  if (!inherits(x, "maihda_analysis"))
    cli::cli_abort("{.arg x} should be a {.class maihda_analysis} object.")

  if (x$mode != "two-model")
    cli::cli_abort("{.arg x} should be computed with `decomposition = \"two-model\"`") # nolint

  l <-
    x$model$strata_vars |>
    purrr::map(\(v) fit_partially_adjusted_maihda(x, v))
  names(l) <-
    x$model_adjusted$original_data |>
    dplyr::select(dplyr::any_of(x$model_adjusted$strata_vars)) |>
    labelled::get_variable_labels(null_action = "fill", unlist = TRUE)

  l0 <- list(x$model)
  names(l0) <- twomodels_labels[1]
  x$model_adjusted$pcv <- x$pcv
  lf <- list(x$model_adjusted)
  names(lf) <- twomodels_labels[2]
  l <- l0 |> append(l) |> append(lf)

  if (return_data) return(l)

  l |>
    tbl_maihda(
      ...,
      global_p = global_p,
      statistics_header = statistics_header,
      statistics_labels = statistics_labels,
      statistics_include = {{ statistics_include }},
      notes = notes,
      notes_labels = notes_labels
    )
}

fit_partially_adjusted_maihda <- function(m, variable) {
  m0 <- m$model
  ma <- m$model_adjusted
  pa <-
    MAIHDA::fit_maihda(
      formula = stats::update(
        m0$formula,
        stats::as.formula(paste("~ . +", variable))
      ),
      data = ma$data,
      engine = m0$engine,
      family = m0$family,
      context = m0$context_vars,
      sampling_weights = m0$sampling_weights
    )
  pa$pcv <- MAIHDA::calculate_pcv(m0, pa)
  pa
}

#' @rdname tbl_maihda
#' @param breaks breaks for sample size per stratum
#' @param type type of table (nested or exclusive size categories)
#' @param column_labels named list of column labels
#' @param total_label string of the total label in the notes
#' @export
tbl_strata_info <- function(
  x,
  breaks = c(10, 20, 30, 50, 100),
  type = c("nested", "exclusive"),
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
  if (!is.list(x) && !"strata_info" %in% names(x))
    cli::cli_abort("{.arg x} should be of class {.class maihda_model} or {.class maihda_analysis} or the result of {.fn MAIHDA::make_strata}.") # nolint

  type <- match.arg(type)

  info <- x$strata_info

  breaks <- breaks |> sort(decreasing = TRUE)

  if (type == "nested") {
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
  }

  if (type == "exclusive") {
    breaks <- breaks |> sort()
    l <- paste(dplyr::lag(breaks), breaks, sep = "-")
    l[1] <- paste0("< ", breaks[1])
    l <- c(l, paste0("\u2265 ", dplyr::last(breaks)))
    info$size <-
      info$n |>
      cut(breaks = c(0, breaks, Inf), right = FALSE, labels = l)
    res <- info |> dplyr::count(dplyr::pick("size"))
    res$prop <- res$n / nrow(info)
  }


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
  n_strata = Inf,
  scale = c("response", "link"),
  which = c("null", "adjusted"),
  column_labels = list(
    rank = "Rank",
    n = "n",
    predicted = "Predicted",
    ci = "95% CI"
  ),
  group_labels = list("highest", "lowest"),
  digits = 1L,
  return_data = FALSE
) {
  rlang::check_installed("gtsummary")
  rlang::check_installed("gt")
  rlang::check_installed("MAIHDA")

  scale <- match.arg(scale)
  which <- match.arg(which)

  if (!inherits(x, "maihda_model") && !inherits(x, "maihda_analysis"))
    cli::cli_abort("{.arg x} should be of class {.class maihda_model} or {.class maihda_analysis}.") # nolint

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

  strata_labels <-
    x$original_data |>
    dplyr::select(dplyr::any_of(x$strata_vars)) |>
    labelled::get_variable_labels(null_action = "fill")

  res <-
    res |>
    labelled::set_variable_labels(.labels = strata_labels)

  if (return_data) return(res)

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
#' @param geom geometry to use for plotting proportions ("point" by default).
#' @param by <[`tidy-select`][dplyr::dplyr_tidy_select]>\cr
#' list of variables to compare by
#' @param sort should the plot be sorted?
#' @param highlight_n_below highlight strata with a number of observations
#' below this number (`NULL` for not highlight, incompatible with `geom = "bar`)
#' @export
plot_strata_predictions <- function(
  x,
  by = NULL,
  geom = c("point", "bar"),
  n_strata = Inf,
  scale = c("response", "link"),
  which = c("null", "adjusted"),
  sort = TRUE,
  highlight_n_below = NULL
) {
  rlang::check_installed("ggstats")
  scale <- match.arg(scale)
  geom <- match.arg(geom)
  highlight_n_below |>
    rlang::check_number_whole(
      allow_null = TRUE,
      allow_infinite = FALSE,
      min = 2
    )

  if (!is.null(highlight_n_below) && geom != "point")
    cli::cli_abort("{.arg geom} should be equal to \"point\" when {.arg highlight_n_below} is provided.") # nolint

  d <-
    x |>
    tbl_strata_predictions(
      which = which,
      scale = scale,
      n_strata = n_strata,
      return_data = TRUE
    ) |>
    dplyr::ungroup()

  if (inherits(x, "maihda_analysis")) x <- x$model

  strata_vars <- x$strata_vars
  by_vars <- d |> dplyr::select({{ by }}) |> colnames()
  y_vars <- setdiff(strata_vars, by_vars)

  if (length(by_vars) > 0) {
    by_strata <- MAIHDA::make_strata(d, by_vars)
    d$.by.. <- by_strata$data$stratum |>
      factor(labels = by_strata$strata_info$label)
    show_color_legend <- TRUE
  } else {
    d$.by.. <- 1
    d$.by.. <- factor(d$.by..)
    show_color_legend <- FALSE
  }

  y_strata <- MAIHDA::make_strata(d, y_vars)
  d$.y.. <- y_strata$data$stratum |>
    factor(labels = y_strata$strata_info$label)

  d <- d |> dplyr::arrange(dplyr::pick(dplyr::all_of(by_vars)))
  d$.by.. <-
    d$.by.. |>
    forcats::fct_inorder() |>
    forcats::fct_rev()

  if (sort) {
    d$.y.. <-
      d$.y.. |>
      forcats::fct_reorder(d$predicted, .fun = mean)
  } else {
    d <- d |> dplyr::arrange(dplyr::pick(dplyr::all_of(y_vars)))
    d$.y.. <-
      d$.y.. |>
      forcats::fct_inorder() |>
      forcats::fct_rev()
  }

  if (geom == "point" && is.null(highlight_n_below)) {
    p <-
      ggplot2::ggplot(d) +
      ggplot2::aes(
        y = .data$.y..,
        color = .data$.by..,
        x = .data$predicted,
        xmin = .data$predicted_lower,
        xmax = .data$predicted_upper
      ) +
      ggstats::geom_stripped_rows(
        mapping = ggplot2::aes(colour = NULL),
        odd = "#11111111",
        show.legend = FALSE
      ) +
      ggplot2::geom_errorbar(
        position = ggplot2::position_dodge(width = .75),
        width = .2,
        show.legend = FALSE
      ) +
      ggplot2::geom_point(
        position = ggplot2::position_dodge(width = .75),
        show.legend = show_color_legend
      ) +
      scale_color_safe() +
      ggplot2::guides(color = ggplot2::guide_legend(reverse = TRUE))
  }

  if (geom == "point" && !is.null(highlight_n_below)) {
    d$.highlight <-
      (d$n >= highlight_n_below) |>
      factor(
        levels = c(FALSE, TRUE),
        labels = paste0(c("n < ", "n \u2265 "), highlight_n_below)
      )

    p <-
      ggplot2::ggplot(d) +
      ggplot2::aes(
        y = .data$.y..,
        color = .data$.by..,
        shape = .data$.highlight,
        x = .data$predicted,
        xmin = .data$predicted_lower,
        xmax = .data$predicted_upper
      ) +
      ggstats::geom_stripped_rows(
        mapping = ggplot2::aes(colour = NULL),
        odd = "#11111111",
        show.legend = FALSE
      ) +
      ggplot2::geom_errorbar(
        position = ggplot2::position_dodge(width = .75),
        width = .2,
        show.legend = FALSE
      ) +
      ggplot2::geom_point(
        position = ggplot2::position_dodge(width = .75),
        stroke = 2,
        show.legend = TRUE,
        fill = "white"
      ) +
      ggplot2::scale_shape_manual(values = c(21, 16))

    if (show_color_legend) {
      p <- p + scale_color_safe()
    } else {
      p <- p + scale_color_safe(guide = "none")
    }
  }

  if (geom == "bar") {
    p <-
      ggplot2::ggplot(d) +
      ggplot2::aes(
        y = .data$.y..,
        fill = .data$.by..,
        x = .data$predicted,
        xmin = .data$predicted_lower,
        xmax = .data$predicted_upper
      ) +
      ggstats::geom_stripped_rows(
        mapping = ggplot2::aes(colour = NULL),
        odd = "#11111111",
        show.legend = FALSE
      ) +
      ggplot2::geom_bar(
        position = ggplot2::position_dodge(width = .75),
        show.legend = show_color_legend,
        stat = "identity",
        width = .75
      ) +
      ggplot2::geom_errorbar(
        position = ggplot2::position_dodge(width = .75),
        width = .2,
        show.legend = FALSE
      ) +
      scale_fill_safe() +
      ggplot2::guides(fill = ggplot2::guide_legend(reverse = TRUE))
  }
  p <-
    p +
    ggplot2::theme_light() +
    ggplot2::theme(
      legend.position = "bottom",
      legend.box = "vertical",
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(linetype = "dashed"),
      axis.title.x = ggplot2::element_text(face = "bold"),
      axis.ticks.y = ggplot2::element_blank()
    ) +
    ggplot2::labs(x = NULL, y = NULL, color = NULL, fill = NULL, shape = NULL) +
    ggplot2::scale_y_discrete(expand = ggplot2::expansion(0, 0.5))

  if (x$family$family == "binomial" && scale == "response") {
    p <-
      p +
      ggplot2::expand_limits(x = 0) +
      ggplot2::scale_x_continuous(labels = scales::percent)
  }

  p
}

#' @export
#' @rdname tbl_maihda
plot_maihda_predictions_by <- function(
  x,
  by = NULL,
  scale = c("response", "link"),
  which = c("null", "adjusted"),
  sort = TRUE
) {
  lifecycle::deprecate_warn(
    "0.12.0",
    "plot_maihda_predictions_by()",
    "plot_maihda_predictions()"
  )
  plot_strata_predictions(
    x = x,
    by = {{ by }},
    n_strata = Inf,
    scale = scale,
    which = which,
    sort = sort
  )
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
        .data$statistic == "Context share (VPC)" ~ "csvpc",
        TRUE ~ tolower(.data$statistic)
      )
    ) |>
    dplyr::select("statistic", "estimate") |>
    tidyr::pivot_wider(names_from = "statistic", values_from = "estimate")

  if (!is.null(x$pcv) && inherits(x$pcv, "pcv_result"))
    res$pcv <- x$pcv$pcv

  res
}

tidy_maihda_model <- function(x, exponentiate = FALSE, ...) {
  rlang::check_installed("broom")
  rlang::check_installed("MAIHDA")

  res <-
    x |>
    broom::tidy(x, ..., component = "fixed")

  if (exponentiate) {
    res$estimate <- res$estimate |> exp()
    res$conf.low <- res$conf.low |> exp()
    res$conf.high <- res$conf.high |> exp()
  }

  res
}

#' @export
#' @importFrom broom.helpers model_get_model_frame
model_get_model_frame.maihda_model <- function(model) {
  stats::model.frame(model$formula, data = model$original_data)
}

#' @export
#' @importFrom broom.helpers model_get_model_matrix
model_get_model_matrix.maihda_model <- function(model, ...) {
  stats::model.matrix(model$formula, data = model$original_data, ...)
}

#' @export
#' @importFrom broom.helpers model_get_terms
model_get_terms.maihda_model <- function(model) {
  stats::terms.formula(model$formula, data = model$original_data)
}

#' @export
#' @importFrom broom.helpers model_get_coefficients_type
model_get_coefficients_type.maihda_model <- function(model) {
  if (!is.null(model$family)) {
    if (model$family$family == "binomial" && model$family$link == "logit") {
      return("logistic")
    }
    if (model$family$family == "binomial" && model$family$link == "log") {
      return("relative_risk")
    }
    if (model$family$family == "binomial" && model$family$link == "cloglog") {
      return("prop_hazard")
    }
    if (model$family$family == "poisson" && model$family$link == "log") {
      return("poisson")
    }
    if (model$family$family == "quasibinomial" && model$family$link == "logit") {
      return("logistic")
    }
    if (model$family$family == "quasipoisson" && model$family$link == "log") {
      return("poisson")
    }
  }
  "generic"
}
