#' Themes for `gtsummary`
#'
#' `theme_gtsummary_prop_n()` displays, by default, proportions before the
#' number of observations (between brackets). This function cannot be used
#' simultaneously with [gtsummary::theme_gtsummary_mean_sd()], but you can use
#' the `mean_sd = TRUE` option of `theme_gtsummary_prop_n()`.
#' `theme_gtsummary_fisher_simulate_p()` modify the default test used for
#' categorical variables by Fisher test, with computation of p-values by
#' Monte Carlo simulation in larger than 2×2 tables.
#' `theme_gtsummary_unweighted_n()` modifies default values of tables returned
#' by [gtsummary::tbl_svysummary()] and displays the unweighted number of
#' observations instead of the weighted n.
#' `theme_gtsummary_bold_labels()` applies automatically
#' [gtsummary::bold_labels()] to all tables generated with `gtsummary`.
#'
#' @param prop_stat (`character`)\cr
#'   Statistics to display for categorical variables (see
#'   [gtsummary::tbl_summary()]).
#' @param prop_digits (non-negative `integer`)\cr
#'   Define the number of decimals to display for proportions.
#' @param mean_sd (scalar `logical`)\cr
#'   Also, set default summary statistics to mean and standard deviation in
#'   [gtsummary::tbl_summary()]. Default is `FALSE`.
#' @param cont_digits (non-negative `integer`)\cr
#'   Define the number of decimals to display for continuous variables.
#' @param set_theme (scalar `logical`)\cr
#'   Logical indicating whether to set the theme. Default is `TRUE`.
#'   When `FALSE` the named list of theme elements is returned invisibly
#' @keywords utilities
#' @name gtsummary_themes
#' @export
#' @examplesIf rlang::is_installed("gtsummary")
#' library(gtsummary)
#' trial |>
#'   tbl_summary(include = c(grade, age), by = trt) |>
#'   add_p()
#'
#' theme_gtsummary_prop_n(mean_sd = TRUE)
#' theme_gtsummary_fisher_simulate_p()
#' theme_gtsummary_bold_labels()
#' trial |>
#'   tbl_summary(include = c(grade, age), by = trt) |>
#'   add_p()
theme_gtsummary_prop_n <- function(
  prop_stat = "{p}% ({n})",
  prop_digits = 1,
  mean_sd = FALSE,
  cont_digits = 1,
  set_theme = TRUE
) {
  rlang::check_installed("gtsummary")

  if (isTRUE(mean_sd)) {
    lst_theme <- list(
      "tbl_summary-arg:statistic" = list(
        all_continuous() ~ "{mean} ({sd})",
        all_categorical() ~ prop_stat
      ),
      "add_p.tbl_summary-attr:test.continuous_by2" = "t.test",
      "add_p.tbl_summary-attr:test.continuous" = "oneway.test"
    )
  } else {
    lst_theme <- list(
      "tbl_summary-arg:statistic" = list(
        all_continuous() ~ "{median} ({p25}, {p75})",
        all_categorical() ~ prop_stat
      )
    )
  }
  lst_theme[["tbl_summary-arg:digits"]] <- list(
    all_categorical() ~ c(p = prop_digits, n = 0),
    all_continuous() ~ cont_digits
  )

  if (isTRUE(set_theme)) gtsummary::set_gtsummary_theme(lst_theme)
  return(invisible(lst_theme))
}

#' @rdname gtsummary_themes
#' @export
theme_gtsummary_fisher_simulate_p <- function(
  set_theme = TRUE
) {
  rlang::check_installed("gtsummary")
  lst_theme <- list(
    "add_p.tbl_summary-attr:test.categorical" =
      "guideR::fisher.simulate.p",
    "add_p.tbl_summary-attr:test.categorical.low_count" =
      "guideR::fisher.simulate.p"
  )
  if (isTRUE(set_theme)) gtsummary::set_gtsummary_theme(lst_theme)
  return(invisible(lst_theme))
}

#' @rdname gtsummary_themes
#' @param n_unweighted_prefix,n_unweighted_suffix (`character`)\cr
#'   Prefix and suffix displayed before and after the unweighted number of
#'   observations.
#' @param overall_string (`character`)\cr
#'   Optional string to name the *overall* column.
#' @export
#' @examplesIf rlang::is_installed(c("gtsummary", "srvyr"))
#' data("api", package = "survey")
#' apistrat$both[1:5] <- NA
#' apistrat |>
#'   srvyr::as_survey(strata = stype, weights = pw) |>
#'   tbl_svysummary(include = c(stype, both), by = awards) |>
#'   add_overall()
#'
#' theme_gtsummary_unweighted_n()
#' apistrat |>
#'   srvyr::as_survey(strata = stype, weights = pw) |>
#'   tbl_svysummary(include = c(stype, both), by = awards) |>
#'   add_overall()
#'
#' gtsummary::reset_gtsummary_theme()
theme_gtsummary_unweighted_n <- function(
  n_unweighted_prefix = "",
  n_unweighted_suffix = " obs.",
  prop_digits = 1,
  mean_sd = FALSE,
  cont_digits = 1,
  overall_string = NULL,
  set_theme = TRUE
) {
  rlang::check_installed("gtsummary")

  un <- paste0(n_unweighted_prefix, "{n_unweighted}", n_unweighted_suffix)
  uN <- paste0(n_unweighted_prefix, "{N_unweighted}", n_unweighted_suffix)

  if (is.null(overall_string))
    overall_string <- "{gtsummary:::translate_string('Overall')}"

  cat_stat <- paste0("{p}% (", un, ")") # nolint

  if (isTRUE(mean_sd)) {
    lst_theme <- list(
      "tbl_svysummary-arg:statistic" = list(
        all_continuous() ~ "{mean} ({sd})",
        all_categorical() ~ cat_stat
      ),
      "add_p.tbl_svysummary-arg:test" = list(
        all_continuous() ~ "svy.t.test",
        all_categorical() ~ "svy.chisq.test"
      )
    )
  } else {
    lst_theme <- list(
      "tbl_svysummary-arg:statistic" = list(
        all_continuous() ~ "{median} ({p25}, {p75})",
        all_categorical() ~ cat_stat
      )
    )
  }
  lst_theme[["tbl_svysummary-arg:digits"]] <- list(
    all_categorical() ~ c(p = prop_digits, n = 0, n_unweighted = 0),
    all_continuous() ~ cont_digits
  )
  lst_theme[["tbl_svysummary-str:header-noby"]] <- uN
  lst_theme[["tbl_svysummary-str:header-withby"]] <-
    paste0("**{level}**\n(", uN, ")")
  lst_theme[["tbl_svysummary-arg:missing_stat"]] <-
    paste0(n_unweighted_prefix, "{N_miss_unweighted}", n_unweighted_suffix)

  lst_theme[["add_overall.tbl_summary-arg:col_label"]] <-
    paste0(
      "**", overall_string, "**\n(",
      n_unweighted_prefix,
      "{ifelse(exists('N_unweighted'), N_unweighted, N)}",
      n_unweighted_suffix,
      ")"
    )

  if (isTRUE(set_theme)) gtsummary::set_gtsummary_theme(lst_theme)
  return(invisible(lst_theme))
}

#' @rdname gtsummary_themes
#' @export
theme_gtsummary_bold_labels <- function(
  set_theme = TRUE
) {
  rlang::check_installed("gtsummary")
  lst_theme <- list(
    "tbl_summary-fn:addnl-fn-to-run" = gtsummary::bold_labels,
    "tbl_svysummary-fn:addnl-fn-to-run" = gtsummary::bold_labels,
    "tbl_regression-fn:addnl-fn-to-run" = gtsummary::bold_labels,
    "tbl_hierarchical-fn:addnl-fn-to-run" = gtsummary::bold_labels,
    "tbl_hierarchical_count-fn:addnl-fn-to-run" = gtsummary::bold_labels
  )
  if (isTRUE(set_theme)) gtsummary::set_gtsummary_theme(lst_theme)
  return(invisible(lst_theme))
}
