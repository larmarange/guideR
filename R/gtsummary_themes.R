#' Themes for gtsummary
#'
#' `theme_gtsummary_prop_n()` displays, by default, proportions before the
#' number of observations (between brackets). This function cannot be used
#' simultaneously with [gtsummary::theme_gtsummary_mean_sd()], but you can use
#' the `mean_sd = TRUE` option of `theme_gtsummary_prop_n()`.
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
#' trial |> tbl_summary(include = c(grade, age))
#' theme_gtsummary_prop_n(mean_sd = TRUE)
#' trial |> tbl_summary(include = c(grade, age))
theme_gtsummary_prop_n <- function(
  mean_sd = FALSE,
  prop_stat = "{p}% ({n})",
  prop_digits = 1,
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
