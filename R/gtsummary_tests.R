#' Additional tests for `gtsummary`
#'
#' See [gtsummary::tests] for more details on how defining custom tests.
#' `fisher.simulate.p()` implements Fisher test with computation of p-values by
#' Monte Carlo simulation in larger than 2×2 tables (see
#' [stats::fisher.test()]).
#'
#' @keywords htest
#' @name gtsummary_test
#' @param data A data set.
#' @param variable Name of the variable to test.
#' @param by Name of the by variable.
#' @param ... Unused.
#' @export
#' @examplesIf rlang::is_installed("gtsummary")
#' library(gtsummary)
#' trial |>
#'   tbl_summary(include = grade, by = trt) |>
#'   add_p(test = all_categorical() ~ "fisher.simulate.p")
fisher.simulate.p <- function(data, variable, by, ...) {
  data <- data[c(variable, by)] |> tidyr::drop_na()
  fisher.test(
    data[[variable]],
    factor(data[[by]]),
    simulate.p.value = TRUE
  ) |>
    broom::tidy()
}
