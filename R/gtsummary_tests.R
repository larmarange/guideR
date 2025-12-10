#' Additional tests for `gtsummary`
#'
#' See [gtsummary::tests] for more details on how defining custom tests.
#' `fisher.simulate.p()` implements Fisher test with computation of p-values by
#' Monte Carlo simulation in larger than 2×2 tables (see
#' [stats::fisher.test()]).
#' `svyttest_oneway()` is designed to compare means between sub-groups for
#' survey objects. It is based on [survey::svyttest()] for comparing 2 means,
#' and on [svyoneway()] for comparing 3 means or more.
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
#' @examplesIf rlang::is_installed(c("gtsummary", "srvyr", "survey"))
#' \donttest{
#' iris |>
#'   srvyr::as_survey() |>
#'   tbl_svysummary(
#'     include = Petal.Length,
#'     by = Species
#'    ) |>
#'    add_p(test = all_continuous() ~ svyttest_oneway)
#' }
fisher.simulate.p <- function(data, variable, by, ...) {
  rlang::check_installed("broom")
  data <- data[c(variable, by)] |> tidyr::drop_na()
  stats::fisher.test(
    data[[variable]],
    factor(data[[by]]),
    simulate.p.value = TRUE
  ) |>
    broom::tidy()
}

#' @rdname gtsummary_test
#' @export
svyttest_oneway <- function(data, variable, by, ...) {
  rlang::check_installed("broom")
  rlang::check_installed("survey")
  data <- data |>
    dplyr::select(dplyr::all_of(c(variable, by))) |>
    tidyr::drop_na()

  svyttest_oneway_formula(
    stats::as.formula(paste(variable, " ~ ", by)),
    design = data
  ) |>
    broom::tidy()
}

# works only if one variable on rhs
svyttest_oneway_formula <- function(formula, design) {
  rlang::check_installed("broom")
  rlang::check_installed("survey")

  if (length(unique(design$variables[[as.character(formula[[3]])]])) == 2) {
    survey::svyttest(formula, design)
  } else {
    svyoneway(formula, design)
  }
}
