#' Test for Equal Means for survey design object
#'
#' This function allows to compare several means using [survey::svyglm()]. More
#' precisely, this is a wrapper for `survey::regTermTest(m, "group")` where
#' `m <- survey::svyglm(x ~ group, design)`.
#' @export
#' @param formula a formula of the form `lhs ~ rhs` where `lhs` gives the sample
#' values and `rhs` the corresponding groups
#' @param design a survey design object
#' @param ... additional parameters passed to [survey::regTermTest()]
#' @return an object of class `"regTermTest"`
#' @keywords htest
#' @seealso [stats::oneway.test()] for classic data frames
#' @examplesIf rlang::is_installed(c("survey", "srvyr"))
#' svyoneway(
#'   Petal.Length ~ Species,
#'   design = srvyr::as_survey(iris)
#' )
svyoneway <- function (formula, design, ...) {
  rlang::check_installed("survey")
  m <- survey::svyglm(formula, design, family = stats::gaussian())
  o <- survey::regTermTest(m, as.character(formula[[3]]), ...)
  o$mcall <- str2lang(paste0(
    "survey::svyglm(",
    format(formula),
    ", design = ",
    deparse(substitute(design)),
    ", family = gaussian)"
  ))
  o$p.value <- c(o$p)
  o
}
