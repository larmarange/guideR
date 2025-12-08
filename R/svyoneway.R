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
#' @return an object of class `"htest"`
#' @keywords htest
#' @seealso [stats::oneway.test()] for classic data frames
#' @examplesIf rlang::is_installed(c("survey", "srvyr"))
#' svyoneway(
#'   Petal.Length ~ Species,
#'   design = srvyr::as_survey(iris)
#' )
svyoneway <- function(formula, design, ...) {
  rlang::check_installed("survey")
  m <- survey::svyglm(formula, design, family = stats::gaussian())
  o <- survey::regTermTest(m, as.character(formula[[3]]), ...)
  h <- list(
    statistic = rlang::set_names(c(o$Ftest), "F"),
    parameter = rlang::set_names(c(o$df, o$ddf), c("df", "ddf")),
    p.value = c(o$p),
    method = "Design-based one-way analysis of means",
    data.name = paste(
      as.character(formula[[2]]),
      "and",
      as.character(formula[[3]])
    )
  )
  class(h) <- "htest"
  h
}
