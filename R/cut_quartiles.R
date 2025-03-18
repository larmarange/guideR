#' Cut a continuous variable in quartiles
#'
#' Convenient function to quickly cut a numeric vector into quartiles, i.e. by
#' applying `cut(x, breaks = fivenum(x))`.
#' @inheritParams base::cut
#' @param ... further arguments passed to [base::cut()].
#' @keywords manip
#' @export
cut_quartiles <- function(x, include.lowest = TRUE, ...) {
  cut(x, breaks = stats::fivenum(x), include.lowest = include.lowest, ...)
}
