#' Cut a continuous variable in quartiles
#'
#' Convenient function to quickly cut a numeric vector into quartiles, i.e. by
#' applying `cut(x, breaks = fivenum(x))`. Variable label is preserved by
#' `cut_quartiles()`.
#' @inheritParams base::cut
#' @param ... further arguments passed to [base::cut()].
#' @keywords manip
#' @export
#' @examples
#' mtcars$mpg |> cut_quartiles() |> summary()
cut_quartiles <- function(x, include.lowest = TRUE, ...) {
  res <- cut(
    x,
    breaks = stats::fivenum(x),
    include.lowest = include.lowest,
    ...
  )
  labelled::var_label(res) <- labelled::var_label(x)
  res
}
