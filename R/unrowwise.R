#' Remove row-wise grouping
#'
#' Remove row-wise grouping created with [dplyr::rowwise()] while preserving
#' any other grouping declared with [dplyr::group_by()].
#' @param data A data frame, data frame extension (e.g. a tibble), or a
#' lazy data frame.
#' @examples
#' titanic |> dplyr::rowwise()
#' titanic |> dplyr::rowwise() |> unrowwise()
#'
#' titanic |> dplyr::group_by(Sex, Class) |> dplyr::rowwise()
#' titanic |> dplyr::group_by(Sex, Class) |> dplyr::rowwise() |> unrowwise()
#' @export
unrowwise <- function(data) {
  data |>
    dplyr::group_by(.add = TRUE)
}
