#' Remove row-wise grouping
#'
#' Remove row-wise grouping created with [dplyr::rowwise()] while preserving
#' any other grouping declared with [dplyr::group_by()].
#' @param data A tibble.
#' @return A tibble.
#' @examples
#' titanic |> dplyr::rowwise()
#' titanic |> dplyr::rowwise() |> unrowwise()
#'
#' titanic |> dplyr::group_by(Sex, Class) |> dplyr::rowwise()
#' titanic |> dplyr::group_by(Sex, Class) |> dplyr::rowwise() |> unrowwise()
#' @export
#' @keywords manip
unrowwise <- function(data) {
  data |>
    dplyr::group_by(.add = TRUE)
}
