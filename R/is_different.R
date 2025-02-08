#' Comparison tests considering `NA` as values to be compared
#'
#' `is_different()` and `is_equal()` performs comparison tests, considering
#' `NA` values as legitimate values (see examples).
#'
#' `cum_different()` allows to identify groups of continuous rows that have
#' the same value. `num_cycle()` could be used to identify sub-groups that
#' respect a certain condition (see examples).
#'
#' @details
#' `is_equal(x, y)` is equivalent to
#' `(x == y & !is.na(x) & !is.na(y)) | (is.na(x) & is.na(y))`, and
#' `is_different(x, y)` is equivalent to
#' `(x != y & !is.na(x) & !is.na(y)) | xor(is.na(x), is.na(y))`.
#'
#' @param x,y Vectors to be compared.
#' @export
#' @examples
#' v <- c("a", "b", NA)
#' is_different(v, "a")
#' is_different(v, NA)
#' is_equal(v, "a")
#' is_equal(v, NA)

is_different <- function(x, y) {
  (x != y & !is.na(x) & !is.na(y)) | xor(is.na(x), is.na(y))
}

#' @rdname is_different
#' @export
is_equal <- function(x, y) {
  (x == y & !is.na(x) & !is.na(y)) | (is.na(x) & is.na(y))
}

#' @rdname is_different
#' @export
#' @examples
#' d <- dplyr::tibble(group = c("a", "a", "b", "b", "a", "b", "c", "a"))
#' d |>
#'   dplyr::mutate(
#'     subgroup = cumdifferent(group),
#'     sub_a = num_cycle(group == "a")
#'   )
cumdifferent <- function(x) {
  cumsum(is_different(x, dplyr::lag(x)))
}

#' @rdname is_different
#' @export
num_cycle <- function(x) {
  if (!is.logical(x))
    stop("'x' should be logical.")
  res <- cumsum(x & is_different(x, dplyr::lag(x)))
  res[!x] <- NA
  res
}
