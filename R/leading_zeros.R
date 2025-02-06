#' Add leading zeros
#'
#' @param x a numeric vector
#' @param left_digits number of digits before decimal point, automatically
#' computed if not provided
#' @param digits number of digits after decimal point
#' @param prefix,suffix Symbols to display before and after value
#' @param ... additional paramaters passed to [base::formatC()],
#' as \code{big.mark} or \code{decimal.mark}
#' @export
#' @seealso [base::formatC()], [base::sprintf()]
#' @examples
#' v <- c(2, 103.24, 1042.147, 12.4566, NA)
#' leading_zeros(v)
#' leading_zeros(v, digits = 1)
#' leading_zeros(v, left_digits = 6, big.mark = " ")
#' leading_zeros(c(0, 6, 12, 18), prefix = "M")
leading_zeros <- function(x,
                          left_digits = NULL,
                          digits = 0,
                          prefix = "",
                          suffix = "",
                          ...) {
  if (is.null(left_digits)) {
    left_digits <- trunc(max(log10(x), na.rm = TRUE)) + 1
  }
  if (digits > 0) {
    width <- left_digits + digits + 1
  } else {
    width <- left_digits
  }
  paste0(
    prefix,
    formatC(
      x,
      width = width,
      digits = digits,
      flag = "0",
      format = "f",
      preserve.width = "common",
      ...
    ),
    suffix
  )
}
