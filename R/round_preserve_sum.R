#' Round values while preserve their rounded sum in R
#'
#' Sometimes, the sum of rounded numbers (e.g., using [base::round()]) is not
#' the same as their rounded sum.
#'
#' This solution applies the following algorithm
#' - Round down to the specified number of decimal places
#' - Order numbers by their remainder values
#' - Increment the specified decimal place of values with ‘k’ largest
#'   remainders, where ‘k’ is the number of values that must be incremented to
#'   preserve their rounded sum
#' @param x vector to sum
#' @param digits number of decimals for rounding
#' @source <https://biostatmatt.com/archives/2902>
#' @examples
#' sum(c(0.333, 0.333, 0.334))
#' round(c(0.333, 0.333, 0.334), 2)
#' sum(round(c(0.333, 0.333, 0.334), 2))
#' round_preserve_sum(c(0.333, 0.333, 0.334), 2)
#' sum(round_preserve_sum(c(0.333, 0.333, 0.334), 2))
#' @export
round_preserve_sum <- function(x, digits = 0) {
  up <- 10^digits
  x <- x * up
  y <- floor(x)
  indices <- tail(order(x - y), round(sum(x)) - sum(y))
  y[indices] <- y[indices] + 1
  y / up
}
