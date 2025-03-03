#' Transform a data frame from period format to long format
#'
#' @param data A data frame, or a data frame extension (e.g. a tibble).
#' @param start <[`tidy-select`][dplyr::dplyr_tidy_select ]>
#' Time variable indicating the beginning of each row
#' @param stop <[`tidy-select`][dplyr::dplyr_tidy_select ]>
#' Optional time variable indicating the end of each row. If not provided, it
#' will be derived from the dataset, considering that each row ends at the
#' beginning of the next one.
#' @param time_step (numeric) Desired value for the time variable.
#' @param time_name (character) Name of the time variable.
#' @param keep (logical) Should start and stop variable be kept in the results?
#' @return A tibble.
#' @seealso [long_to_periods()]
#' @keywords manip
#' @examples
#' d <- dplyr::tibble(
#'   patient = c(1, 2, 3, 3),
#'   begin = c(0, 2, 0, 3),
#'   end = c(6, 4, 2, 8),
#'   covar = c("no", "yes", "no", "yes")
#' )
#' d
#'
#' d |> periods_to_long(start = begin, stop = end)
#' d |> periods_to_long(start = begin, stop = end, time_step = 5)
#' @export
periods_to_long <- function(data,
                            start,
                            stop,
                            time_step = 1,
                            time_name = "time",
                            keep = FALSE) {
  startv <-
    tidyselect::eval_select(
      rlang::enquo(start),
      data = data,
      allow_rename = FALSE
    ) |>
    names()
  stopv <-
    tidyselect::eval_select(
      rlang::enquo(stop),
      data = data,
      allow_rename = FALSE
    ) |>
    names()
  if (length(startv) != 1)
    cli::cli_abort("{.arg start} should select only one column.")
  if (length(stopv) > 1)
    cli::cli_abort("{.arg stop} should select only one column.")

  data <- data |>
    dplyr::rowwise() |>
    dplyr::mutate(
      .time = list(seq(
        from = .data[[startv]],
        to = .data[[stopv]],
        by = time_step
      )
      )) |>
    tidyr::unnest(cols = dplyr::all_of(".time")) |>
    dplyr::relocate(dplyr::all_of(".time"), .before = dplyr::all_of(startv))
  v <- ".time"
  names(v) <- time_name
  data <- data |>
    dplyr::rename(dplyr::all_of(v))
  if (!keep)
    data <- data |>
    dplyr::select(-dplyr::all_of(c(startv, stopv)))
  data
}
