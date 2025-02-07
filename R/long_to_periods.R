#' Transform a data frame from long format to period format
#'
#' @param data a data.frame
#' @param id <[`tidy-select`][dplyr::dplyr_tidy_select ]>
#' column containing individual ids
#' @param start <[`tidy-select`][dplyr::dplyr_tidy_select ]>
#' time variable indicating the beginning of each row
#' @param stop <[`tidy-select`][dplyr::dplyr_tidy_select ]>
#' optional time variable indicating the end of each row. If not provided, it
#' will be derived from the dataset, considering that each row ends at the
#' beginning of the next one.
#' @param by <[`tidy-select`][dplyr::dplyr_tidy_select ]>
#' co-variables to consider (optionnal)
#' @examples
#' d <- dplyr::tibble(
#'   patient = c(1, 2, 3, 3, 4, 4, 4),
#'   begin = c(0, 0, 0, 1, 0, 36, 39),
#'   end = c(50, 6, 1, 16, 36, 39, 45),
#'   covar = c("no", "no", "no", "yes", "no", "yes", "yes")
#' )
#' d
#'
#' d |> long_to_periods(id = patient, start = begin, stop = end)
#' d |> long_to_periods(id = patient, start = begin, stop = end, by = covar)
#'
#' # If stop not provided, it is deduced.
#' # However, it considers that observation ends at the last start time.
#' d |> long_to_periods(id = patient, start = begin)
#' @export
long_to_periods <- function(data, id, start, stop = NULL, by = NULL) {
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
    dplyr::arrange({{ id }}, .data[[startv]])

  if (length(stopv) == 0) {
    data <- data |>
      dplyr::group_by(dplyr::pick({{ id }})) |>
      dplyr::mutate(.stop = dplyr::lead(.data[[startv]])) |>
      dplyr::filter(!is.na(.data$.stop)) # cleaning required
    stopv <- ".stop"
  }

  data <- data |>
    dplyr::group_by(dplyr::pick({{ id }}, {{ by }})) |>
    dplyr::mutate(.grp = dplyr::cur_group_id()) |>
    dplyr::group_by(dplyr::pick({{ id }})) |>
    dplyr::mutate(
      .prev_grp = dplyr::lag(.data$.grp),
      .prev_stop = dplyr::lag(.data[[stopv]])
    )

  periods <- data |>
    dplyr::filter(
      is.na(.data$.prev_grp) |
        .data$.grp != .data$.prev_grp |
        .data[[startv]] != .data$.prev_stop
    ) |>
    dplyr::mutate(.next_prev_stop = dplyr::lead(.data$.prev_stop))
  # trick: using the next value of .prev_stop allows to identify the new value
  # of stop in periods. if no next value, stop remains unchanged

  periods <-
    periods |>
    dplyr::left_join(
      data |>
        dplyr::summarise(.last_stop = max(.data[[stopv]], na.rm = TRUE)),
      by = dplyr::group_vars(periods),
    )
  periods[[stopv]] <- ifelse(
    !is.na(periods$.next_prev_stop),
    periods$.next_prev_stop,
    periods$.last_stop
  )
  class(periods[[stopv]]) <- class(periods$.next_prev_stop) # bug fix

  periods |>
    dplyr::select({{ id }}, .data[[startv]], .data[[stopv]], {{ by }})
}
