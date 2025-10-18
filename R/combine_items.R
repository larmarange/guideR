#' Combine items of a multiple choice question
#'
#' Considering a multiple choice question coded as several binary variables (one
#' per item), create a new variable (list column or character) combining all
#' positive answers. If defined, use variable labels (see examples).
#' @note
#' If `NA` is observed for at least one item, return `NA`.
#' @param data A data frame, data frame extension (e.g. a tibble),
#' or a survey design object.
#' @param items <[`tidy-select`][dplyr::dplyr_tidy_select ]> List of variables
#' identifying the different items of the multiple choice question.
#' @param into Names of new variables to create as character vector.
#' @param value Value indicating a positive answer. By default, will use the
#' maximum observed value and will display a message.
#' @param sep An optional character string to separate the results and return a
#' character. If `NULL`, return a list column (see examples).
#' @keywords manip
#' @export
#' @examples
#' d <-
#'   dplyr::tibble(
#'     q1a = sample(c("y", "n"), size = 200, replace = TRUE),
#'     q1b = sample(c("y", "n", "n", NA), size = 200, replace = TRUE),
#'     q1c = sample(c("y", "y", "n"), size = 200, replace = TRUE),
#'     q1d = sample("n", size = 200, replace = TRUE)
#'   )
#'
#' d |> combine_items(q1a:q1d, into = "combined")
#' d |> combine_items(q1a:q1d, into = "combined", sep = ", ", value = "y")
#' d |> combine_items(q1a:q1d, into = "combined", sep = " | ", value = "n")
combine_items <- function(
  data,
  items,
  into,
  value = NULL,
  sep = NULL
) {
  d <- data |> dplyr::select({{ items }})
  items <- colnames(d)

  # value (if not provied)
  if (is.null(value)) {
    value <- max(unlist(d), na.rm = TRUE)
    cli::cli_alert_warning("Automatically selected value: {.val {value}}")
    cli::cli_alert_info(
      "To remove this message, please specify {.arg value}."
    )
  }

  # variable labels
  if (inherits(d, "survey.design")) {
    vl <- labelled::var_label(
      data$variables,
      null_action = "fill",
      unlist = TRUE
    )
  } else {
    vl <- labelled::var_label(
      d,
      null_action = "fill",
      unlist = TRUE
    )
  }

  data[[into]] <-
    d |>
    dplyr::rowwise() |>
    dplyr::summarise(
      items = list(dplyr::c_across())
    ) |>
    dplyr::pull("items") |>
    purrr::map(
      \(x) {
        if (any(is.na(x))) return(NA)
        vl[items[x == value]]
      }
    )

  if (!is.null(sep)) {
    data <-
      data |>
      dplyr::rowwise() |>
      dplyr::mutate(
        dplyr::across(
          dplyr::all_of(into),
          \(x) {
            if (length(x) == 0) return("")
            if (all(is.na(x))) return(NA_character_)
            paste(x, collapse = sep)
          }
        )
      ) |>
      dplyr::ungroup()
  }

  data
}
