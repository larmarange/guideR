#' Transform a data frame from long format to a sequence obect
#'
#' @param data A data frame or a data frame extension (e.g. a tibble).
#' @param id <[`tidy-select`][dplyr::dplyr_tidy_select ]>\cr
#' Column containing individual ids
#' @param time <[`tidy-select`][dplyr::dplyr_tidy_select ]>\cr
#' Time variable
#' @param outcome <[`tidy-select`][dplyr::dplyr_tidy_select ]>\cr
#' Variable defining the status
#' @param alphabet Optional vector containing the alphabet (the list of all
#' possible states).
#' If `alphabet = "auto"` will be automatically determined from `outcome`.
#' If `outcome` is a labelled vector (`haven_labelled` class),
#' it will be derived from the value labels (using the values).
#' If `outcome` is a factor, the factor will be transformed to a numeric vector
#' with [as.integer()] and the corresponding numeric values will be used as the
#' alphabet. In all other cases, will be equal to `NULL`
#' (see [TraMineR::seqdef()]).
#' @param labels An optional vector containing state labels used for graphics.
#' If `labels = "auto"` will be automatically determined from `outcome`.
#' If `outcome` is a labelled vector (`haven_labelled` class),
#' it will be derived from the value labels (using the labels).
#' If `outcome` is a factor, the levels of the factor will be used.
#' In all other cases, will be equal to `NULL`
#' @param cnames An optional vector containing names of the different time
#' points. If `cnames = "auto"`, it will use the observed values from `time`.
#' @param cpal An optional colour palette for representing the states in the
#' graphics. If `cpal = "auto"`, a palette will be generated with [safe_pal()].
#' @param missing.color Alternative colour for representing missing values
#' inside the sequences.
#' @param ... Additional arguments passed to [TraMineR::seqdef()]
#' @return An object of class `stslist`.
#' @keywords manip
#' @seealso [TraMineR::seqdef()]
#' @examplesIf rlang::is_installed("TraMineR")
#' \donttest{
#' library(TraMineR)
#'
#' # generating a data frame in long format
#' data("biofam")
#' d <-
#'   biofam |>
#'   tibble::rownames_to_column("id_ind") |>
#'   dplyr::select(id_ind, dplyr::starts_with("a")) |>
#'   tidyr::pivot_longer(
#'     cols = dplyr::starts_with("a"),
#'     names_to = "age",
#'     names_prefix = "a",
#'     values_to = "life_state"
#'   ) |>
#'   dplyr::mutate(
#'     age = as.integer(age),
#'     life_state2 = dplyr::case_when(
#'       life_state == 0 ~ "P",
#'       life_state == 1 ~ "L",
#'       life_state == 2 ~ "M",
#'       life_state == 3 ~ "LM",
#'       life_state == 4 ~ "C",
#'       life_state == 5 ~ "LC",
#'       life_state == 6 ~ "LMC",
#'       life_state == 7 ~ "D"
#'     )
#'   ) |>
#'   labelled::set_value_labels(
#'     life_state = c(
#'       "Parent" = 0,
#'       "Left" = 1,
#'       "Married" = 2,
#'       "Left & Married" = 3,
#'       "Child" = 4,
#'       "Left & Child" = 5,
#'       "Left & Married & Child" = 6,
#'       "Divorced" = 7
#'     ),
#'     life_state2 = c(
#'       "Parent" = "P",
#'       "Left" = "L",
#'       "Married" = "M",
#'       "Left & Married" = "LM",
#'       "Child" = "C",
#'       "Left & Child" = "LC",
#'       "Left & Married & Child" = "LMC",
#'       "Divorced" = "D"
#'     )
#'   ) |>
#'   dplyr::mutate(
#'     life_state3 = labelled::to_factor(life_state),
#'     life_state4 = unclass(life_state2)
#'   )
#'
#' d |> long_to_seq(id = id_ind, time = age, outcome = life_state) |> head(10)
#' d |> long_to_seq(id = id_ind, time = age, outcome = life_state2) |> head(10)
#' d |> long_to_seq(id = id_ind, time = age, outcome = life_state3) |> head(10)
#' d |> long_to_seq(id = id_ind, time = age, outcome = life_state4) |> head(10)
#' }
#' @export
long_to_seq <- function(data,
                        id,
                        time,
                        outcome,
                        alphabet = "auto",
                        labels = "auto",
                        cnames = "auto",
                        cpal = "auto",
                        missing.color = "#BBBBBB",
                        ...) {
  rlang::check_installed("TraMineR")

  # variable identification

  idv <-
    tidyselect::eval_select(
      rlang::enquo(id),
      data = data,
      allow_rename = FALSE
    ) |>
    names()

  timev <-
    tidyselect::eval_select(
      rlang::enquo(time),
      data = data,
      allow_rename = FALSE
    ) |>
    names()

  outcomev <-
    tidyselect::eval_select(
      rlang::enquo(outcome),
      data = data,
      allow_rename = FALSE
    ) |>
    names()

  # factors are converted to labelled integer
  if (is.factor(d[[outcomev]]))
    data[[outcomev]] <- labelled::to_labelled(data[[outcomev]])

  # sorting data
  data <-
    data |>
    dplyr::arrange({{ id }}, {{ time }})

  # setting parameters
  if (alphabet == "auto") {
    if (labelled::is.labelled(data[[outcomev]])) {
      alphabet <- data[[outcomev]] |> labelled::get_value_labels() |> unname()
      # checking alphabet covers all observations
      check <- !data[[outcomev]] %in% alphabet
      if (any(check, na.rm = TRUE)) {
        missing <- data[[outcomev]][check] |> unique() |> paste(collapse = ", ")
        cli::cli_abort("State(s) {missing} found in data do not have a label.")
      }
    } else {
      alphabet <- data[[outcomev]] |> unique() |> sort()
    }
  }

  if (labels == "auto") {
    if (labelled::is.labelled(data[[outcomev]])) {
      labels <- data[[outcomev]] |> labelled::get_value_labels() |> names()
    } else {
      labels <- NULL
    }
  }

  if (cnames == "auto") {
    cnames <- data[[timev]] |> unique() |> sort()
  }

  if (cpal == "auto") {
    cpal <- safe_pal()(length(alphabet))
  }

  # transforming into a sequence object
  data |>
    TraMineR::seqformat(
      from = "SPELL",
      to = "STS",
      id = idv,
      begin = timev,
      end = timev,
      status = outcomev,
      process = FALSE
    ) |>
    TraMineR::seqdef(
      informat = "STS",
      alphabet = alphabet,
      labels = labels,
      cnames = cnames,
      cpal = cpal,
      missing.color = missing.color,
      ...
    )
}
