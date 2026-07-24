#' Titanic data set in long format
#' @description
#' This `titanic` dataset is equivalent to
#' `datasets::Titanic |> dplyr::as_tibble() |> tidyr::uncount(n)`.
#' @seealso [datasets::Titanic]
#' @importFrom tidyr uncount
#' @importFrom dplyr as_tibble
#' @keywords datasets
#' @export
titanic <-
  datasets::Titanic |>
  dplyr::as_tibble() |>
  tidyr::uncount(n) |>
  labelled::set_variable_labels(
    Class = "Passenger's class",
    Sex = "Sex of passenger",
    Age = "Adult or Child",
    Survived = "Has survived?"
  )
