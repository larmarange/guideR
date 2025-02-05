#' Titanic data set in long format
#' @description
#' This `titanic` dataset is equivalent to
#' `datasets::Titanic |> dplyr::as_tibble() |> tidyr::uncount(n)`.
#' @seealso [datasets::Titanic]
#' @importFrom tidyr uncount
#' @importFrom dplyr as_tibble
#' @export
titanic <- datasets::Titanic |> dplyr::as_tibble() |> tidyr::uncount(n)
