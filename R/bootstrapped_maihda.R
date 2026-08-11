#' An example of bootstrapped MAIHDA analysis
#'
#' @description A list as returned by [calculate_partially_adjusted_maihda()]
#' @keywords datasets
#' @examples
#' # m <- MAIHDA::maihda(
#' #   Survived ~ Age + Sex + Class + (1 | Age:Sex:Class),
#' #   data = titanic,
#' #   family = binomial
#' # )
#' #
#' # bootstrapped_maihda <-
#' #   m |>
#' #   calculate_partially_adjusted_maihda(
#' #     bootstrap_pcv = TRUE,
#' #     bootstrap_vpc = TRUE
#' #   )
#'
#' \donttest{
#' bootstrapped_maihda |>
#'   tbl_maihda(exponentiate = TRUE)
#' }
"bootstrapped_maihda"
