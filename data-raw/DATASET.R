m <- MAIHDA::maihda(
  Survived ~ Age + Sex + Class + (1 | Age:Sex:Class),
  data = titanic,
  family = binomial
)

bootstrapped_maihda <-
  m |>
  calculate_partially_adjusted_maihda(
    bootstrap_pcv = TRUE,
    bootstrap_vpc = TRUE
  )

usethis::use_data(
  bootstrapped_maihda,
  overwrite = TRUE,
  internal = TRUE
)
