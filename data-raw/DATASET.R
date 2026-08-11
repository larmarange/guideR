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

save(
  bootstrapped_maihda,
  file = "inst/extdata/bootstrapped_maihda.RData",
  compress = "bzip2"
)
