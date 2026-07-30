test_that("tbl function for MAIHDA analysis does not produce an error", {
  skip_if_not_installed("gt")
  skip_if_not_installed("gtsummary")
  skip_if_not_installed("MAIHDA")
  skip_if_not_installed("broom.mixed")

  # gaussian model

  data("maihda_health_data", package = "MAIHDA")
  a <- MAIHDA::maihda(
    BMI ~ Age + Gender + Race + (1 | Gender:Race),
    data = maihda_health_data
  )

  expect_no_error(
    a |> tbl_strata_info(breaks = c(50, 100, 150))
  )
  expect_no_error(
    a |> tbl_maihda()
  )
  expect_no_error(
    a |> tbl_strata_predictions()
  )

  # a binomial example

  titanic$Survived <- as.integer(titanic$Survived == "Yes")
  m <- MAIHDA::maihda(
    Survived ~ Age + Sex + Class + (1 | Age:Sex:Class),
    data = titanic,
    family = binomial
  )

  expect_no_error(
    m |> tbl_strata_info()
  )
  expect_no_error(
    m |> tbl_maihda(exponentiate = TRUE)
  )
  expect_no_error(
    m |> tbl_strata_predictions(n_strata = NULL)
  )

  # Partially adjusted models

  m0 <- MAIHDA::fit_maihda(
    Survived ~ 1 + (1 | Age:Sex:Class),
    data = titanic,
    family = binomial
  )
  m1 <- MAIHDA::fit_maihda(
    Survived ~ Age + (1 | Age:Sex:Class),
    data = titanic,
    family = binomial
  )
  m2 <- MAIHDA::fit_maihda(
    Survived ~ Sex + (1 | Age:Sex:Class),
    data = titanic,
    family = binomial
  )
  m3 <- MAIHDA::fit_maihda(
    Survived ~ Class + (1 | Age:Sex:Class),
    data = titanic,
    family = binomial
  )

  # manually adding PCV
  m1$pcv <- MAIHDA::calculate_pcv(m0, m1)
  m2$pcv <- MAIHDA::calculate_pcv(m0, m2)
  m3$pcv <- MAIHDA::calculate_pcv(m0, m3)

  expect_no_error(
    list(Null = m0, Age = m1, Sex = m2, Class = m3) |>
      tbl_maihda(exponentiate = TRUE)
  )
})
