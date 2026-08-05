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
    a$model_adjusted |> tbl_maihda()
  )
  expect_no_error(
    a |> tbl_maihda(global_p = TRUE)
  )
  expect_no_error(
    a |> tbl_strata_predictions()
  )

  expect_no_error(
    a |> plot_maihda_predictions()
  )
  expect_no_error(
    a |> plot_maihda_predictions(Race)
  )

  # a binomial example

  expect_no_error(
    titanic |>
      MAIHDA::make_strata(c("Age", "Class")) |>
      tbl_strata_info()
  )

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
    m |> tbl_maihda(exponentiate = TRUE, global_p = TRUE)
  )
  expect_no_error(
    m |> tbl_strata_predictions(n_strata = NULL)
  )
  expect_no_error(
    m |> tbl_strata_predictions(n_strata = 3)
  )

  expect_no_error(
    m |> plot_maihda_predictions()
  )
  expect_no_error(
    m |> plot_maihda_predictions(by = c(Sex, Age))
  )

  # Partially adjusted models

  expect_no_error(
    m |> tbl_partially_adjusted_maihda(exponentiate = TRUE)
  )

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

  # weighted MAIHDA
  skip_if_not_installed("WeMix")
  d <- titanic
  d$weight <- 1
  wm <- MAIHDA::maihda(
    Survived ~ Age + Sex + Class + (1 | Age:Sex:Class),
    data = d,
    family = binomial,
    sampling_weights = "weight",
    engine = "wemix"
  )
  expect_no_error(
    wm |> tbl_maihda(exponentiate = TRUE)
  )
})
