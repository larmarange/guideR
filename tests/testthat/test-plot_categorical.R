test_that("plot_categorical() works", {
  set.seed(2026)

  expect_no_error(
    p <-
      titanic |>
      plot_categorical(
        Class,
        by = c(Age, Sex)
      )
  )
  expect_doppelganger("plot_categorical()", p)

  expect_no_error(
    p <-
      titanic |>
      plot_categorical(
        Class,
        by = c(Age, Sex),
        show_overall = FALSE,
        flip = TRUE
      )
  )
  expect_doppelganger("plot_categorical() flip", p)

  expect_no_error(
    p <-
      titanic |>
      plot_categorical(
        Age,
        by = Class,
        stratified_by = Sex
      )
  )
  expect_doppelganger("plot_categorical() stratified_by", p)

  skip_on_cran()
  skip_if_not_installed("gtsummary")

  expect_no_error(
    p <-
      gtsummary::trial |>
      plot_categorical(grade, by = c(age, stage, trt))
  )
  expect_doppelganger("plot_categorical() num by and NAs", p)

  skip_on_cran()
  expect_no_error(
    p <-
      gtsummary::trial |>
      plot_categorical(grade, by = c(age, stage, trt), drop_na_by = TRUE)
  )
  expect_doppelganger("plot_categorical() drop_na_by", p)

  expect_no_error(
    p <-
      gtsummary::trial |>
      plot_categorical(c(grade, stage), by = c(trt, response))
  )
  expect_doppelganger("plot_categorical() multiple outcome", p)

  expect_no_error(
    p <-
      gtsummary::trial |>
      plot_categorical(c(grade, stage), by = c(trt, response), minimal = TRUE)
  )
  expect_doppelganger("plot_categorical() multiple outcome & minimal", p)

  expect_no_error(
    p <-
      gtsummary::trial |>
      plot_categorical(c(grade, stage), by = c(trt, response), flip = TRUE)
  )
  expect_doppelganger("plot_categorical() multiple outcome flip", p)
})
