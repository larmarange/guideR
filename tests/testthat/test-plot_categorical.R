test_that("plot_categorical() works", {
  expect_no_error(
    p <-
      titanic |>
      plot_categorical(
        Class,
        by = c(Age, Sex)
      )
  )
  vdiffr::expect_doppelganger("plot_categorical()", p)

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
  vdiffr::expect_doppelganger("plot_categorical() flip", p)

  skip_on_cran()
  skip_if_not_installed("gtsummary")

  expect_no_error(
    p <-
      gtsummary::trial |>
      plot_categorical(grade, by = c(age, stage, trt))
  )
  vdiffr::expect_doppelganger("plot_categorical() num by and NAs", p)

  skip_on_cran()
  expect_no_error(
    p <-
      gtsummary::trial |>
      plot_categorical(grade, by = c(age, stage, trt), drop_na_by = TRUE)
  )
  vdiffr::expect_doppelganger("plot_categorical() drop_na_by", p)

  expect_no_error(
    p <-
      gtsummary::trial |>
      plot_categorical(c(grade, stage), by = c(trt, response))
  )
  vdiffr::expect_doppelganger("plot_categorical() multiple outcome", p)

  expect_no_error(
    p <-
      gtsummary::trial |>
      plot_categorical(c(grade, stage), by = c(trt, response), flip = TRUE)
  )
  vdiffr::expect_doppelganger("plot_categorical() multiple outcome flip", p)
})
