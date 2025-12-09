test_that("plot_continuous()() works", {
  expect_no_error(
    p <-
      iris |>
      plot_continuous(Petal.Length, by = Species)
  )
  expect_doppelganger("plot_continuous()", p)

  expect_no_error(
    p <-
      iris |>
      plot_continuous(
        dplyr::starts_with("Petal"),
        by = Species,
        free_scale = TRUE,
        fill = "lightblue",
        outlier.color = "red"
      )
  )
  expect_doppelganger("plot_continuous() free_scale", p)

  skip_on_cran()

  expect_no_error(
    mtcars |>
      plot_continuous(
        mpg,
        by = c(cyl, gear),
        flip = TRUE,
        mapping = ggplot2::aes(fill = by)
      )
  )
  expect_doppelganger("plot_continuous() 2 by variables", p)

  expect_no_error(
    p <-
      mtcars |>
      plot_continuous(
        mpg,
        by = c(disp, drat),
        flip = TRUE,
        minimal = TRUE
      )
  )
  expect_doppelganger("plot_continuous() minimal", p)

  expect_no_error(
    p <-
      iris |>
      srvyr::as_survey() |>
      plot_continuous(
        Petal.Length,
        by = c(Species, Petal.Width),
        flip = TRUE
      )
  )
  expect_doppelganger("plot_continuous() survey", p)

  skip_if_not_installed("gtsummary")

  d <- gtsummary::trial
  d$trt[1:10] <- NA
  expect_no_error(
    p <-
      d |>
      plot_continuous(marker, by = c(age, stage, trt), flip = TRUE)
  )
  expect_doppelganger("plot_continuous() num by and NAs", p)

  expect_no_error(
    p <-
      d |>
      plot_continuous(
        marker,
        by = c(age, stage, trt),
        flip = TRUE,
        drop_na_by = TRUE
      )
  )
  expect_doppelganger("plot_continuous() drop_na_by", p)
})
