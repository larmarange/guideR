test_that("plot_means() works", {
  expect_no_error(
    p <-
      iris |>
      plot_means(Petal.Length, by = Species)
  )
  expect_doppelganger("plot_means()", p)

  expect_no_error(
    p <-
      iris |>
      plot_means(
        dplyr::starts_with("Petal"),
        by = Species,
        geom = "bar",
        fill = "lightblue",
        show_overall_line = TRUE
      )
  )
  expect_doppelganger("plot_means() by overall_line", p)

  expect_no_error(
    p <-
      mtcars |>
      plot_means(
        mpg,
        by = c(cyl, gear),
        size = 3,
        colour = "plum",
        flip = TRUE
      )
  )
  expect_doppelganger("plot_means() by and flip", p)

  expect_no_error(
    p <-
      mtcars |>
      plot_means(
        mpg,
        by = c(disp, drat),
        fill = "plum",
        geom = "bar",
        flip = TRUE,
        minimal = TRUE
      )
  )
  expect_doppelganger("plot_means() continuous by", p)

  skip_if_not_installed("srvyr")

  expect_no_error(
    p <-
      iris |>
      srvyr::as_survey() |>
      plot_means(
        Petal.Length,
        by = c(Species, Petal.Width),
        label_y = -1,
        size = 3,
        mapping = ggplot2::aes(colour = by),
        flip = TRUE
      )
  )
  expect_doppelganger("plot_means() survey", p)
})
