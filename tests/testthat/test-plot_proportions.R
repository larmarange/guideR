test_that("plot_proportions() does not produce an error", {
  expect_no_error(
    titanic |>
      plot_proportions(
        Survived == "Yes",
        overall_label = "All",
        labels_colour = "white"
      )
  )
  expect_no_error(
    titanic |>
      plot_proportions(
        Survived == "Yes",
        by = c(Class, Sex),
        fill = "lightblue"
      )
  )
  expect_no_error(
    titanic |>
      plot_proportions(
        Survived == "Yes",
        by = c(Class, Sex),
        fill = "lightblue",
        flip = TRUE
      )
  )
  skip_on_cran()
  expect_no_error(
    titanic |>
      plot_proportions(
        Survived == "Yes",
        by = c(Class, Sex),
        geom = "point",
        colour = "red",
        size = 3,
        show_labels = FALSE
      )
  )
  expect_no_error(
    titanic |>
      srvyr::as_survey() |>
      plot_proportions(
        Survived == "Yes",
        by = c(Class, Sex),
        fill = "darksalmon",
        colour = "black",
        show_overall_line = TRUE
      )
  )
})
