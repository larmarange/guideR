test_that("plot_proportions() does not produce an error", {
  expect_no_error(
    titanic |>
      plot_proportions(
        Survived == "Yes",
        overall_label = "All",
        labels_color = "white"
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
        flip = TRUE,
        pvalues_test = "chisq"
      )
  )
  skip_on_cran()
  expect_no_error(
    titanic |>
      plot_proportions(
        Survived == "Yes",
        by = c(Class, Sex),
        geom = "point",
        color = "red",
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
        color = "black",
        show_overall_line = TRUE
      )
  )
  d <- titanic
  d$Sex[1:50] <- NA
  expect_no_error(
    d |> plot_proportions(Survived == "Yes", by = Sex)
  )
  expect_no_error(
    d |> plot_proportions(Survived == "Yes", by = Sex, drop_na_by = TRUE)
  )

  expect_no_error(
    iris |>
      plot_proportions(Species == "versicolor", by = dplyr::contains("leng"))
  )

  expect_no_error(
    iris |>
      plot_proportions(
        dplyr::tibble(
          "Long sepal" = Sepal.Length > 6,
          "Short petal" = Petal.Width < 1
        ),
        by = Species
      )
  )

  expect_no_error(
    titanic |>
      plot_proportions(
        (Survived == "Yes") |>  stratified_by(Sex),
        by = Class
      )
  )

  expect_no_error(
    titanic |>
      plot_proportions(
        dummy_proportions(Class),
        by = Sex,
        mapping = ggplot2::aes(fill = level)
      )
  )
})
