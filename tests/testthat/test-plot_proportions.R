test_that("plot_proportions() works", {
  expect_no_error(
    p <-
      titanic |>
      plot_proportions(
        Survived == "Yes",
        overall_label = "All",
        labels_color = "white"
      )
  )
  expect_doppelganger("plot_proportions() overall only", p)

  expect_no_error(
    p <-
      titanic |>
      plot_proportions(
        Survived == "Yes",
        by = c(Class, Sex),
        fill = "lightblue"
      )
  )
  expect_doppelganger("plot_proportions() by", p)

  expect_no_error(
    p <-
      titanic |>
      plot_proportions(
        Survived == "Yes",
        by = c(Class, Sex),
        fill = "lightblue",
        flip = TRUE,
        pvalues_test = "chisq"
      )
  )
  expect_doppelganger("plot_proportions() by and flip", p)

  skip_on_cran()
  expect_no_error(
    p <-
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
  expect_doppelganger("plot_proportions() points", p)

  expect_no_error(
    p <-
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
  expect_doppelganger("plot_proportions() survey", p)

  d <- titanic
  d$Sex[1:50] <- NA
  expect_no_error(
    p <- d |> plot_proportions(Survived == "Yes", by = Sex)
  )
  expect_doppelganger("plot_proportions() missing by", p)

  expect_no_error(
    p <- d |> plot_proportions(Survived == "Yes", by = Sex, drop_na_by = TRUE)
  )
  expect_doppelganger("plot_propotions() missing by and drop_na_by", p)

  expect_no_error(
    p <-
      iris |>
      plot_proportions(Species == "versicolor", by = dplyr::contains("leng"))
  )
  expect_doppelganger("plot_proportions() tidyselect by", p)

  expect_no_error(
    p <-
      iris |>
      plot_proportions(
        dplyr::tibble(
          "Long sepal" = Sepal.Length > 6,
          "Short petal" = Petal.Width < 1
        ),
        by = Species
      )
  )
  expect_doppelganger("plot_proportions() multiple conditions", p)

  expect_no_error(
    p <-
      titanic |>
      plot_proportions(
        (Survived == "Yes") |>  stratified_by(Sex),
        by = Class
      )
  )
  expect_doppelganger("plot_proportions() stratified_by", p)

  expect_no_error(
    p <-
      titanic |>
      plot_proportions(
        dummy_proportions(Class),
        by = Sex,
        mapping = ggplot2::aes(fill = level)
      )
  )
  expect_doppelganger("plot_proportions() dummy_proportions", p)

  expect_no_error(
    p <-
      titanic |>
      plot_proportions(
        dplyr::tibble(
          Survived = Survived == "Yes",
          Male = Sex == "Male"
        ),
        by = c(Class),
        mapping = ggplot2::aes(fill = condition),
        free_scale = TRUE
      )
  )
  expect_doppelganger("plot_proportions fill=condition", p)
})
