test_that("plot_proportions() does not produce an error", {
  expect_no_error(
    p <-
      titanic |>
      plot_proportions(
        Survived == "Yes",
        overall_label = "All",
        labels_color = "white"
      )
  )
  vdiffr::expect_doppelganger("plot_proportions() overall only", p)

  expect_no_error(
    p <-
      titanic |>
      plot_proportions(
        Survived == "Yes",
        by = c(Class, Sex),
        fill = "lightblue"
      )
  )
  vdiffr::expect_doppelganger("plot_proportions() by", p)

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
  vdiffr::expect_doppelganger("plot_proportions() by and flip", p)

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
  vdiffr::expect_doppelganger("plot_proportions() points", p)

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
  vdiffr::expect_doppelganger("plot_proportions() survey", p)

  d <- titanic
  d$Sex[1:50] <- NA
  expect_no_error(
    p <- d |> plot_proportions(Survived == "Yes", by = Sex)
  )
  vdiffr::expect_doppelganger("plot_proportions() missing by", p)

  expect_no_error(
    p <- d |> plot_proportions(Survived == "Yes", by = Sex, drop_na_by = TRUE)
  )
  vdiffr::expect_doppelganger("plot_propotions() missing by and drop_na_by", p)

  expect_no_error(
    p <-
      iris |>
      plot_proportions(Species == "versicolor", by = dplyr::contains("leng"))
  )
  vdiffr::expect_doppelganger("plot_proportions() tidyselect by", p)

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
  vdiffr::expect_doppelganger("plot_proportions() multiple conditions", p)

  expect_no_error(
    p <-
    titanic |>
      plot_proportions(
        (Survived == "Yes") |>  stratified_by(Sex),
        by = Class
      )
  )
  vdiffr::expect_doppelganger("plot_proportions() stratified_by", p)

  expect_no_error(
    p <-
      titanic |>
      plot_proportions(
        dummy_proportions(Class),
        by = Sex,
        mapping = ggplot2::aes(fill = level)
      )
  )
  vdiffr::expect_doppelganger("plot_proportions() dummy_proportions", p)

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
  vdiffr::expect_doppelganger("plot_proportions fill=condition", p)
})
