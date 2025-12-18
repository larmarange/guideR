test_that("plot_multiple_answers_dodge() works", {
  skip_on_cran()
  skip_if_not_installed("ggupset")
  skip_if_not_installed("ggstats")

  set.seed(2025)
  d <-
    dplyr::tibble(
      q1a = sample(c("y", "n"), size = 200, replace = TRUE),
      q1b = sample(c("y", "n", "n", NA), size = 200, replace = TRUE),
      q1c = sample(c("y", "y", "n"), size = 200, replace = TRUE),
      q1d = sample("n", size = 200, replace = TRUE),
      group = sample(c("group A", "group B"), size = 200, replace = TRUE)
    )

  expect_no_error(
    p <-
      d |>
      plot_multiple_answers_dodge(q1a:q1d, by = group, value = "y")
  )
  expect_doppelganger("plot_multiple_answers_dodge()", p)

  expect_no_error(
    p <-
      d |>
      plot_multiple_answers_dodge(q1a:q1d, by = group, flip = TRUE, value = "y")
  )
  expect_doppelganger("plot_multiple_answers_dodge() flip", p)

  expect_no_error(
    p <-
      d |>
      plot_multiple_answers_dodge(
        q1a:q1d, by = group, combine_answers = TRUE, value = "y"
      )
  )
  # warning from ggplot2 due to ggupset
  expect_no_error(suppressWarnings(print(p)))
  expect_doppelganger("plot_multiple_answers_dodge() combine", p)

  expect_no_error(
    p <-
      d |>
      srvyr::as_survey() |>
      plot_multiple_answers_dodge(
        q1a:q1d, by = group, combine_answers = TRUE, value = "y"
      )
  )
  # warning from ggplot2 due to ggupset
  expect_no_error(suppressWarnings(print(p)))
  expect_doppelganger("plot_multiple_answers_dodge() combine survey", p)

  expect_no_error(
    p <-
      d |>
      plot_multiple_answers_dodge(
        q1a:q1d, by = group, geom = "point", value = "y"
      )
  )
  expect_doppelganger("plot_multiple_answers_dodge() point", p)
})

test_that("plot_multiple_answers() works", {
  set.seed(2025)
  d <-
    dplyr::tibble(
      q1a = sample(c("y", "n"), size = 200, replace = TRUE),
      q1b = sample(c("y", "n", "n", NA), size = 200, replace = TRUE),
      q1c = sample(c("y", "y", "n"), size = 200, replace = TRUE),
      q1d = sample("n", size = 200, replace = TRUE)
    )

  expect_message(
    p <- d |> plot_multiple_answers(q1a:q1c)
  )
  expect_doppelganger("plot_multiple_answers()", p)

  expect_no_error(
    p <-
      d |>
      labelled::set_variable_labels(
        q1a = "apple",
        q1b = "banana",
        q1c = "chocolate",
        q1d = "Dijon mustard"
      ) |>
      plot_multiple_answers(
        value = "y",
        drop_na = TRUE,
        sort = "desc",
        fill = "lightblue",
        flip = TRUE
      )
  )
  expect_doppelganger("plot_multiple_answers() flip and labels", p)

  skip_on_cran()
  skip_if_not_installed("ggupset")

  expect_no_error(
    p <-
      d |>
      plot_multiple_answers(
        combine_answers = TRUE,
        value = "y",
        fill = "#DDCC77",
        drop_na = TRUE
      )
  )
  expect_doppelganger("plot_multiple_answers() combine", p)

  expect_no_error(
    p <-
      d |>
      srvyr::as_survey() |>
      plot_multiple_answers(
        combine_answers = TRUE,
        value = "y",
        fill = "#DDCC77",
        drop_na = TRUE
      )
  )
  expect_doppelganger("plot_multiple_answers() combine & survey", p)

  expect_no_error(
    p <-
      d |>
      plot_multiple_answers(
        combine_answers = TRUE,
        value = "y",
        flip = TRUE,
        mapping = ggplot2::aes(fill = prop),
        show.legend = FALSE
      ) +
      ggplot2::scale_fill_distiller(palette = "Spectral")
  )
  expect_doppelganger("plot_multiple_answers() combine & flip", p)

  d$group <- sample(c("group A", "groupe B"), size = 200, replace = TRUE)
  expect_no_error(
    p <-
      d |>
      plot_multiple_answers(
        answers = q1a:q1d,
        by = group,
        combine_answers = TRUE,
        sort = "degrees",
        value = "y",
        fill = "grey80"
      )
  )
  expect_doppelganger("plot_multiple_answers() combine sort", p)
})
