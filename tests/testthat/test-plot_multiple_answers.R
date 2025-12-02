test_that("plot_multiple_answers_dodge() does not produce an error", {
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
  vdiffr::expect_doppelganger("plot_multiple_answers_dodge()", p)

  expect_no_error(
    p <-
      d |>
      plot_multiple_answers_dodge(q1a:q1d, by = group, flip = TRUE, value = "y")
  )
  vdiffr::expect_doppelganger("plot_multiple_answers_dodge() flip", p)

  expect_no_error(
    p <-
      d |>
      plot_multiple_answers_dodge(
        q1a:q1d, by = group, combine_answers = TRUE, value = "y"
      )
  )
  suppressWarnings(print(p)) # warning from ggplot2 due to ggupset
  vdiffr::expect_doppelganger("plot_multiple_answers_dodge() combine", p)

  expect_no_error(
    p <-
      d |>
      plot_multiple_answers_dodge(
        q1a:q1d, by = group, geom = "point", value = "y"
      )
  )
  vdiffr::expect_doppelganger("plot_multiple_answers_dodge() point", p)
})

test_that("plot_multiple_answers() does not produce an error", {
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
  vdiffr::expect_doppelganger("plot_multiple_answers()", p)

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
  vdiffr::expect_doppelganger("plot_multiple_answers() flip and labels", p)

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
  vdiffr::expect_doppelganger("plot_multiple_answers() combine", p)

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
  vdiffr::expect_doppelganger("plot_multiple_answers() combine & flip", p)

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
  vdiffr::expect_doppelganger("plot_multiple_answers() combine sort", p)
})
