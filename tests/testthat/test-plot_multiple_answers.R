test_that("plot_multiple_answers_dodge() does not produce an error", {
  skip_on_cran()
  skip_if_not_installed("ggupset")
  skip_if_not_installed("ggstats")

  d <-
    dplyr::tibble(
      q1a = sample(c("y", "n"), size = 200, replace = TRUE),
      q1b = sample(c("y", "n", "n", NA), size = 200, replace = TRUE),
      q1c = sample(c("y", "y", "n"), size = 200, replace = TRUE),
      q1d = sample("n", size = 200, replace = TRUE),
      group = sample(c("group A", "group B"), size = 200, replace = TRUE)
    )

  expect_no_error(
    d |>
      plot_multiple_answers_dodge(q1a:q1d, by = group)
  )
  expect_no_error(
    d |>
      plot_multiple_answers_dodge(q1a:q1d, by = group, flip = TRUE)
  )
  expect_no_error(
    d |>
      plot_multiple_answers_dodge(q1a:q1d, by = group, combine_answers = TRUE)
  )
  expect_no_error(
    d |>
      plot_multiple_answers_dodge(q1a:q1d, by = group, geom = "point")
  )
})

test_that("plot_multiple_answers() does not produce an error", {
  d <-
    dplyr::tibble(
      q1a = sample(c("y", "n"), size = 200, replace = TRUE),
      q1b = sample(c("y", "n", "n", NA), size = 200, replace = TRUE),
      q1c = sample(c("y", "y", "n"), size = 200, replace = TRUE),
      q1d = sample("n", size = 200, replace = TRUE)
    )

  expect_message(
    d |> plot_multiple_answers(q1a:q1c)
  )
  expect_no_error(
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
  expect_no_error(
    d |>
      plot_multiple_answers(
        combine_answers = TRUE,
        value = "y",
        fill = "#DDCC77",
        drop_na = TRUE
      )
  )
  skip_on_cran()
  skip_if_not_installed("ggupset")
  expect_no_error(
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
  d$group <- sample(c("group A", "groupe B"), size = 200, replace = TRUE)
  expect_no_error(
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
})
