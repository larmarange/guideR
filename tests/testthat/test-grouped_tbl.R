testthat::test_that("grouped_tbl helpers works", {
  skip_on_cran()
  skip_if_not_installed("gtsummary")
  skip_if_not_installed("cardx")
  skip_if_not_installed("parameters")

  mod <- nnet::multinom(
    grade ~ stage + marker + age,
    data = gtsummary::trial,
    trace = FALSE
  )
  suppressMessages(
    tbl <- mod |> gtsummary::tbl_regression(exponentiate = TRUE)
  )

  expect_no_error(
    tbl |> grouped_tbl_pivot_wider()
  )
  expect_no_error(
    tbl |> multinom_add_global_p_pivot_wider()
  )
  expect_no_error(
    tbl |> style_grouped_tbl()
  )
  expect_no_error(
    tbl |>
      style_grouped_tbl(
        bold_groups = TRUE,
        uppercase_groups = TRUE,
        bold_labels = TRUE,
        italicize_labels = TRUE,
        indent_labels = 10,
        bold_levels = TRUE,
        italicize_levels = TRUE,
        indent_levels = 20
      )
  )

  expect_error(
    iris |> grouped_tbl_pivot_wider()
  )
  expect_error(
    iris |> style_grouped_tbl()
  )
  tbl2 <- lm(Petal.Length ~ Petal.Width, data = iris) |>
    gtsummary::tbl_regression()
  expect_error(
    tbl2 |> grouped_tbl_pivot_wider()
  )
  expect_error(
    tbl2 |> style_grouped_tbl()
  )
})
