test_that("Test gtsummary utilities", {
  skip_if_not_installed("gtsummary")
  tbl <-
    gtsummary::trial |>
    gtsummary::tbl_summary() |>
    gtsummary::add_variable_group_header(
      header = "Clinical situation at diagnosis",
      variables = c(stage, grade, age, marker)
    ) |>
    gtsummary::add_variable_group_header(
      header = "Treatment and outcome",
      variables = c(trt, response, death, ttdeath)
    )
  expect_no_error(
    tbl |>
      bold_variable_group_headers() |>
      italicize_variable_group_headers() |>
      indent_labels(indent = 4L) |>
      indent_levels(indent = 8L)
  )
})
