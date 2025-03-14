test_that("plot_proportions() does not produce an error", {
  expect_no_error(
    titanic |>
      plot_proportions(Survived == "Yes", by = -Survived)
  )
})
