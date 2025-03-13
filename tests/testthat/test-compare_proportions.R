test_that("compare_proportions() does not produce an error", {
  expect_no_error(
    titanic |>
      compare_proportions(Survived == "Yes", by = -Survived) |>
      plot()
  )
})
