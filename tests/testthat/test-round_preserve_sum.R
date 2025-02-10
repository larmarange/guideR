testthat::test_that("unrowwise() works", {
  v <- c(1 / 3, 1 / 3, 1 / 3)
  expect_equal(
    sum(v),
    sum(round_preserve_sum(v, digits = 1))
  )
})
