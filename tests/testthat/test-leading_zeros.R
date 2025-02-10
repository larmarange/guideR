testthat::test_that("leading_zeros() works", {
  v <- c(2, 103.24, 1042.147, 12.4566, NA)
  expect_equal(
    leading_zeros(v),
    c("0002", "0103", "1042", "0012", "  NA")
  )
  expect_equal(
    leading_zeros(v, digits = 1),
    c("0002.0", "0103.2", "1042.1", "0012.5", "    NA")
  )
  expect_equal(
    leading_zeros(v, left_digits = 6, big.mark = " "),
    c("000 002", "000 103", "001 042", "000 012", "     NA")
  )
  expect_equal(
    leading_zeros(c(0, 6, 12, 18), prefix = "M"),
    c("M00", "M06", "M12", "M18")
  )
})
