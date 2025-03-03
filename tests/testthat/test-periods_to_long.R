test_that("long_to_periods() works", {
  d <- dplyr::tibble(
    patient = c(1, 2, 3, 3),
    begin = c(0, 2, 0, 3),
    end = c(6, 4, 2, 8),
    covar = c("no", "yes", "no", "yes")
  )

  expect_no_error(
    res <- d |>
      periods_to_long(start = begin, stop = end)
  )
  expect_equal(nrow(res), 19)

  expect_no_error(
    res <- d |>
      periods_to_long(start = begin, stop = end, time_step = 5)
  )
  expect_equal(nrow(res), 6)

  expect_no_error(
    res <- d |>
      periods_to_long(start = begin, stop = end, time_name = "temps")
  )
  expect_true("temps" %in% colnames(res))
  expect_equal(ncol(res), 3)

  expect_no_error(
    res <- d |>
      periods_to_long(start = begin, stop = end, keep = TRUE)
  )
  expect_equal(ncol(res), 5)
})
