test_that("long_to_periods() works", {
  d <- dplyr::tibble(
    patient = c(1, 2, 3, 3, 4, 4, 4),
    begin = c(0, 0, 0, 1, 0, 36, 39),
    end = c(50, 6, 1, 16, 36, 39, 45),
    covar = c("no", "no", "no", "yes", "no", "yes", "yes")
  )

  expect_no_error(
    res <- d |>
      long_to_periods(id = patient, start = begin, stop = end)
  )
  expect_equal(nrow(res), 4)

  expect_no_error(
    res <- d |>
      long_to_periods(id = patient, start = begin, stop = end, by = covar)
  )
  expect_equal(nrow(res), 6)

  expect_no_error(
    res <- d |>
      long_to_periods(id = patient, start = begin)
  )
  expect_equal(nrow(res), 2)

  expect_error(
    d |> long_to_periods(id = patient)
  )
  expect_error(
    d |> long_to_periods(id = patient, start = c(begin, end))
  )
  expect_error(
    d |> long_to_periods(id = patient, start = begin, stop = c(end, covar))
  )
})
