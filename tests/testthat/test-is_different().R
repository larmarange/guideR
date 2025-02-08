test_that("Test is_different() and is_equal()", {
  v <- c("a", "b", NA)
  expect_equal(
    is_different(v, "a"),
    c(FALSE, TRUE, TRUE)
  )
  expect_equal(
    is_different(v, NA),
    c(TRUE, TRUE, FALSE)
  )
  expect_equal(
    is_equal(v, "a"),
    c(TRUE, FALSE, FALSE)
  )
  expect_equal(
    is_equal(v, NA),
    c(FALSE, FALSE, TRUE)
  )
})

test_that("cumdifferent() and num_cycle() works", {
  d <- dplyr::tibble(group = c("a", "a", "b", "b", "a", "b", "c", "a"))
  expect_no_error(
    res <-
      d |>
      dplyr::mutate(
        subgroup = cumdifferent(group),
        sub_a = num_cycle(group == "a")
      )
  )
  expect_equal(
    res$subgroup,
    c(1L, 1L, 2L, 2L, 3L, 4L, 5L, 6L)
  )
  expect_equal(
    res$sub_a,
    c(1L, 1L, NA, NA, 2L, NA, NA, 3L)
  )
})
