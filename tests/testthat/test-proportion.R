test_that("proportion() works with data frames", {
  expect_no_error(
    res <- titanic |> proportion(Class, .scale = 1)
  )
  expect_equal(
    res$n,
    as.integer(table(titanic$Class))
  )
  expect_equal(
    res$prop,
    as.numeric(proportions(table(titanic$Class)))
  )

  expect_no_error(
    res <- titanic |> proportion(Class, .conf.int = TRUE)
  )

  expect_no_error(
    res <- titanic |> proportion(Class, .sort = TRUE)
  )
})

test_that("proportion() works with survey designs", {
  skip_if_not_installed("srvyr")
  d <- srvyr::as_survey(titanic)

  expect_no_error(
    res <- d |> proportion(Class, .scale = 1)
  )
  expect_equal(
    res$n,
    as.integer(table(titanic$Class))
  )
  expect_equal(
    res$prop,
    as.numeric(proportions(table(titanic$Class)))
  )

  expect_no_error(
    res <- d |> proportion(Class, .conf.int = TRUE)
  )

  expect_no_error(
    res <- d |> proportion(Class, .sort = TRUE)
  )
})
