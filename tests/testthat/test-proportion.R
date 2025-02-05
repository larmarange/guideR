test_that("proportion() works with data frames", {
  titanic <- Titanic |> dplyr::as_tibble() |> tidyr::uncount(n)

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
})
