test_that("svyoneway() does not produce an error", {
  expect_no_error(
    t <- svyoneway(
      Petal.Length ~ Species,
      design = srvyr::as_survey(iris)
    )
  )
  p <- unlist(as.list(t$p))
  expect_true(p > 0)
  expect_true(p < 1)
})
