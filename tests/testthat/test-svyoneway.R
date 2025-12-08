test_that("svyoneway() does not produce an error", {
  expect_no_error(
    t <- svyoneway(
      Petal.Length ~ Species,
      design = srvyr::as_survey(iris)
    )
  )
  expect_true(t$p.value > 0)
  expect_true(t$p.value < 1)
})
