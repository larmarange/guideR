test_that("contributions() does not produce an error", {
  m <- lm(Sepal.Length ~ Sepal.Width + Species + Petal.Length, data = iris)
  m2 <- glm(Survived == "Yes" ~ ., data = titanic, family = binomial)

  expect_no_error(
    m |> contributions()
  )
  expect_no_error(
    m |> tbl_contributions()
  )
  expect_no_error(
    m2 |> contributions()
  )
  expect_no_error(
    m2 |> tbl_contributions()
  )
  expect_no_error(
    m2 |> contributions(test.statistic = "F")
  )
  expect_no_error(
    m2 |> tbl_contributions(show = "Relative", notes = FALSE)
  )

})
