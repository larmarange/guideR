test_that("contributions() does not produce an error", {
  m <- lm(Sepal.Length ~ Sepal.Width + Species + Petal.Length, data = iris)
  m2 <- glm(Survived == "Yes" ~ ., data = titanic, family = binomial)

  expect_no_error(
    m |> contributions()
  )
  expect_no_error(
    m |> contributions(type = "I")
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

  m3 <- survey::svyglm(
    Survived == "Yes" ~ Class + Sex + Age,
    design = srvyr::as_survey(titanic),
    family = quasibinomial
  )
  expect_no_error(
    m3 |> contributions()
  )
  expect_no_error(
    m3 |> tbl_contributions(type = "drop1")
  )
  expect_no_error(
    m3 |> tbl_contributions(type = "add1")
  )
})
