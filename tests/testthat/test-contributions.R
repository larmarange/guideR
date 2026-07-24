test_that("contributions() does not produce an error", {
  skip_if_not_installed("broom.helpers")

  m <- lm(Sepal.Length ~ Sepal.Width + Species + Petal.Length, data = iris)
  m2 <- glm(
    Survived == "Yes" ~ Class + Sex + Age,
    data = titanic,
    family = binomial
  )

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
  expect_no_error(
    m2 |> contributions(type = "I")
  )
  expect_no_error(
    m2 |> tbl_contributions(type = "drop1")
  )
  expect_no_error(
    m2 |> tbl_contributions(type = "add1")
  )

  skip_if_not_installed("survey")
  suppressWarnings(library(survey))
  m3 <- survey::svyglm(
    Survived == "Yes" ~ Class + Sex + Age,
    design = srvyr::as_survey(titanic),
    family = quasibinomial
  )
  expect_no_error(
    m3 |> contributions()
  )
  expect_no_error(
    m3 |> tbl_contributions("drop1")
  )
})

test_that("tbl_dominance() does not produce an error", {
  skip_if_not_installed("broom.helpers")
  skip_if_not_installed("dominanceanalysis")

  m <- lm(Sepal.Length ~ Sepal.Width + Species + Petal.Length, data = iris)
  m2 <- glm(
    Survived == "Yes" ~ Class + Sex + Age,
    data = titanic,
    family = binomial
  )

  expect_no_error(
    m |> tbl_dominance()
  )
  expect_no_error(
    m2 |> tbl_dominance()
  )

  skip_if_not_installed("survey")
  suppressWarnings(library(survey))
  m3 <- svyglm(
    Survived == "Yes" ~ Class + Sex + Age,
    design = srvyr::as_survey(titanic),
    family = quasibinomial
  )
  expect_no_error(
    m3 |> tbl_dominance()
  )
})
