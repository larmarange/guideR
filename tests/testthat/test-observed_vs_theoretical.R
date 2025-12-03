test_that("observed_vs_theoretical() works", {
  set.seed(2025)
  mod <- glm(
    as.factor(Survived) ~ Class + Sex,
    data = titanic,
    family = binomial()
  )
  expect_no_error(
    p <- mod |> observed_vs_theoretical()
  )
  expect_doppelganger("observed_vs_theoretical()", p)
})
