test_that("observed_vs_theoretical() does not produce an error", {
  mod <- glm(
    as.factor(Survived) ~ Class + Sex,
    data = titanic,
    family = binomial()
  )
  expect_no_error(
    mod |> observed_vs_theoretical()
  )
})
