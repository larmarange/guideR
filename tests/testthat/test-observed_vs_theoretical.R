test_that("observed_vs_theoretical() does not produce an error", {
  set.seed(2025)
  mod <- glm(
    as.factor(Survived) ~ Class + Sex,
    data = titanic,
    family = binomial()
  )
  expect_no_error(
    p <- mod |> observed_vs_theoretical()
  )
  vdiffr::expect_doppelganger("observed_vs_theoretical()", p)
})
