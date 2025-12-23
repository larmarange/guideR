test_that("add_interactions_by_step() works with glm()", {
  mod <- glm(as.factor(Survived) ~ ., data = titanic, family = binomial())
  expect_no_error(
    mod2 <- mod |> add_interactions_by_step()
  )
  expect_true(length(mod2$model) >= length(mod$model))
})
