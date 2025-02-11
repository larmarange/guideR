test_that("step_with_na() works with glm() and svyglm()", {
  d <- titanic |>
    dplyr::mutate(
      Group = sample(
        c("a", "b", NA),
        dplyr::n(),
        replace = TRUE
      )
    )
  mod <- glm(as.factor(Survived) ~ ., data = d, family = binomial())
  expect_no_error(
    mod2 <- step_with_na(mod)
  )
  expect_true(length(mod2$model) < length(mod$model))

  skip_if_not_installed("survey")
  skip_if_not_installed("srvyr")
  skip_on_cran()

  ds <- d |>
    dplyr::mutate(Survived = as.factor(Survived)) |>
    srvyr::as_survey()
  mod <- survey::svyglm(
    Survived ~ Class + Group + Sex,
    design = ds,
    family = quasibinomial()
  )
  expect_no_error(
    mod2 <- step_with_na(mod, design = ds)
  )
})
