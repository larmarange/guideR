test_that("step_with_na() works with glm() and svyglm()", {
  set.seed(42)
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
    mod2 <- step_with_na(mod, full_data = d)
  )
  expect_true(length(mod2$model) < length(mod$model))

  skip_if_not_installed("survey")
  skip_if_not_installed("srvyr")
  skip_on_cran()

  library(survey)
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
  expect_true(length(mod2$model) < length(mod$model))
})

test_that("step_with_na() works with coxph()", {
  skip_on_cran()
  skip_if_not_installed("survival")

  df <- dplyr::tibble(
    time = c(4, 3, 1, 1, 2, 2, 3),
    status = c(1, 1, 1, 0, 1, 1, 0),
    x = c(0, 2, 1, 1, 1, NA, 1),
    sex = c(0, 0, 0, 0, 1, 1, 1)
  )
  mod <- survival::coxph(survival::Surv(time, status) ~ x + sex, df)
  expect_no_error(
    mod2 <- step_with_na(mod, full_data = df)
  )
})
