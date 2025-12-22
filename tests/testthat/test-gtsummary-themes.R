test_that("Test gtsummary themes", {
  skip_if_not_installed("gtsummary")
  skip_on_cran()

  library(gtsummary)

  expect_no_error(theme_gtsummary_prop_n())
  expect_no_error(
    theme_gtsummary_prop_n(
      mean_sd = TRUE,
      missing_text = "custom missing",
      overall_string = "custom overall"
    )
  )
  expect_no_error(theme_gtsummary_fisher_simulate_p())
  expect_no_error(theme_gtsummary_bold_labels())

  expect_no_error(
    trial |>
      tbl_summary(include = c(grade, age), by = trt) |>
      add_overall() |>
      add_p()
  )

  skip_if_not_installed("survey")

  data("api", package = "survey")
  apistrat$both[1:5] <- NA

  expect_no_error(theme_gtsummary_unweighted_n())
  expect_no_error(
    theme_gtsummary_unweighted_n(
      mean_sd = TRUE,
      missing_text = "custom missing",
      overall_string = "custom overall"
    )
  )

  expect_no_error(
    apistrat |>
      srvyr::as_survey(strata = stype, weights = pw) |>
      tbl_svysummary(include = c(stype, both, api.stu), by = awards) |>
      add_overall() |>
      add_p()
  )

  expect_no_error(
    apistrat |>
      srvyr::as_survey(strata = stype, weights = pw) |>
      tbl_svysummary(include = c(stype, both, api.stu), by = awards) |>
      add_overall() |>
      add_p()
  )

  expect_no_error(
    iris |>
      srvyr::as_survey() |>
      tbl_svysummary(
        include = Petal.Length,
        by = Species
      ) |>
      add_p()
  )

  gtsummary::reset_gtsummary_theme()
})
