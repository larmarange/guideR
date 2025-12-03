# adapted from https://github.com/r-lib/vdiffr/issues/141
if (nzchar(Sys.getenv("CI")) || !rlang::is_installed("vdiffr")) {
  #if we are running tests remotely
  # we are opting out of using vdiffr
  # assigning a dummy function

  expect_doppelganger <- function(...) {
    testthat::skip("`vdiffr` tests not run")
  }
} else {
  expect_doppelganger <- vdiffr::expect_doppelganger
}

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
