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

test_that("plot_inertia_from_tree() works", {
  hc <- hclust(dist(USArrests))
  expect_no_error(
    get_inertia_from_tree(hc)
  )
  expect_no_error(
    p <- plot_inertia_from_tree(hc)
  )
  expect_doppelganger("plot_inertia_from_tree()", p)
})
