test_that("plot_inertia_from_tree() does not produce an error", {
  hc <- hclust(dist(USArrests))
  expect_no_error(
    get_inertia_from_tree(hc)
  )
  expect_no_error(
    p <- plot_inertia_from_tree(hc)
  )
  vdiffr::expect_doppelganger("plot_inertia_from_tree()", p)
})
