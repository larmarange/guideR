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
