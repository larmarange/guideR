test_that("plot_inertia_from_tree() does not produce an error", {
  hc <- hclust(dist(USArrests))
  expect_no_error(
    get_inertia_from_tree(hc)
  )
  expect_no_error(
    plot_inertia_from_tree(hc)
  )
})
