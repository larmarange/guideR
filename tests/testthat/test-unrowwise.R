testthat::test_that("unrowwise() works", {
  expect_equal(
    titanic,
    titanic |> dplyr::rowwise() |> unrowwise()
  )
  expect_equal(
    titanic |> dplyr::group_by(Sex, Class),
    titanic |> dplyr::group_by(Sex, Class) |> dplyr::rowwise() |> unrowwise()
  )
})
