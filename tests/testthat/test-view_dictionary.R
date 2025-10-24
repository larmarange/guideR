test_that("view_dictionary() does not produce an error", {
  skip_if_not_installed("DT")
  skip_if_not_installed("htmltools")
  skip_if_not_installed("htmlwidgets")


  expect_no_error(
    iris |> view_dictionary()
  )
  expect_no_error(
    iris |> view_dictionary(details = "none")
  )
  expect_no_error(
    iris |> view_detailed_dictionary()
  )

  d <- mtcars |>
    labelled::set_na_range(
      hp = c(800, 900)
    ) |>
    labelled::set_na_values(
      cyl = 9:12
    )
  expect_no_error(
    d |> view_detailed_dictionary()
  )

  expect_error(
    view_dictionary("hello")
  )
  expect_error(
    view_dictionary(letters)
  )

  skip_if_not_installed("gtsummary")
  expect_no_error(
    gtsummary::trial |> view_detailed_dictionary()
  )
})
