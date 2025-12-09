test_that("median_iqr() works with data frames", {
  expect_no_error(
    res <- iris |> median_iqr(Petal.Length, .outliers = TRUE)
  )
  expect_equal(
    res$n,
    150
  )
  expect_equal(res$median, stats::median(iris$Petal.Length))
  expect_equal(res$iqr, stats::IQR(iris$Petal.Length))

  expect_no_error(
    res <- iris |> median_iqr(Petal.Length, .by = Species)
  )
  expect_no_error(
    res2 <- iris |> dplyr::group_by(Species) |> median_iqr(Petal.Length)
  )
  expect_equal(res, res2)
  expect_equal(nrow(res), 3)

  expect_no_error(
    res <- mtcars |> median_iqr(mpg, .by = c(cyl, gear))
  )
  expect_equal(nrow(res), 8)

  expect_no_error(
    res <- iris |> median_iqr(Petal.Length, Petal.Width)
  )
  expect_equal(nrow(res), 2)

  expect_no_error(
    res <- iris |>
      median_iqr(dplyr::pick(dplyr::starts_with("Petal")), .by = Species)
  )
  expect_equal(nrow(res), 6)

  d <- iris
  d$Petal.Length[1:10] <- NA
  expect_no_error(
    res <- d |> median_iqr(Petal.Length)
  )
  expect_equal(res$n, 140)
  expect_equal(res$missing, 10)
})

test_that("median_iqr() works with survey designs", {
  skip_if_not_installed("srvyr")
  skip_if_not_installed("survey")
  ds <- srvyr::as_survey(iris)

  expect_no_error(
    res <- ds |> median_iqr(Petal.Length)
  )
  q <- survey::svyquantile(~ Petal.Length, quantiles = .5, design = ds)
  expect_equal(res$median, q$Petal.Length[1, 1])

  expect_no_error(
    res <- ds |> median_iqr(Petal.Length, .by = Species, .outliers = TRUE)
  )
  expect_equal(sum(res$n), 150)
})

test_that("median_iqr() works with atomic vectors", {
  expect_no_error(
    res <- iris$Petal.Length |> median_iqr()
  )
  expect_equal(res$median, stats::median(iris$Petal.Length))
})
