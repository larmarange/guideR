test_that("mean_sd() works with data frames", {
  expect_no_error(
    res <- iris |> mean_sd(Petal.Length, .conf.int = TRUE)
  )
  expect_equal(
    res$n,
    150
  )
  expect_equal(res$mean, mean(iris$Petal.Length))
  expect_equal(res$sd, sd(iris$Petal.Length))

  expect_no_error(
    res <- iris |> mean_sd(Petal.Length, .by = Species)
  )
  expect_no_error(
    res2 <- iris |> dplyr::group_by(Species) |> mean_sd(Petal.Length)
  )
  expect_equal(res, res2)
  expect_equal(nrow(res), 3)

  expect_no_error(
    res <- mtcars |> mean_sd(mpg, .by = c(cyl, gear))
  )
  expect_equal(nrow(res), 8)

  expect_no_error(
    res <- iris |> mean_sd(Petal.Length, Petal.Width)
  )
  expect_equal(nrow(res), 2)

  expect_no_error(
    res <- iris |>
      mean_sd(dplyr::pick(dplyr::starts_with("Petal")), .by = Species)
  )
  expect_equal(nrow(res), 6)

  d <- iris
  d$Petal.Length[1:10] <- NA
  expect_no_error(
    res <- d |> mean_sd(Petal.Length)
  )
  expect_equal(res$n, 140)
  expect_equal(res$missing, 10)

  d$Species[30:50] <- NA
  expect_no_error(
    res <- d |> mean_sd(Petal.Length, .by = Species)
  )
  expect_equal(nrow(res), 4)
  expect_no_error(
    res <- d |> mean_sd(Petal.Length, .by = Species, .drop_na_by = TRUE)
  )
  expect_equal(nrow(res), 3)
})

test_that("mean_sd() works with survey designs", {
  skip_if_not_installed("srvyr")
  ds <- srvyr::as_survey(iris)

  expect_no_error(
    res <- ds |> mean_sd(Petal.Length)
  )
  expect_equal(res$mean, mean(iris$Petal.Length))

  expect_no_error(
    res <- ds |> mean_sd(Petal.Length, .by = Species, .conf.int = TRUE)
  )
  expect_equal(sum(res$n), 150)
})

test_that("mean_sd() works with atomic vectors", {
  expect_no_error(
    res <- iris$Petal.Length |> mean_sd()
  )
  expect_equal(res$mean, mean(iris$Petal.Length))
})
