test_that("proportion() works with data frames", {
  expect_no_error(
    res <- titanic |> proportion(Class, .scale = 1)
  )
  expect_equal(
    res$n,
    as.integer(table(titanic$Class))
  )
  expect_equal(
    res$prop,
    as.numeric(proportions(table(titanic$Class)))
  )

  expect_no_error(
    res <- titanic |> proportion(Class, .conf.int = TRUE)
  )

  expect_no_error(
    res <- titanic |> proportion(Class, .sort = TRUE)
  )

  dna <- titanic
  dna$Survived[c(1:20, 500:530)] <- NA
  expect_equal(
    dna |> proportion(Survived) |> nrow(),
    3
  )
  expect_equal(
    dna |> proportion(Survived, .na.rm = TRUE) |> nrow(),
    2
  )

  expect_equal(
    dna |> proportion(Sex, .by = Survived) |> nrow(),
    5
  )
  expect_equal(
    dna |> proportion(Sex, .by = Survived, .drop_na_by = TRUE) |> nrow(),
    4
  )

  # rows with N = 0
  d <- titanic
  d$Sex <- factor(d$Sex, c("Male", "Female", "Other"))
  expect_no_error(
    d |> proportion(Survived, .by = Sex, .conf.int = TRUE)
  )
})

test_that("proportion() works with survey designs", {
  skip_if_not_installed("srvyr")
  d <- srvyr::as_survey(titanic)

  expect_no_error(
    res <- d |> proportion(Class, .scale = 1)
  )
  expect_equal(
    res$n,
    as.integer(table(titanic$Class))
  )
  expect_equal(
    res$prop,
    as.numeric(proportions(table(titanic$Class)))
  )

  expect_no_error(
    res <- d |> proportion(Class, .conf.int = TRUE)
  )

  expect_no_error(
    res <- d |> proportion(Class, .sort = TRUE)
  )

  dna <- titanic
  dna$Survived[c(1:20, 500:530)] <- NA
  dsna <- dna |> srvyr::as_survey()
  expect_equal(
    dsna |> proportion(Survived) |> nrow(),
    3
  )
  expect_equal(
    dsna |> proportion(Survived, .na.rm = TRUE) |> nrow(),
    2
  )

  expect_equal(
    dsna |> proportion(Sex, .by = Survived) |> nrow(),
    5
  )
  expect_equal(
    dsna |> proportion(Sex, .by = Survived, .drop_na_by = TRUE) |> nrow(),
    4
  )
})

test_that("proportion() works with atomic vectors", {
  expect_no_error(
    res <- titanic$Survived |> proportion()
  )
  expect_equal(
    res$n,
    titanic |> proportion(Survived) |> dplyr::pull("n")
  )
})
