test_that("long_to_seq() works", {
  skip_on_cran()
  skip_if_not_installed("TraMineR")

  data("biofam", package = "TraMineR")

  d <-
    biofam |>
    tibble::rownames_to_column("id_ind") |>
    dplyr::select(id_ind, dplyr::starts_with("a")) |>
    tidyr::pivot_longer(
      cols = dplyr::starts_with("a"),
      names_to = "age",
      names_prefix = "a",
      values_to = "life_state"
    ) |>
    dplyr::mutate(
      age = as.integer(age),
      life_state2 = dplyr::case_when(
        life_state == 0 ~ "P",
        life_state == 1 ~ "L",
        life_state == 2 ~ "M",
        life_state == 3 ~ "LM",
        life_state == 4 ~ "C",
        life_state == 5 ~ "LC",
        life_state == 6 ~ "LMC",
        life_state == 7 ~ "D"
      ),
      life_state_bug = life_state2
    ) |>
    labelled::set_value_labels(
      life_state = c(
        "Parent" = 0,
        "Left" = 1,
        "Married" = 2,
        "Left & Married" = 3,
        "Child" = 4,
        "Left & Child" = 5,
        "Left & Married & Child" = 6,
        "Divorced" = 7
      ),
      life_state2 = c(
        "Parent" = "P",
        "Left" = "L",
        "Married" = "M",
        "Left & Married" = "LM",
        "Child" = "C",
        "Left & Child" = "LC",
        "Left & Married & Child" = "LMC",
        "Divorced" = "D"
      ),
      life_state_bug = c(
        "Parent" = "P",
        "Left" = "L",
        "Divorced" = "D"
      )
    ) |>
    dplyr::mutate(
      life_state3 = labelled::to_factor(life_state),
      life_state4 = unclass(life_state2),
      life_state5 = unclass(life_state)
    )

  expect_no_error(
    suppressMessages(
      d |> long_to_seq(id = id_ind, time = age, outcome = life_state)
    )
  )
  expect_no_error(
    suppressMessages(
      d |> long_to_seq(id = id_ind, time = age, outcome = life_state2)
    )
  )
  expect_no_error(
    suppressMessages(
      d |> long_to_seq(id = id_ind, time = age, outcome = life_state3)
    )
  )
  expect_no_error(
    suppressMessages(
      d |> long_to_seq(id = id_ind, time = age, outcome = life_state4)
    )
  )
  expect_no_error(
    suppressMessages(
      d |> long_to_seq(id = id_ind, time = age, outcome = life_state5)
    )
  )
  expect_no_error(
    suppressMessages(
      d |>
        long_to_seq(
          id = id_ind,
          time = age,
          outcome = life_state5,
          states = c("P", "L", "M", "LM", "C", "LC", "LMC", "D")
        )
    )
  )

  expect_error(
    d |> long_to_seq(id = id_ind, time = age, outcome = life_state_bug)
  )
})
