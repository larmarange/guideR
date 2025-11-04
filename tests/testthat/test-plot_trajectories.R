test_that("plot_trajectories() and plot_periods() do not produce an error", {
  d <- dplyr::tibble(
    id = c(1, 1, 1, 1, 2, 2, 2, 3, 3, 3, 3, 3),
    time = c(0:3, 0:2, 0:4),
    status = c("a", "a", "b", "b", "b", "b", "a", "b", "b", "b", "b", "a"),
    group = c("f", "f", "f", "f", "f", "f", "f", "m", "m", "m", "m", "m")
  )

  expect_no_error(
    d |>
      plot_trajectories(id = id, time = time, fill = status, colour = "black")
  )
  expect_no_error(
    d |>
      plot_trajectories(id = id, time = time, fill = status, nudge_x = .5)
  )
  expect_no_error(
    d |>
      plot_trajectories(id = id, time = time, fill = status, by = group)
  )

  d2 <- d |>
    dplyr::mutate(end = time + 1) |>
    long_to_periods(id = id, start = time, stop = end, by = status)
  expect_no_error(
    d2 |>
      plot_periods(
        id = id, start = time, stop = end,
        fill = status, height = 0.8
      )
  )
})
