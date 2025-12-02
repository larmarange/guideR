test_that("plot_trajectories() and plot_periods() works properly", {
  d <- dplyr::tibble(
    id = c(1, 1, 1, 1, 2, 2, 2, 3, 3, 3, 3, 3),
    time = c(0:3, 0:2, 0:4),
    status = c("a", "a", "b", "b", "b", "b", "a", "b", "b", "b", "b", "a"),
    group = c("f", "f", "f", "f", "f", "f", "f", "m", "m", "m", "m", "m")
  )

  expect_no_error(
    p <-
      d |>
      plot_trajectories(id = id, time = time, fill = status, colour = "black")
  )
  vdiffr::expect_doppelganger("plot_trajectories()", p)

  expect_no_error(
    p <-
      d |>
      plot_trajectories(id = id, time = time, fill = status, nudge_x = .5)
  )
  vdiffr::expect_doppelganger("plot_trajectories() nudge_x", p)

  expect_no_error(
    p<-
      d |>
      plot_trajectories(id = id, time = time, fill = status, by = group)
  )
  vdiffr::expect_doppelganger("plot_trajectories() by", p)

  d$group2 <- "C"
  expect_no_error(
    p <-
      d |>
      plot_trajectories(
        id = id, time = time, fill = status,
        by = c(group, group2)
      )
  )
  vdiffr::expect_doppelganger("plot_trajectories() by 2", p)

  d2 <- d |>
    dplyr::mutate(end = time + 1) |>
    long_to_periods(id = id, start = time, stop = end, by = status)
  expect_no_error(
    p <-
      d2 |>
      plot_periods(
        id = id, start = time, stop = end,
        fill = status, height = 0.8
      )
  )
  vdiffr::expect_doppelganger("plot_periods()", p)

  expect_no_error(
    p <-
      d2 |>
      plot_periods(
        id = id, start = time, stop = end,
        fill = status, height = 0.8,
        hide_y_labels = TRUE
      )
  )
  vdiffr::expect_doppelganger("plot_periods() hide_y_labels", p)

  # expected errors
  expect_error(
    d |>
      plot_trajectories(id = c(id, time), time = time, fill = status)
  )
  expect_error(
    d |>
      plot_trajectories(id = id, time = c(id, time), fill = status)
  )
  expect_error(
    d |>
      plot_trajectories(id = id, time = time, fill = c(id, time))
  )
  expect_error(
    d2 |>
      plot_periods(
        id = id, start = c(time, end),
        stop = end, fill = status
      )
  )
  expect_error(
    d2 |>
      plot_periods(
        id = id, start = time,
        stop = c(time, end), fill = status
      )
  )
})
