#' Plot trajectories
#'
#' Create a trajectory index plot (similar to sequence index plot) from a data
#' frame in long or period format.
#'
#' @note
#' `plot_trajectories()` assumes that data are stored in a long format (i.e.
#' one row per unit of time). You can use [tidyr::pivot_longer()] or
#' [periods_to_long()] to transform your data in such format. By default, tiles
#' are centered on the value of `time`. You can adjust horizontal position with
#' `nudge_x`. By default, each row is assumed to represent one unit of time and
#' represented with a width of 1. You can adjust tiles' width with `width`.
#'
#' `plot_periods()` is adapted for period format with a start and a stop
#' variable. You can use [long_to_periods()] to transform your data in such
#' format. Beginning and ending of each tile is determined by `start` and
#' `stop` arguments.
#'
#' For survey design objects, weights are not taken into account. Each
#' individual trajectory as the same height.
#' @param data A data frame, a data frame extension (e.g. a tibble), , or a
#' survey design object.
#' @param id <[`tidy-select`][dplyr::dplyr_tidy_select ]>\cr
#' Column containing individual ids.
#' @param time <[`tidy-select`][dplyr::dplyr_tidy_select ]>\cr
#' Time variable.
#' @param fill <[`tidy-select`][dplyr::dplyr_tidy_select ]>\cr
#' Variable mapped to `fill` aesthetic.
#' @param by <[`tidy-select`][dplyr::dplyr_tidy_select ]>\cr
#' Optional variables to group by.
#' @param sort_by <[`tidy-select`][dplyr::dplyr_tidy_select ]>\cr
#' Optional variables to sort trajectories.
#' @param nudge_x Optional amount of horizontal distance to move.
#' @param hide_y_labels Hide y labels? If `NULL`, hide them when more than 20
#' trajectories are displayed.
#' @param facet_labeller Labeller function for strip labels.
#' @param ... Additional arguments passed to [ggplot2::geom_tile()]
#' @keywords hplot
#' @export
#' @examples
#' d <- dplyr::tibble(
#'   id = c(1, 1, 1, 1, 2, 2, 2, 3, 3, 3, 3, 3),
#'   time = c(0:3, 0:2, 0:4),
#'   status = c("a", "a", "b", "b", "b", "b", "a", "b", "b", "b", "b", "a"),
#'   group = c("f", "f", "f", "f", "f", "f", "f", "m", "m", "m", "m", "m")
#' )
#'
#' d |> plot_trajectories(id = id, time = time, fill = status, colour = "black")
#' d |> plot_trajectories(id = id, time = time, fill = status, nudge_x = .5)
#' d |> plot_trajectories(id = id, time = time, fill = status, by = group)
#'
#' d2 <- d |>
#'   dplyr::mutate(end = time + 1) |>
#'   long_to_periods(id = id, start = time, stop = end, by = status)
#' d2
#' d2 |> plot_periods(
#'   id = id,
#'   start = time,
#'   stop = end,
#'   fill = status,
#'   colour = "black",
#'   height = 0.8
#' )
plot_trajectories <- function(
  data,
  id,
  time,
  fill,
  by = NULL,
  sort_by = NULL,
  nudge_x = NULL,
  hide_y_labels = NULL,
  facet_labeller = ggplot2::label_wrap_gen(width = 50, multi_line = TRUE),
  ...
) {
  if (inherits(data, "survey.design")) {
    data <- data$variables
  }

  # selection and checks
  idv <-
    tidyselect::eval_select(
      rlang::enquo(id),
      data = data,
      allow_empty = FALSE
    ) |>
    names()
  if (length(idv) > 1)
    cli::cli_abort("{.arg id} should select only one variable.")
  timev <-
    tidyselect::eval_select(
      rlang::enquo(time),
      data = data,
      allow_empty = FALSE
    ) |>
    names()
  if (length(timev) > 1)
    cli::cli_abort("{.arg time} should select only one variable.")
  fillv <-
    tidyselect::eval_select(
      rlang::enquo(fill),
      data = data,
      allow_empty = FALSE
    ) |>
    names()
  if (length(fillv) > 1)
    cli::cli_abort("{.arg fill} should select only one variable.")
  byv <-
    tidyselect::eval_select(
      rlang::enquo(by),
      data = data
    ) |>
    names()
  sort_byv <-
    tidyselect::eval_select(
      rlang::enquo(sort_by),
      data = data
    ) |>
    names()

  # data preparation
  if (!is.factor(data[[idv]]))
    data[[idv]] <- factor(data[[idv]])
  if (!is.null(nudge_x)) data[[timev]] <- data[[timev]] + nudge_x
  data <- data |>
    dplyr::ungroup() |>
    dplyr::arrange(!!!rlang::syms(sort_byv), {{ id }}, {{ time }})
  data[[idv]] <- data[[idv]] |>
    forcats::fct_inorder() |>
    forcats::fct_drop()

  # plot
  p <-
    ggplot2::ggplot(data) +
    ggplot2::aes(x = .data[[timev]], y = .data[[idv]], fill = .data[[fillv]]) +
    ggplot2::geom_tile(...) +
    ggplot2::ylab(NULL) +
    ggplot2::theme_light() +
    ggplot2::theme(
      legend.position = "bottom",
      panel.grid.major.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank()
    ) +
    scale_fill_safe()

  if (is.null(hide_y_labels)) {
    hide_y_labels <- length(unique(data[[idv]])) > 20
  }

  if (hide_y_labels)
    p <-
      p +
      ggplot2::theme(
        axis.text.y = ggplot2::element_blank()
      )

  if (length(byv) > 0) {
    p <-
      p +
      ggplot2::facet_grid(
        rows = ggplot2::vars(!!!rlang::syms(byv)),
        scales = "free_y",
        space = "free_y",
        labeller = facet_labeller,
        switch = "y"
      ) +
      ggplot2::theme(
        strip.placement = "outside",
        strip.text.y.left = ggplot2::element_text(
          face = "bold", angle = 0, color = "black",
          hjust = 0, vjust = 1
        ),
        strip.background.y = ggplot2::element_blank()
      )
  }

  p
}

#' @rdname plot_trajectories
#' @param start,stop <[`tidy-select`][dplyr::dplyr_tidy_select ]>
#' Start and stop variables of the periods.
#' @export
plot_periods <- function(
  data,
  id,
  start,
  stop,
  fill,
  by = NULL,
  sort_by = NULL,
  nudge_x = NULL,
  hide_y_labels = NULL,
  facet_labeller = ggplot2::label_wrap_gen(width = 50, multi_line = TRUE),
  ...
) {
  if (inherits(data, "survey.design")) {
    data <- data$variables
  }

  startv <-
    tidyselect::eval_select(
      rlang::enquo(start),
      data = data,
      allow_empty = FALSE
    ) |>
    names()
  if (length(startv) > 1)
    cli::cli_abort("{.arg start} should select only one variable.")
  stopv <-
    tidyselect::eval_select(
      rlang::enquo(stop),
      data = data,
      allow_empty = FALSE
    ) |>
    names()
  if (length(stopv) > 1)
    cli::cli_abort("{.arg stop} should select only one variable.")

  data$.period.width... <- data[[stopv]] - data[[startv]]
  data[[startv]] <- data[[startv]] + (data$.period.width... / 2)

  plot_trajectories(
    data = data,
    id = {{ id }},
    time = {{ start }},
    fill = {{ fill }},
    by = {{ by }},
    sort_by = {{ sort_by }},
    nudge_x = nudge_x,
    hide_y_labels = hide_y_labels,
    facet_labeller = facet_labeller,
    ...
  ) +
    ggplot2::aes(width = .data$.period.width...)
}
