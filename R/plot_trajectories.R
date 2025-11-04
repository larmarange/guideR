#' Plot trajectories
#'
#' `plot_trajectories()` assumes that data are stored in a long format (i.e.
#' one row per time step). You can use [tidyr::pivot_longer()] or
#' [periods_to_long()] to transform your data in such format.
#' @param data A data frame, or a data frame extension (e.g. a tibble).
#' @param id <[`tidy-select`][dplyr::dplyr_tidy_select ]>
#' Column containing individual ids.
#' @param time <[`tidy-select`][dplyr::dplyr_tidy_select ]>
#' Time variable.
#' @param fill <[`tidy-select`][dplyr::dplyr_tidy_select ]>
#' Variable mapped to `fill` aesthetic.
#' @param by <[`tidy-select`][dplyr::dplyr_tidy_select ]>
#' Optional variables to group by.
#' @param sort_by <[`tidy-select`][dplyr::dplyr_tidy_select ]>
#' Optional variables to sort trajectories.
#' @param nudge_x Amount of horizontal distance to move.
#' @param facet_labeller Labeller function for strip labels.
#' @param ... Additional arguments passed to [ggplot2::geom_tile()]
#' @keywords hplot
#' @export
plot_trajectories <- function(
  data,
  id,
  time,
  fill,
  by = NULL,
  sort_by = NULL,
  nudge_x = 0,
  facet_labeller = ggplot2::label_wrap_gen(width = 50, multi_line = TRUE),
  ...
) {
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
  d <- data |>
    dplyr::ungroup() |>
    dplyr::select(
      {{ id }}, {{ time }}, {{ fill }},
      {{ by }}, {{ sort_by }},
      dplyr::any_of(".period.width...")
    )
  if (!is.factor(d[[idv]]))
    d[[idv]] <- factor(d[[idv]])
  d[[timev]] <- d[[timev]] + nudge_x
  d <- d |>
    dplyr::arrange(!!!rlang::syms(sort_byv), {{ id }}, {{ time }})
  d[[idv]] <- d[[idv]] |>
    forcats::fct_inorder() |>
    forcats::fct_drop()

  # plot
  p <-
    ggplot2::ggplot(d) +
    ggplot2::aes(x = .data[[timev]], y = .data[[idv]], fill = .data[[fillv]]) +
    ggplot2::geom_tile(...) +
    ggplot2::ylab(NULL) +
    ggplot2::scale_y_discrete(label = NULL) +
    ggplot2::theme_light() +
    ggplot2::theme(
      legend.position = "bottom",
      panel.grid.major.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank()
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
    nudge_x = 0,
    facet_labeller = ggplot2::label_wrap_gen(width = 50, multi_line = TRUE),
    ...
) {
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
    facet_labeller = facet_labeller,
    ...
  ) +
    ggplot2::aes(width = .data$.period.width...)
}
