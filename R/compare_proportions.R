#' Compare a proportion by sub-groups and plot them
#' @param data A data frame, data frame extension (e.g. a tibble),
#' or a survey design object.
#' @param condition <[`data-masking`][rlang::args_data_masking]> A condition
#' defining a proportion (see examples).
#' @param by <[`tidy-select`][dplyr::dplyr_tidy_select ]> List of variables to
#' group by (comparison is done separately for each variable).
#' @param conf.level Confidence level for the confidence intervals.
#' @export
#' @examples
#' titanic |>
#'   compare_proportions(Survived == "Yes", by = c(Class, Sex))
#'
#' titanic |>
#'   compare_proportions(Survived == "Yes", by = c(Class, Sex)) |>
#'   plot()
compare_proportions <- function(data, condition, by, conf.level = 0.95) {
  vars <- data |> dplyr::select({{ by }}) |> colnames()
  if (length(vars) == 0)
    cli::cli_abort("No variable selected by {.arg by}.")
  data <- data |> dplyr::mutate(.condition = {{ condition }})
  d <- vars |>
    purrr::map(
      ~ data |>
        dplyr::mutate(level = .data[[.x]]) |>
        proportion(
          .data$.condition,
          .by = dplyr::all_of("level"),
          .conf.int = TRUE,
          .scale = 1,
          .conf.level = conf.level
        ) |>
        dplyr::mutate(
          variable = .x,
          level = .data$level |> forcats::fct_inorder()
        )
    ) |>
    dplyr::bind_rows() |>
    dplyr::filter(.data$.condition) |>
    dplyr::select(-.data$.condition)

  if (inherits(data, "survey.design")) {
    vl <- labelled::var_label(
      data$variables[, vars],
      null_action = "fill",
      unlist = TRUE
    )
    test <- survey::svychisq # nolint
  } else {
    vl <- labelled::var_label(
      data[, vars],
      null_action = "fill",
      unlist = TRUE
    )
    test <- function(formula, data) {
      xtabs(formula, data) |> chisq.test()
    }
  }
  d$variable_label <- vl[d$variable] |> forcats::fct_inorder()

  pvalues <- vars |>
    purrr::map(
      ~ paste("~ .condition +", .x) |>
        as.formula() |>
        test(data) |>
        purrr:::pluck("p.value")
    )
  pvalues <- unlist(pvalues)
  pvalues <- dplyr::tibble(
    variable = vars,
    p = pvalues
  )

  d <-
    d |>
    dplyr::left_join(pvalues, by = "variable") |>
    dplyr::relocate(dplyr::all_of(c("variable", "variable_label")))
  class(d) <- c("compare_proportions", class(d))
  d
}

#' @rdname compare_proportions
#' @param x A tibble returned by `compare_proportions()`.
#' @param fill Fill colour, passed to [ggplot2::geom_bar()].
#' @param label_wrap Maximum number of characters before wrapping the strip
#' (variable names).
#' @param ... Not used.
#' @export
plot.compare_proportions <- function(x,
                                     fill = "lightblue",
                                     label_wrap = 50,
                                     add_p = TRUE,
                                     ...) {
  plot <- x |>
    ggplot2::ggplot() +
    ggplot2::aes(
      x = .data$level,
      y = .data$prop,
      ymin = .data$prop_low,
      ymax = .data$prop_high
    ) +
    ggplot2::geom_bar(stat = "identity", fill = fill) +
    ggplot2::geom_errorbar(width = .1) +
    ggplot2::facet_grid(
      cols = vars(.data$variable_label),
      scales = "free_x",
      space = "free_x",
      labeller = ggplot2::label_wrap_gen(width = label_wrap, multi_line = TRUE)
    ) +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::theme_light() +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank()
    )

  if (add_p) {
    pvalues <- x |>
      dplyr::ungroup() |>
      dplyr::mutate(y = max(.data$prop, na.rm = TRUE)) |>
      dplyr::group_by(.data$variable_label, .data$y, .data$p) |>
      dplyr::summarise(
        level = dplyr::last(.data$level),
        .groups = "drop"
      ) |>
      dplyr::mutate(label = scales::label_pvalue(add_p = TRUE)(.data$p))

    plot <- plot +
      ggplot2::geom_text(
        data = pvalues,
        mapping = ggplot2::aes(
          y = .data$y,
          label = .data$label,
          ymin = NULL,
          ymax = NULL
        ),
        nudge_y = .05,
        nudge_x = 0.5,
        hjust = 1
      )
  }

  plot
}
