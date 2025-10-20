#' Plot a multiple answers question
#'
#' Considering a multiple answers question coded as several binary variables
#' (one per answer), plot the proportion of positive answers.
#' If `combine_answers = FALSE`, plot the proportion of positive answers of each
#' item, separately. If `combine_answers = FALSE`, combine the different answers
#' (see [combine_answers()]) and plot the proportion of each combination
#' ([`ggupset`][ggupset::axis_combmatrix] package required when
#' `flip = FALSE`).
#' See [proportion()] for more details on the way proportions and
#' confidence intervals are computed. By default, return a bar plot, but other
#' geometries could be used (see examples). If defined, use variable labels
#' (see examples).
#' @note
#' If `drop_na = TRUE`, any observation with at least one `NA` value for one
#' item will be dropped.
#' If `drop_na = FALSE` and `combine_answers = FALSE`, `NA` values for a
#' specific answer are excluded the denominator when computing
#' proportions. Therefore, all proportions may be computed on different
#' population sizes.
#' If `drop_na = FALSE` and `combine_answers = TRUE`, any observation with at
#' least one `NA` value will be labeled with `missing_label`.
#' @param data A data frame, data frame extension (e.g. a tibble),
#' or a survey design object.
#' @param answers <[`tidy-select`][dplyr::dplyr_tidy_select ]> List of variables
#' identifying the different answers of the question.
#' @param value Value indicating a positive answer. By default, will use the
#' maximum observed value and will display a message.
#' @param by <[`tidy-select`][dplyr::dplyr_tidy_select ]> Optional list of
#' variables to compare (using facets).
#' @param combine_answers Should answers be combined? (see examples)
#' @param combine_sep Character string to separate combined answers.
#' @param missing_label When combining answers and
#' `drop_na = FALSE`, label for missing values.
#' @param none_label When combining answers and `flip = TRUE`,
#' label when no item is selected.
#' @param drop_na Should any observation with a least one `NA` value be dropped?
#' @param show_ci Display confidence intervals?
#' @param conf_level Confidence level for the confidence intervals.
#' @param sort Should answers be sorted according to their proportion? They
#' could also be sorted by degrees (number of elements) when combining answers.
#' @param geom Geometry to use for plotting proportions (`"bar"` by default).
#' @param ... Additional arguments passed to the geom defined by `geom`.
#' @param show_ci Display confidence intervals?
#' @param conf_level Confidence level for the confidence intervals.
#' @param ci_color Color of the error bars representing confidence intervals.
#' @param show_labels Display proportion labels?
#' @param labels_labeller Labeller function for proportion labels.
#' @param labels_size Size of proportion labels.
#' @param labels_color Color of proportion labels.
#' @param flip Flip x and y axis?
#' @param return_data Return computed data instead of the plot?
#' @export
#' @keywords univar
#' @examples
#' d <-
#'   dplyr::tibble(
#'     q1a = sample(c("y", "n"), size = 200, replace = TRUE),
#'     q1b = sample(c("y", "n", "n", NA), size = 200, replace = TRUE),
#'     q1c = sample(c("y", "y", "n"), size = 200, replace = TRUE),
#'     q1d = sample("n", size = 200, replace = TRUE)
#'   )
#'
#' d |> plot_multiple_answers(q1a:q1c)
#'
#' d |>
#'   labelled::set_variable_labels(
#'     q1a = "apple",
#'     q1b = "banana",
#'     q1c = "chocolate",
#'     q1d = "Dijon mustard"
#'   ) |>
#'   plot_multiple_answers(
#'     value = "y",
#'     drop_na = TRUE,
#'     sort = "desc",
#'     fill = "lightblue",
#'     flip = TRUE
#'   )
#' @examplesIf rlang::is_installed("ggupset")
#' d |>
#'   plot_multiple_answers(
#'     combine_answers = TRUE,
#'     value = "y",
#'     fill = "#DDCC77",
#'     drop_na = TRUE
#'   )
#'
#' d |>
#'   plot_multiple_answers(
#'     combine_answers = TRUE,
#'     value = "y",
#'     flip = TRUE,
#'     mapping = ggplot2::aes(fill = prop),
#'     show.legend = FALSE
#'   ) +
#'   ggplot2::scale_fill_distiller(palette = "Spectral")
#'
#' d$group <- sample(c("group A", "group B"), size = 200, replace = TRUE)
#' d |>
#'   plot_multiple_answers(
#'     answers = q1a:q1d,
#'     by = group,
#'     combine_answers = TRUE,
#'     sort = "degrees",
#'     value = "y",
#'     fill = "grey80"
#'   )
plot_multiple_answers <- function(
  data,
  answers = dplyr::everything(),
  value = NULL,
  by = NULL,
  combine_answers = FALSE,
  combine_sep = " | ",
  missing_label = " missing",
  none_label = "none",
  drop_na = FALSE,
  sort = c("none", "ascending", "descending", "degrees"),
  geom = "bar",
  ...,
  show_ci = TRUE,
  conf_level = 0.95,
  ci_color = "black",
  show_labels = TRUE,
  labels_labeller = scales::label_percent(1),
  labels_size = 3.5,
  labels_color = "black",
  flip = FALSE,
  return_data = FALSE
) {
  answers <- data |> dplyr::select({{ answers }}) |> colnames()
  sort <- match.arg(sort)

  if (drop_na)
    data <- tidyr::drop_na(data, dplyr::all_of(answers))

  if (is.null(value)) {
    value <- max(unlist(data), na.rm = TRUE)
    cli::cli_alert_warning("Automatically selected value: {.val {value}}")
    cli::cli_alert_info(
      "To remove this message, please specify {.arg value}."
    )
  }

  if (combine_answers) {
    d <-
      data |>
      combine_answers(
        dplyr::all_of(answers),
        into = "item_label",
        value = value,
        sep = combine_sep
      ) |>
      proportion(
        .data$item_label,
        .conf.int = show_ci,
        .scale = 1,
        .conf.level = conf_level,
        .by = {{ by }}
      ) |>
      dplyr::mutate(
        item = .data$item_label |>
          stringr::str_split(stringr::str_escape(combine_sep)),
        item = .data$item |>
          purrr::map(
            \(x) {
              if (all(!is.na(x) & x == "")) return(NULL)
              x
            }
          ),
        degrees = .data$item |> purrr::map(length) |> unlist(),
        degrees = dplyr::if_else(is.na(.data$item_label), NA, .data$degrees),
        item_label = dplyr::if_else(
          .data$item_label == "" & flip,
          none_label,
          .data$item_label
        ),
        item_label = dplyr::if_else(
          is.na(.data$item_label),
          missing_label,
          .data$item_label
        ),
        item_label = .data$item_label |>
          forcats::fct_reorder(.data$n, .fun = sum, .desc = TRUE)
      ) |>
      dplyr::relocate(dplyr::all_of("item")) |>
      dplyr::arrange(.data$item_label)
    if (sort == "degrees")
      d <- d |>
        dplyr::arrange(.data$degrees, .data$item_label)
  } else {
    d <-
      answers |>
      purrr::map(
        \(v) {
          data |>
            dplyr::mutate(
              .value = factor(.data[[v]] == value, levels = c(FALSE, TRUE))
            ) |>
            proportion(
              .data$.value,
              .conf.int = show_ci,
              .scale = 1,
              .conf.level = conf_level,
              .by = {{ by }},
              .na.rm = TRUE
            ) |>
            dplyr::mutate(item = v)
        }
      ) |>
      dplyr::bind_rows() |>
      dplyr::filter(.data$.value == "TRUE") |>
      dplyr::select(-.data$.value)

    # variable labels
    if (inherits(data, "survey.design")) {
      vl <- labelled::var_label(
        data$variables,
        null_action = "fill",
        unlist = TRUE
      )
    } else {
      vl <- labelled::var_label(
        data,
        null_action = "fill",
        unlist = TRUE
      )
    }

    d$item_label <- vl[d$item]
  }

  d$item_label <-
    switch(
      sort,
      none = d$item_label |> forcats::fct_inorder(),
      ascending = d$item_label |>
        forcats::fct_reorder(d$prop, .fun = mean),
      descending = d$item_label |>
        forcats::fct_reorder(d$prop, .fun = mean, .desc = TRUE),
      degrees = d$item_label |> forcats::fct_inorder()
    )

  if (flip) d$item_label <- d$item_label |> forcats::fct_rev()

  d <-
    d |>
    dplyr::relocate(dplyr::all_of(c("item", "item_label"))) |>
    dplyr::ungroup()

  # proportion labels
  d$prop_label <- labels_labeller(d$prop)
  d$y_label <- 0

  if (return_data) return(d)

  # main plot
  plot <- d |>
    ggplot2::ggplot() +
    ggplot2::aes(
      x = .data$item_label,
      y = .data$prop,
      group = 1
    )
  if (geom != "point") # if point, should be drawn after ci
    plot <-
      plot +
      ggplot2::stat_identity(geom = geom, ...)

  # plotting confidence intervals
  if (show_ci) {
    plot <-
      plot +
      ggplot2::geom_errorbar(
        mapping = ggplot2::aes(
          ymin = .data$prop_low,
          ymax = .data$prop_high
        ),
        width = .1,
        color = ci_color
      )
  }

  if (geom == "point")
    plot <-
      plot +
      ggplot2::stat_identity(geom = geom, ...)

  # plotting proportion labels
  if (show_labels) {
    plot <-
      plot +
      ggplot2::geom_text(
        mapping = ggplot2::aes(
          label = .data$prop_label,
          y = .data$y_label
        ),
        size = labels_size,
        color = labels_color,
        vjust = ifelse(flip, .5, 0),
        hjust = ifelse(flip, 0, 0.5),
        nudge_y = .01
      )
  }

  # scales and theme
  plot <-
    plot +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::scale_y_continuous(
      labels = scales::percent,
      expand = ggplot2::expansion(mult = c(0, .1))
    ) +
    ggplot2::theme_light() +
    ggplot2::theme(strip.background = ggplot2::element_rect(fill = "grey50"))

  if (flip) {
    plot <-
      plot +
      ggplot2::coord_flip() +
      ggplot2::facet_grid(cols = ggplot2::vars({{ by }})) +
      ggplot2::theme(
        panel.grid.major.y = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank()
      )
  } else {
    plot <-
      plot +
      ggplot2::facet_grid(
        rows = ggplot2::vars({{ by }}),
        switch = "y"
      ) +
      ggplot2::theme(
        panel.grid.major.x = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank(),
        strip.placement = "outside",
        strip.text.y.left = ggplot2::element_text(
          face = "bold", angle = 0, color = "black",
          hjust = 0, vjust = 1
        ),
        strip.background.y = ggplot2::element_blank()
      )
  }

  if (combine_answers && !flip) {
    rlang::check_installed("ggupset")
    plot <-
      plot +
      ggupset::axis_combmatrix(sep = stringr::str_escape(combine_sep))
  }

  plot
}


#' @rdname plot_multiple_answers
#' @param width Dodging width.
#' @export
#' @examplesIf rlang::is_installed(c("ggupset", "ggstats"))
#' d |>
#'   plot_multiple_answers_dodge(q1a:q1d, by = group)
#' d |>
#'   plot_multiple_answers_dodge(q1a:q1d, by = group, flip = TRUE)
#' d |>
#'   plot_multiple_answers_dodge(q1a:q1d, by = group, combine_answers = TRUE)
plot_multiple_answers_dodge <- function(
    data,
    answers = dplyr::everything(),
    value = NULL,
    by,
    combine_answers = FALSE,
    combine_sep = " | ",
    missing_label = " missing",
    none_label = "none",
    drop_na = FALSE,
    sort = c("none", "ascending", "descending", "degrees"),
    geom = c("bar", "point"),
    width = .75,
    ...,
    show_ci = TRUE,
    conf_level = 0.95,
    ci_color = "black",
    show_labels = TRUE,
    labels_labeller = scales::label_percent(1),
    labels_size = 3.5,
    labels_color = "black",
    flip = FALSE
) {
  rlang::check_installed("ggstats")

  d <-
    data |>
    plot_multiple_answers(
      answers = {{ answers }},
      value = value,
      by = {{ by }},
      combine_answers = combine_answers,
      combine_sep = combine_sep,
      missing_label = missing_label,
      none_label = none_label,
      drop_na = drop_na,
      sort = sort,
      show_ci = show_ci,
      conf_level = conf_level,
      ci_color = ci_color,
      show_labels = show_labels,
      labels_labeller = labels_labeller,
      return_data = TRUE
    )
  geom <- match.arg(geom)

  d <-
    d |>
    dplyr::mutate(y_label = .data$y_label + 0.01) |>
    dplyr::ungroup() |>
    dplyr::rowwise() |>
    dplyr::mutate(
      .fill = paste(dplyr::c_across({{ by }}), sep = ", ")
    ) |>
    dplyr::ungroup()

  # main plot
  plot <- d |>
    ggplot2::ggplot() +
    ggplot2::aes(
      x = .data$item_label,
      y = .data$prop,
      group = .data$.fill
    ) +
    ggstats::geom_stripped_cols()

  if (geom == "bar") # if point, should be drawn after ci
    plot <-
      plot +
      ggplot2::geom_bar(
        stat = "identity",
        mapping = ggplot2::aes(fill = .data$.fill),
        position = ggplot2::position_dodge(width = width),
        width = width,
        ...
      )

  # plotting confidence intervals
  if (show_ci) {
    plot <-
      plot +
      ggplot2::geom_errorbar(
        mapping = ggplot2::aes(
          ymin = .data$prop_low,
          ymax = .data$prop_high
        ),
        width = .1,
        color = ci_color,
        position = ggplot2::position_dodge(width = width)
      )
  }

  if (geom == "point")
    plot <-
      plot +
      ggplot2::geom_point(
        stat = "identity",
        mapping = ggplot2::aes(colour = .data$.fill),
        position = ggplot2::position_dodge(width = width),
        ...
      )

  # plotting proportion labels
  if (show_labels) {
    plot <-
      plot +
      ggplot2::geom_text(
        mapping = ggplot2::aes(
          label = .data$prop_label,
          y = .data$y_label
        ),
        size = labels_size,
        color = labels_color,
        vjust = ifelse(flip, .5, 0),
        hjust = ifelse(flip, 0, 0.5),
        position = ggplot2::position_dodge(width = width)
      )
  }

  # scales and theme
  plot <-
    plot +
    ggplot2::labs(x = NULL, y = NULL, fill = NULL, colour = NULL) +
    ggplot2::scale_y_continuous(
      labels = scales::percent,
      expand = ggplot2::expansion(mult = c(0, .1))
    ) +
    ggplot2::theme_light() +
    ggplot2::theme(legend.position = "bottom")

  if (flip) {
    plot <-
      plot +
      ggplot2::coord_flip() +
      ggplot2::theme(
        panel.grid.major.y = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank()
      )
  } else {
    plot <-
      plot +
      ggplot2::theme(
        panel.grid.major.x = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank()
      )
  }

  if (combine_answers && !flip) {
    rlang::check_installed("ggupset")
    plot <-
      plot +
      ggupset::axis_combmatrix(sep = stringr::str_escape(combine_sep))
  }

  plot
}
