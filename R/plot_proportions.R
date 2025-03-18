#' Plot proportions by sub-groups
#'
#' `r lifecycle::badge("experimental")`
#' See [proportion()] for more details on the way proportions and confidence
#' intervals are computed.
#'
#' @param data A data frame, data frame extension (e.g. a tibble),
#' or a survey design object.
#' @param condition <[`data-masking`][rlang::args_data_masking]> A condition
#' defining a proportion (see examples).
#' @param by <[`tidy-select`][dplyr::dplyr_tidy_select ]> List of variables to
#' group by (comparison is done separately for each variable).
#' @param drop_na_by Remove `NA` values in `by` variables?
#' @param convert_continuous Should continuous variables (with 5 unique values
#' or more) be converted to quartiles (using `cut_quartiles()`)?
#' @param geom Geometry to use for plotting proportions (`"bar"` by default).
#' @param ... Additional arguments passed to the geom defined by `geom`.
#' @param show_overall Display "Overall" column?
#' @param overall_label Label for the overall column.
#' @param show_ci Display confidence intervals?
#' @param conf_level Confidence level for the confidence intervals.
#' @param ci_color Color of the error bars representing confidence intervals.
#' @param show_pvalues Display p-values in the top-left corner?
#' @param pvalues_test Test to compute p-values for data frames: `"fisher"` for
#' [stats::fisher.test()] (with `simulate.p.value = TRUE`) or `"chisq"` for
#' [stats::chisq.test()]. Has no effect on survey objects for those
#' [survey::svychisq()] is used.
#' @param pvalues_labeller Labeller function for p-values.
#' @param pvalues_size Text size for p-values.
#' @param show_labels Display proportion labels?
#' @param labels_labeller Labeller function for proportion labels.
#' @param labels_size Size of proportion labels.
#' @param labels_color Color of proportion labels.
#' @param show_overall_line Add an overall line?
#' @param overall_line_type Line type of the overall line.
#' @param overall_line_color Color of the overall line.
#' @param overall_line_width Line width of the overall line.
#' @param facet_labeller Labeller function for strip labels.
#' @param flip Flip x and y axis?
#' @param return_data Return data used instead of the plot?
#' @export
#' @keywords univar
#' @examples
#' titanic |>
#'   plot_proportions(
#'     Survived == "Yes",
#'     overall_label = "All",
#'     labels_color = "white"
#'   )
#'
#' titanic |>
#'   plot_proportions(
#'     Survived == "Yes",
#'     by = c(Class, Sex),
#'     fill = "lightblue"
#'   )
#'
#' \donttest{
#' titanic |>
#'   plot_proportions(
#'     Survived == "Yes",
#'     by = c(Class, Sex),
#'     fill = "lightblue",
#'     flip = TRUE
#'   )
#'
#' titanic |>
#'   plot_proportions(
#'     Survived == "Yes",
#'     by = c(Class, Sex),
#'     geom = "point",
#'     color = "red",
#'     size = 3,
#'     show_labels = FALSE
#'   )
#'
#' titanic |>
#'   plot_proportions(
#'     Survived == "Yes",
#'     by = c(Class, Sex),
#'     geom = "area",
#'     fill = "lightgreen",
#'     show_overall = FALSE
#'   )
#'
#' titanic |>
#'   plot_proportions(
#'     Survived == "Yes",
#'     by = c(Class, Sex),
#'     geom = "line",
#'     color = "purple",
#'     ci_color = "darkblue",
#'     show_overall = FALSE
#'   )
#'
#' titanic |>
#'   plot_proportions(
#'     Survived == "Yes",
#'     by = -Survived,
#'     mapping = ggplot2::aes(fill = variable),
#'     color = "black",
#'     show.legend = FALSE,
#'     show_overall_line = TRUE,
#'     show_pvalues = FALSE
#'  )
#'
#' # works with continuous by variables
#' iris |>
#'   labelled::set_variable_labels(
#'     Sepal.Length = "Length of the sepal"
#'   ) |>
#'   plot_proportions(
#'     Species == "versicolor",
#'     by = dplyr::contains("leng"),
#'     fill = "plum",
#'     colour = "plum4"
#'   )
#'
#' # works with survey object
#' titanic |>
#'   srvyr::as_survey() |>
#'   plot_proportions(
#'     Survived == "Yes",
#'     by = c(Class, Sex),
#'     fill = "darksalmon",
#'     color = "black",
#'     show_overall_line = TRUE
#'  )
#' }
plot_proportions <- function(
  data,
  condition,
  by = NULL,
  drop_na_by = FALSE,
  convert_continuous = TRUE,
  geom = "bar",
  ...,
  show_overall = TRUE,
  overall_label = "Overall",
  show_ci = TRUE,
  conf_level = 0.95,
  ci_color = "black",
  show_pvalues = TRUE,
  pvalues_test = c("fisher", "chisq"),
  pvalues_labeller = scales::label_pvalue(add_p = TRUE),
  pvalues_size = 3.5,
  show_labels = TRUE,
  labels_labeller = scales::label_percent(1),
  labels_size = 3.5,
  labels_color = "black",
  show_overall_line = FALSE,
  overall_line_type = "dashed",
  overall_line_color = "black",
  overall_line_width = .5,
  facet_labeller = ggplot2::label_wrap_gen(width = 50, multi_line = TRUE),
  flip = FALSE,
  return_data = FALSE
) {
  # variable identification
  vars <- data |> dplyr::select({{ by }}) |> colnames()

  if (show_overall || length(vars) == 0) {
    data <- data |> dplyr::mutate(.overall = overall_label)
    if (inherits(data, "survey.design")) {
      labelled::var_label(data$variables$.overall) <- overall_label
    } else {
      labelled::var_label(data$.overall) <- overall_label
    }
    vars <- c(".overall", vars)
  }
  data <- data |>
    dplyr::mutate(.condition = factor({{ condition }}, c(FALSE, TRUE))) |>
    dplyr::filter(!is.na(.data$.condition))

  # conversion of numeric variable
  data <-
    data |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(vars),
        .convert_continuous,
        convert_continuous
      )
    )

  # proportion computation
  d <- vars |>
    purrr::map(
      ~ data |>
        dplyr::mutate(level = .data[[.x]]) |>
        proportion(
          .data$.condition,
          .by = dplyr::all_of("level"),
          .conf.int = show_ci,
          .scale = 1,
          .conf.level = conf_level,
          .drop_na_by = drop_na_by
        ) |>
        dplyr::mutate(
          variable = .x
        )
    ) |>
    dplyr::bind_rows() |>
    dplyr::ungroup() |>
    dplyr::filter(.data$.condition == "TRUE") |>
    dplyr::mutate(
      num_level = paste(dplyr::row_number(), .data$level, sep = "_"),
      num_level = forcats::fct_inorder(.data$num_level),
      variable = forcats::fct_inorder(.data$variable)
    ) |>
    dplyr::select(-.data$.condition)
  if (flip) d$num_level <- d$num_level |> forcats::fct_rev()

  # variable labels
  if (inherits(data, "survey.design")) {
    vl <- labelled::var_label(
      data$variables[, vars],
      null_action = "fill",
      unlist = TRUE
    )
  } else {
    vl <- labelled::var_label(
      data[, vars, drop = FALSE],
      null_action = "fill",
      unlist = TRUE
    )
  }
  d$variable_label <- vl[d$variable] |> forcats::fct_inorder()
  d <-
    d |>
    dplyr::relocate(dplyr::all_of(c("variable", "variable_label")))

  # proportion labels
  d$prop_label <- labels_labeller(d$prop)
  d$y_label <- 0

  # selecting only variables with at least 2 levels
  v2 <-
    d |>
    dplyr::count(.data$variable, name = "n") |>
    dplyr::filter(.data$n >= 2) |>
    dplyr::pull("variable") |>
    as.character()

  # computing p-values
  pvalues <- NULL
  if (show_pvalues && length(v2) > 0) {
    if (inherits(data, "survey.design")) {
      test_fun <- survey::svychisq # nolint
    } else {
      pvalues_test <- match.arg(pvalues_test)
      if (pvalues_test == "fisher") {
        test_fun <- function(formula, data) {
          stats::xtabs(formula, data, addNA = !drop_na_by) |>
            stats::fisher.test(simulate.p.value = TRUE)
        }
      } else {
        test_fun <- function(formula, data) {
          stats::xtabs(formula, data, addNA = !drop_na_by) |>
            stats::chisq.test()
        }
      }
    }

    pvalues <- v2 |>
      purrr::map(
        ~ paste("~ .condition +", .x) |>
          as.formula() |>
          test_fun(data) |>
          purrr::pluck("p.value")
      )
    pvalues <- unlist(pvalues)
    pvalues <- dplyr::tibble(
      variable = v2,
      p = pvalues
    )
  }

  if (return_data) {
    if (!is.null(pvalues))
      d <-
        d |>
        dplyr::left_join(pvalues, by = "variable")
    return(d)
  }

  # main plot
  plot <- d |>
    ggplot2::ggplot() +
    ggplot2::aes(
      x = .data$num_level,
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

  # plotting p-values
  if (show_pvalues && !is.null(pvalues)) {
    pvalues <-
      pvalues |>
      dplyr::left_join(
        d |>
          dplyr::arrange(.data$num_level) |>
          dplyr::group_by(.data$variable, .data$variable_label) |>
          dplyr::summarise(num_level = dplyr::last(.data$num_level)),
        by = "variable"
      )
    pvalues$y <- ifelse(show_ci, max(d$prop_high), max(d$prop))
    pvalues$label <- pvalues_labeller(pvalues$p)

    plot <-
      plot +
      ggplot2::geom_text(
        data = pvalues,
        mapping = ggplot2::aes(
          y = .data$y,
          label = .data$label,
          ymin = NULL,
          ymax = NULL
        ),
        nudge_y = .01,
        nudge_x = 0.5,
        vjust = ifelse(flip, 1, 0),
        hjust = 1,
        size = pvalues_size
      )
  }

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

  # overall line
  if (show_overall_line) {
    yintercept <-
      data |>
      proportion(.data$.condition, .scale = 1) |>
      dplyr::filter(.data$.condition == "TRUE") |>
      dplyr::pull("prop")
    plot <-
      plot +
      ggplot2::geom_hline(
        yintercept = yintercept,
        color = overall_line_color,
        linetype = overall_line_type,
        linewidth = overall_line_width
      )
  }

  # facet and theme
  plot <-
    plot +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::scale_x_discrete(
      labels = \(x) {
        stringr::str_remove(x, "^[0-9]*_")
      }
    ) +
    ggplot2::theme_light()

  if (flip) {
    plot <-
      plot +
      ggplot2::coord_flip() +
      ggplot2::facet_grid(
        rows = ggplot2::vars(.data$variable_label),
        scales = "free_y",
        space = "free_y",
        labeller = facet_labeller,
        switch = "y"
      ) +
      ggplot2::theme(
        panel.grid.major.y = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank(),
        strip.placement = "outside",
        strip.text.y.left = ggplot2::element_text(
          face = "bold", angle = 0, color = "black",
          hjust = 0, vjust = 1
        ),
        strip.background = ggplot2::element_blank()
      )
  } else {
    plot <-
      plot +
      ggplot2::facet_grid(
        cols = ggplot2::vars(.data$variable_label),
        scales = "free_x",
        space = "free_x",
        labeller = facet_labeller
      ) +
      ggplot2::theme(
        panel.grid.major.x = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank()
      )
  }

  plot
}

.convert_continuous <- function(x, convert_continuous) {
  if (is.numeric(x) && length(unique(x)) > 4 && convert_continuous) {
    cut_quartiles(x)
  } else if (is.numeric(x)) {
    res <- factor(x)
    labelled::var_label(res) <- labelled::var_label(x)
    res
  } else {
    x
  }
}
