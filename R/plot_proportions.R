#' Plot proportions by sub-groups
#'
#' Plot one or several proportions (defined by logical conditions) by
#' sub-groups. See [proportion()] for more details on the way proportions and
#' confidence intervals are computed. By default, return a bar plot, but other
#' geometries could be used (see examples). `stratified_by()` is an helper
#' function facilitating a stratified analyses (i.e. proportions by groups
#' stratified according to a third variable, see examples).
#' `dummy_proportions()` is an helper to easily convert a categorical variable
#' into dummy variables and therefore showing the proportion of each level of
#' the original variable (see examples).
#'
#' @param data A data frame, data frame extension (e.g. a tibble),
#' or a survey design object.
#' @param condition <[`data-masking`][rlang::args_data_masking]> A condition
#' defining a proportion, or a [dplyr::tibble()] defining several proportions
#' (see examples).
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
#' @param free_scale Allow y axis to vary between conditions?
#' @param return_data Return computed data instead of the plot?
#' @export
#' @keywords hplot
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
#'
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
#'     mapping = ggplot2::aes(fill = by),
#'     color = "black",
#'     show.legend = FALSE,
#'     show_overall_line = TRUE,
#'     show_pvalues = FALSE
#'  )
#'
#' # defining several proportions
#'
#' titanic |>
#'   plot_proportions(
#'     dplyr::tibble(
#'       Survived = Survived == "Yes",
#'       Male = Sex == "Male"
#'     ),
#'     by = c(Class),
#'     mapping = ggplot2::aes(fill = condition)
#'   )
#'
#' titanic |>
#'   plot_proportions(
#'     dplyr::tibble(
#'       Survived = Survived == "Yes",
#'       Male = Sex == "Male"
#'     ),
#'     by = c(Class),
#'     mapping = ggplot2::aes(fill = condition),
#'     free_scale = TRUE
#'   )
#'
#' iris |>
#'   plot_proportions(
#'     dplyr::tibble(
#'       "Long sepal" = Sepal.Length > 6,
#'       "Short petal" = Petal.Width < 1
#'     ),
#'     by = Species,
#'     fill = "palegreen"
#'   )
#'
#' iris |>
#'   plot_proportions(
#'     dplyr::tibble(
#'       "Long sepal" = Sepal.Length > 6,
#'       "Short petal" = Petal.Width < 1
#'     ),
#'     by = Species,
#'     fill = "palegreen",
#'     flip = TRUE
#'   )
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
#'     show_overall_line = TRUE,
#'     labels_labeller = scales::label_percent(.1)
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
  free_scale = FALSE,
  return_data = FALSE
) {
  # variable identification
  by_variables <- data |> dplyr::select({{ by }}) |> colnames()

  if (show_overall || length(by_variables) == 0) {
    data <- data |> .add_overall(overall_label)
    by_variables <- c(".overall", by_variables)
  }

  # conversion of numeric by variables
  data <- data |> .convert_numeric_by(by_variables, convert_continuous)

  # computation of conditions
  condition_variables <- data |>
    dplyr::mutate({{ condition }}, .keep = "none") |>
    colnames()
  data <-
    data |>
    dplyr::mutate({{ condition }}) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(condition_variables),
        \(x) {
          factor(x, levels = c(FALSE, TRUE))
        }
      )
    )

  # proportion computation
  fn_one_cond <- function(cond_var) {
    by_variables |>
      purrr::map(
        ~ data |>
          dplyr::mutate(level = .data[[.x]]) |>
          proportion(
            .data[[cond_var]],
            .by = dplyr::all_of("level"),
            .conf.int = show_ci,
            .scale = 1,
            .conf.level = conf_level,
            .na.rm = TRUE,
            .drop_na_by = drop_na_by
          ) |>
          dplyr::mutate(
            by = .x,
            condition = cond_var
          )
      ) |>
      dplyr::bind_rows() |>
      dplyr::ungroup() |>
      dplyr::filter(.data[[cond_var]] == "TRUE") |>
      dplyr::mutate(
        num_level = paste(dplyr::row_number(), .data$level, sep = "_"),
        num_level = forcats::fct_inorder(.data$num_level),
        by = forcats::fct_inorder(.data$by)
      ) |>
      dplyr::select(-dplyr::all_of(cond_var))
  }

  d <-
    condition_variables |>
    purrr::map(fn_one_cond) |>
    dplyr::bind_rows()
  d$condition <- forcats::fct_inorder(d$condition)

  if (flip) d$num_level <- d$num_level |> forcats::fct_rev()

  # variable labels
  vl <- data |> .get_vl(by_variables)
  d$by_label <- vl[d$by] |> forcats::fct_inorder()
  d <-
    d |>
    dplyr::relocate(dplyr::all_of(c("by", "by_label")))

  # proportion labels
  d$prop_label <- labels_labeller(d$prop)
  d$y_label <- 0.01

  # computing p-values
  pvalues <- NULL
  if (show_pvalues) {
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

    p_one_cond <- function(cond_var) {
      v2 <- # selecting only variables with at least 2 levels
        d |>
        dplyr::filter(condition == cond_var) |>
        dplyr::count(.data$by, name = "n") |>
        dplyr::filter(.data$n >= 2) |>
        dplyr::pull("by") |>
        as.character()

      res <- v2 |>
        purrr::map(
          ~ paste0("~ `", cond_var, "` + ", .x) |>
            as.formula() |>
            test_fun(data) |>
            purrr::pluck("p.value")
        )
      res <- unlist(res)
      res <- dplyr::tibble(
        by = v2,
        condition = cond_var,
        p = res
      )
    }

    pvalues <-
      condition_variables |>
      purrr::map(p_one_cond) |>
      dplyr::bind_rows()
  }

  if (return_data) {
    if (!is.null(pvalues))
      d <-
        d |>
        dplyr::left_join(pvalues, by = c("by", "condition"))
    return(d)
  }

  # computing overall line
  yintercepts <- NULL
  if (show_overall_line) {
    yintercepts <-
      condition_variables |>
      purrr::map(
        ~ data |>
          proportion(.data[[.x]], .scale = 1, .na.rm = TRUE) |>
          dplyr::filter(.data[[.x]] == "TRUE") |>
          dplyr::pull("prop")
      ) |>
      unlist()
    yintercepts <- dplyr::tibble(
      condition = condition_variables,
      yintercept = yintercepts
    )
  }

  # facets per condition
  if (length(condition_variables) > 1) {
    cols_facet <- ggplot2::vars(.data$condition)
  } else {
    cols_facet <- NULL
  }

  # plot
  .guideR_generic_plot_by(
    d,
    y = "prop",
    outcome = "condition",
    geom = geom,
    position = "identity",
    ...,
    show_ci = show_ci,
    ci_ymin = "prop_low",
    ci_ymax = "prop_high",
    ci_color = ci_color,
    show_pvalues = show_pvalues,
    pvalues = pvalues,
    pvalues_labeller = pvalues_labeller,
    pvalues_size = pvalues_size,
    show_labels = show_labels,
    label = "prop_label",
    label_position = "nudge",
    labels_size = labels_size,
    labels_color = labels_color,
    show_overall_line = show_overall_line,
    yintercepts = yintercepts,
    overall_line_type = overall_line_type,
    overall_line_color = overall_line_color,
    overall_line_width = overall_line_width,
    facet_labeller = facet_labeller,
    flip = flip,
    scale_y_labels = scales::percent,
    free_scale = free_scale,
    cols_facet = cols_facet
  )
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

#' @rdname plot_proportions
#' @export
#' @param strata Stratification variable
#' @examples
#'
#' # stratified analysis
#' titanic |>
#'   plot_proportions(
#'     (Survived == "Yes") |>  stratified_by(Sex),
#'     by = Class,
#'     mapping = ggplot2::aes(fill = condition)
#'   ) +
#'   ggplot2::theme(legend.position = "bottom") +
#'   ggplot2::labs(fill = NULL)
stratified_by <- function(condition, strata) {
  if (is.numeric(strata)) strata <- .convert_continuous(strata)
  if (!is.factor(strata)) strata <- factor(strata)
  res <-
    strata |>
    levels() |>
    purrr::map(
      ~ dplyr::if_else(
        strata == .x,
        condition,
        NA
      )
    )
  names(res) <- levels(strata)
  dplyr::as_tibble(res)
}

#' @rdname plot_proportions
#' @param variable Variable to be converted into dummy variables.
#' @export
#' @examples
#'
#' # Convert Class into dummy variables
#' titanic |>
#'   plot_proportions(
#'     dummy_proportions(Class),
#'     by = Sex,
#'     mapping = ggplot2::aes(fill = level)
#'   )
dummy_proportions <- function(variable) {
  if (is.numeric(variable)) variable <- .convert_continuous(variable)
  if (!is.factor(variable)) variable <- factor(variable)
  res <-
    variable |>
    levels() |>
    purrr::map(
      ~ variable == .x
    )
  names(res) <- levels(variable)
  dplyr::as_tibble(res)
}

.add_overall <- function(data, overall_label) {
  data <- data |> dplyr::mutate(.overall = overall_label)
  if (inherits(data, "survey.design")) {
    labelled::var_label(data$variables$.overall) <- overall_label
  } else {
    labelled::var_label(data$.overall) <- overall_label
  }
  data
}

.convert_numeric_by <- function(data, by_variables, convert_continuous) {
  data |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(by_variables),
        \(x) {
          .convert_continuous(x, convert_continuous)
        }
      )
    )
}

.get_vl <- function(data, variables) {
  if (inherits(data, "survey.design")) {
    vl <- labelled::var_label(
      data$variables[, variables],
      null_action = "fill",
      unlist = TRUE
    )
  } else {
    vl <- labelled::var_label(
      data[, variables, drop = FALSE],
      null_action = "fill",
      unlist = TRUE
    )
  }
  vl
}

.guideR_generic_plot_by <- function(
    d,
    y = "prop",
    outcome = "condition",
    geom = "bar",
    position = "identity",
    ...,
    show_ci = TRUE,
    ci_ymin = "prop_low",
    ci_ymax = "prop_high",
    ci_color = "black",
    show_pvalues = TRUE,
    pvalues = NULL,
    pvalues_labeller = scales::label_pvalue(add_p = TRUE),
    pvalues_size = 3.5,
    show_labels = TRUE,
    label = "prop_label",
    label_position = "nudge",
    labels_size = 3.5,
    labels_color = "black",
    show_overall_line = FALSE,
    yintercepts = NULL,
    overall_line_type = "dashed",
    overall_line_color = "black",
    overall_line_width = .5,
    facet_labeller = ggplot2::label_wrap_gen(width = 50, multi_line = TRUE),
    flip = FALSE,
    scale_y_labels = scales::percent,
    free_scale = FALSE,
    cols_facet = NULL
) {
  # main plot
  plot <- d |>
    ggplot2::ggplot() +
    ggplot2::aes(
      x = .data$num_level,
      y = .data[[y]],
      group = 1
    )
  if (geom != "point") # if point, should be drawn after ci
    plot <-
    plot +
    ggplot2::stat_identity(geom = geom, ..., position = position)

  # plotting confidence intervals
  if (show_ci) {
    plot <-
      plot +
      ggplot2::geom_errorbar(
        mapping = ggplot2::aes(
          ymin = .data[[ci_ymin]],
          ymax = .data[[ci_ymax]]
        ),
        width = .1,
        color = ci_color,
        position = position
      )
  }

  if (geom == "point")
    plot <-
    plot +
    ggplot2::stat_identity(geom = geom, ..., position = position)

  # plotting p-values
  if (show_pvalues && !is.null(pvalues) && nrow(pvalues) > 0) {
    pvalues <-
      pvalues |>
      dplyr::left_join(
        d |>
          dplyr::arrange(.data$num_level) |>
          dplyr::group_by(
            .data$by,
            .data$by_label,
            .data[[outcome]]
          ) |>
          dplyr::summarise(num_level = dplyr::last(.data$num_level)),
        by = c("by", outcome)
      ) |>
      dplyr::left_join(
        d |>
          dplyr::group_by(.data[[outcome]]) |>
          dplyr::summarise(
            y = ifelse(show_ci, max(.data$prop_high), max(.data$prop))
          ),
        by = outcome
      )
    if (!free_scale) pvalues$y <- max(pvalues$y)
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

  # plotting labels
  if (show_labels) {
    plot <-
      plot +
      ggplot2::geom_text(
        mapping = ggplot2::aes(
          label = .data[[label]],
          y = .data$y_label
        ),
        size = labels_size,
        color = labels_color,
        vjust = ifelse(flip, .5, 0),
        hjust = ifelse(flip, 0, 0.5),
        position = label_position
      )
  }

  # overall line
  if (show_overall_line) {
    plot <-
      plot +
      ggplot2::geom_hline(
        data = yintercepts,
        mapping = ggplot2::aes(yintercept = .data$yintercept),
        color = overall_line_color,
        linetype = overall_line_type,
        linewidth = overall_line_width
      )
  }

  # facet and theme
  plot <-
    plot +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::scale_y_continuous(
      labels = scale_y_labels,
      expand = ggplot2::expansion(mult = c(0, .1))
    ) +
    ggplot2::scale_x_discrete(
      labels = \(x) {
        stringr::str_remove(x, "^[0-9]*_")
      }
    ) +
    ggplot2::theme_light() +
    ggplot2::theme(strip.background = ggplot2::element_rect(fill = "grey50"))

  if (flip) {
    plot <-
      plot +
      ggplot2::coord_flip() +
      ggplot2::facet_grid(
        rows = ggplot2::vars(.data$by_label),
        cols = cols_facet,
        scales = ifelse(free_scale, "free", "free_y"),
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
        strip.background.y = ggplot2::element_blank()
      )
  } else {
    plot <-
      plot +
      ggplot2::facet_grid(
        cols = ggplot2::vars(.data$by_label),
        rows = cols_facet,
        scales = ifelse(free_scale, "free", "free_x"),
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
