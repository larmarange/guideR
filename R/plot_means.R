#' Plot means by sub-groups
#'
#' Plot one or several means by sub-groups. See [mean_sd()] for more details on
#' the way means and confidence intervals are computed.
#' By default, return a point plot, but other geometries could be used
#' (see examples).
#'
#' @param data A data frame, data frame extension (e.g. a tibble),
#' or a survey design object.
#' @param outcome <[`tidy-select`][dplyr::dplyr_tidy_select ]> List of
#' continuous variables to be plotted.
#' @param geom Geometry to use for plotting means (`"point"` by default).
#' @param show_pvalues Display p-values in the top-left corner? p-values are
#' computed with [stats::oneway.test()] for data frames, and with
#' [survey::svyttest()] (2 groups) or [svyoneway()] (3 groups or more)
#' for survey objects.
#' @param show_labels Display mean labels?
#' @inheritParams plot_proportions
#' @export
#' @keywords hplot
#' @examples
#' iris |>
#'   plot_means(Petal.Length, by = Species)
#'
#' iris |>
#'   plot_means(
#'     dplyr::starts_with("Petal"),
#'     by = Species,
#'     geom = "bar",
#'     fill = "lightblue",
#'     show_overall_line = TRUE
#'   )
#'
#' \donttest{
#'
#' mtcars |>
#'   plot_means(
#'     mpg,
#'     by = c(cyl, gear),
#'     size = 3,
#'     colour = "plum",
#'     flip = TRUE
#'   )
#'
#' # works with continuous by variables
#' mtcars |>
#'   plot_means(
#'     mpg,
#'     by = c(disp, drat),
#'     fill = "plum",
#'     geom = "bar",
#'     flip = TRUE,
#'     minimal = TRUE
#'   )
#'
#' # works with survey object
#' iris |>
#'   srvyr::as_survey() |>
#'   plot_means(
#'     Petal.Length,
#'     by = c(Species, Petal.Width),
#'     label_y = -1,
#'     size = 3,
#'     mapping = ggplot2::aes(colour = by),
#'     flip = TRUE
#'   )
#' }
plot_means <- function(
  data,
  outcome,
  by = NULL,
  drop_na_by = FALSE,
  convert_continuous = TRUE,
  geom = "point",
  ...,
  show_overall = TRUE,
  overall_label = "Overall",
  show_ci = TRUE,
  conf_level = 0.95,
  ci_color = "black",
  show_pvalues = TRUE,
  pvalues_labeller = scales::label_pvalue(add_p = TRUE),
  pvalues_size = 3.5,
  show_labels = TRUE,
  label_y = NULL,
  labels_labeller = scales::label_number(.1),
  labels_size = 3.5,
  labels_color = "black",
  show_overall_line = FALSE,
  overall_line_type = "dashed",
  overall_line_color = "black",
  overall_line_width = .5,
  facet_labeller = ggplot2::label_wrap_gen(width = 50, multi_line = TRUE),
  flip = FALSE,
  minimal = FALSE,
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

  # identification of outcome variables
  outcome_variables <-
    data |>
    dplyr::select({{ outcome }}) |>
    colnames()

  # mean computation
  fn_one_outcome <- function(outcome_var) {
    by_variables |>
      purrr::map(
        ~ data |>
          dplyr::mutate(level = .data[[.x]]) |>
          mean_sd(
            .data[[outcome_var]],
            .by = dplyr::all_of("level"),
            .conf.int = show_ci,
            .conf.level = conf_level,
            .drop_na_by = drop_na_by
          ) |>
          dplyr::mutate(
            by = .x,
            outcome = outcome_var
          )
      ) |>
      dplyr::bind_rows() |>
      dplyr::ungroup() |>
      dplyr::mutate(
        num_level = paste(dplyr::row_number(), .data$level, sep = "_"),
        num_level = forcats::fct_inorder(.data$num_level),
        by = forcats::fct_inorder(.data$by)
      )
  }

  d <-
    outcome_variables |>
    purrr::map(fn_one_outcome) |>
    dplyr::bind_rows()

  d$outcome <- forcats::fct_inorder(d$outcome)

  if (flip) d$num_level <- d$num_level |> forcats::fct_rev()

  # variable labels
  vl <- data |> .get_vl(by_variables)
  d$by_label <- vl[d$by] |> forcats::fct_inorder()
  d <-
    d |>
    dplyr::relocate(dplyr::all_of(c("outcome", "by", "by_label"))) |>
    dplyr::select(-dplyr::all_of("x"))

  # mean labels
  d$mean_label <- labels_labeller(d$mean)
  if (!is.null(label_y)) {
    d$y_label <- label_y
  } else if (geom %in% c("bar", "area")) {
    d$y_label <- 0.1
  } else if (show_ci) {
    d$y_label <- min(d$mean_low) - .1 * diff(range(d$mean_low, d$mean_high))
  } else {
    d$y_label <- min(d$mean) - .1 * diff(range(d$mean))
  }

  # computing p-values
  pvalues <- NULL
  if (show_pvalues) {
    if (inherits(data, "survey.design")) {
      test_fun <- svyttest_oneway_formula # nolint
    } else {
      test_fun <- stats::oneway.test
    }

    p_one_outcome <- function(outcome_var) {
      v2 <- # selecting only variables with at least 2 levels
        d |>
        dplyr::filter(outcome == outcome_var) |>
        dplyr::count(.data$by, name = "n") |>
        dplyr::filter(.data$n >= 2) |>
        dplyr::pull("by") |>
        as.character()

      res <- v2 |>
        purrr::map(
          ~ paste(outcome_var, " ~ ", .x) |>
            stats::as.formula() |>
            test_fun(data) |>
            purrr::pluck("p.value")
        )
      res <- unlist(res)
      res <- dplyr::tibble(
        by = v2,
        outcome = outcome_var,
        p = res
      )
    }

    pvalues <-
      outcome_variables |>
      purrr::map(p_one_outcome) |>
      dplyr::bind_rows()
  }

  if (return_data) {
    if (!is.null(pvalues))
      d <-
        d |>
        dplyr::left_join(pvalues, by = c("by", "outcome"))
    return(d)
  }

  # computing overall line
  yintercepts <- NULL
  if (show_overall_line) {
    yintercepts <-
      data |>
      mean_sd(dplyr::pick(dplyr::all_of(outcome_variables))) |>
      dplyr::pull("mean") |>
      unlist()
    yintercepts <- dplyr::tibble(
      outcome = outcome_variables,
      yintercept = yintercepts
    )
  }

  # facets per outcome
  if (length(outcome_variables) > 1) {
    cols_facet <- ggplot2::vars(.data$outcome)
  } else {
    cols_facet <- NULL
  }

  # plot
  p <- .guideR_generic_plot_by(
    d,
    y = "mean",
    outcome = "outcome",
    geom = geom,
    position = "identity",
    ...,
    show_ci = show_ci,
    ci_ymin = "mean_low",
    ci_ymax = "mean_high",
    ci_color = ci_color,
    show_pvalues = show_pvalues,
    pvalues = pvalues,
    pvalues_labeller = pvalues_labeller,
    pvalues_size = pvalues_size,
    show_labels = show_labels,
    label = "mean_label",
    y_label = "y_label",
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
    minimal = minimal,
    free_scale = free_scale,
    cols_facet = cols_facet
  )

  if (geom %in% c("bar", "area"))
    p <-
      p +
      ggplot2::scale_y_continuous(
        expand = ggplot2::expansion(mult = c(0, .1))
      )

  p
}
