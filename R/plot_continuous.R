#' Plot a continuous variable by sub-groups
#'
#' Plot one or several continuous variables by sub-groups. See [median_iqr()]
#' for more details on the way statistics are
#' computed. Return a box plot (see examples).
#'
#' @param data A data frame, data frame extension (e.g. a tibble),
#' or a survey design object.
#' @param outcome <[`tidy-select`][dplyr::dplyr_tidy_select ]> List of
#' continuous variables to be plotted.
#' @param ... Additional arguments passed to [ggplot2::geom_boxplot()].
#' @param show_pvalues Display p-values in the top-left corner? p-values are
#' computed with [stats::kruskal.test()] for data frames, and with
#' [survey::svyranktest()] for survey objects.
#' @inheritParams plot_proportions
#' @export
#' @keywords hplot
#' @examples
#' iris |>
#'   plot_continuous(Petal.Length, by = Species)
#'
#' iris |>
#'   plot_continuous(
#'     dplyr::starts_with("Petal"),
#'     by = Species,
#'     free_scale = TRUE,
#'     fill = "lightblue",
#'     outlier.color = "red"
#'   )
#'
#' \donttest{
#'
#' mtcars |>
#'   plot_continuous(
#'     mpg,
#'     by = c(cyl, gear),
#'     flip = TRUE,
#'     mapping = ggplot2::aes(fill = by)
#'   )
#'
#' # works with continuous by variables
#' mtcars |>
#'   plot_continuous(
#'     mpg,
#'     by = c(disp, drat),
#'     flip = TRUE,
#'     minimal = TRUE
#'   )
#'
#' # works with survey object
#' iris |>
#'   srvyr::as_survey() |>
#'   plot_continuous(
#'     Petal.Length,
#'     by = c(Species, Petal.Width),
#'     flip = TRUE
#'   )
#' }
plot_continuous <- function(
  data,
  outcome,
  by = NULL,
  drop_na_by = FALSE,
  convert_continuous = TRUE,
  ...,
  show_overall = TRUE,
  overall_label = "Overall",
  show_pvalues = TRUE,
  pvalues_labeller = scales::label_pvalue(add_p = TRUE),
  pvalues_size = 3.5,
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
          median_iqr(
            .data[[outcome_var]],
            .by = dplyr::all_of("level"),
            .drop_na_by = drop_na_by,
            .outliers = TRUE
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

  # computing p-values
  pvalues <- NULL
  if (show_pvalues) {
    if (inherits(data, "survey.design")) {
      test_fun <- survey::svyranktest # nolint
    } else {
      test_fun <- stats::kruskal.test
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

  # facets per outcome
  if (length(outcome_variables) > 1) {
    cols_facet <- ggplot2::vars(.data$outcome)
  } else {
    cols_facet <- NULL
  }

  # plot
  .guideR_generic_plot_by(
    d,
    y = "max", # for labels
    outcome = "outcome",
    geom = "boxplot",
    position = "dodge2",
    ...,
    show_ci = FALSE,
    show_pvalues = show_pvalues,
    pvalues = pvalues,
    pvalues_labeller = pvalues_labeller,
    pvalues_size = pvalues_size,
    show_labels = FALSE,
    show_overall_line = FALSE,
    facet_labeller = facet_labeller,
    flip = flip,
    minimal = minimal,
    free_scale = free_scale,
    cols_facet = cols_facet
  ) +
    ggplot2::aes(
      middle = .data$median,
      lower = .data$q1,
      upper = .data$q3,
      ymin = .data$whisker_low,
      ymax = .data$whisker_high,
      outliers = .data$outliers,
      group = .data$num_level
    )
}
