#' Plot a categorical variable by sub-groups
#'
#' Plot one or several categorical variables by sub-groups. See [proportion()]
#' for more details on the way proportions and confidence intervals are
#' computed. Return a bar plot (see examples).
#'
#' @param outcome <[`tidy-select`][dplyr::dplyr_tidy_select ]> List of
#' categorical variables to be plotted.
#' @param na.rm Should `NA` values be removed from the `outcome`?
#' @param ... Additional arguments passed to [ggplot2::geom_bar()].
#' @param pvalues_y Y position of p-values.
#' @inheritParams plot_proportions
#' @export
#' @keywords hplot
#' @examples
#' titanic |>
#'   plot_categorical(
#'     Class,
#'     by = c(Age, Sex)
#'   )
#'
#' \donttest{
#' titanic |>
#'   plot_categorical(
#'     Class,
#'     by = c(Age, Sex),
#'     show_overall = FALSE,
#'     flip = TRUE
#'   )
#'
#' titanic |>
#'   plot_categorical(
#'     Class,
#'     by = c(Age, Sex),
#'     flip = TRUE,
#'     minimal = TRUE
#'   )
#' }
#' @examplesIf rlang::is_installed("gtsummary")
#' \donttest{
#' gtsummary::trial |>
#'   plot_categorical(grade, by = c(age, stage, trt))
#' gtsummary::trial |>
#'   plot_categorical(grade, by = c(age, stage, trt), drop_na_by = TRUE)
#' gtsummary::trial |>
#'   plot_categorical(c(grade, stage), by = c(trt, response))
#' }
plot_categorical <- function(
  data,
  outcome,
  na.rm = TRUE,
  by = NULL,
  drop_na_by = FALSE,
  convert_continuous = TRUE,
  ...,
  show_overall = TRUE,
  overall_label = "Overall",
  show_pvalues = TRUE,
  pvalues_test = c("fisher", "chisq"),
  pvalues_labeller = scales::label_pvalue(add_p = TRUE),
  pvalues_size = 3.5,
  pvalues_y = ifelse(flip, 1.05, 1),
  show_labels = TRUE,
  labels_labeller = scales::label_percent(1),
  labels_size = 3.5,
  labels_color = "auto",
  facet_labeller = ggplot2::label_wrap_gen(width = 50, multi_line = TRUE),
  flip = FALSE,
  minimal = FALSE,
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

  # proportion computation
  fn_one_outcome <- function(outcome_var) {
    by_variables |>
      purrr::map(
        ~ data |>
          dplyr::mutate(level = factor(.data[[.x]])) |>
          proportion(
            .data[[outcome_var]],
            .by = dplyr::all_of("level"),
            .conf.int = FALSE,
            .scale = 1,
            .na.rm = na.rm,
            .drop_na_by = drop_na_by
          ) |>
          dplyr::mutate(
            by = .x,
            outcome = outcome_var,
            outcome_level = .data[[outcome_var]],
            num_level = paste(as.integer(.data$level), .data$level, sep = "_"),
          )
      ) |>
      dplyr::bind_rows() |>
      dplyr::mutate(
        by = forcats::fct_inorder(.data$by),
        num_level = forcats::fct_inorder(.data$num_level)
      ) |>
      dplyr::ungroup() |>
      dplyr::select(-dplyr::all_of(outcome_var))
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
    dplyr::relocate(
      dplyr::all_of(c("by", "by_label", "outcome", "outcome_level"))
    )

  # proportion labels
  d$prop_label <- labels_labeller(d$prop)


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

    p_one_outcome <- function(outcome_var) {
      v2 <- # selecting only variables with at least 2 levels
        d |>
        dplyr::filter(outcome == outcome_var) |>
        dplyr::group_by(.data$outcome_level) |>
        dplyr::count(.data$by, name = "n") |>
        dplyr::filter(.data$n >= 2) |>
        dplyr::pull("by") |>
        as.character() |>
        unique()

      res <- v2 |>
        purrr::map(
          ~ paste0("~ `", outcome_var, "` + ", .x) |>
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
  p <-
    .guideR_generic_plot_by(
      d,
      y = "prop",
      outcome = "outcome",
      geom = "bar",
      position = "fill",
      ...,
      show_ci = FALSE,
      show_pvalues = show_pvalues,
      pvalues = pvalues,
      pvalues_labeller = pvalues_labeller,
      pvalues_size = pvalues_size,
      pvalues_y = pvalues_y,
      show_labels = show_labels,
      label = "prop_label",
      y_label = NULL,
      label_position = ggplot2::position_fill(.5),
      labels_size = labels_size,
      labels_color = labels_color,
      show_overall_line = FALSE,
      facet_labeller = facet_labeller,
      flip = flip,
      minimal = minimal,
      free_scale = FALSE,
      cols_facet = cols_facet
    ) +
    ggplot2::aes(fill = .data$outcome_level) +
    ggplot2::scale_y_continuous(
      labels = scales::percent,
      expand = ggplot2::expansion(mult = c(.02, .02))
    ) +
    scale_fill_safe() +
    ggplot2::labs(fill = NULL)

  if (flip)
    p <- p + ggplot2::theme(legend.position = "bottom")

  p
}
