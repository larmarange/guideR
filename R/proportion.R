#' Compute proportions
#' @description
#' `proportion()` lets you quickly count observations (like [dplyr::count()])
#' and compute relative proportions. Proportions are computed separately by
#' group (see examples).
#'
#' @param .data A data frame, data frame extension (e.g. a tibble),
#' or a survey design object.
#' @param ... <[`data-masking`][rlang::args_data_masking]> Variable(s) for those
#' computing proportions.
#' @param .by <[`tidy-select`][dplyr::dplyr_tidy_select ]> Optional additional
#' variables to group by (in addition to those eventually previously declared
#' using [dplyr::group_by()]).
#' @export
proportion <- function(.data, ..., .by = NULL) {
  UseMethod("proportion")
}

#' @export
#' @rdname proportion
#' @param .weight <[`data-masking`][rlang::args_data_masking]> Frequency
#' weights. Can be `NULL` or a variable.
#' @param .scale A scaling factor applied to proportion. Use `1` for keeping
#' proportions unchanged.
#' @param .sort If `TRUE`, will show the highest proportions at the top.
#' @param .drop If `TRUE`, will remove empty groups from the output.
#' @param .conf.int If `TRUE`, will estimate confidence intervals with
#' [stats::prop.test()].
#' @param .conf.level Confidence level for the returned confidence intervals.
#' @param .correct Whether Yates' continuity correction should be applied to
#' estimate confidence intervals (see [stats::prop.test()]).
#' @return A tibble with one row per group.
#' @examples
#'
#' # univariable table
#' titanic |> proportion(Class)
#' titanic |> proportion(Class, .sort = TRUE)
#' titanic |> proportion(Class, .conf.int = TRUE)
#' titanic |> proportion(Class, .conf.int = TRUE, .scale = 1)
#'
#' # bivariable table
#' titanic |> proportion(Class, Survived) # proportions of the total
#' titanic |> proportion(Survived, .by = Class) # row proportions
#' titanic |> # equivalent syntax
#'   dplyr::group_by(Class) |>
#'   proportion(Survived)
#'
#' # combining 3 variables or more
#' titanic |> proportion(Class, Sex, Survived)
#' titanic |> proportion(Sex, Survived, .by = Class)
#' titanic |> proportion(Survived, .by = c(Class, Sex))
proportion.data.frame <- function(.data,
                                  ...,
                                  .by = NULL,
                                  .weight = NULL,
                                  .scale = 100,
                                  .sort = FALSE,
                                  .drop = FALSE,
                                  .conf.int = FALSE,
                                  .conf.level = .95,
                                  .correct = TRUE) {
  res <-
    .data |>
    dplyr::group_by(
      dplyr::pick(
        dplyr::all_of(dplyr::group_vars(.data)),
        {{ .by }}
      )
    ) |>
    dplyr::count(..., wt = {{ .weight }}, .drop = .drop, name = "n") |>
    dplyr::mutate(
      N = sum(.data$n),
      prop = proportions(.data$n) * .scale
    )
  if (.sort)
    res <-
      res |>
      dplyr::arrange(dplyr::desc(.data$prop))
  if (.conf.int) {
    res <-
      res |>
      dplyr::rowwise() |>
      dplyr::mutate(
        .ci_prop(
          .data$n,
          .data$N,
          conf.level = .conf.level,
          correct = .correct,
          scale = .scale
        )
      ) |>
      unrowwise()
  }

  res |>
    labelled::copy_labels_from(.data)
}

.ci_prop <- function(n, N,
                     conf.level = .95,
                     correct = TRUE,
                     scale = 1) {
  t <- stats::prop.test(n, N, conf.level = conf.level, correct = correct)
  dplyr::tibble(
    prop_low = t$conf.int[[1]] * scale,
    prop_high = t$conf.int[[2]] * scale
  )
}

#' Remove row-wise grouping
#'
#' Remove row-wise grouping created with [dplyr::rowwise()] while preserving
#' any other grouping declared with [dplyr::group_by()].
#' @param data A data frame, data frame extension (e.g. a tibble), or a
#' lazy data frame.
#' @examples
#' titanic |> dplyr::rowwise()
#' titanic |> dplyr::rowwise() |> unrowwise()
#'
#' titanic |> dplyr::group_by(Sex, Class) |> dplyr::rowwise()
#' titanic |> dplyr::group_by(Sex, Class) |> dplyr::rowwise() |> unrowwise()
#' @export
unrowwise <- function(data) {
  data |>
    dplyr::group_by(dplyr::pick(dplyr::all_of(dplyr::group_vars(data))))
}
