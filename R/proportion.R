#' Compute proportions
#' @description
#' `proportion()` lets you quickly count observations (like [dplyr::count()])
#' and compute relative proportions. Proportions are computed separately by
#' group (see examples).
#'
#' @param data A data frame, data frame extension (e.g. a tibble),
#' or a survey design object.
#' @param ... <[`data-masking`][rlang::args_data_masking]> Variable(s) for those
#' computing proportions.
#' @param .by <[`tidy-select`][dplyr::dplyr_tidy_select ]> Optional additional
#' variables to group by (in addition to those eventually previously declared
#' using [dplyr::group_by()]).
#' @param .weight <[`data-masking`][rlang::args_data_masking]> Frequency
#' weights. Can be `NULL` or a variable.
#' @param .scale A scaling factor applied to proportion. Use `1` for keeping
#' proportions unchanged.
#' @param .sort If `TRUE`, will show the highest proportions at the top.
#' @param .drop If `TRUE`, will remove empty groups from the output.
#' @param .conf.int If `TRUE`, will estimate confidence intervals.
#' @param .conf.level Confidence level for the returned confidence intervals.
#' @param .options Additional arguments passed to [stats::prop.test()]
#' or [srvyr::survey_prop()].
#' @export
proportion <- function(.data,
                       ...,
                       .by = NULL,
                       .weight = NULL,
                       .scale = 100,
                       .sort = FALSE,
                       .drop = FALSE,
                       .conf.int = FALSE,
                       .conf.level = .95,
                       .options = NULL) {
  UseMethod("proportion")
}

#' @export
#' @rdname proportion
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
proportion.data.frame <- function(data,
                                  ...,
                                  .by = NULL,
                                  .weight = NULL,
                                  .scale = 100,
                                  .sort = FALSE,
                                  .drop = FALSE,
                                  .conf.int = FALSE,
                                  .conf.level = .95,
                                  .options = list(correct = TRUE)) {
  res <-
    data |>
    dplyr::group_by(dplyr::pick({{ .by }}), .add = TRUE) |>
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
          options = .options,
          scale = .scale
        )
      ) |>
      unrowwise()
  }

  res |>
    labelled::copy_labels_from(data)
}

.ci_prop <- function(n, N,
                     conf.level = .95,
                     options = list(correct = TRUE),
                     scale = 1) {
  t <- rlang::inject(
    stats::prop.test(n, N, conf.level = conf.level, !!!options)
  )
  dplyr::tibble(
    prop_low = t$conf.int[[1]] * scale,
    prop_high = t$conf.int[[2]] * scale
  )
}

#' @export
#' @rdname proportion
#' @param .prop_method Type of proportion method to use
#' (see [survey::svyciprop()]).
#' @param .df A numeric value indicating the degrees of freedom. The default
#' (`NULL`) uses [survey::degf()] (see [survey::svyciprop()]).
#' @examples
#' ## SURVEY DATA
#'
#' d <- srvyr::as_survey(titanic)
#'
#' # univariable table
#' d |> proportion(Class)
#' d |> proportion(Class, .sort = TRUE)
#' d |> proportion(Class, .conf.int = TRUE)
#' d |> proportion(Class, .conf.int = TRUE, .scale = 1)
#'
#' # bivariable table
#' d |> proportion(Class, Survived) # proportions of the total
#' d |> proportion(Survived, .by = Class) # row proportions
#' d |> dplyr::group_by(Class) |> proportion(Survived)
#'
#' # combining 3 variables or more
#' d |> proportion(Class, Sex, Survived)
#' d |> proportion(Sex, Survived, .by = Class)
#' d |> proportion(Survived, .by = c(Class, Sex))
proportion.survey.design <- function(data,
                                     ...,
                                     .by = NULL,
                                     .weight = NULL,
                                     .scale = 100,
                                     .sort = FALSE,
                                     .conf.int = FALSE,
                                     .conf.level = .95,
                                     .options = NULL) {
  res <-
    data |>
    dplyr::group_by(dplyr::pick({{ .by }}), .add = TRUE) |>
    .add_interact(...) |>
    dplyr::summarise(
      n = srvyr::survey_total(vartype = NULL),
      prop = rlang::inject(
        srvyr::survey_prop(
          proportion = TRUE,
          vartype = if (.conf.int) "ci" else NULL,
          level = .conf.level,
          !!! .options
        )
      ) * .scale
    ) |>
    dplyr::group_by(
      dplyr::pick(
        dplyr::all_of(dplyr::group_vars(data)),
        {{ .by }}
      )
    ) |>
    dplyr::mutate(
      N = sum(.data$n)
    ) |>
    dplyr::relocate(.data$N, .after = .data$n)
  if (.conf.int)
    res <-
      res |>
      dplyr::rename(prop_high = .data$prop_upp)
  if (.sort)
    res <-
      res |>
      dplyr::arrange(dplyr::desc(.data$prop))
  res
}

.add_interact <- function(data, ...) {
  v <-
    tidyselect::eval_select(
      rlang::expr(c(...)),
      data = data$variables,
      allow_rename = FALSE
    ) |>
    names()
  if (length(v) == 0) return(data)
  i <- paste0("srvyr::interact(", paste(v, collapse = ", "), ")")
  data |>
    dplyr::group_by(!! rlang::parse_expr(i), .add = TRUE)
}
