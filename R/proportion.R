#' Compute proportions
#' @description
#' `proportion()` lets you quickly count observations (like [dplyr::count()])
#' and compute relative proportions. Proportions are computed separately by
#' group (see examples).
#'
#' @param data A vector, a data frame, data frame extension (e.g. a tibble),
#' or a survey design object.
#' @param ... <[`data-masking`][rlang::args_data_masking]> Variable(s) for those
#' computing proportions.
#' @param .by <[`tidy-select`][dplyr::dplyr_tidy_select ]> Optional additional
#' variables to group by (in addition to those eventually previously declared
#' using [dplyr::group_by()]).
#' @param .na.rm Should `NA` values be removed
#' (from variables declared in `...`)?
#' @param .weight <[`data-masking`][rlang::args_data_masking]> Frequency
#' weights. Can be `NULL` or a variable.
#' @param .scale A scaling factor applied to proportion. Use `1` for keeping
#' proportions unchanged.
#' @param .sort If `TRUE`, will show the highest proportions at the top.
#' @param .drop If `TRUE`, will remove empty groups from the output.
#' @param .drop_na_by If `TRUE`, will remove any `NA` values observed in the
#' `.by` variables (or variables defined with [dplyr::group_by()]).
#' @param .conf.int If `TRUE`, will estimate confidence intervals.
#' @param .conf.level Confidence level for the returned confidence intervals.
#' @param .options Additional arguments passed to [stats::prop.test()]
#' or [srvyr::survey_prop()].
#' @return A tibble.
#' @keywords univar
#' @export
proportion <- function(data,
                       ...) {
  UseMethod("proportion")
}

#' @export
#' @rdname proportion
#' @return A tibble with one row per group.
#' @examples
#' # using a vector
#' titanic$Class |> proportion()
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
#'
#' # missing values
#' dna <- titanic
#' dna$Survived[c(1:20, 500:530)] <- NA
#' dna |> proportion(Survived)
#' dna |> proportion(Survived, .na.rm = TRUE)
proportion.data.frame <- function(data,
                                  ...,
                                  .by = NULL,
                                  .na.rm = FALSE,
                                  .weight = NULL,
                                  .scale = 100,
                                  .sort = FALSE,
                                  .drop = FALSE,
                                  .drop_na_by = FALSE,
                                  .conf.int = FALSE,
                                  .conf.level = .95,
                                  .options = list(correct = TRUE)) {
  if (.na.rm)
    data <-
      data |>
      tidyr::drop_na(...)

  res <-
    data |>
    dplyr::group_by(dplyr::pick({{ .by }}), .add = TRUE, .drop = FALSE) |>
    dplyr::count(..., wt = {{ .weight }}, .drop = .drop, name = "n") |>
    dplyr::mutate(
      N = sum(.data$n),
      prop = proportions(.data$n) * .scale
    )

  if (.drop_na_by)
    res <-
      res |>
      tidyr::drop_na(dplyr::all_of(dplyr::group_vars(res)))
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
  if (N == 0)
    return(dplyr::tibble(prop_low = NA_real_, prop_high = NA_real_))
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
#' @examples
#'
#' \donttest{
#' ## SURVEY DATA ------------------------------------------------------
#'
#' ds <- srvyr::as_survey(titanic)
#'
#' # univariable table
#' ds |> proportion(Class)
#' ds |> proportion(Class, .sort = TRUE)
#' ds |> proportion(Class, .conf.int = TRUE)
#' ds |> proportion(Class, .conf.int = TRUE, .scale = 1)
#'
#' # bivariable table
#' ds |> proportion(Class, Survived) # proportions of the total
#' ds |> proportion(Survived, .by = Class) # row proportions
#' ds |> dplyr::group_by(Class) |> proportion(Survived)
#'
#' # combining 3 variables or more
#' ds |> proportion(Class, Sex, Survived)
#' ds |> proportion(Sex, Survived, .by = Class)
#' ds |> proportion(Survived, .by = c(Class, Sex))
#'
#' # missing values
#' dsna <- srvyr::as_survey(dna)
#' dsna |> proportion(Survived)
#' dsna |> proportion(Survived, .na.rm = TRUE)
#' }
proportion.survey.design <- function(data,
                                     ...,
                                     .by = NULL,
                                     .na.rm = FALSE,
                                     .scale = 100,
                                     .sort = FALSE,
                                     .drop_na_by = FALSE,
                                     .conf.int = FALSE,
                                     .conf.level = .95,
                                     .options = NULL) {
  if (.na.rm) {
    v <-
      tidyselect::eval_select(
        rlang::expr(c(...)),
        data = data$variables,
        allow_rename = FALSE
      ) |>
      names()
    data <-
      data |>
      tidyr::drop_na(dplyr::all_of(v))
  }
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
  if (.drop_na_by)
    res <-
      res |>
      tidyr::drop_na(dplyr::all_of(dplyr::group_vars(res)))
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

#' @rdname proportion
#' @export
proportion.default <- function(data,
                               ...,
                               .na.rm = FALSE,
                               .scale = 100,
                               .sort = FALSE,
                               .drop = FALSE,
                               .conf.int = FALSE,
                               .conf.level = .95,
                               .options = list(correct = TRUE)) {
  if (!is.atomic(data))
    cli::cli_abort("Objects of class `{class(data)}` are not covered.")
  data <- dplyr::tibble(value = data)
  data |>
    proportion(
      .data$value,
      .na.rm = .na.rm,
      .scale = .scale,
      .sort = .sort,
      .drop = .drop,
      .conf.int = .conf.int,
      .conf.level = .conf.level,
      .options = .options
    )
}
