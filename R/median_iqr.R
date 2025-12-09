#' Compute median, quartiles and interquartile range by sub-groups
#' @description
#' `median_iqr()` lets you quickly compute median, quartiles and interquartile
#' range by sub-groups. Use `.outliers = TRUE` to also return whiskers and
#' outliers (see [ggplot2::stat_boxplot()]).
#' @param data A vector, a data frame, data frame extension (e.g. a tibble),
#' or a survey design object.
#' @param ... <[`data-masking`][rlang::args_data_masking]> Variable(s) for which
#' to compute median, quartiles and interquartile range.
#' @param .by <[`tidy-select`][dplyr::dplyr_tidy_select]> Optional additional
#' variables to group by (in addition to those eventually previously declared
#' using [dplyr::group_by()]).
#' @param .drop If `TRUE`, will remove empty groups from the output.
#' @param .drop_na_by If `TRUE`, will remove any `NA` values observed in the
#' `.by` variables (or variables defined with [dplyr::group_by()]).
#' @param .outliers If `TRUE`, will estimate whiskers and outliers.
#' @return A tibble. Column `"n"` reports the number of valid observations
#' and `"missing"` the number of missing (`NA`) observations, unweighted for
#' survey objects.
#' @keywords univar
#' @export
median_iqr <- function(data,
                       ...) {
  UseMethod("median_iqr")
}

#' @export
#' @rdname median_iqr
#' @return A tibble with one row per group.
#' @examples
#' # using a vector
#' iris$Petal.Length |> median_iqr()
#'
#' # one variable
#' iris |> median_iqr(Petal.Length)
#' iris |> median_iqr(Petal.Length, .outliers = TRUE)
#' iris |> median_iqr(Petal.Length, .by = Species)
#' mtcars |> median_iqr(mpg, .by = c(cyl, gear))
#'
#' # two variables
#' iris |> median_iqr(Petal.Length, Petal.Width)
#' iris |> median_iqr(dplyr::pick(dplyr::starts_with("Petal")), .by = Species)
#'
#' # missing values
#' d <- iris
#' d$Petal.Length[1:10] <- NA
#' d |> median_iqr(Petal.Length)
#' d |> median_iqr(Petal.Length, .by = Species)
median_iqr.data.frame <- function(data,
                                  ...,
                                  .by = NULL,
                                  .drop = FALSE,
                                  .drop_na_by = FALSE,
                                  .outliers = FALSE) {
  v <-
    data |>
    dplyr::ungroup() |>
    dplyr::mutate(..., .keep = "none") |>
    colnames()

  data <-
    data |>
    dplyr::mutate(...)

  f <- function(x) {
    data |>
      dplyr::group_by(dplyr::pick({{ .by }}), .add = TRUE, .drop = .drop) |>
      dplyr::summarise(
        x = x,
        .compute_median_iqr(.data[[x]], .outliers = .outliers)
      ) |>
      dplyr::relocate(dplyr::all_of("x"))
  }

  res <- v |>
    purrr::map(f) |>
    dplyr::bind_rows()

  if (.drop_na_by)
    res <-
      res |>
      tidyr::drop_na(dplyr::all_of(dplyr::group_vars(res)))

  res |>
    labelled::copy_labels_from(data |> dplyr::select({{ .by }}))
}

.compute_median_iqr <- function(x, .outliers = FALSE, coef = 1.5) {
  q <- stats::quantile(x, probs = 0:4 / 4, na.rm = TRUE) |> unname()
  iqr <- q[4] - q[2]

  res <- dplyr::tibble(
    median = q[3],
    min = q[1],
    q1 = q[2],
    q3 = q[4],
    max = q[5],
    iqr = iqr
  )

  if (.outliers) {
    is_outlier <- x < (q[2] - coef * iqr) | x > (q[4] + coef * iqr)
    r <- range(x[!is_outlier], na.rm = TRUE)
    res$whisker_low <- r[1]
    res$whisker_high <- r[2]
    res$outliers <- list(x[is_outlier])
  }

  res$n <- sum(!is.na(x))
  res$missing <- sum(is.na(x))

  res
}

#' @export
#' @rdname median_iqr
#' @examples
#'
#' \donttest{
#' ## SURVEY DATA ------------------------------------------------------
#'
#' ds <- srvyr::as_survey(iris)
#' ds |> median_iqr(Petal.Length, .by = Species, .outliers = TRUE)
#' }
median_iqr.survey.design <- function(data,
                                     ...,
                                     .by = NULL,
                                     .drop = FALSE,
                                     .drop_na_by = FALSE,
                                     .outliers = FALSE) {
  v <-
    data |>
    dplyr::ungroup() |>
    dplyr::mutate(..., .keep = "none") |>
    colnames()

  data <-
    data |>
    dplyr::mutate(...)

  f <- function(x) {
    res <-
      data |>
      dplyr::group_by(dplyr::pick({{ .by }}), .add = TRUE, .drop = .drop) |>
      dplyr::summarise(
        x = x,
        q = srvyr::survey_quantile(.data[[x]], 0:4 / 4, vartype = NULL),
        .x = list(.data[[x]]),
        n = sum(!is.na(.data[[x]])),
        missing = sum(is.na(.data[[x]]))
      ) |>
      dplyr::rename(
        median = dplyr::all_of("q_q50"),
        min = dplyr::all_of("q_q00"),
        q1 = dplyr::all_of("q_q25"),
        q3 = dplyr::all_of("q_q75"),
        max = dplyr::all_of("q_q100")
      ) |>
      dplyr::relocate(dplyr::all_of("median"), .before = dplyr::all_of("min")) |>
      dplyr::mutate(iqr = .data$q3 - .data$q1) |>
      dplyr::relocate(dplyr::all_of("x"))

    if (.outliers) {
      res <-
        res |>
        dplyr::rowwise() |>
        dplyr::mutate(
          .compute_outliers(.data$.x, .data$q1, .data$q3)
        )
    }

    res |>
      dplyr::ungroup() |>
      dplyr::select(-dplyr::all_of(".x")) |>
      dplyr::relocate(
        dplyr::all_of(c("n", "missing")),
        .after = dplyr::last_col()
      )
  }

  res <- v |>
    purrr::map(f) |>
    dplyr::bind_rows()

  if (.drop_na_by)
    res <-
      res |>
      tidyr::drop_na(dplyr::all_of(dplyr::group_vars(res)))

  res |>
    labelled::copy_labels_from(data |> dplyr::select({{ .by }}))
}

.compute_outliers <- function(x, q1, q3, coef = 1.5) {
  iqr <- q3 - q1
  is_outlier <- x < (q1 - coef * iqr) | x > (q3 + coef * iqr)
  r <- range(x[!is_outlier], na.rm = TRUE)
  dplyr::tibble(
    whisker_low = r[1],
    whisker_high = r[2],
    outliers = list(x[is_outlier])
  )
}

#' @rdname median_iqr
#' @export
median_iqr.default <- function(data,
                               ...,
                               .drop = FALSE,
                               .outliers = FALSE) {
  if (!is.atomic(data))
    cli::cli_abort("Objects of class `{class(data)}` are not covered.")
  data <- dplyr::tibble(vector = data)
  data |>
    median_iqr(
      dplyr::pick(dplyr::all_of("vector")),
      .drop = .drop,
      .outliers = .outliers
    )
}
