#' Compute means, standard deviations and confidence intervals by sub-groups
#' @description
#' `mean_sd()` lets you quickly compute mean and standard deviation by
#' sub-groups. Use `.conf.int = TRUE` to also return confidence intervals of the
#' mean.
#' @param data A vector, a data frame, data frame extension (e.g. a tibble),
#' or a survey design object.
#' @param ... <[`data-masking`][rlang::args_data_masking]> Variable(s) for those
#' computing mean and standard deviation.
#' @param .by <[`tidy-select`][dplyr::dplyr_tidy_select]> Optional additional
#' variables to group by (in addition to those eventually previously declared
#' using [dplyr::group_by()]).
#' @param .drop If `TRUE`, will remove empty groups from the output.
#' @param .drop_na_by If `TRUE`, will remove any `NA` values observed in the
#' `.by` variables (or variables defined with [dplyr::group_by()]).
#' @param .conf.int If `TRUE`, will estimate confidence intervals.
#' @param .conf.level Confidence level for the returned confidence intervals.
#' @param .options Additional arguments passed to [stats::t.test()]
#' or [srvyr::survey_mean()].
#' @return A tibble. Column `"n"` reports the number of valid observations
#' and `"missing"` the number of missing (`NA`) observations, unweighted for
#' survey objects.
#' @keywords univar
#' @export
mean_sd <- function(data,
                    ...) {
  UseMethod("mean_sd")
}

#' @export
#' @rdname mean_sd
#' @return A tibble with one row per group.
#' @examples
#' # using a vector
#' iris$Petal.Length |> mean_sd()
#'
#' # one variable
#' iris |> mean_sd(Petal.Length)
#' iris |> mean_sd(Petal.Length, .conf.int = TRUE)
#' iris |> mean_sd(Petal.Length, .by = Species)
#' mtcars |> mean_sd(mpg, .by = c(cyl, gear))
#'
#' # two variables
#' iris |> mean_sd(Petal.Length, Petal.Width)
#' iris |> mean_sd(dplyr::pick(dplyr::starts_with("Petal")), .by = Species)
#'
#' # missing values
#' d <- iris
#' d$Petal.Length[1:10] <- NA
#' d |> mean_sd(Petal.Length)
#' d |> mean_sd(Petal.Length, .by = Species)
mean_sd.data.frame <- function(data,
                               ...,
                               .by = NULL,
                               .drop = FALSE,
                               .drop_na_by = FALSE,
                               .conf.int = FALSE,
                               .conf.level = .95,
                               .options = NULL) {
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
        mean = mean(.data[[x]], na.rm = TRUE),
        .data[[x]] |>
          .ci_mean(
            conf.int = .conf.int,
            conf.level = .conf.level,
            options = .options
          ),
        sd = sd(.data[[x]], na.rm = TRUE),
        n = sum(!is.na(.data[[x]])),
        missing = sum(is.na(.data[[x]]))
      ) |>
      dplyr::relocate(.data$x)
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

.ci_mean <- function(x,
                     conf.int = TRUE,
                     conf.level = .95,
                     options = NULL) {
  if (!conf.int) return(NULL)
  t <- rlang::inject(
    stats::t.test(x, conf.level = conf.level, !!!options)
  )
  dplyr::tibble(
    mean_low = t$conf.int[[1]],
    mean_high = t$conf.int[[2]]
  )
}

#' @export
#' @rdname mean_sd
#' @examples
#'
#' \donttest{
#' ## SURVEY DATA ------------------------------------------------------
#'
#' ds <- srvyr::as_survey(iris)
#' ds |> mean_sd(Petal.Length, .by = Species, .conf.int = TRUE)
#' }
mean_sd.survey.design <- function(data,
                                  ...,
                                  .by = NULL,
                                  .drop = FALSE,
                                  .drop_na_by = FALSE,
                                  .conf.int = FALSE,
                                  .conf.level = .95,
                                  .options = NULL) {
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
        mean = rlang::inject(
          srvyr::survey_mean(
            .data[[x]],
            vartype = if (.conf.int) c("ci", "var") else "var",
            level = .conf.level,
            !!! .options
          )
        ),
        n = sum(!is.na(.data[[x]])),
        missing = sum(is.na(.data[[x]]))
      ) |>
      dplyr::mutate(sd = sqrt(.data$mean_var)) |>
      dplyr::relocate(.data$sd, .before = .data$n) |>
      dplyr::select(-.data$mean_var) |>
      dplyr::relocate(.data$x)
  }

  res <- v |>
    purrr::map(f) |>
    dplyr::bind_rows()

  if (.drop_na_by)
    res <-
    res |>
    tidyr::drop_na(dplyr::all_of(dplyr::group_vars(res)))

  if (.conf.int)
    res <-
      res |>
      dplyr::rename(mean_high = .data$mean_upp)

  res |>
    labelled::copy_labels_from(data |> dplyr::select({{ .by }}))
}

#' @rdname mean_sd
#' @export
mean_sd.default <- function(data,
                            ...,
                            .drop = FALSE,
                            .conf.int = FALSE,
                            .conf.level = .95,
                            .options = list(correct = TRUE)) {
  if (!is.atomic(data))
    cli::cli_abort("Objects of class `{class(data)}` are not covered.")
  data <- dplyr::tibble(vector = data)
  data |>
    mean_sd(
      .data$vector,
      .drop = .drop,
      .conf.int = .conf.int,
      .conf.level = .conf.level,
      .options = .options
    )
}
