#' Utilities for `gtsummary`
#'
#' Utilities for tables generated with [gtsummary][gtsummary::gtsummary]
#'
#' @param x A `gtsummary` object.
#' @keywords utilities
#' @name gtsummary_utilities
#' @export
#' @seealso [gtsummary::modify_bold()], [gtsummary::modify_italic()],
#' [gtsummary::modify_indent()]
#' @examplesIf rlang::is_installed("gtsummary")
#' tbl <-
#'   gtsummary::trial |>
#'   gtsummary::tbl_summary(
#'     include = c(stage, grade, age, trt, response, death)
#'   ) |>
#'   gtsummary::add_variable_group_header(
#'     header = "Clinical situation at diagnosis",
#'     variables = c(stage, grade, age)
#'   ) |>
#'   gtsummary::add_variable_group_header(
#'     header = "Treatment and outcome",
#'     variables = c(trt, response, death)
#'   )
#' tbl
#'
#' tbl |>
#'   bold_variable_group_headers() |>
#'   gtsummary::italicize_labels() |>
#'   indent_levels(indent = 8L)
bold_variable_group_headers <- function(x) {
  rlang::check_installed("gtsummary")
  x |>
    gtsummary::modify_bold(
      columns = dplyr::all_of("label"),
      rows = .data$row_type == "variable_group"
    )
}

#' @rdname gtsummary_utilities
#' @export
italicize_variable_group_headers <- function(x) {
  rlang::check_installed("gtsummary")
  x |>
    gtsummary::modify_italic(
      columns = dplyr::all_of("label"),
      rows = .data$row_type == "variable_group"
    )
}

#' @rdname gtsummary_utilities
#' @param indent An integer indicating how many space to indent text.
#' @export
indent_levels <- function(x, indent = 8L) {
  rlang::check_installed("gtsummary")
  x |>
    gtsummary::modify_indent(
      columns = dplyr::all_of("label"),
      rows = .data$row_type %in% c("level", "missing"),
      indent = indent
    )
}

#' @rdname gtsummary_utilities
#' @export
indent_labels <- function(x, indent = 4L) {
  rlang::check_installed("gtsummary")
  x |>
    gtsummary::modify_indent(
      columns = dplyr::all_of("label"),
      rows = .data$row_type == "label",
      indent = indent
    )
}
