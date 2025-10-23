#' Display a data frame dictionary in RStudio viewer
#'
#' Generate an interactive dictionary based on [labelled::look_for()].
#'
#' @param data a data frame, a tibble or a survey object
#' (if `NULL`, will use the text you currently select in **RStudio**, usefull
#' if the function is called through the corresponding addin)
#' @param details add details about each variable (see [labelled::look_for()])
#' @export
#' @keywords utilities
#' @examplesIf interactive()
#' iris |> view_dictionary()
view_dictionary <- function(
  data = NULL,
  details = c("basic", "none", "full")
) {
  rlang::check_installed("gt")
  if (!is.null(data)) {
    if (is.character(data) && length(data) == 1) {
      df_name <- data
      data <- get(df_name)
    } else {
      df_name <- deparse(substitute(data))
    }
  } else {
    if (rstudioapi::isAvailable()) {
      # if text is selected, use that
      context <- rstudioapi::getActiveDocumentContext()

      # Set the default data to use based on the selection.
      df_name <- context$selection[[1]]$text

      if (!is.null(df_name) && df_name != "" && exists(df_name)) {
        data <- get(df_name)
      }

      if (!is.null(df_name) && df_name != "" && !exists(df_name)) {
        cli::cli_abort("Object {.arg {df_name}} not found.")
      }

      if (is.null(df_name) || df_name == "") {
        cli::cli_abort("No data frame selected.")
      }
    }
  }

  if (!inherits(data, c("data.frame", "survey.design", "svyrep.design")))
    cli::cli_abort(
      "{.arg data} should be a data frame, a tibble or a survey object."
    )

  data |>
    labelled::look_for(details = details) |>
    to_DT(caption = df_name)
}

#' @rdname view_dictionary
#' @export
view_detailed_dictionary <- function(data = NULL) {
  if (is.null(data)) {
    view_dictionary(details = "full")
  } else {
    df_name <- deparse(substitute(data))
    if (exists(df_name)) {
      view_dictionary(df_name, details = "full")
    } else {
      view_dictionary(data, details = "full")
    }

  }
}

#' @rdname view_dictionary
#' @param x a tibble returned by `look_for()`
#' @param caption an optional caption for the table
#' @param column_labels Optional column labels
#' @export
#' @examplesIf rlang::is_installed(c("DT", "htmltools", "htmlwidgets"))
#' iris |> labelled::look_for(details = TRUE) |> to_DT()
to_DT <- function(
  x,
  caption = NULL,
  column_labels = list(
    pos = "#",
    variable = "Variable",
    col_type = "Type",
    label = "Variable label",
    values = "Values",
    missing = "Missing values",
    unique_values = "Unique values",
    na_values = "User-defined missings (values)",
    na_range = "User-defined missings (range)"
  )
) {
  rlang::check_installed(c("DT", "htmltools", "htmlwidgets"))
  if (!inherits(x, "look_for"))
    cli::cli_abort("{.arg x} shoud be a {.class look_for} object.")
  if (nrow(x) == 0) return(gt::gt(data.frame()))

  x <-
    x |>
    dplyr::mutate(
      variable = paste0("<strong>", .data$variable, "</strong>"),
      label = dplyr::if_else(is.na(.data$label), "\u2014", .data$label)
    )
  if ("levels" %in% names(x)) {
    x <-
      x |>
      dplyr::mutate(
        levels = purrr::map(
          .data$levels,
          function(x) {
            if (is.null(x)) return("")
            paste(x, collapse = "<br />")
          }
        )
      )
  }
  if ("value_labels" %in% names(x)) {
    x <-
      x |>
      dplyr::mutate(
        value_labels = purrr::map(
          .data$value_labels,
          function(x) {
            if (is.null(x)) return("")
            paste(
              labelled::names_prefixed_by_values(x) |>
                stringr::str_replace("] ", "]&nbsp;"),
              collapse = "<br />"
            )
          }
        )
      )
  }
  if ("range" %in% names(x)) {
    x <-
      x |>
      dplyr::mutate(
        range = purrr::map(
          .data$range,
          function(x) {
            if (is.null(x)) return("")
            paste(x, collapse = " \u2013 ")
          }
        )
      )
  } else {
    x$range <- ""
    x$range <- as.list(x$range)
  }
  if (all(c("levels", "value_labels") %in% names(x))) {
    x$values <-
      dplyr::case_when(
        x$value_labels != "" ~ x$value_labels,
        x$levels != "" ~ x$levels,
        TRUE ~ x$range
      ) |>
      unlist()
  }

  keep <- c(
    "pos", "variable", "col_type", "label",
    "values", "missing", "unique_values"
  )

  if ("na_values" %in% names(x) && !is.null(unlist(x$na_values)))
    keep <- c(keep, "na_values")
  if ("na_range" %in% names(x) && !is.null(unlist(x$na_range)))
    keep <- c(keep, "na_range")

  if (!is.null(caption))
    caption <- htmltools::HTML(paste0("<h1>", caption, "</h1>"))

  x <-
    x |>
    dplyr::select(
      dplyr::any_of(keep)
    ) |>
    labelled::set_variable_labels(.labels = column_labels, .strict = FALSE)
  x |>
    DT::datatable(
      escape = FALSE,
      class = "stripe compact hover",
      rownames = FALSE,
      colnames = labelled::var_label(x, unlist = TRUE) |> unname(),
      caption = caption,
      extensions = "Buttons",
      options = list(
        paging = FALSE,
        initComplete = htmlwidgets::JS(
          "function(settings, json) {",
          "$(\'body\').css({\'font-family\': \'\"Source Sans Pro\",Calibri,Candara,Arial,sans-serif\'});", # nolint
          "}"
        ),
        dom = "Bfrtip",
        buttons = c("csv", "excel", "pdf")
      )
    ) |>
    DT::formatStyle(seq_len(ncol(x)), "vertical-align" = "top")
}
