#' Setp with NA
#'
#' @param model A model object.
#' @param ... Additional parameters passed to [stats::step()].
#' @export
step_with_na <- function(model,
                         ...) {
  UseMethod("step_with_na")
}

#' @rdname step_with_na
#' @param full_data Full data frame used for the model, including missing data.
#' @export
step_with_na.default <- function(model,
                                 ...,
                                 full_data = eval(model$call$data)) {
  # data with no NAs
  if (is.null(full_data)) {
    ...data_no_na <-
      model |>
      stats::model.frame()
  } else {
    ...data_no_na <-
      model |>
      stats::get_all_vars(data = full_data) |>
      stats::na.omit()
  }

  # assign ...data_no_na in parent.frame()
  assign("...data_no_na", ...data_no_na, envir = parent.frame())

  # refit the model without NAs
  model_no_na <- stats::update(
    model,
    formula = stats::terms(model),
    data = ...data_no_na
  )

  # apply step()
  model_simplified <- stats::step(model_no_na, ...)

  # recompute simplified model using full data
  if (is.null(full_data)) {
    stats::update(
      model,
      formula = stats::terms(model_simplified)
    )
  } else {
    stats::update(
      model,
      formula = stats::terms(model_simplified),
      data = full_data
    )
  }
}

#' @rdname step_with_na
#' @param design Survey design previously passed to [survey::svyglm()].
#' @export
step_with_na.svyglm <- function(model, ..., design) {
  rlang::check_installed("broom.helpers")
  # list of variables
  variables <- broom.helpers::model_list_variables(
    model,
    only_variable = TRUE
  )
  # design with no na
  design_no_na <- design |>
    srvyr::drop_na(dplyr::any_of(variables))
  # refit the model without NAs
  model_no_na <- stats::update(model, data = design_no_na)
  # apply step()
  model_simplified <- stats::step(model_no_na, ...)
  # recompute simplified model using full data
  stats::update(model, formula = stats::terms(model_simplified))
}
