#' Apply `step()`, taking into account missing values
#'
#' When your data contains missing values, concerned observations are removed
#' from a model. However, then at a later stage, you try to apply a descending
#' stepwise approach to reduce your model by minimization of AIC, you may
#' encounter an error because the number of rows has changed.
#'
#' `step_with_na()` applies the following strategy:
#' - recomputes the models using only complete cases;
#' - applies [stats::step()];
#' - recomputes the reduced model using the full original dataset.
#'
#' `step_with_na()` has been tested with [stats::lm()], [stats::glm()],
#' [nnet::multinom()] and [survey::svyglm()]. It may be working with other
#' types of models, but with no warranty.
#'
#' In some cases, it may be necessary to provide the full dataset initially
#' used to estimate the model.
#'
#' `step_with_na()` may not work inside other functions. In that case, you
#' may try to pass `full_data` to the function.
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
#' @examples
#' set.seed(42)
#' d <- titanic |>
#'   dplyr::mutate(
#'     Group = sample(
#'       c("a", "b", NA),
#'       dplyr::n(),
#'       replace = TRUE
#'     )
#'   )
#' mod <- glm(as.factor(Survived) ~ ., data = d, family = binomial())
#' # step(mod) should produce an error
#' mod2 <- step_with_na(mod)
#' mod2
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
#' @examples
#'
#' \dontrun{
#' ## WITH SURVEY ---------------------------------------
#'
#' ds <- d |>
#'   dplyr::mutate(Survived = as.factor(Survived)) |>
#'   srvyr::as_survey()
#' mods <- survey::svyglm(
#'   Survived ~ Class + Group + Sex,
#'   design = ds,
#'   family = quasibinomial()
#' )
#' mod2s <- step_with_na(mods, design = ds)
#' mod2s
#' }
step_with_na.svyglm <- function(model, ..., design) {
  # list of variables
  rlang::check_installed("broom.helpers")
  variables <- broom.helpers::model_list_variables(
    model,
    only_variable = TRUE
  )

  # design with no na
  design_no_na <- design |>
    srvyr::drop_na(dplyr::any_of(variables))

  # refit the model without NAs
  model_no_na <- stats::update(model, design = design_no_na)

  # apply step()
  model_simplified <- stats::step(model_no_na, ...)

  # recompute simplified model using full data
  stats::update(
    model,
    formula = stats::terms(model_simplified),
    design = design
  )
}
