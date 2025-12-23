#' Add potential relevant interactions using `step()`
#'
#' `r lifecycle::badge("experimental")`
#' Add potential relevant interactions to a model usind [stats::step()]. The
#' function extract the formula of the model, identifies all potential
#' interactions and pass them as the **upper** component of the `scope` argument
#' to [stats::step()]. The current model formula is passed as the **lower**
#' component of `scope`.
#'
#' @param model A model object.
#' @param ... Additional parameters passed to [stats::step()].
#' @return The stepwise-selected model.
#' @keywords models
#' @export
add_interactions_by_step <- function(model, ...) {
  UseMethod("add_interactions_by_step")
}

#' @rdname add_interactions_by_step
#' @export
#' @examples
#' mod <- glm(as.factor(Survived) ~ ., data = titanic, family = binomial())
#' mod |> add_interactions_by_step()
add_interactions_by_step.default <- function(model, ...) {
  f_low <-
    model |>
    stats::terms() |>
    rlang::f_text()
  f_upp <-
    f_low |>
    stringr::str_replace_all("\\+", "\\*") |>
    stats::reformulate()
  f_low <-
    f_low |>
    stats::reformulate()
  step(model, scope = list(lower = f_low, upper = f_upp), ...)
}
