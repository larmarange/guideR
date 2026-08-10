#' Deprecated functions
#' @name deprecated
#' @inheritParams plot_strata_predictions
#' @export
plot_maihda_predictions_by <- function(
    x,
    by = NULL,
    scale = c("response", "link"),
    which = c("null", "adjusted"),
    sort = TRUE
) {
  lifecycle::deprecate_warn(
    "0.12.0",
    "plot_maihda_predictions_by()",
    "plot_maihda_predictions()"
  )
  plot_strata_predictions(
    x = x,
    by = {{ by }},
    n_strata = Inf,
    scale = scale,
    which = which,
    sort = sort
  )
}
