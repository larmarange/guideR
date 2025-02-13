#' Plot observed vs predicted distribution of a fitted model
#' @param model A statistical model.
#' @details
#' Has been tested with [stats::lm()] and [stats::glm()] models. It may work
#' with other types of models, but without any warranty.
#' @return A `ggplot2` plot.
#' @export
#' @keywords models
#' @examples
#' # a linear model
#' mod <- lm(Sepal.Length ~ Sepal.Width + Species, data = iris)
#' mod |> observed_vs_theoretical()
#'
#' # a logistic regression
#' mod <- glm(
#'   as.factor(Survived) ~ Class + Sex,
#'   data = titanic,
#'   family = binomial()
#' )
#' mod |> observed_vs_theoretical()
observed_vs_theoretical <- function(model) {
  observed <- stats::model.response(stats::model.frame(model))
  theoretical <- stats::simulate(model, nsim = 1)
  theoretical <- theoretical[[1]]
  df <- dplyr::tibble(
    status = c(
      rep.int("observed", length(observed)),
      rep.int("theoretical", length(theoretical))
    ),
    values = c(observed, theoretical)
  )
  if (is.numeric(observed) && any(observed != as.integer(observed))) {
    ggplot2::ggplot(df) +
      ggplot2::aes(x = .data[["values"]], fill = .data[["status"]]) +
      ggplot2::geom_density(
        alpha = .5,
        position = "identity"
      ) +
      ggplot2::theme_light() +
      ggplot2::labs(fill = NULL)
  } else {
    ggplot2::ggplot(df) +
      ggplot2::aes(x = .data[["values"]], fill = .data[["status"]]) +
      ggplot2::geom_bar(
        alpha = .5,
        position = "identity"
      ) +
      ggplot2::theme_light() +
      ggplot2::theme(
        panel.grid.major.x = ggplot2::element_blank(),
        panel.grid.minor.x = ggplot2::element_blank()
      ) +
      ggplot2::labs(fill = NULL)
  }
}
