#' Plot inertia, absolute loss and relative loss from a classification tree
#'
#' @param tree A dendrogram, i.e. an [stats::hclust] object,
#' an [FactoMineR::HCPC] object or an object that can be converted to an
#' [stats::hclust] object with [stats::as.hclust()].
#' @param k_max Maximum number of clusters to return / plot.
#' @export
#' @return A `ggplot2` plot or a `tibble`.
#' @examples
#' hc <- hclust(dist(USArrests))
#' get_inertia_from_tree(hc)
#' plot_inertia_from_tree(hc)
plot_inertia_from_tree <- function(tree, k_max = 15) {
  d <- get_inertia_from_tree(tree, k_max)
  p_inertia <-
    ggplot2::ggplot(d) +
    ggplot2::aes(x = k, y = inertia) +
    ggplot2::geom_step(na.rm = TRUE) +
    ggplot2::ylab("Inertia")
  p_absolute <-
    ggplot2::ggplot(d) +
    ggplot2::aes(x = k, y = absolute_loss) +
    ggplot2::geom_bar(stat = "identity", fill = "#4477AA", na.rm = TRUE) +
    ggplot2::ylab("Absolute loss")
  p_relative <-
    ggplot2::ggplot(d) +
    ggplot2::aes(x = k, y = relative_loss) +
    ggplot2::geom_line(color = "#AA3377", na.rm = TRUE) +
    ggplot2::geom_point(size = 3, color = "#AA3377", na.rm = TRUE) +
    ggplot2::scale_y_continuous(label = scales::percent) +
    ggplot2::ylab("Relative loss")
  patchwork::wrap_plots(
    p_inertia,
    p_absolute,
    p_relative,
    ncol = 1
  ) &
    ggplot2::theme_light() &
    ggplot2::xlab("Number of clusters") &
    ggplot2::scale_x_continuous(
      breaks = d$k,
      minor_breaks = NULL,
      limits = c(1, k_max)
    )
}

#' @rdname plot_inertia_from_tree
#' @export
get_inertia_from_tree <- function(tree, k_max = 15) {
  if (inherits(tree, "HCPC"))
    tree <- tree$call$t$tree
  if (!inherits(tree, "hclust"))
    tree <- stats::as.hclust(tree)
  inertia <- tree$height |>
    sort(decreasing = TRUE) |>
    utils::head(k_max)
  prev_inertia <- dplyr::lag(inertia)
  dplyr::tibble(
    k = seq_along(inertia),
    inertia = inertia,
    absolute_loss = inertia - prev_inertia,
    relative_loss = (inertia - prev_inertia) / prev_inertia
  )
}
