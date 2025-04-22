#' Install / Update project dependencies
#'
#' This function uses [renv::dependencies()] to identify R package dependencies
#' in a project and then calls [pak::pkg_install()] to install / update these
#' packages. If some packages are not found, the function will install those
#' available and returns a message indicated packages not installed/updated.
#'
#' @param ask Whether to ask for confirmation when installing a different
#' version of a package that is already installed. Installations that only add
#' new packages never require confirmation.
#' @export
#' @return (Invisibly) A data frame with information about the installed
#' package(s).
#' @keywords utilities
#' @examples
#' \dontrun{
#' install_dependencies()
#' }
install_dependencies <- function(ask = TRUE) {
  d <- renv::dependencies() |>
    purrr::pluck("Package") |>
    unique()
  pak::meta_update()
  m <- pak::meta_list()

  r <- d[d %in% m$package] |>
    pak::pkg_install(upgrade = TRUE, ask = ask)

  missing <- d[!d %in% m$package]
  if (length(missing) > 0)
    cli::cli_alert_danger("Packages {.pkg {missing}} not installed/updated.")

  invisible(r)
}
