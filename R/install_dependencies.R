#' Install / Update project dependencies
#'
#' This function uses [renv::dependencies()] to identify R package dependencies
#' in a project and then calls [pak::pkg_install()] to install / update these
#' packages. If some packages are not found, the function will install those
#' available and returns a message indicated packages not installed/updated.
#'
#' @param dependencies An optional list of dependencies. If `NULL`, will be
#' determined with [renv::dependencies()]. If equal to `"old"`, will use the
#' list returned by [utils::old.packages()].
#' @param ask Whether to ask for confirmation when installing a different
#' version of a package that is already installed. Installations that only add
#' new packages never require confirmation.
#' @param dev If `TRUE`, install required dependencies plus optional and
#' development dependencies. If `FALSE`, install only required dependencies.
#' @param engine If `"utils"`, uses #' [utils::install.packages()] (on Linux
#' could be useful if using #' [`rspm`](https://cran4linux.github.io/rspm/) or
#' [`CoprManager`](https://github.com/cran4linux/cran2copr)). If `"pak"`, uses
#' [pak::pkg_install()].
#' @export
#' @return (Invisibly) A data frame with information about the installed
#' package(s) or a vector with the list of installed packages.
#' @keywords utilities
#' @examples
#' \dontrun{
#' install_dependencies()
#' }
install_dependencies <- function(
  dependencies = NULL,
  ask = TRUE,
  dev = FALSE,
  engine = c("utils", "pak")
) {
  if (length(dependencies) == 1 && dependencies == "old") {
    dependencies <- utils::old.packages()[, 1] |> sort()
    cli::cli_alert_info("Old packages:")
    cli::cli_li(dependencies)
  }

  if (is.null(dependencies)) {
    dependencies <- renv::dependencies() |>
      purrr::pluck("Package") |>
      unique() |>
      sort()
    cli::cli_alert_info("Found dependencies:")
    cli::cli_li(dependencies)
  }

  engine <- match.arg(engine)

  if (engine == "pak") {
    rlang::check_installed("pak")
    pak::meta_update()
    m <- pak::meta_list()

    r <- dependencies[dependencies %in% m$package]
    d <- ifelse(dev, TRUE, NA)
    if (length(r) > 0)
      r <- r |>
        pak::pkg_install(upgrade = TRUE, ask = ask, dependencies = d)

    missing <- dependencies[!dependencies %in% m$package]
    if (length(missing) > 0)
      cli::cli_alert_danger("Packages {.pkg {missing}} not installed/updated.")
  }

  if (engine == "utils") {
    m <- utils::available.packages() |> rownames()
    r <- dependencies[dependencies %in% m] |> sort()

    o <- utils::old.packages()[, 1] # old packages
    i <- utils::installed.packages() |> rownames() # installed packages
    tbi <- r[!r %in% i] # not installed
    tbu <- r[r %in% o] # too old

    d <- ifelse(
      dev,
      c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances"),
      NA
    )
    if (length(tbi) > 0)
      cli::cli_alert_info("Packages to install: {.pkg {tbi}}.")
    if (length(tbu) > 0)
      cli::cli_alert_info("Packages to update: {.pkg {tbu}}.")
    if (length(tbi) + length(tbu) > 0) {
      if (ask)
        if (!.get_confirmation2())
          cli::cli_abort("Installation aborted.")
      c(tbi, tbu) |>
        utils::install.packages(dependencies = d)
    } else {
      cli::cli_alert_info("No package to install or update.")
    }

    missing <- dependencies[!dependencies %in% m]
    if (length(missing) > 0)
      cli::cli_alert_danger("Packages {.pkg {missing}} not installed/updated.")
  }

  invisible(r)
}

.get_confirmation2 <- function(q = "? Do you want to continue (Y/n) ") {
  ans <- readline(q)
  tolower(ans) %in% c("", "y", "yes", "yeah", "yep")
}
