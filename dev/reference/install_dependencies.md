# Install / Update project dependencies

This function uses
[`renv::dependencies()`](https://rstudio.github.io/renv/reference/dependencies.html)
to identify R package dependencies in a project and then calls
[`pak::pkg_install()`](https://pak.r-lib.org/reference/pkg_install.html)
to install / update these packages. If some packages are not found, the
function will install those available and returns a message indicated
packages not installed/updated.

## Usage

``` r
install_dependencies(
  dependencies = NULL,
  ask = TRUE,
  dev = FALSE,
  engine = c("utils", "pak")
)
```

## Arguments

- dependencies:

  An optional list of dependencies. If `NULL`, will be determined with
  [`renv::dependencies()`](https://rstudio.github.io/renv/reference/dependencies.html).
  If equal to `"old"`, will use the list returned by
  [`utils::old.packages()`](https://rdrr.io/r/utils/update.packages.html).

- ask:

  Whether to ask for confirmation when installing a different version of
  a package that is already installed. Installations that only add new
  packages never require confirmation.

- dev:

  If `TRUE`, install required dependencies plus optional and development
  dependencies. If `FALSE`, install only required dependencies.

- engine:

  If `"utils"`, uses
  [`utils::install.packages()`](https://rdrr.io/r/utils/install.packages.html)
  (on Linux could be useful if using \#'
  [`rspm`](https://cran4linux.github.io/rspm/) or
  [`CoprManager`](https://github.com/cran4linux/cran2copr)). If `"pak"`,
  uses
  [`pak::pkg_install()`](https://pak.r-lib.org/reference/pkg_install.html).

## Value

(Invisibly) A data frame with information about the installed package(s)
or a vector with the list of installed packages.

## Examples

``` r
if (FALSE) { # \dontrun{
install_dependencies()
} # }
```
