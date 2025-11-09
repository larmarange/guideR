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
install_dependencies(dependencies = NULL, ask = TRUE)
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

## Value

(Invisibly) A data frame with information about the installed
package(s).

## Examples

``` r
if (FALSE) { # \dontrun{
install_dependencies()
} # }
```
