## Test environments

* local R installation (windows 11): R 4.4.2
* macos-latest (on github actions): R-release
* windows-latest (on github actions): R-release
* ubuntu-latest  (on github actions): R-devel, R-release, R-oldrel-1

cf. https://github.com/larmarange/broom.helpers/actions/workflows/R-CMD-check.yaml

## R CMD check results

0 errors | 0 warnings | 0 note

* This is a new release.
* Description field has been completed and expanded.
* `\dontrun{}` has been replaced by `\donttest{}`, except for
  `install_dependencies()`
* The purpose of `install_dependencies()` is explicitly to install, and only
  to install, the packages required by a project. The name of the function is
  explicit. This function is not called within tests and vignette. The example
  use specifically `\dontrun{}` to avoid the function to be called within the
  examples (as done in the documentation of `remotes::install_cran()`).
