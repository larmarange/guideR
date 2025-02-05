
<!-- README.md is generated from README.Rmd. Please edit that file -->

# guideR

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/larmarange/guideR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/larmarange/guideR/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/larmarange/guideR/graph/badge.svg)](https://app.codecov.io/gh/larmarange/gguideR)
[![CRAN
status](https://www.r-pkg.org/badges/version/guideR)](https://CRAN.R-project.org/package=guideR)
<!-- badges: end -->

`guideR` package is a companion for the manual *guide-R : Guide pour
l’analyse de données d’enquêtes avec R* available at
<https://larmarange.github.io/guide-R/>.

`guideR` implements miscellaneous functions introduced in *guide-R* to
facilitate statistical analysis.

## Installation & Documentation

To install **stable version**:

``` r
install.packages("guideR")
```

Documentation of stable version: <https://larmarange.github.io/guideR/>

To install **development version**:

``` r
# install.packages("pak")
pak::pak("larmarange/guideR")
```

Documentation of development version:
<https://larmarange.github.io/guideR/dev/>

## Compute proportions

``` r
titanic <- Titanic |> dplyr::as_tibble() |> tidyr::uncount(n)

library(guideR)

titanic |> proportion(Class, .conf.int = TRUE)
#> # A tibble: 4 × 6
#> # Rowwise: 
#>   Class     n     N  prop prop_low prop_high
#>   <chr> <int> <int> <dbl>    <dbl>     <dbl>
#> 1 1st     325  2201  14.8     13.3      16.3
#> 2 2nd     285  2201  12.9     11.6      14.4
#> 3 3rd     706  2201  32.1     30.1      34.1
#> 4 Crew    885  2201  40.2     38.2      42.3

titanic |> proportion(Survived, .by = Class)
#> # A tibble: 8 × 5
#> # Groups:   Class [4]
#>   Class Survived     n     N  prop
#>   <chr> <chr>    <int> <int> <dbl>
#> 1 1st   No         122   325  37.5
#> 2 1st   Yes        203   325  62.5
#> 3 2nd   No         167   285  58.6
#> 4 2nd   Yes        118   285  41.4
#> 5 3rd   No         528   706  74.8
#> 6 3rd   Yes        178   706  25.2
#> 7 Crew  No         673   885  76.0
#> 8 Crew  Yes        212   885  24.0
```
