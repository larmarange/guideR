# Compute means, standard deviations and confidence intervals by sub-groups

`mean_sd()` lets you quickly compute mean and standard deviation by
sub-groups. Use `.conf.int = TRUE` to also return confidence intervals
of the mean.

## Usage

``` r
mean_sd(data, ...)

# S3 method for class 'data.frame'
mean_sd(
  data,
  ...,
  .by = NULL,
  .drop = FALSE,
  .drop_na_by = FALSE,
  .conf.int = FALSE,
  .conf.level = 0.95,
  .options = NULL
)

# S3 method for class 'survey.design'
mean_sd(
  data,
  ...,
  .by = NULL,
  .drop = FALSE,
  .drop_na_by = FALSE,
  .conf.int = FALSE,
  .conf.level = 0.95,
  .options = NULL
)

# Default S3 method
mean_sd(
  data,
  ...,
  .drop = FALSE,
  .conf.int = FALSE,
  .conf.level = 0.95,
  .options = NULL
)
```

## Arguments

- data:

  A vector, a data frame, data frame extension (e.g. a tibble), or a
  survey design object.

- ...:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  Variable(s) for which to compute mean and standard deviation.

- .by:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  Optional additional variables to group by (in addition to those
  eventually previously declared using
  [`dplyr::group_by()`](https://dplyr.tidyverse.org/reference/group_by.html)).

- .drop:

  If `TRUE`, will remove empty groups from the output.

- .drop_na_by:

  If `TRUE`, will remove any `NA` values observed in the `.by` variables
  (or variables defined with
  [`dplyr::group_by()`](https://dplyr.tidyverse.org/reference/group_by.html)).

- .conf.int:

  If `TRUE`, will estimate confidence intervals.

- .conf.level:

  Confidence level for the returned confidence intervals.

- .options:

  Additional arguments passed to
  [`stats::t.test()`](https://rdrr.io/r/stats/t.test.html) or
  [`srvyr::survey_mean()`](http://gdfe.co/srvyr/reference/survey_mean.md).

## Value

A tibble. Column `"n"` reports the number of valid observations and
`"missing"` the number of missing (`NA`) observations, unweighted for
survey objects.

A tibble with one row per group.

## Examples

``` r
# using a vector
iris$Petal.Length |> mean_sd()
#> # A tibble: 1 × 5
#>   x       mean    sd     n missing
#>   <chr>  <dbl> <dbl> <int>   <int>
#> 1 vector  3.76  1.77   150       0

# one variable
iris |> mean_sd(Petal.Length)
#> # A tibble: 1 × 5
#>   x             mean    sd     n missing
#>   <chr>        <dbl> <dbl> <int>   <int>
#> 1 Petal.Length  3.76  1.77   150       0
iris |> mean_sd(Petal.Length, .conf.int = TRUE)
#> # A tibble: 1 × 7
#>   x             mean mean_low mean_high    sd     n missing
#>   <chr>        <dbl>    <dbl>     <dbl> <dbl> <int>   <int>
#> 1 Petal.Length  3.76     3.47      4.04  1.77   150       0
iris |> mean_sd(Petal.Length, .by = Species)
#> # A tibble: 3 × 6
#>   x            Species     mean    sd     n missing
#>   <chr>        <fct>      <dbl> <dbl> <int>   <int>
#> 1 Petal.Length setosa      1.46 0.174    50       0
#> 2 Petal.Length versicolor  4.26 0.470    50       0
#> 3 Petal.Length virginica   5.55 0.552    50       0
mtcars |> mean_sd(mpg, .by = c(cyl, gear))
#> # A tibble: 8 × 7
#> # Groups:   cyl [3]
#>   x       cyl  gear  mean     sd     n missing
#>   <chr> <dbl> <dbl> <dbl>  <dbl> <int>   <int>
#> 1 mpg       4     3  21.5 NA         1       0
#> 2 mpg       4     4  26.9  4.81      8       0
#> 3 mpg       4     5  28.2  3.11      2       0
#> 4 mpg       6     3  19.8  2.33      2       0
#> 5 mpg       6     4  19.8  1.55      4       0
#> 6 mpg       6     5  19.7 NA         1       0
#> 7 mpg       8     3  15.0  2.77     12       0
#> 8 mpg       8     5  15.4  0.566     2       0

# two variables
iris |> mean_sd(Petal.Length, Petal.Width)
#> # A tibble: 2 × 5
#>   x             mean    sd     n missing
#>   <chr>        <dbl> <dbl> <int>   <int>
#> 1 Petal.Length  3.76 1.77    150       0
#> 2 Petal.Width   1.20 0.762   150       0
iris |> mean_sd(dplyr::pick(dplyr::starts_with("Petal")), .by = Species)
#> # A tibble: 6 × 6
#>   x            Species     mean    sd     n missing
#>   <chr>        <fct>      <dbl> <dbl> <int>   <int>
#> 1 Petal.Length setosa     1.46  0.174    50       0
#> 2 Petal.Length versicolor 4.26  0.470    50       0
#> 3 Petal.Length virginica  5.55  0.552    50       0
#> 4 Petal.Width  setosa     0.246 0.105    50       0
#> 5 Petal.Width  versicolor 1.33  0.198    50       0
#> 6 Petal.Width  virginica  2.03  0.275    50       0

# missing values
d <- iris
d$Petal.Length[1:10] <- NA
d |> mean_sd(Petal.Length)
#> # A tibble: 1 × 5
#>   x             mean    sd     n missing
#>   <chr>        <dbl> <dbl> <int>   <int>
#> 1 Petal.Length  3.92  1.71   140      10
d |> mean_sd(Petal.Length, .by = Species)
#> # A tibble: 3 × 6
#>   x            Species     mean    sd     n missing
#>   <chr>        <fct>      <dbl> <dbl> <int>   <int>
#> 1 Petal.Length setosa      1.46 0.187    40      10
#> 2 Petal.Length versicolor  4.26 0.470    50       0
#> 3 Petal.Length virginica   5.55 0.552    50       0

# \donttest{
## SURVEY DATA ------------------------------------------------------

ds <- srvyr::as_survey(iris)
ds |> mean_sd(Petal.Length, .by = Species, .conf.int = TRUE)
#> # A tibble: 3 × 8
#>   x            Species     mean mean_low mean_high     sd     n missing
#>   <chr>        <fct>      <dbl>    <dbl>     <dbl>  <dbl> <int>   <int>
#> 1 Petal.Length setosa      1.46     1.41      1.51 0.0244    50       0
#> 2 Petal.Length versicolor  4.26     4.13      4.39 0.0660    50       0
#> 3 Petal.Length virginica   5.55     5.40      5.71 0.0775    50       0
# }
```
