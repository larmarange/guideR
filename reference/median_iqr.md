# Compute median, quartiles and interquartile range by sub-groups

`median_iqr()` lets you quickly compute median, quartiles and
interquartile range by sub-groups. Use `.outliers = TRUE` to also return
whiskers and outliers (see
[`ggplot2::stat_boxplot()`](https://ggplot2.tidyverse.org/reference/geom_boxplot.html)).

## Usage

``` r
median_iqr(data, ...)

# S3 method for class 'data.frame'
median_iqr(
  data,
  ...,
  .by = NULL,
  .drop = FALSE,
  .drop_na_by = FALSE,
  .outliers = FALSE
)

# S3 method for class 'survey.design'
median_iqr(
  data,
  ...,
  .by = NULL,
  .drop = FALSE,
  .drop_na_by = FALSE,
  .outliers = FALSE
)

# Default S3 method
median_iqr(data, ..., .drop = FALSE, .outliers = FALSE)
```

## Arguments

- data:

  A vector, a data frame, data frame extension (e.g. a tibble), or a
  survey design object.

- ...:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  Variable(s) for which to compute median, quartiles and interquartile
  range.

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

- .outliers:

  If `TRUE`, will estimate whiskers and outliers.

## Value

A tibble. Column `"n"` reports the number of valid observations and
`"missing"` the number of missing (`NA`) observations, unweighted for
survey objects.

A tibble with one row per group.

## Examples

``` r
# using a vector
iris$Petal.Length |> median_iqr()
#> # A tibble: 1 × 9
#>   x      median   min    q1    q3   max   iqr     n missing
#>   <chr>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <int>   <int>
#> 1 vector   4.35     1   1.6   5.1   6.9   3.5   150       0

# one variable
iris |> median_iqr(Petal.Length)
#> # A tibble: 1 × 9
#>   x            median   min    q1    q3   max   iqr     n missing
#>   <chr>         <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <int>   <int>
#> 1 Petal.Length   4.35     1   1.6   5.1   6.9   3.5   150       0
iris |> median_iqr(Petal.Length, .outliers = TRUE)
#> # A tibble: 1 × 12
#>   x       median   min    q1    q3   max   iqr whisker_low whisker_high outliers
#>   <chr>    <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>       <dbl>        <dbl> <list>  
#> 1 Petal.…   4.35     1   1.6   5.1   6.9   3.5           1          6.9 <dbl>   
#> # ℹ 2 more variables: n <int>, missing <int>
iris |> median_iqr(Petal.Length, .by = Species)
#> # A tibble: 3 × 10
#>   x            Species    median   min    q1    q3   max   iqr     n missing
#>   <chr>        <fct>       <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <int>   <int>
#> 1 Petal.Length setosa       1.5    1     1.4  1.58   1.9 0.175    50       0
#> 2 Petal.Length versicolor   4.35   3     4    4.6    5.1 0.600    50       0
#> 3 Petal.Length virginica    5.55   4.5   5.1  5.88   6.9 0.775    50       0
mtcars |> median_iqr(mpg, .by = c(cyl, gear))
#> # A tibble: 8 × 11
#> # Groups:   cyl [3]
#>   x       cyl  gear median   min    q1    q3   max   iqr     n missing
#>   <chr> <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <int>   <int>
#> 1 mpg       4     3   21.5  21.5  21.5  21.5  21.5 0         1       0
#> 2 mpg       4     4   25.8  21.4  22.8  30.9  33.9 8.1       8       0
#> 3 mpg       4     5   28.2  26    27.1  29.3  30.4 2.20      2       0
#> 4 mpg       6     3   19.8  18.1  18.9  20.6  21.4 1.65      2       0
#> 5 mpg       6     4   20.1  17.8  18.8  21    21   2.15      4       0
#> 6 mpg       6     5   19.7  19.7  19.7  19.7  19.7 0         1       0
#> 7 mpg       8     3   15.2  10.4  14.0  16.6  19.2 2.57     12       0
#> 8 mpg       8     5   15.4  15    15.2  15.6  15.8 0.400     2       0

# two variables
iris |> median_iqr(Petal.Length, Petal.Width)
#> # A tibble: 2 × 9
#>   x            median   min    q1    q3   max   iqr     n missing
#>   <chr>         <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <int>   <int>
#> 1 Petal.Length   4.35   1     1.6   5.1   6.9   3.5   150       0
#> 2 Petal.Width    1.3    0.1   0.3   1.8   2.5   1.5   150       0
iris |> median_iqr(dplyr::pick(dplyr::starts_with("Petal")), .by = Species)
#> # A tibble: 6 × 10
#>   x            Species    median   min    q1    q3   max   iqr     n missing
#>   <chr>        <fct>       <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <int>   <int>
#> 1 Petal.Length setosa       1.5    1     1.4  1.58   1.9 0.175    50       0
#> 2 Petal.Length versicolor   4.35   3     4    4.6    5.1 0.600    50       0
#> 3 Petal.Length virginica    5.55   4.5   5.1  5.88   6.9 0.775    50       0
#> 4 Petal.Width  setosa       0.2    0.1   0.2  0.3    0.6 0.1      50       0
#> 5 Petal.Width  versicolor   1.3    1     1.2  1.5    1.8 0.3      50       0
#> 6 Petal.Width  virginica    2      1.4   1.8  2.3    2.5 0.5      50       0

# missing values
d <- iris
d$Petal.Length[1:10] <- NA
d |> median_iqr(Petal.Length)
#> # A tibble: 1 × 9
#>   x            median   min    q1    q3   max   iqr     n missing
#>   <chr>         <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <int>   <int>
#> 1 Petal.Length    4.5     1  1.67   5.1   6.9  3.42   140      10
d |> median_iqr(Petal.Length, .by = Species)
#> # A tibble: 3 × 10
#>   x            Species    median   min    q1    q3   max   iqr     n missing
#>   <chr>        <fct>       <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <int>   <int>
#> 1 Petal.Length setosa       1.5    1    1.37  1.6    1.9 0.225    40      10
#> 2 Petal.Length versicolor   4.35   3    4     4.6    5.1 0.600    50       0
#> 3 Petal.Length virginica    5.55   4.5  5.1   5.88   6.9 0.775    50       0

# \donttest{
## SURVEY DATA ------------------------------------------------------

ds <- srvyr::as_survey(iris)
ds |> median_iqr(Petal.Length, .by = Species, .outliers = TRUE)
#> # A tibble: 3 × 13
#> # Rowwise: 
#>   x        Species median   min    q1    q3   max   iqr whisker_low whisker_high
#>   <chr>    <fct>    <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>       <dbl>        <dbl>
#> 1 Petal.L… setosa     1.5   1     1.4   1.6   1.9 0.200         1.1          1.9
#> 2 Petal.L… versic…    4.3   3     4     4.6   5.1 0.600         3.3          5.1
#> 3 Petal.L… virgin…    5.5   4.5   5.1   5.9   6.9 0.800         4.5          6.9
#> # ℹ 3 more variables: outliers <list>, n <int>, missing <int>
# }
```
