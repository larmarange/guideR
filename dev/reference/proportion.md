# Compute proportions

`proportion()` lets you quickly count observations (like
[`dplyr::count()`](https://dplyr.tidyverse.org/reference/count.html))
and compute relative proportions. Proportions are computed separately by
group (see examples).

## Usage

``` r
proportion(data, ...)

# S3 method for class 'data.frame'
proportion(
  data,
  ...,
  .by = NULL,
  .na.rm = FALSE,
  .weight = NULL,
  .scale = 100,
  .sort = FALSE,
  .drop = FALSE,
  .drop_na_by = FALSE,
  .conf.int = FALSE,
  .conf.level = 0.95,
  .options = list(correct = TRUE)
)

# S3 method for class 'survey.design'
proportion(
  data,
  ...,
  .by = NULL,
  .na.rm = FALSE,
  .scale = 100,
  .sort = FALSE,
  .drop_na_by = FALSE,
  .conf.int = FALSE,
  .conf.level = 0.95,
  .options = NULL
)

# Default S3 method
proportion(
  data,
  ...,
  .na.rm = FALSE,
  .scale = 100,
  .sort = FALSE,
  .drop = FALSE,
  .conf.int = FALSE,
  .conf.level = 0.95,
  .options = list(correct = TRUE)
)
```

## Arguments

- data:

  A vector, a data frame, data frame extension (e.g. a tibble), or a
  survey design object.

- ...:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>  
  Variable(s) for those computing proportions.

- .by:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>  
  Optional additional variables to group by (in addition to those
  eventually previously declared using
  [`dplyr::group_by()`](https://dplyr.tidyverse.org/reference/group_by.html)).

- .na.rm:

  Should `NA` values be removed (from variables declared in `...`)?

- .weight:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>  
  Frequency weights. Can be `NULL` or a variable.

- .scale:

  A scaling factor applied to proportion. Use `1` for keeping
  proportions unchanged.

- .sort:

  If `TRUE`, will show the highest proportions at the top.

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
  [`stats::prop.test()`](https://rdrr.io/r/stats/prop.test.html) or
  [`srvyr::survey_prop()`](http://gdfe.co/srvyr/reference/survey_mean.md).

## Value

A tibble.

A tibble with one row per group.

## Examples

``` r
# using a vector
titanic$Class |> proportion()
#> # A tibble: 4 × 4
#>   value     n     N  prop
#>   <chr> <int> <int> <dbl>
#> 1 1st     325  2201  14.8
#> 2 2nd     285  2201  12.9
#> 3 3rd     706  2201  32.1
#> 4 Crew    885  2201  40.2

# univariable table
titanic |> proportion(Class)
#> # A tibble: 4 × 4
#>   Class     n     N  prop
#>   <chr> <int> <int> <dbl>
#> 1 1st     325  2201  14.8
#> 2 2nd     285  2201  12.9
#> 3 3rd     706  2201  32.1
#> 4 Crew    885  2201  40.2
titanic |> proportion(Class, .sort = TRUE)
#> # A tibble: 4 × 4
#>   Class     n     N  prop
#>   <chr> <int> <int> <dbl>
#> 1 Crew    885  2201  40.2
#> 2 3rd     706  2201  32.1
#> 3 1st     325  2201  14.8
#> 4 2nd     285  2201  12.9
titanic |> proportion(Class, .conf.int = TRUE)
#> # A tibble: 4 × 6
#>   Class     n     N  prop prop_low prop_high
#>   <chr> <int> <int> <dbl>    <dbl>     <dbl>
#> 1 1st     325  2201  14.8     13.3      16.3
#> 2 2nd     285  2201  12.9     11.6      14.4
#> 3 3rd     706  2201  32.1     30.1      34.1
#> 4 Crew    885  2201  40.2     38.2      42.3
titanic |> proportion(Class, .conf.int = TRUE, .scale = 1)
#> # A tibble: 4 × 6
#>   Class     n     N  prop prop_low prop_high
#>   <chr> <int> <int> <dbl>    <dbl>     <dbl>
#> 1 1st     325  2201 0.148    0.133     0.163
#> 2 2nd     285  2201 0.129    0.116     0.144
#> 3 3rd     706  2201 0.321    0.301     0.341
#> 4 Crew    885  2201 0.402    0.382     0.423

# bivariable table
titanic |> proportion(Class, Survived) # proportions of the total
#> # A tibble: 8 × 5
#>   Class Survived     n     N  prop
#>   <chr> <chr>    <int> <int> <dbl>
#> 1 1st   No         122  2201  5.54
#> 2 1st   Yes        203  2201  9.22
#> 3 2nd   No         167  2201  7.59
#> 4 2nd   Yes        118  2201  5.36
#> 5 3rd   No         528  2201 24.0 
#> 6 3rd   Yes        178  2201  8.09
#> 7 Crew  No         673  2201 30.6 
#> 8 Crew  Yes        212  2201  9.63
titanic |> proportion(Survived, .by = Class) # row proportions
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
titanic |> # equivalent syntax
  dplyr::group_by(Class) |>
  proportion(Survived)
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

# combining 3 variables or more
titanic |> proportion(Class, Sex, Survived)
#> # A tibble: 16 × 6
#>    Class Sex    Survived     n     N   prop
#>    <chr> <chr>  <chr>    <int> <int>  <dbl>
#>  1 1st   Female No           4  2201  0.182
#>  2 1st   Female Yes        141  2201  6.41 
#>  3 1st   Male   No         118  2201  5.36 
#>  4 1st   Male   Yes         62  2201  2.82 
#>  5 2nd   Female No          13  2201  0.591
#>  6 2nd   Female Yes         93  2201  4.23 
#>  7 2nd   Male   No         154  2201  7.00 
#>  8 2nd   Male   Yes         25  2201  1.14 
#>  9 3rd   Female No         106  2201  4.82 
#> 10 3rd   Female Yes         90  2201  4.09 
#> 11 3rd   Male   No         422  2201 19.2  
#> 12 3rd   Male   Yes         88  2201  4.00 
#> 13 Crew  Female No           3  2201  0.136
#> 14 Crew  Female Yes         20  2201  0.909
#> 15 Crew  Male   No         670  2201 30.4  
#> 16 Crew  Male   Yes        192  2201  8.72 
titanic |> proportion(Sex, Survived, .by = Class)
#> # A tibble: 16 × 6
#> # Groups:   Class [4]
#>    Class Sex    Survived     n     N   prop
#>    <chr> <chr>  <chr>    <int> <int>  <dbl>
#>  1 1st   Female No           4   325  1.23 
#>  2 1st   Female Yes        141   325 43.4  
#>  3 1st   Male   No         118   325 36.3  
#>  4 1st   Male   Yes         62   325 19.1  
#>  5 2nd   Female No          13   285  4.56 
#>  6 2nd   Female Yes         93   285 32.6  
#>  7 2nd   Male   No         154   285 54.0  
#>  8 2nd   Male   Yes         25   285  8.77 
#>  9 3rd   Female No         106   706 15.0  
#> 10 3rd   Female Yes         90   706 12.7  
#> 11 3rd   Male   No         422   706 59.8  
#> 12 3rd   Male   Yes         88   706 12.5  
#> 13 Crew  Female No           3   885  0.339
#> 14 Crew  Female Yes         20   885  2.26 
#> 15 Crew  Male   No         670   885 75.7  
#> 16 Crew  Male   Yes        192   885 21.7  
titanic |> proportion(Survived, .by = c(Class, Sex))
#> # A tibble: 16 × 6
#> # Groups:   Class, Sex [8]
#>    Class Sex    Survived     n     N  prop
#>    <chr> <chr>  <chr>    <int> <int> <dbl>
#>  1 1st   Female No           4   145  2.76
#>  2 1st   Female Yes        141   145 97.2 
#>  3 1st   Male   No         118   180 65.6 
#>  4 1st   Male   Yes         62   180 34.4 
#>  5 2nd   Female No          13   106 12.3 
#>  6 2nd   Female Yes         93   106 87.7 
#>  7 2nd   Male   No         154   179 86.0 
#>  8 2nd   Male   Yes         25   179 14.0 
#>  9 3rd   Female No         106   196 54.1 
#> 10 3rd   Female Yes         90   196 45.9 
#> 11 3rd   Male   No         422   510 82.7 
#> 12 3rd   Male   Yes         88   510 17.3 
#> 13 Crew  Female No           3    23 13.0 
#> 14 Crew  Female Yes         20    23 87.0 
#> 15 Crew  Male   No         670   862 77.7 
#> 16 Crew  Male   Yes        192   862 22.3 

# missing values
dna <- titanic
dna$Survived[c(1:20, 500:530)] <- NA
dna |> proportion(Survived)
#> # A tibble: 3 × 4
#>   Survived     n     N  prop
#>   <chr>    <int> <int> <dbl>
#> 1 No        1439  2201 65.4 
#> 2 Yes        711  2201 32.3 
#> 3 NA          51  2201  2.32
dna |> proportion(Survived, .na.rm = TRUE)
#> # A tibble: 2 × 4
#>   Survived     n     N  prop
#>   <chr>    <int> <int> <dbl>
#> 1 No        1439  2150  66.9
#> 2 Yes        711  2150  33.1

# \donttest{
## SURVEY DATA ------------------------------------------------------

ds <- srvyr::as_survey(titanic)

# univariable table
ds |> proportion(Class)
#> # A tibble: 4 × 4
#>   Class     n     N  prop
#>   <chr> <dbl> <dbl> <dbl>
#> 1 1st     325  2201  14.8
#> 2 2nd     285  2201  12.9
#> 3 3rd     706  2201  32.1
#> 4 Crew    885  2201  40.2
ds |> proportion(Class, .sort = TRUE)
#> # A tibble: 4 × 4
#>   Class     n     N  prop
#>   <chr> <dbl> <dbl> <dbl>
#> 1 Crew    885  2201  40.2
#> 2 3rd     706  2201  32.1
#> 3 1st     325  2201  14.8
#> 4 2nd     285  2201  12.9
ds |> proportion(Class, .conf.int = TRUE)
#> # A tibble: 4 × 6
#>   Class     n     N  prop prop_low prop_high
#>   <chr> <dbl> <dbl> <dbl>    <dbl>     <dbl>
#> 1 1st     325  2201  14.8     13.3      16.3
#> 2 2nd     285  2201  12.9     11.6      14.4
#> 3 3rd     706  2201  32.1     30.2      34.1
#> 4 Crew    885  2201  40.2     38.2      42.3
ds |> proportion(Class, .conf.int = TRUE, .scale = 1)
#> # A tibble: 4 × 6
#>   Class     n     N  prop prop_low prop_high
#>   <chr> <dbl> <dbl> <dbl>    <dbl>     <dbl>
#> 1 1st     325  2201 0.148    0.133     0.163
#> 2 2nd     285  2201 0.129    0.116     0.144
#> 3 3rd     706  2201 0.321    0.302     0.341
#> 4 Crew    885  2201 0.402    0.382     0.423

# bivariable table
ds |> proportion(Class, Survived) # proportions of the total
#> # A tibble: 8 × 5
#>   Class Survived     n     N  prop
#>   <chr> <chr>    <dbl> <dbl> <dbl>
#> 1 1st   No         122  2201  5.54
#> 2 1st   Yes        203  2201  9.22
#> 3 2nd   No         167  2201  7.59
#> 4 2nd   Yes        118  2201  5.36
#> 5 3rd   No         528  2201 24.0 
#> 6 3rd   Yes        178  2201  8.09
#> 7 Crew  No         673  2201 30.6 
#> 8 Crew  Yes        212  2201  9.63
ds |> proportion(Survived, .by = Class) # row proportions
#> # A tibble: 8 × 5
#> # Groups:   Class [4]
#>   Class Survived     n     N  prop
#>   <chr> <chr>    <dbl> <dbl> <dbl>
#> 1 1st   No         122   325  37.5
#> 2 1st   Yes        203   325  62.5
#> 3 2nd   No         167   285  58.6
#> 4 2nd   Yes        118   285  41.4
#> 5 3rd   No         528   706  74.8
#> 6 3rd   Yes        178   706  25.2
#> 7 Crew  No         673   885  76.0
#> 8 Crew  Yes        212   885  24.0
ds |> dplyr::group_by(Class) |> proportion(Survived)
#> # A tibble: 8 × 5
#> # Groups:   Class [4]
#>   Class Survived     n     N  prop
#>   <chr> <chr>    <dbl> <dbl> <dbl>
#> 1 1st   No         122   325  37.5
#> 2 1st   Yes        203   325  62.5
#> 3 2nd   No         167   285  58.6
#> 4 2nd   Yes        118   285  41.4
#> 5 3rd   No         528   706  74.8
#> 6 3rd   Yes        178   706  25.2
#> 7 Crew  No         673   885  76.0
#> 8 Crew  Yes        212   885  24.0

# combining 3 variables or more
ds |> proportion(Class, Sex, Survived)
#> # A tibble: 16 × 6
#>    Class Sex    Survived     n     N   prop
#>    <chr> <chr>  <chr>    <dbl> <dbl>  <dbl>
#>  1 1st   Female No           4  2201  0.182
#>  2 1st   Female Yes        141  2201  6.41 
#>  3 1st   Male   No         118  2201  5.36 
#>  4 1st   Male   Yes         62  2201  2.82 
#>  5 2nd   Female No          13  2201  0.591
#>  6 2nd   Female Yes         93  2201  4.23 
#>  7 2nd   Male   No         154  2201  7.00 
#>  8 2nd   Male   Yes         25  2201  1.14 
#>  9 3rd   Female No         106  2201  4.82 
#> 10 3rd   Female Yes         90  2201  4.09 
#> 11 3rd   Male   No         422  2201 19.2  
#> 12 3rd   Male   Yes         88  2201  4.00 
#> 13 Crew  Female No           3  2201  0.136
#> 14 Crew  Female Yes         20  2201  0.909
#> 15 Crew  Male   No         670  2201 30.4  
#> 16 Crew  Male   Yes        192  2201  8.72 
ds |> proportion(Sex, Survived, .by = Class)
#> # A tibble: 16 × 6
#> # Groups:   Class [4]
#>    Class Sex    Survived     n     N   prop
#>    <chr> <chr>  <chr>    <dbl> <dbl>  <dbl>
#>  1 1st   Female No           4   325  1.23 
#>  2 1st   Female Yes        141   325 43.4  
#>  3 1st   Male   No         118   325 36.3  
#>  4 1st   Male   Yes         62   325 19.1  
#>  5 2nd   Female No          13   285  4.56 
#>  6 2nd   Female Yes         93   285 32.6  
#>  7 2nd   Male   No         154   285 54.0  
#>  8 2nd   Male   Yes         25   285  8.77 
#>  9 3rd   Female No         106   706 15.0  
#> 10 3rd   Female Yes         90   706 12.7  
#> 11 3rd   Male   No         422   706 59.8  
#> 12 3rd   Male   Yes         88   706 12.5  
#> 13 Crew  Female No           3   885  0.339
#> 14 Crew  Female Yes         20   885  2.26 
#> 15 Crew  Male   No         670   885 75.7  
#> 16 Crew  Male   Yes        192   885 21.7  
ds |> proportion(Survived, .by = c(Class, Sex))
#> # A tibble: 16 × 6
#> # Groups:   Class, Sex [8]
#>    Class Sex    Survived     n     N  prop
#>    <chr> <chr>  <chr>    <dbl> <dbl> <dbl>
#>  1 1st   Female No           4   145  2.76
#>  2 1st   Female Yes        141   145 97.2 
#>  3 1st   Male   No         118   180 65.6 
#>  4 1st   Male   Yes         62   180 34.4 
#>  5 2nd   Female No          13   106 12.3 
#>  6 2nd   Female Yes         93   106 87.7 
#>  7 2nd   Male   No         154   179 86.0 
#>  8 2nd   Male   Yes         25   179 14.0 
#>  9 3rd   Female No         106   196 54.1 
#> 10 3rd   Female Yes         90   196 45.9 
#> 11 3rd   Male   No         422   510 82.7 
#> 12 3rd   Male   Yes         88   510 17.3 
#> 13 Crew  Female No           3    23 13.0 
#> 14 Crew  Female Yes         20    23 87.0 
#> 15 Crew  Male   No         670   862 77.7 
#> 16 Crew  Male   Yes        192   862 22.3 

# missing values
dsna <- srvyr::as_survey(dna)
dsna |> proportion(Survived)
#> # A tibble: 3 × 4
#>   Survived     n     N  prop
#>   <chr>    <dbl> <dbl> <dbl>
#> 1 No        1439  2201 65.4 
#> 2 Yes        711  2201 32.3 
#> 3 NA          51  2201  2.32
dsna |> proportion(Survived, .na.rm = TRUE)
#> # A tibble: 2 × 4
#>   Survived     n     N  prop
#>   <chr>    <dbl> <dbl> <dbl>
#> 1 No        1439  2150  66.9
#> 2 Yes        711  2150  33.1
# }
```
