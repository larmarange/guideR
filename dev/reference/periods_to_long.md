# Transform a data frame from period format to long format

Transform a data frame from period format to long format

## Usage

``` r
periods_to_long(
  data,
  start,
  stop,
  time_step = 1,
  time_name = "time",
  keep = FALSE
)
```

## Arguments

- data:

  A data frame, or a data frame extension (e.g. a tibble).

- start:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>  
  Time variable indicating the beginning of each row

- stop:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>  
  Optional time variable indicating the end of each row. If not
  provided, it will be derived from the dataset, considering that each
  row ends at the beginning of the next one.

- time_step:

  (numeric) Desired value for the time variable.

- time_name:

  (character) Name of the time variable.

- keep:

  (logical) Should start and stop variable be kept in the results?

## Value

A tibble.

## See also

[`long_to_periods()`](https://larmarange.github.io/guideR/dev/reference/long_to_periods.md)

## Examples

``` r
d <- dplyr::tibble(
  patient = c(1, 2, 3, 3),
  begin = c(0, 2, 0, 3),
  end = c(6, 4, 2, 8),
  covar = c("no", "yes", "no", "yes")
)
d
#> # A tibble: 4 × 4
#>   patient begin   end covar
#>     <dbl> <dbl> <dbl> <chr>
#> 1       1     0     6 no   
#> 2       2     2     4 yes  
#> 3       3     0     2 no   
#> 4       3     3     8 yes  

d |> periods_to_long(start = begin, stop = end)
#> # A tibble: 19 × 3
#>    patient  time covar
#>      <dbl> <dbl> <chr>
#>  1       1     0 no   
#>  2       1     1 no   
#>  3       1     2 no   
#>  4       1     3 no   
#>  5       1     4 no   
#>  6       1     5 no   
#>  7       1     6 no   
#>  8       2     2 yes  
#>  9       2     3 yes  
#> 10       2     4 yes  
#> 11       3     0 no   
#> 12       3     1 no   
#> 13       3     2 no   
#> 14       3     3 yes  
#> 15       3     4 yes  
#> 16       3     5 yes  
#> 17       3     6 yes  
#> 18       3     7 yes  
#> 19       3     8 yes  
d |> periods_to_long(start = begin, stop = end, time_step = 5)
#> # A tibble: 6 × 3
#>   patient  time covar
#>     <dbl> <dbl> <chr>
#> 1       1     0 no   
#> 2       1     5 no   
#> 3       2     2 yes  
#> 4       3     0 no   
#> 5       3     3 yes  
#> 6       3     8 yes  
```
