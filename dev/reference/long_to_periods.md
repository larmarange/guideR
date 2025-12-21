# Transform a data frame from long format to period format

Transform a data frame from long format to period format

## Usage

``` r
long_to_periods(data, id, start, stop = NULL, by = NULL)
```

## Arguments

- data:

  A data frame, or a data frame extension (e.g. a tibble).

- id:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>  
  Column containing individual ids

- start:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>  
  Time variable indicating the beginning of each row

- stop:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>  
  Optional time variable indicating the end of each row. If not
  provided, it will be derived from the dataset, considering that each
  row ends at the beginning of the next one.

- by:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>  
  Co-variables to consider (optional)

## Value

A tibble.

## See also

[`periods_to_long()`](https://larmarange.github.io/guideR/dev/reference/periods_to_long.md)

## Examples

``` r
d <- dplyr::tibble(
  patient = c(1, 2, 3, 3, 4, 4, 4),
  begin = c(0, 0, 0, 1, 0, 36, 39),
  end = c(50, 6, 1, 16, 36, 39, 45),
  covar = c("no", "no", "no", "yes", "no", "yes", "yes")
)
d
#> # A tibble: 7 × 4
#>   patient begin   end covar
#>     <dbl> <dbl> <dbl> <chr>
#> 1       1     0    50 no   
#> 2       2     0     6 no   
#> 3       3     0     1 no   
#> 4       3     1    16 yes  
#> 5       4     0    36 no   
#> 6       4    36    39 yes  
#> 7       4    39    45 yes  

d |> long_to_periods(id = patient, start = begin, stop = end)
#> # A tibble: 4 × 3
#> # Groups:   patient [4]
#>   patient begin   end
#>     <dbl> <dbl> <dbl>
#> 1       1     0    50
#> 2       2     0     6
#> 3       3     0    16
#> 4       4     0    45
d |> long_to_periods(id = patient, start = begin, stop = end, by = covar)
#> # A tibble: 6 × 4
#> # Groups:   patient [4]
#>   patient begin   end covar
#>     <dbl> <dbl> <dbl> <chr>
#> 1       1     0    50 no   
#> 2       2     0     6 no   
#> 3       3     0     1 no   
#> 4       3     1    16 yes  
#> 5       4     0    36 no   
#> 6       4    36    45 yes  

# If stop not provided, it is deduced.
# However, it considers that observation ends at the last start time.
d |> long_to_periods(id = patient, start = begin)
#> # A tibble: 2 × 3
#> # Groups:   patient [2]
#>   patient begin .stop
#>     <dbl> <dbl> <dbl>
#> 1       3     0     1
#> 2       4     0    39
```
