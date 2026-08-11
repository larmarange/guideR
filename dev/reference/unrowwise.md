# Remove row-wise grouping

Remove row-wise grouping created with
[`dplyr::rowwise()`](https://dplyr.tidyverse.org/reference/rowwise.html)
while preserving any other grouping declared with
[`dplyr::group_by()`](https://dplyr.tidyverse.org/reference/group_by.html).

## Usage

``` r
unrowwise(data)
```

## Arguments

- data:

  A tibble.

## Value

A tibble.

## Examples

``` r
titanic |> dplyr::rowwise()
#> # A tibble: 2,201 × 5
#> # Rowwise: 
#>    Class Sex   Age   Survived     n
#>    <fct> <fct> <fct> <fct>    <dbl>
#>  1 3rd   Male  Child No          35
#>  2 3rd   Male  Child No          35
#>  3 3rd   Male  Child No          35
#>  4 3rd   Male  Child No          35
#>  5 3rd   Male  Child No          35
#>  6 3rd   Male  Child No          35
#>  7 3rd   Male  Child No          35
#>  8 3rd   Male  Child No          35
#>  9 3rd   Male  Child No          35
#> 10 3rd   Male  Child No          35
#> # ℹ 2,191 more rows
titanic |> dplyr::rowwise() |> unrowwise()
#> # A tibble: 2,201 × 5
#>    Class Sex   Age   Survived     n
#>    <fct> <fct> <fct> <fct>    <dbl>
#>  1 3rd   Male  Child No          35
#>  2 3rd   Male  Child No          35
#>  3 3rd   Male  Child No          35
#>  4 3rd   Male  Child No          35
#>  5 3rd   Male  Child No          35
#>  6 3rd   Male  Child No          35
#>  7 3rd   Male  Child No          35
#>  8 3rd   Male  Child No          35
#>  9 3rd   Male  Child No          35
#> 10 3rd   Male  Child No          35
#> # ℹ 2,191 more rows

titanic |> dplyr::group_by(Sex, Class) |> dplyr::rowwise()
#> # A tibble: 2,201 × 5
#> # Rowwise:  Sex, Class
#>    Class Sex   Age   Survived     n
#>    <fct> <fct> <fct> <fct>    <dbl>
#>  1 3rd   Male  Child No          35
#>  2 3rd   Male  Child No          35
#>  3 3rd   Male  Child No          35
#>  4 3rd   Male  Child No          35
#>  5 3rd   Male  Child No          35
#>  6 3rd   Male  Child No          35
#>  7 3rd   Male  Child No          35
#>  8 3rd   Male  Child No          35
#>  9 3rd   Male  Child No          35
#> 10 3rd   Male  Child No          35
#> # ℹ 2,191 more rows
titanic |> dplyr::group_by(Sex, Class) |> dplyr::rowwise() |> unrowwise()
#> # A tibble: 2,201 × 5
#> # Groups:   Sex, Class [8]
#>    Class Sex   Age   Survived     n
#>    <fct> <fct> <fct> <fct>    <dbl>
#>  1 3rd   Male  Child No          35
#>  2 3rd   Male  Child No          35
#>  3 3rd   Male  Child No          35
#>  4 3rd   Male  Child No          35
#>  5 3rd   Male  Child No          35
#>  6 3rd   Male  Child No          35
#>  7 3rd   Male  Child No          35
#>  8 3rd   Male  Child No          35
#>  9 3rd   Male  Child No          35
#> 10 3rd   Male  Child No          35
#> # ℹ 2,191 more rows
```
