# Comparison tests considering `NA` as values to be compared

`is_different()` and `is_equal()` performs comparison tests, considering
`NA` values as legitimate values (see examples).

## Usage

``` r
is_different(x, y)

is_equal(x, y)

cumdifferent(x)

num_cycle(x)
```

## Arguments

- x, y:

  Vectors to be compared.

## Value

A vector of the same length as `x`.

## Details

`cum_different()` allows to identify groups of continuous rows that have
the same value. `num_cycle()` could be used to identify sub-groups that
respect a certain condition (see examples).

`is_equal(x, y)` is equivalent to
`(x == y & !is.na(x) & !is.na(y)) | (is.na(x) & is.na(y))`, and
`is_different(x, y)` is equivalent to
`(x != y & !is.na(x) & !is.na(y)) | xor(is.na(x), is.na(y))`.

## Examples

``` r
v <- c("a", "b", NA)
is_different(v, "a")
#> [1] FALSE  TRUE  TRUE
is_different(v, NA)
#> [1]  TRUE  TRUE FALSE
is_equal(v, "a")
#> [1]  TRUE FALSE FALSE
is_equal(v, NA)
#> [1] FALSE FALSE  TRUE
d <- dplyr::tibble(group = c("a", "a", "b", "b", "a", "b", "c", "a"))
d |>
  dplyr::mutate(
    subgroup = cumdifferent(group),
    sub_a = num_cycle(group == "a")
  )
#> # A tibble: 8 × 3
#>   group subgroup sub_a
#>   <chr>    <int> <int>
#> 1 a            1     1
#> 2 a            1     1
#> 3 b            2    NA
#> 4 b            2    NA
#> 5 a            3     2
#> 6 b            4    NA
#> 7 c            5    NA
#> 8 a            6     3
```
