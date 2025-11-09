# Round values while preserve their rounded sum in R

Sometimes, the sum of rounded numbers (e.g., using
[`base::round()`](https://rdrr.io/r/base/Round.html)) is not the same as
their rounded sum.

## Usage

``` r
round_preserve_sum(x, digits = 0)
```

## Source

<https://biostatmatt.com/archives/2902>

## Arguments

- x:

  Numerical vector to sum.

- digits:

  Number of decimals for rounding.

## Value

A numerical vector of same length as `x`.

## Details

This solution applies the following algorithm

- Round down to the specified number of decimal places

- Order numbers by their remainder values

- Increment the specified decimal place of values with *k* largest
  remainders, where *k* is the number of values that must be incremented
  to preserve their rounded sum

## Examples

``` r
sum(c(0.333, 0.333, 0.334))
#> [1] 1
round(c(0.333, 0.333, 0.334), 2)
#> [1] 0.33 0.33 0.33
sum(round(c(0.333, 0.333, 0.334), 2))
#> [1] 0.99
round_preserve_sum(c(0.333, 0.333, 0.334), 2)
#> [1] 0.33 0.33 0.34
sum(round_preserve_sum(c(0.333, 0.333, 0.334), 2))
#> [1] 1
```
