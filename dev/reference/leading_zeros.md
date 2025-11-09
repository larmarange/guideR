# Add leading zeros

Add leading zeros

## Usage

``` r
leading_zeros(x, left_digits = NULL, digits = 0, prefix = "", suffix = "", ...)
```

## Arguments

- x:

  a numeric vector

- left_digits:

  number of digits before decimal point, automatically computed if not
  provided

- digits:

  number of digits after decimal point

- prefix, suffix:

  Symbols to display before and after value

- ...:

  additional parameters passed to
  [`base::formatC()`](https://rdrr.io/r/base/formatc.html), as
  `big.mark` or `decimal.mark`

## Value

A character vector of the same length as `x`.

## See also

[`base::formatC()`](https://rdrr.io/r/base/formatc.html),
[`base::sprintf()`](https://rdrr.io/r/base/sprintf.html)

## Examples

``` r
v <- c(2, 103.24, 1042.147, 12.4566, NA)
leading_zeros(v)
#> [1] "0002" "0103" "1042" "0012" "  NA"
leading_zeros(v, digits = 1)
#> [1] "0002.0" "0103.2" "1042.1" "0012.5" "    NA"
leading_zeros(v, left_digits = 6, big.mark = " ")
#> [1] "000 002" "000 103" "001 042" "000 012" "     NA"
leading_zeros(c(0, 6, 12, 18), prefix = "M")
#> [1] "M00" "M06" "M12" "M18"
```
