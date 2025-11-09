# Cut a continuous variable in quartiles

Convenient function to quickly cut a numeric vector into quartiles, i.e.
by applying `cut(x, breaks = fivenum(x))`. Variable label is preserved
by `cut_quartiles()`.

## Usage

``` r
cut_quartiles(x, include.lowest = TRUE, ...)
```

## Arguments

- x:

  a numeric vector which is to be converted to a factor by cutting.

- include.lowest:

  logical, indicating if an ‘x\[i\]’ equal to the lowest (or highest,
  for `right = FALSE`) ‘breaks’ value should be included.

- ...:

  further arguments passed to
  [`base::cut()`](https://rdrr.io/r/base/cut.html).

## Examples

``` r
mtcars$mpg |> cut_quartiles() |> summary()
#> [10.4,15.3] (15.3,19.2] (19.2,22.8] (22.8,33.9] 
#>           8           9           8           7 
```
