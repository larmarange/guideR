# An example of bootstrapped MAIHDA analysis

A list as returned by
[`calculate_partially_adjusted_maihda()`](https://larmarange.github.io/guideR/dev/reference/tbl_maihda.md)

## Usage

``` r
bootstrapped_maihda
```

## Format

An object of class `list` of length 5.

## Examples

``` r
# m <- MAIHDA::maihda(
#   Survived ~ Age + Sex + Class + (1 | Age:Sex:Class),
#   data = titanic,
#   family = binomial
# )
#
# bootstrapped_maihda <-
#   m |>
#   calculate_partially_adjusted_maihda(
#     bootstrap_pcv = TRUE,
#     bootstrap_vpc = TRUE
#   )

bootstrapped_maihda |>
  tbl_maihda(exponentiate = TRUE)


  


Characteristic
```
