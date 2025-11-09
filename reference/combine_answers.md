# Combine answers of a multiple answers question

Considering a multiple answers question coded as several binary
variables (one per item), create a new variable (list column or
character) combining all positive answers. If defined, use variable
labels (see examples).

## Usage

``` r
combine_answers(data, answers, into, value = NULL, sep = NULL)
```

## Arguments

- data:

  A data frame, data frame extension (e.g. a tibble), or a survey design
  object.

- answers:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  List of variables identifying the different answers of the question.

- into:

  Names of new variables to create as character vector.

- value:

  Value indicating a positive answer. By default, will use the maximum
  observed value and will display a message.

- sep:

  An optional character string to separate the results and return a
  character. If `NULL`, return a list column (see examples).

## Note

If `NA` is observed for at least one item, return `NA`.

## Examples

``` r
d <-
  dplyr::tibble(
    q1a = sample(c("y", "n"), size = 200, replace = TRUE),
    q1b = sample(c("y", "n", "n", NA), size = 200, replace = TRUE),
    q1c = sample(c("y", "y", "n"), size = 200, replace = TRUE),
    q1d = sample("n", size = 200, replace = TRUE)
  )

d |> combine_answers(q1a:q1d, into = "combined")
#> ! Automatically selected value: "y"
#> ℹ To remove this message, please specify `value`.
#> # A tibble: 200 × 5
#>    q1a   q1b   q1c   q1d   combined 
#>    <chr> <chr> <chr> <chr> <list>   
#>  1 n     n     y     n     <chr [1]>
#>  2 y     NA    n     n     <lgl [1]>
#>  3 y     NA    y     n     <lgl [1]>
#>  4 n     y     y     n     <chr [2]>
#>  5 y     y     y     n     <chr [3]>
#>  6 y     n     y     n     <chr [2]>
#>  7 y     n     n     n     <chr [1]>
#>  8 n     y     y     n     <chr [2]>
#>  9 n     NA    y     n     <lgl [1]>
#> 10 y     NA    y     n     <lgl [1]>
#> # ℹ 190 more rows
d |> combine_answers(q1a:q1d, into = "combined", sep = ", ", value = "y")
#> # A tibble: 200 × 5
#>    q1a   q1b   q1c   q1d   combined     
#>    <chr> <chr> <chr> <chr> <chr>        
#>  1 n     n     y     n     q1c          
#>  2 y     NA    n     n     NA           
#>  3 y     NA    y     n     NA           
#>  4 n     y     y     n     q1b, q1c     
#>  5 y     y     y     n     q1a, q1b, q1c
#>  6 y     n     y     n     q1a, q1c     
#>  7 y     n     n     n     q1a          
#>  8 n     y     y     n     q1b, q1c     
#>  9 n     NA    y     n     NA           
#> 10 y     NA    y     n     NA           
#> # ℹ 190 more rows
d |> combine_answers(q1a:q1d, into = "combined", sep = " | ", value = "n")
#> # A tibble: 200 × 5
#>    q1a   q1b   q1c   q1d   combined       
#>    <chr> <chr> <chr> <chr> <chr>          
#>  1 n     n     y     n     q1a | q1b | q1d
#>  2 y     NA    n     n     NA             
#>  3 y     NA    y     n     NA             
#>  4 n     y     y     n     q1a | q1d      
#>  5 y     y     y     n     q1d            
#>  6 y     n     y     n     q1b | q1d      
#>  7 y     n     n     n     q1b | q1c | q1d
#>  8 n     y     y     n     q1a | q1d      
#>  9 n     NA    y     n     NA             
#> 10 y     NA    y     n     NA             
#> # ℹ 190 more rows
```
