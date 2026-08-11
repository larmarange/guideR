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
#>  1 y     n     y     n     <chr [2]>
#>  2 n     n     y     n     <chr [1]>
#>  3 n     n     n     n     <chr [0]>
#>  4 n     n     n     n     <chr [0]>
#>  5 y     y     n     n     <chr [2]>
#>  6 n     n     y     n     <chr [1]>
#>  7 y     n     y     n     <chr [2]>
#>  8 n     n     y     n     <chr [1]>
#>  9 y     n     y     n     <chr [2]>
#> 10 y     n     y     n     <chr [2]>
#> # ℹ 190 more rows
d |> combine_answers(q1a:q1d, into = "combined", sep = ", ", value = "y")
#> # A tibble: 200 × 5
#>    q1a   q1b   q1c   q1d   combined  
#>    <chr> <chr> <chr> <chr> <chr>     
#>  1 y     n     y     n     "q1a, q1c"
#>  2 n     n     y     n     "q1c"     
#>  3 n     n     n     n     ""        
#>  4 n     n     n     n     ""        
#>  5 y     y     n     n     "q1a, q1b"
#>  6 n     n     y     n     "q1c"     
#>  7 y     n     y     n     "q1a, q1c"
#>  8 n     n     y     n     "q1c"     
#>  9 y     n     y     n     "q1a, q1c"
#> 10 y     n     y     n     "q1a, q1c"
#> # ℹ 190 more rows
d |> combine_answers(q1a:q1d, into = "combined", sep = " | ", value = "n")
#> # A tibble: 200 × 5
#>    q1a   q1b   q1c   q1d   combined             
#>    <chr> <chr> <chr> <chr> <chr>                
#>  1 y     n     y     n     q1b | q1d            
#>  2 n     n     y     n     q1a | q1b | q1d      
#>  3 n     n     n     n     q1a | q1b | q1c | q1d
#>  4 n     n     n     n     q1a | q1b | q1c | q1d
#>  5 y     y     n     n     q1c | q1d            
#>  6 n     n     y     n     q1a | q1b | q1d      
#>  7 y     n     y     n     q1b | q1d            
#>  8 n     n     y     n     q1a | q1b | q1d      
#>  9 y     n     y     n     q1b | q1d            
#> 10 y     n     y     n     q1b | q1d            
#> # ℹ 190 more rows

# works with survey objects
d |>
  srvyr::as_survey() |>
  combine_answers(q1a:q1d, into = "combined")
#> ! Automatically selected value: "y"
#> ℹ To remove this message, please specify `value`.
#> Independent Sampling design (with replacement)
#> Called via srvyr
#> Sampling variables:
#>   - ids: `1` 
#> Data variables: 
#>   - q1a (chr), q1b (chr), q1c (chr), q1d (chr), combined (list)
```
