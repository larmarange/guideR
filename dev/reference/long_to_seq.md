# Transform a data frame from long format to a sequence obect

Transform a data frame from long format to a sequence obect

## Usage

``` r
long_to_seq(
  data,
  id,
  time,
  outcome,
  alphabet = "auto",
  labels = "auto",
  cnames = "auto",
  cpal = "auto",
  missing.color = "#BBBBBB",
  ...
)
```

## Arguments

- data:

  A data frame or a data frame extension (e.g. a tibble).

- id:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>  
  Column containing individual ids

- time:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>  
  Time variable

- outcome:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>  
  Variable defining the status

- alphabet:

  Optional vector containing the alphabet (the list of all possible
  states). If `alphabet = "auto"` will be automatically determined from
  `outcome`. If `outcome` is a labelled vector (`haven_labelled` class),
  it will be derived from the value labels (using the values). If
  `outcome` is a factor, the factor will be transformed to a numeric
  vector with [`as.integer()`](https://rdrr.io/r/base/integer.html) and
  the corresponding numeric values will be used as the alphabet. In all
  other cases, will be equal to `NULL` (see
  [`TraMineR::seqdef()`](https://rdrr.io/pkg/TraMineR/man/seqdef.html)).

- labels:

  An optional vector containing state labels used for graphics. If
  `labels = "auto"` will be automatically determined from `outcome`. If
  `outcome` is a labelled vector (`haven_labelled` class), it will be
  derived from the value labels (using the labels). If `outcome` is a
  factor, the levels of the factor will be used. In all other cases,
  will be equal to `NULL`

- cnames:

  An optional vector containing names of the different time points. If
  `cnames = "auto"`, it will use the observed values from `time`.

- cpal:

  An optional colour palette for representing the states in the
  graphics. If `cpal = "auto"`, a palette will be generated with
  [`safe_pal()`](https://larmarange.github.io/guideR/dev/reference/safe_pal.md).

- missing.color:

  Alternative colour for representing missing values inside the
  sequences.

- ...:

  Additional arguments passed to
  [`TraMineR::seqdef()`](https://rdrr.io/pkg/TraMineR/man/seqdef.html)

## Value

An object of class `stslist`.

## See also

[`TraMineR::seqdef()`](https://rdrr.io/pkg/TraMineR/man/seqdef.html)

## Examples

``` r
# \donttest{
library(TraMineR)
#> 
#> TraMineR stable version 2.2-13 (Built: 2025-12-15)
#> Website: http://traminer.unige.ch
#> Please type 'citation("TraMineR")' for citation information.

# generating a data frame in long format
data("biofam")
d <-
  biofam |>
  tibble::rownames_to_column("id_ind") |>
  dplyr::select(id_ind, dplyr::starts_with("a")) |>
  tidyr::pivot_longer(
    cols = dplyr::starts_with("a"),
    names_to = "age",
    names_prefix = "a",
    values_to = "life_state"
  ) |>
  dplyr::mutate(
    age = as.integer(age),
    life_state2 = dplyr::case_when(
      life_state == 0 ~ "P",
      life_state == 1 ~ "L",
      life_state == 2 ~ "M",
      life_state == 3 ~ "LM",
      life_state == 4 ~ "C",
      life_state == 5 ~ "LC",
      life_state == 6 ~ "LMC",
      life_state == 7 ~ "D"
    )
  ) |>
  labelled::set_value_labels(
    life_state = c(
      "Parent" = 0,
      "Left" = 1,
      "Married" = 2,
      "Left & Married" = 3,
      "Child" = 4,
      "Left & Child" = 5,
      "Left & Married & Child" = 6,
      "Divorced" = 7
    ),
    life_state2 = c(
      "Parent" = "P",
      "Left" = "L",
      "Married" = "M",
      "Left & Married" = "LM",
      "Child" = "C",
      "Left & Child" = "LC",
      "Left & Married & Child" = "LMC",
      "Divorced" = "D"
    )
  ) |>
  dplyr::mutate(
    life_state3 = labelled::to_factor(life_state),
    life_state4 = unclass(life_state2)
  )

d |> long_to_seq(id = id_ind, time = age, outcome = life_state) |> head(10)
#>  [>] time axis: 15 -> 30
#>  [>] converting SPELL data into 2000 STS sequences (internal format)
#>  [>] 8 distinct states appear in the data: 
#>      1 = 0
#>      2 = 1
#>      3 = 2
#>      4 = 3
#>      5 = 4
#>      6 = 5
#>      7 = 6
#>      8 = 7
#>  [>] state coding:
#>        [alphabet]  [label]  [long label] 
#>      1             0        0Parent
#>      2             1        1Left
#>      3             2        2Married
#>      4             3        3Left & Married
#>      5             4        4Child
#>      6             5        5Left & Child
#>      7             6        6Left & Married & Child
#>      8             7        7Divorced
#>  [>] 2000 sequences in the data set
#>  [>] min/max sequence length: 16/16
#>      Sequence                       
#> 1    0-0-0-0-0-0-0-0-0-0-0-0-0-0-0-2
#> 10   0-0-0-0-0-0-0-0-0-0-0-0-0-0-0-0
#> 100  0-0-0-0-0-0-0-0-2-2-2-2-2-2-2-2
#> 1000 0-0-0-0-0-0-0-0-2-2-2-2-2-2-2-2
#> 1002 0-0-0-1-1-1-6-6-6-6-6-6-6-6-7-7
#> 1003 0-0-0-0-0-0-0-0-0-0-3-3-3-3-3-3
#> 1004 0-0-0-0-0-0-0-0-0-3-3-3-3-3-3-3
#> 1005 0-0-0-0-0-0-0-0-0-0-0-0-1-6-6-6
#> 1006 0-0-0-0-0-0-0-1-1-1-1-1-3-6-6-6
#> 1007 0-0-0-0-0-0-0-0-0-0-6-6-6-6-7-7
d |> long_to_seq(id = id_ind, time = age, outcome = life_state2) |> head(10)
#>  [>] time axis: 15 -> 30
#>  [>] converting SPELL data into 2000 STS sequences (internal format)
#>  [>] 8 distinct states appear in the data: 
#>      1 = C
#>      2 = D
#>      3 = L
#>      4 = LC
#>      5 = LM
#>      6 = LMC
#>      7 = M
#>      8 = P
#>  [>] state coding:
#>        [alphabet]  [label]  [long label] 
#>      1  P           P        Parent
#>      2  L           L        Left
#>      3  M           M        Married
#>      4  LM          LM       Left & Married
#>      5  C           C        Child
#>      6  LC          LC       Left & Child
#>      7  LMC         LMC      Left & Married & Child
#>      8  D           D        Divorced
#>  [>] 2000 sequences in the data set
#>  [>] min/max sequence length: 16/16
#>      Sequence                                       
#> 1    P-P-P-P-P-P-P-P-P-P-P-P-P-P-P-M                
#> 10   P-P-P-P-P-P-P-P-P-P-P-P-P-P-P-P                
#> 100  P-P-P-P-P-P-P-P-M-M-M-M-M-M-M-M                
#> 1000 P-P-P-P-P-P-P-P-M-M-M-M-M-M-M-M                
#> 1002 P-P-P-L-L-L-LMC-LMC-LMC-LMC-LMC-LMC-LMC-LMC-D-D
#> 1003 P-P-P-P-P-P-P-P-P-P-LM-LM-LM-LM-LM-LM          
#> 1004 P-P-P-P-P-P-P-P-P-LM-LM-LM-LM-LM-LM-LM         
#> 1005 P-P-P-P-P-P-P-P-P-P-P-P-L-LMC-LMC-LMC          
#> 1006 P-P-P-P-P-P-P-L-L-L-L-L-LM-LMC-LMC-LMC         
#> 1007 P-P-P-P-P-P-P-P-P-P-LMC-LMC-LMC-LMC-D-D        
d |> long_to_seq(id = id_ind, time = age, outcome = life_state3) |> head(10)
#>  [>] time axis: 15 -> 30
#>  [>] converting SPELL data into 2000 STS sequences (internal format)
#>  [>] 8 distinct states appear in the data: 
#>      1 = 1
#>      2 = 2
#>      3 = 3
#>      4 = 4
#>      5 = 5
#>      6 = 6
#>      7 = 7
#>      8 = 8
#>  [>] state coding:
#>        [alphabet]  [label]  [long label] 
#>      1             1        1Parent
#>      2             2        2Left
#>      3             3        3Married
#>      4             4        4Left & Married
#>      5             5        5Child
#>      6             6        6Left & Child
#>      7             7        7Left & Married & Child
#>      8             8        8Divorced
#>  [>] 2000 sequences in the data set
#>  [>] min/max sequence length: 16/16
#>      Sequence                       
#> 1    1-1-1-1-1-1-1-1-1-1-1-1-1-1-1-3
#> 10   1-1-1-1-1-1-1-1-1-1-1-1-1-1-1-1
#> 100  1-1-1-1-1-1-1-1-3-3-3-3-3-3-3-3
#> 1000 1-1-1-1-1-1-1-1-3-3-3-3-3-3-3-3
#> 1002 1-1-1-2-2-2-7-7-7-7-7-7-7-7-8-8
#> 1003 1-1-1-1-1-1-1-1-1-1-4-4-4-4-4-4
#> 1004 1-1-1-1-1-1-1-1-1-4-4-4-4-4-4-4
#> 1005 1-1-1-1-1-1-1-1-1-1-1-1-2-7-7-7
#> 1006 1-1-1-1-1-1-1-2-2-2-2-2-4-7-7-7
#> 1007 1-1-1-1-1-1-1-1-1-1-7-7-7-7-8-8
d |> long_to_seq(id = id_ind, time = age, outcome = life_state4) |> head(10)
#>  [>] time axis: 15 -> 30
#>  [>] converting SPELL data into 2000 STS sequences (internal format)
#>  [>] 8 distinct states appear in the data: 
#>      1 = C
#>      2 = D
#>      3 = L
#>      4 = LC
#>      5 = LM
#>      6 = LMC
#>      7 = M
#>      8 = P
#>  [>] state coding:
#>        [alphabet]  [label]  [long label] 
#>      1  C           C        C
#>      2  D           D        D
#>      3  L           L        L
#>      4  LC          LC       LC
#>      5  LM          LM       LM
#>      6  LMC         LMC      LMC
#>      7  M           M        M
#>      8  P           P        P
#>  [>] 2000 sequences in the data set
#>  [>] min/max sequence length: 16/16
#>      Sequence                                       
#> 1    P-P-P-P-P-P-P-P-P-P-P-P-P-P-P-M                
#> 10   P-P-P-P-P-P-P-P-P-P-P-P-P-P-P-P                
#> 100  P-P-P-P-P-P-P-P-M-M-M-M-M-M-M-M                
#> 1000 P-P-P-P-P-P-P-P-M-M-M-M-M-M-M-M                
#> 1002 P-P-P-L-L-L-LMC-LMC-LMC-LMC-LMC-LMC-LMC-LMC-D-D
#> 1003 P-P-P-P-P-P-P-P-P-P-LM-LM-LM-LM-LM-LM          
#> 1004 P-P-P-P-P-P-P-P-P-LM-LM-LM-LM-LM-LM-LM         
#> 1005 P-P-P-P-P-P-P-P-P-P-P-P-L-LMC-LMC-LMC          
#> 1006 P-P-P-P-P-P-P-L-L-L-L-L-LM-LMC-LMC-LMC         
#> 1007 P-P-P-P-P-P-P-P-P-P-LMC-LMC-LMC-LMC-D-D        
# }
```
