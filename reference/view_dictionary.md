# Display the variable dictionary of a data frame in the RStudio viewer

Generates an interactive variable dictionary based on
[`labelled::look_for()`](https://larmarange.github.io/labelled/reference/look_for.html).
Accepts data frames, tibbles, and also survey objects.

## Usage

``` r
view_dictionary(data = NULL, details = c("basic", "none", "full"))

view_detailed_dictionary(data = NULL)

to_DT(
  x,
  caption = NULL,
  column_labels = list(pos = "#", variable = "Variable", col_type = "Type", label =
    "Variable label", values = "Values", missing = "Missing values", unique_values =
    "Unique values", na_values = "User-defined missings (values)", na_range =
    "User-defined missings (range)")
)
```

## Arguments

- data:

  a data frame, a tibble or a survey object (if `NULL`, will use the
  text you currently select in **RStudio**, useful if the function is
  called through the corresponding addin)

- details:

  add details about each variable (see
  [`labelled::look_for()`](https://larmarange.github.io/labelled/reference/look_for.html))

- x:

  a tibble returned by `look_for()`

- caption:

  an optional caption for the table

- column_labels:

  Optional column labels

## Note

`to_DT()` is an utility to convert the result of
[`labelled::look_for()`](https://larmarange.github.io/labelled/reference/look_for.html)
into a [`DT::datatable()`](https://rdrr.io/pkg/DT/man/datatable.html).

## Examples

``` r
if (FALSE) { # interactive()
iris |> view_dictionary()
}
iris |> labelled::look_for(details = TRUE) |> to_DT()

{"x":{"filter":"none","vertical":false,"extensions":["Buttons"],"data":[[1,2,3,4,5],["<strong>Sepal.Length<\/strong>","<strong>Sepal.Width<\/strong>","<strong>Petal.Length<\/strong>","<strong>Petal.Width<\/strong>","<strong>Species<\/strong>"],["dbl","dbl","dbl","dbl","fct"],["—","—","—","—","—"],["4.3 – 7.9","2 – 4.4","1 – 6.9","0.1 – 2.5","setosa<br />versicolor<br />virginica"],[0,0,0,0,0],[35,23,43,22,3]],"container":"<table class=\"stripe compact hover\">\n  <thead>\n    <tr>\n      <th>#<\/th>\n      <th>Variable<\/th>\n      <th>Type<\/th>\n      <th>Variable label<\/th>\n      <th>Values<\/th>\n      <th>Missing values<\/th>\n      <th>Unique values<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"paging":false,"initComplete":"function(settings, json) {\n$('body').css({'font-family': '-apple-system, BlinkMacSystemFont, \"Segoe UI\", Roboto, Helvetica, Arial, sans-serif'});\n}","dom":"Bfrtip","buttons":["csv","excel","pdf"],"columnDefs":[{"className":"dt-right","targets":[0,5,6]},{"name":"pos","targets":0},{"name":"variable","targets":1},{"name":"col_type","targets":2},{"name":"label","targets":3},{"name":"values","targets":4},{"name":"missing","targets":5},{"name":"unique_values","targets":6}],"order":[],"autoWidth":false,"orderClasses":false,"rowCallback":"function(row, data, displayNum, displayIndex, dataIndex) {\nvar value=data[0]; $(this.api().cell(row, 0).node()).css({'vertical-align':'top'});\nvar value=data[1]; $(this.api().cell(row, 1).node()).css({'vertical-align':'top'});\nvar value=data[2]; $(this.api().cell(row, 2).node()).css({'vertical-align':'top'});\nvar value=data[3]; $(this.api().cell(row, 3).node()).css({'vertical-align':'top'});\nvar value=data[4]; $(this.api().cell(row, 4).node()).css({'vertical-align':'top'});\nvar value=data[5]; $(this.api().cell(row, 5).node()).css({'vertical-align':'top'});\nvar value=data[6]; $(this.api().cell(row, 6).node()).css({'vertical-align':'top'});\n}"}},"evals":["options.initComplete","options.rowCallback"],"jsHooks":[]}
```
