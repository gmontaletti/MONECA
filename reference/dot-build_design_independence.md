# Build design for independence model

Returns data frame with Origin and Destination as factors. R handles
dummy coding for the Poisson GLM.

## Usage

``` r
.build_design_independence(df, categories)
```

## Arguments

- df:

  Long-format data frame from `.matrix_to_long`.

- categories:

  Character vector of category names.

## Value

The input data frame with factors set.
