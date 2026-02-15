# Build design for quasi-independence model

Adds n binary indicator columns for diagonal cells (one per category).

## Usage

``` r
.build_design_quasi_independence(df, categories)
```

## Arguments

- df:

  Long-format data frame from `.matrix_to_long`.

- categories:

  Character vector of category names.

## Value

Data frame with added diagonal indicator columns.
