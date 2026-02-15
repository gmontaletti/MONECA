# Build design for quasi-symmetry model

Starts with quasi-independence columns, then adds symmetric pair
indicators. For each unordered pair {i, j} where i \< j, adds column
`symm_i_j` = 1 when (Origin == i & Dest == j) OR (Origin == j & Dest ==
i). This gives n + n\*(n-1)/2 extra columns beyond Origin + Destination.

## Usage

``` r
.build_design_quasi_symmetry(df, categories)
```

## Arguments

- df:

  Long-format data frame from `.matrix_to_long`.

- categories:

  Character vector of category names.

## Value

Data frame with diagonal and symmetry indicator columns.
