# Extract Sub-matrix with Margins for a Segment

Given the original mobility matrix (with margins) and a vector of member
indices, builds the corresponding sub-matrix including recalculated row,
column, and grand totals.

## Usage

``` r
.extract_submatrix_with_margins(mat, members)
```

## Arguments

- mat:

  Full mobility matrix with margins (last row/col are totals).

- members:

  Integer vector of row/column indices (1-based, excluding the margin
  row/col).

## Value

A square matrix of dimension \`(length(members) + 1)\` with recalculated
margins.
