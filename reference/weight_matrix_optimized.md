# Optimized Weight Matrix Calculation

Vectorized computation of relative risk weight matrix with optional
data.table acceleration for very large matrices.

## Usage

``` r
weight_matrix_optimized(
  mx,
  cut.off = 1,
  small.cell.reduction = 0,
  symmetric = TRUE
)
```

## Arguments

- mx:

  Mobility matrix with totals

- cut.off:

  Minimum relative risk threshold

- small.cell.reduction:

  Minimum cell count

- symmetric:

  Whether to symmetrize the matrix

## Value

Weight matrix with NA for values below cutoff
