# Perform NMF Reduction

Applies Non-negative Matrix Factorization and returns both
reconstruction and original matrix for use as a filter.

## Usage

``` r
perform_nmf_reduction(mx, k, verbose = FALSE, seed = NULL)
```

## Arguments

- mx:

  Input matrix (should be non-negative)

- k:

  Number of components

- verbose:

  Print progress

- seed:

  Random seed

## Value

List with reconstructed matrix and original (non-negative) matrix
