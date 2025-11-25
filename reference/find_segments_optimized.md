# Optimized Segment Finding with Vectorized Operations

High-performance implementation of the segment finding algorithm
using: - Pre-sorted edges for single-pass processing - Union-find for
O(alpha(n)) group merging - Vectorized clique membership testing -
Optional sparse matrix support

## Usage

``` r
find_segments_optimized(mat, cliques, cut.off = 1, progress = FALSE)
```

## Arguments

- mat:

  Weight matrix from weight.matrix()

- cliques:

  List of cliques from igraph

- cut.off:

  Minimum weight threshold

- progress:

  Show progress bar

## Value

List with membership and cliques
