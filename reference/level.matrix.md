# Extract and Label Hierarchical Mobility Matrix

Extracts the mobility matrix at a given hierarchical level from a moneca
result, strips the "Total" margin (unless `include_total = TRUE`), and
labels rows and columns by the most central node within each segment.
Centrality is measured as the sum of incoming and outgoing relative-risk
weights within the segment subgraph.

## Usage

``` r
level.matrix(segments, level = 2, include_total = FALSE)
```

## Arguments

- segments:

  A moneca object returned by
  [`moneca`](https://gmontaletti.github.io/MONECA/reference/moneca.md)
  or
  [`moneca_fast`](https://gmontaletti.github.io/MONECA/reference/moneca_fast.md).

- level:

  Integer. The hierarchical level to extract (default 2). Level 1
  returns the original input matrix.

- include_total:

  Logical. If `TRUE`, keep the "Total" row and column in the returned
  matrix (default `FALSE`).

## Value

A matrix with rows and columns labeled by the most central node in each
segment. The matrix carries a `"segment_map"` attribute: a `data.frame`
with columns `segment` (integer index), `main_node` (label chosen for
that segment), `members` (comma-separated string of all member names),
and `n_members` (number of members in the segment).

## Examples

``` r
mob <- generate_mobility_data(n_classes = 6, n_total = 500, seed = 42)
seg <- moneca_fast(mob, segment.levels = 2)
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |                                                                      |   0%
mx2 <- level.matrix(seg, level = 2)
mx2
#>           Class 6+2 Class 1+1 Class 3
#> Class 6+2       134         6       9
#> Class 1+1        17       223      22
#> Class 3          20        14      55
#> attr(,"segment_map")
#>   segment main_node                   members n_members
#> 1       1   Class 6 Class 4, Class 5, Class 6         3
#> 2       2   Class 1          Class 1, Class 2         2
#> 3       3   Class 3                   Class 3         1
attr(mx2, "segment_map")
#>   segment main_node                   members n_members
#> 1       1   Class 6 Class 4, Class 5, Class 6         3
#> 2       2   Class 1          Class 1, Class 2         2
#> 3       3   Class 3                   Class 3         1
```
