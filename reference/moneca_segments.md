# Canonical Segment Metadata for MONECA Results

Constructs a `moneca_segments` object that provides a single,
authoritative description of every segment at every hierarchical level.
Group membership is built using the same progressive-merge algorithm as
[`level.matrix`](https://gmontaletti.github.io/MONECA/reference/level.matrix.md),
and the representative node for each group is chosen by maximising total
directed strength in the asymmetric weight matrix.

## Usage

``` r
moneca_segments(segments)
```

## Arguments

- segments:

  A moneca object returned by
  [`moneca`](https://gmontaletti.github.io/MONECA/reference/moneca.md)
  or
  [`moneca_fast`](https://gmontaletti.github.io/MONECA/reference/moneca_fast.md).

## Value

A `moneca_segments` object (S3 class) containing:

- `levels`:

  A list with one entry per hierarchical level. Each entry has `$groups`
  (list of integer vectors of member indices) and `$map` (data.frame
  with columns `segment`, `main_node`, `members`, `n_members`, `label`).

- `weight_matrix`:

  The asymmetric relative-risk matrix used for centrality computation
  (no `NA` values).

- `original_names`:

  Character vector of node names (excluding "Total").

- `n_levels`:

  Integer, number of hierarchical levels.

- `small.cell.reduction`:

  Numeric, the `small.cell.reduction` parameter stored in the moneca
  object.

## Examples

``` r
mob <- generate_mobility_data(n_classes = 6, n_total = 500, seed = 42)
seg <- moneca_fast(mob, segment.levels = 2)
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |                                                                      |   0%
meta <- moneca_segments(seg)
print(meta)
#> moneca_segments object
#>   Nodes: 6 
#>   Levels: 3 
#>     Level 1: 6 groups
#>     Level 2: 3 groups
#>     Level 3: 2 groups
get_segment_map(meta, level = 2)
#>   segment main_node                   members n_members     label
#> 1       1   Class 6 Class 4, Class 5, Class 6         3 Class 6+2
#> 2       2   Class 1          Class 1, Class 2         2 Class 1+1
#> 3       3   Class 3                   Class 3         1   Class 3
```
