# Compute Asymmetry Scores for MONECA Segments

Quantifies the degree of directional asymmetry within each segment at a
given hierarchical level. For every within-segment pair (i, j), the
asymmetry is \`abs(RR_ij - RR_ji) / (RR_ij + RR_ji)\`, ranging from 0
(perfectly symmetric) to 1 (completely one-directional).

## Usage

``` r
compute_asymmetry_scores(segments, level = 2, method = "max_pair")
```

## Arguments

- segments:

  A moneca object returned by \[moneca()\] or \[moneca_fast()\].

- level:

  Integer. Hierarchical level to evaluate (default 2).

- method:

  Character. Aggregation method per segment: \* \`"max_pair"\`
  (default): maximum pairwise asymmetry. \* \`"mean_pair"\`: mean
  pairwise asymmetry weighted by the sum of the bidirectional relative
  risks (\`rr_ij + rr_ji\`). Note: this weights pairs by combined RR
  intensity, not by raw flow counts.

## Value

A \`data.frame\` with one row per segment and columns:

- segment_id:

  Integer segment index at the requested level.

- n_members:

  Number of categories in the segment.

- asymmetry_score:

  Aggregate asymmetry (max or mean).

- max_pair_from:

  Origin category of the most asymmetric pair.

- max_pair_to:

  Destination category of the most asymmetric pair.

- max_pair_asym:

  Asymmetry value of the most asymmetric pair.

## Details

The function retrieves the original mobility matrix from
\`segments\$mat.list\[\[1\]\]\` and computes an asymmetric relative risk
matrix via \[weight.matrix()\] with \`symmetric = FALSE\` and \`cut.off
= 0\`. Pairs with zero flow in both directions receive an asymmetry of
0. Pairs with flow in only one direction receive the maximum asymmetry
of 1.

## Examples

``` r
mob <- generate_mobility_data(n_classes = 6, seed = 42)
seg <- moneca(mob, segment.levels = 3)
#>   |                                                                              |                                                                      |   0%  |                                                                              |============                                                          |  17%
#> 29% ready!
#>   |                                                                              |=======================                                               |  33%  |                                                                              |===================================                                   |  50%
#> 57% ready!
#>   |                                                                              |===============================================                       |  67%  |                                                                              |==========================================================            |  83%
#> 86% ready!
#>   |                                                                              |======================================================================| 100%
#> 100% ready!
#> 
#>   |                                                                              |                                                                      |   0%
#> 100% ready!
#> 
compute_asymmetry_scores(seg, level = 2)
#>   segment_id n_members asymmetry_score max_pair_from max_pair_to max_pair_asym
#> 1          1         3      0.23199448       Class 4     Class 6    0.23199448
#> 2          2         2      0.03168966       Class 2     Class 3    0.03168966
```
