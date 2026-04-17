# Flag Segments Exceeding an Asymmetry Threshold

Convenience wrapper around \[compute_asymmetry_scores()\] that
identifies segments whose asymmetry score exceeds a user-defined
threshold.

## Usage

``` r
flag_asymmetric_segments(
  segments,
  threshold = 0.5,
  level = 2,
  method = "max_pair"
)
```

## Arguments

- segments:

  A moneca object returned by \[moneca()\] or \[moneca_fast()\].

- threshold:

  Numeric in \\0, 1\\. Segments with an asymmetry score strictly above
  this value are flagged (default 0.5).

- level:

  Integer. Hierarchical level to evaluate (default 2).

- method:

  Character. Aggregation method per segment: \* \`"max_pair"\`
  (default): maximum pairwise asymmetry. \* \`"mean_pair"\`: mean
  pairwise asymmetry weighted by the sum of the bidirectional relative
  risks (\`rr_ij + rr_ji\`). Note: this weights pairs by combined RR
  intensity, not by raw flow counts.

## Value

A list with components:

- scores:

  The full \`data.frame\` returned by \[compute_asymmetry_scores()\].

- flagged:

  Logical vector of length equal to the number of segments; \`TRUE\` for
  segments above the threshold.

- threshold:

  The threshold used.

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
flags <- flag_asymmetric_segments(seg, threshold = 0.3)
flags$scores[flags$flagged, ]
#> [1] segment_id      n_members       asymmetry_score max_pair_from  
#> [5] max_pair_to     max_pair_asym  
#> <0 rows> (or 0-length row.names)
```
