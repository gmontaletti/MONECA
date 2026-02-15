# Get Segment Map at a Given Level

Returns the segment map data frame for a specific hierarchical level.

## Usage

``` r
get_segment_map(meta, level = 2)
```

## Arguments

- meta:

  A `moneca_segments` object.

- level:

  Integer. The hierarchical level (default 2).

## Value

A data.frame with columns `segment`, `main_node`, `members`,
`n_members`, and `label`.

## Examples

``` r
mob <- generate_mobility_data(n_classes = 6, n_total = 500, seed = 42)
seg <- moneca_fast(mob, segment.levels = 2)
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |                                                                      |   0%
meta <- moneca_segments(seg)
get_segment_map(meta, level = 2)
#>   segment main_node                   members n_members     label
#> 1       1   Class 6 Class 4, Class 5, Class 6         3 Class 6+2
#> 2       2   Class 1          Class 1, Class 2         2 Class 1+1
#> 3       3   Class 3                   Class 3         1   Class 3
```
