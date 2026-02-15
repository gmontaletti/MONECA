# Get Segment Groups at a Given Level

Returns the list of integer vectors representing group membership.

## Usage

``` r
get_segment_groups(meta, level = 2)
```

## Arguments

- meta:

  A `moneca_segments` object.

- level:

  Integer. The hierarchical level (default 2).

## Value

A list of integer vectors, one per group, containing member indices.

## Examples

``` r
mob <- generate_mobility_data(n_classes = 6, n_total = 500, seed = 42)
seg <- moneca_fast(mob, segment.levels = 2)
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |                                                                      |   0%
meta <- moneca_segments(seg)
get_segment_groups(meta, level = 2)
#> [[1]]
#> [1] 4 5 6
#> 
#> [[2]]
#> [1] 1 2
#> 
#> [[3]]
#> [1] 3
#> 
```
