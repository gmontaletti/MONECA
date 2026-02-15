# Get Representative Node for a Segment

Returns the name of the most central node in a specific group.

## Usage

``` r
get_representative(meta, level, group_id)
```

## Arguments

- meta:

  A `moneca_segments` object.

- level:

  Integer. The hierarchical level.

- group_id:

  Integer. The group (segment) index.

## Value

A character string: the representative node name.

## Examples

``` r
mob <- generate_mobility_data(n_classes = 6, n_total = 500, seed = 42)
seg <- moneca_fast(mob, segment.levels = 2)
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |                                                                      |   0%
meta <- moneca_segments(seg)
get_representative(meta, level = 2, group_id = 1)
#> [1] "Class 6"
```
