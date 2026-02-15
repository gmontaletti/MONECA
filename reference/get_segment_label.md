# Get Segment Label

Returns the formatted label for a specific group (e.g., `"A+2"` for a
group of 3 with representative `"A"`).

## Usage

``` r
get_segment_label(meta, level, group_id)
```

## Arguments

- meta:

  A `moneca_segments` object.

- level:

  Integer. The hierarchical level.

- group_id:

  Integer. The group (segment) index.

## Value

A character string: the formatted label.

## Examples

``` r
mob <- generate_mobility_data(n_classes = 6, n_total = 500, seed = 42)
seg <- moneca_fast(mob, segment.levels = 2)
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |                                                                      |   0%
meta <- moneca_segments(seg)
get_segment_label(meta, level = 2, group_id = 1)
#> [1] "Class 6+2"
```
