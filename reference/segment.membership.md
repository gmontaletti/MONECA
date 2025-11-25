# Extract Segment Membership Information

Returns a data frame showing which segment each original category
belongs to across the specified hierarchical levels of a moneca
analysis.

## Usage

``` r
segment.membership(segments, level = seq(segments$segment.list))
```

## Arguments

- segments:

  A moneca object returned by
  [`moneca`](https://gmontaletti.github.io/MONECA/reference/moneca.md).

- level:

  Integer vector specifying which hierarchical levels to include.
  Default includes all available levels.

## Value

A data frame with two columns:

- name:

  Character vector of original category names

- membership:

  Character vector indicating segment membership, formatted as
  "level.segment" (e.g., "2.1" for level 2, segment 1)

## Details

The membership strings indicate both the hierarchical level and the
specific segment within that level. For example, "2.3" means the
category belongs to segment 3 at level 2 of the hierarchy.

## See also

[`moneca`](https://gmontaletti.github.io/MONECA/reference/moneca.md),
[`plot_moneca_ggraph`](https://gmontaletti.github.io/MONECA/reference/plot_moneca_ggraph.md)

## Examples

``` r
# Generate data and run analysis
mob_data <- generate_mobility_data(n_classes = 5, seed = 42)
seg <- moneca(mob_data, segment.levels = 3)
#>   |                                                                              |                                                                      |   0%  |                                                                              |==================                                                    |  25%
#> 40% ready!
#>   |                                                                              |===================================                                   |  50%
#> 60% ready!
#>   |                                                                              |====================================================                  |  75%
#> 80% ready!
#>   |                                                                              |======================================================================| 100%
#> 100% ready!
#> 
#>   |                                                                              |                                                                      |   0%
#> 50% ready!
#>   |                                                                              |======================================================================| 100%
#> 100% ready!
#> 

# Get membership information
membership <- segment.membership(seg)
print(membership)
#>      name membership
#> 1 Class 1        4.1
#> 2 Class 2        4.1
#> 3 Class 3        4.1
#> 4 Class 4        4.1
#> 5 Class 5        4.1

# Get membership for specific levels only
membership_level2 <- segment.membership(seg, level = 2)
```
