# Evaluate Segment Quality Metrics

Calculates comprehensive quality metrics for each segment across all
hierarchical levels of a moneca analysis. This function provides
detailed statistics about segment cohesion, size, network properties,
and mobility patterns.

## Usage

``` r
segment.quality(segments, final.solution = FALSE, segment_naming = "auto")
```

## Arguments

- segments:

  A moneca object returned by
  [`moneca`](https://gmontaletti.github.io/MONECA/reference/moneca.md).

- final.solution:

  Logical indicating whether to return only the final (most aggregated)
  solution for each unique segment. Default is FALSE, which returns
  metrics for all levels.

- segment_naming:

  Character string or data frame specifying how to name segments. Used
  to create readable segment labels in the output. Options include:

  - "auto" (default): Use simple "Segment X" format

  - "concat": Concatenate member names

  - "pattern": Use pattern-based naming

  - "custom": Custom naming strategy

  - Data frame: Must have columns "name" and "segment_label" for custom
    mapping

  When using a data frame, the function will match original category
  names to custom labels and use them for segment naming. This creates a
  "segment_label" column in the output that can be used by plotting
  functions.

## Value

A data frame with the following columns for each hierarchical level:

- Membership:

  Segment membership identifier in format "level.segment" (e.g., "2.1"
  means level 2, segment 1)

- \[Level\]: Segment:

  Integer segment identifier at each level

- \[Level\]: within.mobility:

  Proportion of mobility that stays within the segment (diagonal/total).
  Higher values indicate more cohesive segments. Range: 0-1, where 1
  means all mobility is internal.

- \[Level\]: share.of.mobility:

  Segment's share of total mobility in the system. Indicates the
  relative importance/size of the segment. Range: 0-1, values sum to 1
  across segments.

- \[Level\]: Density:

  Network density within the segment (proportion of possible edges that
  exist). Higher values indicate more interconnected nodes. Range: 0-1,
  where 1 means fully connected.

- \[Level\]: Nodes:

  Number of original categories/nodes in the segment

- \[Level\]: Max.path:

  Maximum shortest path length (diameter) within the segment. Lower
  values indicate more compact segments.

- \[Level\]: share.of.total:

  Segment's share of total population/observations based on marginal
  totals. Differs from share.of.mobility by considering population size
  rather than flow volume.

When `final.solution = TRUE`, returns a simplified data frame with only
the most aggregated metrics for each unique segment.

## Details

The function evaluates multiple aspects of segmentation quality:

**Cohesion Metrics:**

- `within.mobility`: Key quality indicator - proportion of mobility
  contained within segment boundaries. Values \> 0.7 suggest strong
  segments.

- `Density`: How interconnected nodes are within each segment. Dense
  segments (\> 0.5) indicate tight communities.

**Size Metrics:**

- `share.of.mobility`: Relative importance based on flow volume

- `share.of.total`: Relative size based on population

- `Nodes`: Absolute size in terms of categories

**Structure Metrics:**

- `Max.path`: Network diameter - smaller values indicate more compact,
  well-connected segments

The output is ordered by the final level's segment sizes for easier
interpretation.

## See also

[`moneca`](https://gmontaletti.github.io/MONECA/reference/moneca.md) for
the main analysis function,
[`plot_segment_quality`](https://gmontaletti.github.io/MONECA/reference/plot_segment_quality.md)
for graphical representation,
[`segment.membership`](https://gmontaletti.github.io/MONECA/reference/segment.membership.md)
for segment assignments

## Examples

``` r
# Generate data and run MONECA
mobility_data <- generate_mobility_data(n_classes = 8, seed = 123)
seg <- moneca(mobility_data, segment.levels = 3)
#>   |                                                                              |                                                                      |   0%  |                                                                              |=======                                                               |  10%  |                                                                              |==============                                                        |  20%
#> 27% ready!
#>   |                                                                              |=====================                                                 |  30%  |                                                                              |============================                                          |  40%  |                                                                              |===================================                                   |  50%
#> 55% ready!
#>   |                                                                              |==========================================                            |  60%  |                                                                              |=================================================                     |  70%  |                                                                              |========================================================              |  80%
#> 82% ready!
#>   |                                                                              |===============================================================       |  90%  |                                                                              |======================================================================| 100%
#> 100% ready!
#> 
#>   |                                                                              |                                                                      |   0%
#> 100% ready!
#> 

# Get detailed quality metrics for all levels
quality_full <- segment.quality(seg)
print(quality_full)
#>         Membership 1: Segment 1: within.mobility 1: share.of.mobility
#> Class 5        4.1          5              0.648                0.284
#> Class 4        4.1          4              0.491                0.139
#> Class 3        4.1          3              0.108                0.054
#> Class 6        4.1          6              0.567                0.158
#> Class 7        4.1          7              0.319                0.048
#> Class 8        4.1          8              0.290                0.021
#> Class 2        4.1          2              0.676                0.215
#> Class 1        4.1          1              0.559                0.082
#>         1: Density 1: Nodes 1: Max.path 1: share.of.total 2: Segment
#> Class 5        NaN        1           0             0.284          3
#> Class 4        NaN        1           0             0.139          3
#> Class 3        NaN        1           0             0.054          3
#> Class 6        NaN        1           0             0.158          1
#> Class 7        NaN        1           0             0.048          1
#> Class 8        NaN        1           0             0.021          1
#> Class 2        NaN        1           0             0.215          2
#> Class 1        NaN        1           0             0.082          2
#>         2: within.mobility 2: share.of.mobility 2: Density 2: Nodes 2: Max.path
#> Class 5              0.777                0.476  0.3333333        3           1
#> Class 4              0.777                0.476  0.3333333        3           1
#> Class 3              0.777                0.476  0.3333333        3           1
#> Class 6              0.692                0.227  0.8333333        3           2
#> Class 7              0.692                0.227  0.8333333        3           2
#> Class 8              0.692                0.227  0.8333333        3           2
#> Class 2              0.825                0.297  1.0000000        2           1
#> Class 1              0.825                0.297  1.0000000        2           1
#>         2: share.of.total 3: Segment 3: within.mobility 3: share.of.mobility
#> Class 5             0.476          1              0.926                0.703
#> Class 4             0.476          1              0.926                0.703
#> Class 3             0.476          1              0.926                0.703
#> Class 6             0.227          1              0.926                0.703
#> Class 7             0.227          1              0.926                0.703
#> Class 8             0.227          1              0.926                0.703
#> Class 2             0.297         NA                 NA                   NA
#> Class 1             0.297         NA                 NA                   NA
#>         3: Density 3: Nodes 3: Max.path 3: share.of.total 4: Segment
#> Class 5  0.2666667        6           3             0.703          1
#> Class 4  0.2666667        6           3             0.703          1
#> Class 3  0.2666667        6           3             0.703          1
#> Class 6  0.2666667        6           3             0.703          1
#> Class 7  0.2666667        6           3             0.703          1
#> Class 8  0.2666667        6           3             0.703          1
#> Class 2         NA       NA          NA                NA          1
#> Class 1         NA       NA          NA                NA          1
#>         4: within.mobility 4: share.of.mobility 4: Density 4: Nodes 4: Max.path
#> Class 5                  1                    1  0.2142857        8           3
#> Class 4                  1                    1  0.2142857        8           3
#> Class 3                  1                    1  0.2142857        8           3
#> Class 6                  1                    1  0.2142857        8           3
#> Class 7                  1                    1  0.2142857        8           3
#> Class 8                  1                    1  0.2142857        8           3
#> Class 2                  1                    1  0.2142857        8           3
#> Class 1                  1                    1  0.2142857        8           3
#>         4: share.of.total
#> Class 5                 1
#> Class 4                 1
#> Class 3                 1
#> Class 6                 1
#> Class 7                 1
#> Class 8                 1
#> Class 2                 1
#> Class 1                 1

# Get only final solution summary
quality_final <- segment.quality(seg, final.solution = TRUE)
print(quality_final)
#>         Membership within.mobility share.of.mobility   Density Nodes Max.path
#> Class 5        4.1               1                 1 0.2142857     8        3
#>         share.of.total Segment segment_label
#> Class 5              1     4.1     Class 8+7

# Use custom segment naming with final solution
custom_names <- data.frame(
  name = c("Class1", "Class2", "Class3"),
  segment_label = c("Executive", "Professional", "Technical"),
  stringsAsFactors = FALSE
)
quality_labeled <- segment.quality(seg, final.solution = TRUE, segment_naming = custom_names)
print(quality_labeled)
#>         Membership within.mobility share.of.mobility   Density Nodes Max.path
#> Class 5        4.1               1                 1 0.2142857     8        3
#>         share.of.total Segment segment_label
#> Class 5              1     4.1     Class 8+7

if (FALSE) { # \dontrun{
# Visualize segment quality
plot_segment_quality(seg)
} # }
```
