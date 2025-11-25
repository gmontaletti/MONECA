# Print Method for moneca Objects (Enhanced)

Displays comprehensive mobility analysis results from moneca clustering
with clear, interpretable statistics and improved labeling. Output is
structured to follow the natural flow of analysis from overall patterns
to detailed metrics.

## Usage

``` r
# S3 method for class 'moneca'
print(
  x,
  small.cell.reduction = x$small.cell.reduction,
  show.degree.stats = TRUE,
  digits = 1,
  ...
)
```

## Arguments

- x:

  A moneca object returned by
  [`moneca`](https://gmontaletti.github.io/MONECA/reference/moneca.md).

- small.cell.reduction:

  Numeric threshold for small cell handling. If NULL, uses the value
  from the moneca object.

- show.degree.stats:

  Logical. If TRUE (default), displays detailed degree distribution
  statistics for each hierarchical level.

- digits:

  Integer. Number of decimal places for numeric output (default 1).

- ...:

  Additional arguments (currently unused).

## Value

Invisibly returns NULL. This function is called for its side effect of
printing a formatted summary.

## Details

The output is organized into four main sections that follow the analysis
workflow:

**Section 1: OVERALL MOBILITY PATTERNS**

- **Overall Population Mobility Rate**: Percentage of the total
  population that experiences any form of mobility (moves from origin to
  a different destination). Calculated as: (total off-diagonal cells) /
  (grand total). A rate of 20

- **Average Mobility Concentration**: Mean percentage of mobility
  captured by significant pathways (edges with relative risk \> 1)
  across all segmentation levels. Higher values indicate mobility is
  concentrated in fewer, stronger pathways rather than dispersed
  randomly.

**Section 2: HIERARCHICAL SEGMENTATION ANALYSIS**

*Internal Mobility Within Segments (percent):*

- Shows the percentage of mobility that remains within segment
  boundaries at each level. Level 1 represents original categories
  (always 100 category is its own segment). Higher levels show
  increasing aggregation.

- Values decrease at higher levels as segments combine different origin
  categories, naturally reducing internal cohesion.

- High values (\>70

*Mobility Concentration in Significant Pathways by Level (percent):*

- Percentage of total mobility flowing through edges with relative risk
  \> 1.

- Indicates how much mobility follows stronger-than-expected pathways
  versus random distribution.

- Higher values suggest clearer mobility structure; lower values
  indicate more dispersed patterns.

*Network Structure by Level:*

- **Active Segments/Classes**: Number of nodes in the network. Decreases
  at higher levels as categories aggregate into segments.

- **Significant Edges**: Count of mobility pathways with relative risk
  \> 1. These represent stronger-than-expected mobility connections.

- **Network Density**: Proportion of possible edges that exist.
  Calculated as: (actual edges) / (possible edges). Range 0-1.

- **Isolated Segments**: Number of segments with no significant
  connections to other segments. Isolates represent positions that
  exchange members primarily internally or have only weak external
  connections.

**Section 3: DETAILED WEIGHTED DEGREE DISTRIBUTIONS (optional)**

Shows distribution statistics (Min, Q1, Median, Mean, Q3, Max) for:

- **Total Weighted Connections**: Sum of weighted incoming and outgoing
  connections (strength) for each node. Represents the total volume of
  mobility flows, not just the count of connections.

- **Outward Mobility Strength**: Sum of weights on outgoing edges from
  each origin. High out-strength indicates positions that send large
  volumes to other destinations, weighted by relative risk.

- **Inward Mobility Strength**: Sum of weights on incoming edges to each
  destination. High in-strength indicates positions that receive large
  volumes from other origins, weighted by relative risk.

- **Edge Weight Distribution**: Distribution of relative risk values for
  significant edges. Values \> 1 indicate mobility above expected
  levels. Higher values represent stronger mobility pathways.

## Technical Terms

- **Isolate**:

  A node (category or segment) with no significant connections to other
  nodes. In mobility analysis, isolates represent positions that either
  have very high internal retention or only weak exchange with other
  positions. Not necessarily problematic - may indicate genuinely
  distinct positions.

- **Relative Risk**:

  Ratio of observed to expected mobility under independence. Values \> 1
  indicate mobility exceeds random expectation; \< 1 indicates below
  expectation. Used as edge weights in the network.

- **Network Density**:

  Proportion of all possible connections that actually exist. In a fully
  connected network (density = 1), every position has significant
  mobility to every other position. Low density indicates mobility is
  channeled through specific pathways.

- **Degree/Strength**:

  In weighted networks, we use **strength** instead of simple degree:
  the sum of edge weights connected to a node. In-strength = sum of
  incoming edge weights, out-strength = sum of outgoing edge weights,
  total strength = sum of both. High strength nodes handle large volumes
  of mobility flow, not just many connections.

- **Segmentation Level**:

  Hierarchical aggregation level. Level 1 = original categories, Level 2
  = first aggregation based on cliques, etc. Higher levels represent
  coarser groupings.

## See also

[`moneca`](https://gmontaletti.github.io/MONECA/reference/moneca.md) for
the main analysis function,
[`segment.quality`](https://gmontaletti.github.io/MONECA/reference/segment.quality.md)
for detailed segment metrics,
[`vignette("moneca-introduction")`](https://gmontaletti.github.io/MONECA/articles/moneca-introduction.md)
for methodology details

## Examples

``` r
# Generate data and run analysis
mobility_data <- generate_mobility_data(n_classes = 6, seed = 42)
seg <- moneca(mobility_data, segment.levels = 3)
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

# Print comprehensive summary with all statistics
print(seg)
#> 
#> ================================================================================
#>                         moneca MOBILITY ANALYSIS RESULTS                        
#> ================================================================================
#> 
#> OVERALL MOBILITY PATTERNS
#> -------------------------------------------------------------------------------
#> Overall Population Mobility Rate:                    39.3%
#> Average Mobility Concentration (all levels):         85.7%
#> 
#> HIERARCHICAL SEGMENTATION ANALYSIS
#> -------------------------------------------------------------------------------
#> 
#> Internal Mobility Within Segments (%):
#> Level 1 Level 2 Level 3 Level 4 
#>    60.7    74.0    89.2   100.0 
#> 
#> Mobility Concentration in Significant Pathways by Level (%):
#> Level 1 Level 2 Level 3 Level 4 
#>    79.5    74.0    89.2   100.0 
#> 
#> Network Structure by Level:
#>                                    Level 1      Level 2      Level 3      Level 4 
#> -------------------------------------------------------------------------------
#> Active Segments/Classes:                 6            3            2            1 
#> Significant Edges:                       7            0            0            0 
#> Network Density:                     0.233        0.000        0.000          NaN 
#> Isolated Segments:                       0            3            2            1 
#> 
#> DETAILED WEIGHTED DEGREE DISTRIBUTIONS (STRENGTH)
#> -------------------------------------------------------------------------------
#> 
#> Total Weighted Connections (Strength In + Out):
#>          Min   Q1 Median Mean   Q3  Max
#> Level 1 1.11 1.16   2.64 2.97 4.62 5.47
#> Level 2 0.00 0.00   0.00 0.00 0.00 0.00
#> Level 3 0.00 0.00   0.00 0.00 0.00 0.00
#> Level 4 0.00 0.00   0.00 0.00 0.00 0.00
#> 
#> Outward Mobility Strength (Weighted Out-Degree):
#>         Min   Q1 Median Mean   Q3  Max
#> Level 1   0 1.04   1.21 1.48 2.09 3.12
#> Level 2   0 0.00   0.00 0.00 0.00 0.00
#> Level 3   0 0.00   0.00 0.00 0.00 0.00
#> Level 4   0 0.00   0.00 0.00 0.00 0.00
#> 
#> Inward Mobility Strength (Weighted In-Degree):
#>         Min   Q1 Median Mean   Q3  Max
#> Level 1   0 0.28   1.36 1.48 2.17 3.82
#> Level 2   0 0.00   0.00 0.00 0.00 0.00
#> Level 3   0 0.00   0.00 0.00 0.00 0.00
#> Level 4   0 0.00   0.00 0.00 0.00 0.00
#> 
#> Edge Weight Distribution (Relative Risk Values):
#>          Min   Q1 Median Mean   Q3  Max
#> Level 1 1.02 1.06    1.3 1.27 1.42 1.62
#> Level 2   NA   NA     NA  NaN   NA   NA
#> Level 3   NA   NA     NA  NaN   NA   NA
#> Level 4   NA   NA     NA  NaN   NA   NA
#> 
#> ================================================================================

# Hide detailed degree distributions for cleaner output
print(seg, show.degree.stats = FALSE)
#> 
#> ================================================================================
#>                         moneca MOBILITY ANALYSIS RESULTS                        
#> ================================================================================
#> 
#> OVERALL MOBILITY PATTERNS
#> -------------------------------------------------------------------------------
#> Overall Population Mobility Rate:                    39.3%
#> Average Mobility Concentration (all levels):         85.7%
#> 
#> HIERARCHICAL SEGMENTATION ANALYSIS
#> -------------------------------------------------------------------------------
#> 
#> Internal Mobility Within Segments (%):
#> Level 1 Level 2 Level 3 Level 4 
#>    60.7    74.0    89.2   100.0 
#> 
#> Mobility Concentration in Significant Pathways by Level (%):
#> Level 1 Level 2 Level 3 Level 4 
#>    79.5    74.0    89.2   100.0 
#> 
#> Network Structure by Level:
#>                                    Level 1      Level 2      Level 3      Level 4 
#> -------------------------------------------------------------------------------
#> Active Segments/Classes:                 6            3            2            1 
#> Significant Edges:                       7            0            0            0 
#> Network Density:                     0.233        0.000        0.000          NaN 
#> Isolated Segments:                       0            3            2            1 
#> 
#> ================================================================================

# Show more decimal places for precision
print(seg, digits = 2)
#> 
#> ================================================================================
#>                         moneca MOBILITY ANALYSIS RESULTS                        
#> ================================================================================
#> 
#> OVERALL MOBILITY PATTERNS
#> -------------------------------------------------------------------------------
#> Overall Population Mobility Rate:                    39.3%
#> Average Mobility Concentration (all levels):         85.7%
#> 
#> HIERARCHICAL SEGMENTATION ANALYSIS
#> -------------------------------------------------------------------------------
#> 
#> Internal Mobility Within Segments (%):
#> Level 1 Level 2 Level 3 Level 4 
#>   60.68   74.03   89.21  100.00 
#> 
#> Mobility Concentration in Significant Pathways by Level (%):
#> Level 1 Level 2 Level 3 Level 4 
#>   79.48   74.03   89.21  100.00 
#> 
#> Network Structure by Level:
#>                                    Level 1      Level 2      Level 3      Level 4 
#> -------------------------------------------------------------------------------
#> Active Segments/Classes:                 6            3            2            1 
#> Significant Edges:                       7            0            0            0 
#> Network Density:                     0.233        0.000        0.000          NaN 
#> Isolated Segments:                       0            3            2            1 
#> 
#> DETAILED WEIGHTED DEGREE DISTRIBUTIONS (STRENGTH)
#> -------------------------------------------------------------------------------
#> 
#> Total Weighted Connections (Strength In + Out):
#>          Min   Q1 Median Mean   Q3  Max
#> Level 1 1.11 1.16   2.64 2.97 4.62 5.47
#> Level 2 0.00 0.00   0.00 0.00 0.00 0.00
#> Level 3 0.00 0.00   0.00 0.00 0.00 0.00
#> Level 4 0.00 0.00   0.00 0.00 0.00 0.00
#> 
#> Outward Mobility Strength (Weighted Out-Degree):
#>         Min   Q1 Median Mean   Q3  Max
#> Level 1   0 1.04   1.21 1.48 2.09 3.12
#> Level 2   0 0.00   0.00 0.00 0.00 0.00
#> Level 3   0 0.00   0.00 0.00 0.00 0.00
#> Level 4   0 0.00   0.00 0.00 0.00 0.00
#> 
#> Inward Mobility Strength (Weighted In-Degree):
#>         Min   Q1 Median Mean   Q3  Max
#> Level 1   0 0.28   1.36 1.48 2.17 3.82
#> Level 2   0 0.00   0.00 0.00 0.00 0.00
#> Level 3   0 0.00   0.00 0.00 0.00 0.00
#> Level 4   0 0.00   0.00 0.00 0.00 0.00
#> 
#> Edge Weight Distribution (Relative Risk Values):
#>          Min   Q1 Median Mean   Q3  Max
#> Level 1 1.02 1.06    1.3 1.27 1.42 1.62
#> Level 2   NA   NA     NA  NaN   NA   NA
#> Level 3   NA   NA     NA  NaN   NA   NA
#> Level 4   NA   NA     NA  NaN   NA   NA
#> 
#> ================================================================================
```
