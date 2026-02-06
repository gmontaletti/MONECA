# MONECA Fast - Memory-Optimized Mobility Network Clustering Analysis

Memory-optimized version of the main function for performing
hierarchical clustering analysis on mobility tables. MONECA creates
weighted networks from mobility data and uses cliques to identify
discrete and nested clusters of positions with high internal mobility.

An optimized version of the moneca algorithm with improved performance
for large datasets. Implements various performance optimizations
including vectorization and early stopping. Supports automatic margin
detection and optional density reduction for large matrices.

## Usage

``` r
moneca_fast(
  mx = mx,
  segment.levels = 3,
  cut.off = 1,
  mode = "symmetric",
  delete.upper.tri = TRUE,
  small.cell.reduction = 0,
  use.sparse = FALSE,
  min.density = 0,
  max.clique.size = NULL,
  progress = TRUE,
  auto_tune = FALSE,
  tune_method = "stability",
  tune_verbose = FALSE,
  use_maximal_cliques = FALSE,
  isolates = FALSE,
  has_margins = "auto",
  reduce_density = "auto",
  density_params = list()
)

moneca_fast(
  mx = mx,
  segment.levels = 3,
  cut.off = 1,
  mode = "symmetric",
  delete.upper.tri = TRUE,
  small.cell.reduction = 0,
  use.sparse = FALSE,
  min.density = 0,
  max.clique.size = NULL,
  progress = TRUE,
  auto_tune = FALSE,
  tune_method = "stability",
  tune_verbose = FALSE,
  use_maximal_cliques = FALSE,
  isolates = FALSE,
  has_margins = "auto",
  reduce_density = "auto",
  density_params = list()
)
```

## Arguments

- mx:

  A mobility table (square matrix). Can include row and column totals in
  the last row/column, or be a core matrix without totals. Row names
  should identify the categories/classes. See `has_margins` parameter.

- segment.levels:

  Integer specifying the number of hierarchical segmentation levels to
  compute. Default is 3.

- cut.off:

  Numeric threshold for the minimum relative risk to be considered a
  significant tie. Default is 1.

- mode:

  Character string specifying edge mode. Currently uses symmetric mode.

- delete.upper.tri:

  Logical indicating whether to use only lower triangle. Default is
  TRUE.

- small.cell.reduction:

  Numeric value to handle small cell counts. Default is 0.

- use.sparse:

  Logical indicating whether to use sparse matrices for large data.
  Default is FALSE.

- min.density:

  Minimum strength-based density to continue processing. Calculated as
  mean(strength)/max(strength). Default is 0 (disabled for algorithmic
  fidelity). Set to 0.01 or higher for early stopping optimization on
  sparse graphs.

- max.clique.size:

  Maximum size of cliques to consider (NULL for no limit). Default is
  NULL.

- progress:

  Logical indicating whether to show progress bars. Default is TRUE.

- auto_tune:

  Logical indicating whether to automatically tune the
  small.cell.reduction parameter. When TRUE, uses optimization methods
  to select the best value. Default is FALSE.

- tune_method:

  Character string specifying the auto-tuning method when auto_tune is
  TRUE. Options are "stability" (default), "quality", or "performance".
  See
  [`auto_tune_small_cell_reduction`](https://gmontaletti.github.io/MONECA/reference/auto_tune_small_cell_reduction.md)
  for details.

- tune_verbose:

  Logical indicating whether to print verbose messages during
  auto-tuning. Default is FALSE.

- use_maximal_cliques:

  Logical indicating whether to use maximal cliques (faster, fewer
  cliques) instead of all cliques (slower, more complete enumeration).
  Default is FALSE (use all cliques for algorithmic correctness). Set to
  TRUE for performance optimization on very dense graphs.

- isolates:

  Logical. If TRUE, returns additional summary information about
  isolates (categories not belonging to any multi-member segment at the
  final level). Isolates are grouped into a category called "altri".
  Default is FALSE.

- has_margins:

  Controls whether the input matrix includes totals row/column.

  - `"auto"` (default): Automatically detects if the last row/column
    contains marginal totals

  - `TRUE`: Matrix already has totals in the last row/column

  - `FALSE`: Matrix is a core matrix without totals; margins will be
    generated automatically

- reduce_density:

  Controls density reduction preprocessing for large matrices.

  - `"auto"` (default): Applies density reduction when the matrix has
    30+ categories and the weight matrix count density exceeds 0.7

  - `TRUE`: Always apply density reduction

  - `FALSE`: Never apply density reduction

- density_params:

  Named list of additional parameters passed to
  [`reduce_density`](https://gmontaletti.github.io/MONECA/reference/reduce_density.md).
  For example: `list(method = "svd", normalization = "ppmi", k = 20)`.
  See
  [`reduce_density`](https://gmontaletti.github.io/MONECA/reference/reduce_density.md)
  for all available parameters.

## Value

An object of class "moneca" containing:

- segment.list:

  A list of segment memberships for each hierarchical level. Each
  element is a list of vectors containing the original row indices.

- mat.list:

  A list of aggregated mobility matrices for each level, where
  rows/columns represent segments instead of original categories.

- small.cell.reduction:

  The small cell reduction parameter used.

An object of class "moneca" with the same structure as the original
moneca() function. The returned list always includes:

- margins_added:

  Logical indicating whether margins were generated

- density_reduction:

  NULL if no density reduction was applied, or a list with metadata
  about the reduction (method, k, variance_explained, etc.)

When `isolates = TRUE`, the returned list also includes:

- isolates_summary:

  A list containing:

  membership

  :   Data frame with columns `name` (category name) and `group`
      (segment name or "altri" for isolates)

  mobility_matrix

  :   Matrix of mobility counts between groups, including "altri" group
      for isolates

## Details

This version includes intelligent memory management for dense graphs and
optimized clique enumeration to prevent memory issues with large
datasets.

MONECA implements an iterative algorithm that:

1.  Converts the mobility table to a relative risk matrix

2.  Identifies network cliques based on the threshold

3.  Groups nodes into segments using the clique structure

4.  Aggregates the mobility table by segments

5.  Repeats the process for the specified number of levels

The algorithm stops early if no further segmentation is possible (e.g.,
all nodes collapse into a single segment).

\*\*Algorithm Features:\*\*

- Simple and memory-efficient clique testing using direct subset
  checking

- Intelligent dense graph detection with fallback to maximal cliques

- Streamlined implementation based on the original bejler.test approach

- Maintains algorithmic correctness while minimizing memory overhead

This implementation is optimized for single-core performance using:

- Vectorized matrix operations for improved speed

- Optional sparse matrix support via `use.sparse` parameter

- Early stopping via `min.density` threshold (disabled by default)

- Clique size limiting via `max.clique.size` parameter

- Pre-computed max clique size for early skip of impossible merges

- Optimized node ordering in clique membership tests

**Margin Handling:** When `has_margins = "auto"`, the function checks
whether the last row and column of the matrix contain the row/column
sums of the core matrix. If not detected, margins are generated
automatically.

**Density Reduction:** Large matrices (30+ categories) with high density
can be preprocessed using
[`reduce_density`](https://gmontaletti.github.io/MONECA/reference/reduce_density.md)
to remove noise before clustering. This is controlled by
`reduce_density` and `density_params`.

**Produces identical results to**
[`moneca`](https://gmontaletti.github.io/MONECA/reference/moneca.md)
when using default parameters and `reduce_density = FALSE`.

**When to use:**

- Any system - recommended for most use cases

- Small to large datasets

- When you want explicit control over optimization parameters

## References

Toubøl, J., & Larsen, A. G. (2017). Mapping the Social Class Structure:
From Occupational Mobility to Social Class Categories Using Network
Analysis. Sociology, 51(6), 1257-1276.

## See also

[`moneca`](https://gmontaletti.github.io/MONECA/reference/moneca.md) for
the standard version with progress bar,
[`find.segments`](https://gmontaletti.github.io/MONECA/reference/find.segments.md)
for the core segmentation algorithm,
[`weight.matrix`](https://gmontaletti.github.io/MONECA/reference/weight.matrix.md)
for relative risk calculation,
[`plot_moneca_ggraph`](https://gmontaletti.github.io/MONECA/reference/plot_moneca_ggraph.md)
for modern visualization,
[`segment.membership`](https://gmontaletti.github.io/MONECA/reference/segment.membership.md)
for extracting memberships

[`moneca`](https://gmontaletti.github.io/MONECA/reference/moneca.md) for
the original implementation,
[`reduce_density`](https://gmontaletti.github.io/MONECA/reference/reduce_density.md)
for density reduction details

## Examples

``` r
# Generate synthetic mobility data
mobility_data <- generate_mobility_data(n_classes = 6, seed = 42)

# Run moneca analysis (fast version)
seg <- moneca_fast(mobility_data, segment.levels = 3)
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |                                                                      |   0%
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

# Run with auto-tuning for optimal parameters
seg_tuned <- moneca_fast(mobility_data, segment.levels = 3,
                         auto_tune = TRUE, tune_method = "stability")
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |                                                                      |   0%
print(seg_tuned)
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

# Examine segment membership
membership <- segment.membership(seg)
print(membership)
#>      name membership
#> 1 Class 1        4.1
#> 2 Class 2        4.1
#> 3 Class 3        4.1
#> 4 Class 4        4.1
#> 5 Class 5        4.1
#> 6 Class 6        4.1

# Visualize with modern plotting
if (FALSE) { # \dontrun{
plot_moneca_ggraph(seg, node_color = "segment", title = "MONECA Clustering")
} # }
```
