# New MONECA - Fast Clustering with Maximal Cliques

A variant of MONECA that uses maximal cliques for improved performance.
Maximal cliques are complete subgraphs that cannot be extended by adding
another node, providing faster computation at the cost of potentially
different clustering results compared to the standard algorithm.

## Usage

``` r
new_moneca(
  mx = mx,
  segment.levels = 3,
  cut.off = 1,
  mode = "symmetric",
  delete.upper.tri = TRUE,
  small.cell.reduction = 0,
  auto_tune = FALSE,
  tune_method = "stability",
  tune_verbose = FALSE,
  max.clique.size = NULL,
  progress = FALSE,
  use.sparse = FALSE,
  min.density = 0
)
```

## Arguments

- mx:

  A mobility table (square matrix) with row and column totals in the
  last row/column. Row names should identify the categories/classes.

- segment.levels:

  Integer specifying the number of hierarchical segmentation levels to
  compute. Default is 3. The algorithm may return fewer levels if no
  further meaningful segmentation is possible.

- cut.off:

  Numeric threshold for the minimum relative risk to be considered a
  significant tie. Default is 1 (no mobility above random expectation
  required).

- mode:

  Character string specifying edge mode ("symmetric", "Mutual", or
  "Unmutual"). Currently not fully implemented - uses symmetric mode.

- delete.upper.tri:

  Logical indicating whether to use only lower triangle for efficiency.
  Default is TRUE.

- small.cell.reduction:

  Numeric value to handle small cell counts. Cells with counts below
  this threshold are set to 0. Default is 0 (no reduction).

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

- max.clique.size:

  Maximum size of cliques to consider (NULL for no limit). Default is
  NULL. Useful for preventing memory issues with very dense graphs. When
  specified, uses the optimized
  [`moneca_fast`](https://gmontaletti.github.io/MONECA/reference/moneca_fast.md)
  implementation.

- progress:

  Logical indicating whether to show progress bars during computation.
  Default is FALSE. Set to TRUE for long-running analyses.

- use.sparse:

  Logical indicating whether to use sparse matrices for large data.
  Default is FALSE. Only applicable when max.clique.size is specified.

- min.density:

  Minimum strength-based density to continue processing. Calculated as
  mean(strength)/max(strength). Default is 0 (disabled). Only applicable
  when max.clique.size is specified. Set to 0.01 or higher for early
  stopping optimization on sparse graphs.

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

## Details

This function always uses maximal cliques (via
[`igraph::max_cliques()`](https://r.igraph.org/reference/cliques.html)),
which typically results in:

- **Faster computation:** Maximal cliques are fewer in number and faster
  to find than all cliques

- **Different results:** Clustering may differ from standard MONECA as
  maximal cliques are more restrictive

- **Better scalability:** Recommended for large networks (\>20 nodes) or
  dense graphs

The function intelligently selects the underlying implementation:

- If `max.clique.size`, `progress`, `use.sparse`, or `min.density` are
  specified, uses
  [`moneca_fast`](https://gmontaletti.github.io/MONECA/reference/moneca_fast.md)
  for advanced performance features

- Otherwise, uses the standard
  [`moneca`](https://gmontaletti.github.io/MONECA/reference/moneca.md)
  implementation

**When to use new_moneca():**

- Working with large networks (\>20 nodes)

- Dense mobility tables where standard MONECA is slow

- When computational speed is prioritized over exhaustive clique
  enumeration

- Exploratory analysis where approximate clustering is acceptable

**When to use standard moneca():**

- Small to medium networks (\<20 nodes)

- When you need the most comprehensive clique-based segmentation

- Publication-quality results requiring the original algorithm

- When computational time is not a constraint

## Performance Considerations

The performance gain from using maximal cliques depends on graph
density:

- **Sparse graphs:** 2-5x speedup

- **Medium density:** 5-20x speedup

- **Dense graphs:** 20-100x speedup

## Result Differences

Maximal cliques produce different but valid results because they
represent a more restrictive interpretation of network structure. Key
differences:

- Maximal cliques may merge some segments that all-cliques would
  separate

- Hierarchical structure may have fewer levels

- Overall clustering quality remains high but interpretation differs

## References

Toubøl, J., & Larsen, A. G. (2017). Mapping the Social Class Structure:
From Occupational Mobility to Social Class Categories Using Network
Analysis. Sociology, 51(6), 1257-1276.

## See also

**Related functions:**
[`moneca`](https://gmontaletti.github.io/MONECA/reference/moneca.md) for
the standard algorithm using all cliques,
[`moneca_fast`](https://gmontaletti.github.io/MONECA/reference/moneca_fast.md)
for single-core optimization

**Visualization:**
[`plot_moneca_ggraph`](https://gmontaletti.github.io/MONECA/reference/plot_moneca_ggraph.md)
for network visualization,
[`segment.membership`](https://gmontaletti.github.io/MONECA/reference/segment.membership.md)
for extracting segment memberships

## Examples

``` r
if (FALSE) { # \dontrun{
# Generate synthetic mobility data
mobility_data <- generate_mobility_data(n_classes = 6, seed = 42)

# Basic usage - uses maximal cliques
seg_new <- new_moneca(mobility_data, segment.levels = 3)
print(seg_new)

# Compare with standard moneca
seg_standard <- moneca(mobility_data, segment.levels = 3)

# Visualize results
plot_moneca_ggraph(seg_new, node_color = "segment")
} # }
```
