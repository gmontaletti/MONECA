# Multi-Level Segmentation Visualization (Stair Plot)

Creates a series of network plots showing how segmentation evolves
across hierarchical levels in a moneca analysis. This "stair plot"
provides insight into the progressive clustering of social positions.

## Usage

``` r
plot_stair_ggraph(
  segments,
  levels = seq_along(segments$segment.list),
  layout = NULL,
  ncol = 2,
  segment_naming = "auto",
  include_first_level = TRUE,
  ...
)
```

## Arguments

- segments:

  A moneca object returned by
  [`moneca`](https://gmontaletti.github.io/MONECA/reference/moneca.md).

- levels:

  Integer vector specifying which hierarchical levels to visualize.
  Default includes all levels.

- layout:

  Layout specification for consistency across plots. Can be:

  - NULL (default): Use layout.matrix() for consistent positioning

  - Character string: Layout algorithm name ("fr", "kk", "stress", etc.)

  - Matrix: Custom coordinate matrix for node positions

- ncol:

  Integer specifying the number of columns in the plot grid. Default
  is 2. Set to 1 for vertical arrangement.

- segment_naming:

  Specifies how to name segments across all visualization levels. Can
  be:

  - Character string: "auto" (default), "concat", "pattern", or
    "custom" - these are passed to
    [`segment.membership.enhanced`](https://gmontaletti.github.io/MONECA/reference/segment.membership.enhanced.md)
    for automatic naming

  - data.frame: Custom segment labels with columns "name" (node names
    from the mobility matrix) and "segment_label" (desired custom
    labels). This allows complete control over segment naming across all
    hierarchical levels

  - NULL: Uses default "auto" strategy

  When a data.frame is provided, custom labels override automatically
  generated names consistently across all levels. The data.frame
  approach is particularly useful for stair plots as it maintains
  consistent naming across the hierarchical progression.

- include_first_level:

  Logical indicating whether to include the first level (individual
  classes without segmentation). Default is TRUE.

- ...:

  Additional arguments passed to
  [`plot_moneca_ggraph`](https://gmontaletti.github.io/MONECA/reference/plot_moneca_ggraph.md).

## Value

If `combine_plots = TRUE`, returns a combined plot grid object. If
`combine_plots = FALSE`, returns a list of ggplot objects, one for each
level.

## Details

The stair plot helps visualize the hierarchical nature of MONECA
segmentation by showing how larger segments at higher levels break down
into smaller, more specific segments at lower levels. This is
particularly useful for:

- Understanding the segmentation process

- Identifying optimal levels of analysis

- Presenting results to different audiences

- Comparing segmentation stability across levels

When using a consistent layout across all plots, the relative positions
of nodes remain the same, making it easier to track how segments evolve.

## See also

[`plot_moneca_ggraph`](https://gmontaletti.github.io/MONECA/reference/plot_moneca_ggraph.md)
for single-level visualization,
[`plot_ego_ggraph`](https://gmontaletti.github.io/MONECA/reference/plot_ego_ggraph.md)
for ego network analysis,
[`layout.matrix`](https://gmontaletti.github.io/MONECA/reference/layout.matrix.md)
for consistent layouts,
[`moneca`](https://gmontaletti.github.io/MONECA/reference/moneca.md) for
the main analysis function

## Examples

``` r
if (FALSE) { # \dontrun{
# Generate synthetic data and run MONECA
mobility_data <- generate_mobility_data(n_classes = 6, seed = 123)
seg <- moneca(mobility_data, segment.levels = 4)

# Basic stair plot
stair_plots <- plot_stair_ggraph(seg)

# Customized stair plot with specific levels
custom_stair <- plot_stair_ggraph(seg,
                                 levels = c(2, 3),
                                 layout = "stress",
                                 ncol = 1)
} # }
```
