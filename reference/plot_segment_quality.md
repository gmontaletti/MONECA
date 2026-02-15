# Visualize Segment Quality Metrics

Creates comprehensive visualizations of segment quality metrics from
MONECA analysis. This function provides multiple plot types to help
assess the quality and characteristics of the segmentation.

## Usage

``` r
plot_segment_quality(
  segments,
  plot_type = "overview",
  level = 2,
  metrics = NULL,
  color_palette = NULL,
  theme_style = "minimal",
  title = NULL,
  show_labels = TRUE,
  label_size = 3,
  segment_naming = "auto"
)
```

## Arguments

- segments:

  A moneca object returned by
  [`moneca`](https://gmontaletti.github.io/MONECA/reference/moneca.md).

- plot_type:

  Character string specifying the type of visualization:

  - "overview" (default): Multi-panel overview of key metrics

  - "cohesion": Within-mobility vs segment size scatter plot

  - "radar": Radar/spider plot of normalized metrics per segment

  - "heatmap": Heatmap of all metrics across levels

  - "evolution": Line plot showing metric evolution across levels

- level:

  Integer specifying which hierarchical level to visualize. Default
  is 2. Use NULL for all levels (only valid for some plot types).

- metrics:

  Character vector of metrics to include. Default includes all. Options:
  "within.mobility", "share.of.mobility", "Density", "Nodes",
  "Max.path", "share.of.total"

- color_palette:

  Character string specifying the color palette. Default is
  `"okabe-ito"` (CVD-safe) for categorical data, `"RdYlBu"` for
  continuous. Also accepts RColorBrewer names or viridis options.

- theme_style:

  Character string specifying the plot theme: "minimal" (default),
  "classic", or "void".

- title:

  Character string for plot title. Default is auto-generated based on
  plot type.

- show_labels:

  Logical indicating whether to show segment labels. Default is TRUE.

- label_size:

  Numeric size for labels. Default is 3.

- segment_naming:

  Specifies how to name segments in the visualization. Can be:

  - Character string: "auto" (default), "concat", "pattern", or
    "custom" - these are passed to
    [`segment.membership.enhanced`](https://gmontaletti.github.io/MONECA/reference/segment.membership.enhanced.md)
    for automatic naming

  - data.frame: Custom segment labels with required columns "name" and
    "segment_label". **This parameter accepts the direct output from
    [`segment.membership.enhanced`](https://gmontaletti.github.io/MONECA/reference/segment.membership.enhanced.md),**
    making it easy to maintain consistent segment naming across all
    visualizations. The data.frame must have:

    - "name": Node names from the original mobility matrix

    - "segment_label": Meaningful segment names (e.g.,
      "Executive_Leadership")

  - NULL: Uses default "auto" strategy

  **Recommended workflow:** Generate enhanced membership with
  `enhanced <- segment.membership.enhanced(segments, naming_strategy = "pattern")`
  and pass directly:
  `plot_segment_quality(segments, segment_naming = enhanced)`. This
  ensures consistent, meaningful segment names across all plot types and
  visualizations.

## Value

A ggplot2 object (or list of ggplot2 objects for "overview" type).

## Details

Each plot type serves a different analytical purpose:

**Overview**: Four-panel display showing:

- Within-mobility by segment (bar chart)

- Segment sizes (nodes)

- Network density

- Share of total mobility

**Cohesion**: Scatter plot with within-mobility on y-axis and segment
size on x-axis. Ideal segments appear in the upper-right (large and
cohesive). Points are sized by share of mobility.

**Radar**: Multi-dimensional comparison showing all metrics normalized
to 0-1 scale. Useful for comparing segment profiles.

**Heatmap**: Shows all metrics across all levels and segments. Colors
indicate metric values, making patterns easy to spot.

**Evolution**: Line plots showing how each metric changes across
hierarchical levels for each segment lineage.

## See also

[`segment.quality`](https://gmontaletti.github.io/MONECA/reference/segment.quality.md)
for the underlying metrics,
[`moneca`](https://gmontaletti.github.io/MONECA/reference/moneca.md) for
the main analysis function,
[`plot_moneca_ggraph`](https://gmontaletti.github.io/MONECA/reference/plot_moneca_ggraph.md)
for network visualization

## Examples

``` r
if (FALSE) { # \dontrun{
# Generate data and run MONECA
mobility_data <- generate_mobility_data(n_classes = 8, seed = 123)
seg <- moneca(mobility_data, segment.levels = 3)

# Overview of level 2 quality
plot_segment_quality(seg)

# Radar plot for segment comparison
plot_segment_quality(seg, plot_type = "radar", level = 2)
} # }
```
