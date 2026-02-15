# Modern Network Visualization for MONECA Results

Creates sophisticated network visualizations of MONECA clustering
results using ggraph and ggplot2. This function provides modern, highly
customizable plots with support for multiple layout algorithms, node
aesthetics, and segment highlighting.

## Usage

``` r
plot_moneca_ggraph(
  segments,
  level = NULL,
  layout = "fr",
  edges = "auto",
  node_size = "total",
  node_color = "segment",
  node_alpha = 0.8,
  edge_width = "weight",
  edge_color = "grey50",
  edge_alpha = 0.6,
  show_labels = FALSE,
  label_size = 3,
  show_segments = TRUE,
  segment_alpha = 0.3,
  color_palette = "Set3",
  theme_style = "void",
  title = NULL,
  segment_naming = "auto",
  ...
)
```

## Arguments

- segments:

  A moneca object returned by
  [`moneca`](https://gmontaletti.github.io/MONECA/reference/moneca.md).

- level:

  Integer vector specifying which hierarchical levels to visualize.
  Default displays all levels except the first (which represents
  individual categories).

- layout:

  Character string or matrix specifying the layout algorithm:

  - "fr" (default): Fruchterman-Reingold force-directed layout

  - "kk": Kamada-Kawai layout

  - "dh": Davidson-Harel layout

  - "mds": Multidimensional scaling

  - "stress": Stress majorization

  - Matrix: Custom coordinate matrix (n_nodes x 2)

- edges:

  Edge matrix or "auto" to automatically generate using
  [`segment.edges`](https://gmontaletti.github.io/MONECA/reference/segment.edges.md).
  Default is "auto".

- node_size:

  Aesthetic mapping for node size:

  - "total": Size by total mobility volume (default)

  - "mobility": Size by off-diagonal mobility rate

  - Numeric vector: Custom sizes for each node

  - Single numeric: Fixed size for all nodes

- node_color:

  Aesthetic mapping for node color:

  - "segment" (default): Color by segment membership

  - "mobility": Color by mobility rate

  - Character vector: Custom colors for each node

  - Single color: Fixed color for all nodes

- node_alpha:

  Numeric value (0-1) for node transparency. Default is 0.8.

- edge_width:

  Aesthetic for edge width:

  - "weight" (default): Width proportional to edge weight

  - Numeric: Fixed width for all edges

- edge_color:

  Color for edges. Default is "grey50".

- edge_alpha:

  Numeric value (0-1) for edge transparency. Default is 0.6.

- show_labels:

  Logical indicating whether to display individual node labels. Default
  is FALSE (only segment/cluster labels are shown).

- label_size:

  Numeric size for node labels. Default is 3.

- show_segments:

  Logical indicating whether to highlight segment boundaries. Default is
  TRUE.

- segment_alpha:

  Numeric value (0-1) for segment boundary transparency. Default is 0.3.

- color_palette:

  Character string specifying the color palette for segments. Can be any
  RColorBrewer palette name. Default is "Set3".

- theme_style:

  Character string specifying the plot theme:

  - "void" (default): Clean background with no axes

  - "minimal": Minimal theme with subtle gridlines

  - "classic": Traditional ggplot2 theme

- title:

  Character string for plot title. Default is NULL (no title).

- segment_naming:

  Specifies how to name segments in the visualization. Can be:

  - Character string: "auto" (default), "concat", "pattern", or
    "custom" - these are passed to
    [`segment.membership.enhanced`](https://gmontaletti.github.io/MONECA/reference/segment.membership.enhanced.md)
    for automatic naming

  - data.frame: Custom segment labels with columns "name" (node names
    from the mobility matrix) and "segment_label" (desired custom
    labels). This allows complete control over segment naming

  - NULL: Uses default "auto" strategy

  When a data.frame is provided, custom labels override automatically
  generated names. The data.frame approach is useful for meaningful
  business names (e.g., "Upper Management" instead of "Segment 1") or
  multilingual applications.

- ...:

  Additional arguments passed to ggraph layout functions.

## Value

When `level` is a single integer, a `ggplot2` object that can be further
customized. When `level` is a vector (including the default), a named
list of `ggplot2` objects (one per level), each viewable individually at
full size. Names are `"level_2"`, `"level_3"`, etc.

## Details

This function creates publication-quality network visualizations with
extensive customization options. It automatically handles node
positioning, edge rendering, and segment highlighting. The resulting
plot can be further modified using standard ggplot2 syntax.

For interactive exploration, different layout algorithms may work better
with different network structures. Force-directed layouts ("fr") work
well for most cases, while "stress" layouts often produce cleaner
results for dense networks.

**Segment Naming**: The new data.frame approach for segment_naming
provides complete control over how segments are labeled. This is
particularly useful for:

- Business applications where meaningful names are essential (e.g.,
  "Senior Management" vs "Segment 1")

- Multilingual visualizations where labels need translation

- Presentations where consistent, professional terminology is required

- Partial customization where only specific segments need custom names

## See also

[`moneca`](https://gmontaletti.github.io/MONECA/reference/moneca.md) for
the main analysis function,
[`plot_ego_ggraph`](https://gmontaletti.github.io/MONECA/reference/plot_ego_ggraph.md)
for ego network visualization,
[`plot_stair_ggraph`](https://gmontaletti.github.io/MONECA/reference/plot_stair_ggraph.md)
for multi-level visualization,
[`segment.edges`](https://gmontaletti.github.io/MONECA/reference/segment.edges.md)
for edge matrix generation

## Examples

``` r
if (FALSE) { # \dontrun{
# Generate synthetic data and run MONECA
mobility_data <- generate_mobility_data(n_classes = 6, seed = 123)
seg <- moneca(mobility_data, segment.levels = 3)

# Basic network plot
plot_moneca_ggraph(seg)

# Customized plot with different aesthetics
plot_moneca_ggraph(seg,
  layout = "stress",
  node_color = "mobility",
  color_palette = "Spectral",
  title = "Social Mobility Network"
)
} # }
```
