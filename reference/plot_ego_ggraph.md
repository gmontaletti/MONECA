# Ego Network Visualization with ggraph

Creates focused visualizations of mobility patterns from a single focal
node (ego). This function shows all incoming and outgoing mobility flows
for a specific category, making it ideal for understanding individual
position dynamics.

## Usage

``` r
plot_ego_ggraph(
  segments,
  mobility_matrix,
  ego_id,
  min_weight = 0,
  layout = "stress",
  highlight_color = "red",
  flow_color = "viridis",
  node_size_range = c(2, 8),
  edge_width_range = c(0.2, 3),
  title = NULL,
  segment_naming = "auto",
  ...
)
```

## Arguments

- segments:

  A moneca object returned by
  [`moneca`](https://gmontaletti.github.io/MONECA/reference/moneca.md).

- mobility_matrix:

  The original mobility matrix used in the moneca analysis. Should
  include row and column totals.

- ego_id:

  Integer or character specifying the focal node. Can be:

  - Integer: Row/column index in the mobility matrix

  - Character: Row/column name from the mobility matrix

- min_weight:

  Numeric threshold for minimum edge weight to include nodes. Only nodes
  connected to the ego with edge weights \>= min_weight will be shown.
  Default is 0 (show all connected nodes). Use higher values to focus on
  stronger mobility flows.

- layout:

  Character string specifying the layout algorithm. Default is "stress"
  which often works well for ego networks. Other options include "fr",
  "kk", "dh".

- highlight_color:

  Color for the ego (focal) node. Default is "red".

- flow_color:

  Character string specifying the color scheme for mobility flows.
  Default is "viridis". Can be any viridis variant ("viridis", "plasma",
  "inferno", etc.).

- node_size_range:

  Numeric vector of length 2 specifying the range for node sizes.
  Default is c(3, 12).

- edge_width_range:

  Numeric vector of length 2 specifying the range for edge widths.
  Default is c(0.5, 3).

- title:

  Character string for plot title. Default is NULL.

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
  business names or multilingual applications.

- ...:

  Additional arguments passed to the ggraph layout function.

## Value

A ggplot2 object showing the ego network visualization.

## Details

Ego networks are particularly useful for understanding the mobility
patterns of specific social positions. The visualization highlights:

- The focal position (ego) in a distinct color

- Incoming mobility flows (edges pointing to ego)

- Outgoing mobility flows (edges from ego)

- The relative strength of different flows through edge width and color

Only non-zero mobility flows are displayed to reduce visual clutter.
Edge colors and widths are scaled to represent the volume of mobility
flows.

## See also

[`plot_moneca_ggraph`](https://gmontaletti.github.io/MONECA/reference/plot_moneca_ggraph.md)
for full network visualization,
[`plot_stair_ggraph`](https://gmontaletti.github.io/MONECA/reference/plot_stair_ggraph.md)
for multi-level visualization,
[`moneca`](https://gmontaletti.github.io/MONECA/reference/moneca.md) for
the main analysis function

## Examples

``` r
if (FALSE) { # \dontrun{
# Generate synthetic data and run MONECA
mobility_data <- generate_mobility_data(n_classes = 6, seed = 123)
seg <- moneca(mobility_data, segment.levels = 3)

# Ego network for the middle category (index 3)
plot_ego_ggraph(seg, mobility_data, ego_id = 3,
                title = "Mobility from Middle Class")
} # }
```
