# Legacy ggplot2 Visualization for moneca Objects

**Deprecated**. Use
[`plot_moneca_ggraph`](https://gmontaletti.github.io/MONECA/reference/plot_moneca_ggraph.md)
instead. This function will be removed in moneca 1.2.0.

## Usage

``` r
gg.moneca(
  segments,
  level = NULL,
  layout = NULL,
  edges = NULL,
  mode = "directed",
  vertex.size = "total",
  vertex.fill = "segment",
  vertex.alpha = 1,
  vertex.color = "black",
  vertex.shape = 21,
  show.edges = TRUE,
  edge.size = 1,
  edge.alpha = "weight",
  edge.color = "weight",
  edge.line = "solid",
  show.text = TRUE,
  text.size = 3,
  text.color = "black",
  text.alpha = 1,
  text.vjust = 1.5,
  show.borders = TRUE,
  border.size = 1,
  border.fill = NA,
  border.color = "black",
  border.alpha = 1,
  border.padding = 0.7,
  border.text = TRUE,
  border.labels = "segments",
  border.text.size = 4,
  border.text.color = "black",
  border.text.vjust = -0.2,
  border.text.hjust = 1,
  midpoints = TRUE,
  midpoint.arrow = arrow(angle = 20, length = unit(0.33, "cm"), ends = "last", type =
    "closed"),
  edge.text = FALSE,
  edge.text.size = 3,
  edge.text.alpha = 0.9,
  legend = "side"
)
```

## Arguments

- segments:

  A moneca object returned by
  [`moneca`](https://gmontaletti.github.io/MONECA/reference/moneca.md).

- level:

  Integer vector specifying which hierarchical levels to display.

- layout:

  Matrix of node coordinates or layout function result.

- edges:

  Edge matrix or transformed edge weights for visualization.

- mode:

  Character string specifying graph mode ("directed" or "undirected").

- vertex.size:

  Aesthetic for vertex size. Can be "total", "mobility", or numeric.

- vertex.fill:

  Aesthetic for vertex fill color. Can be "segment" or color
  specification.

- vertex.alpha:

  Numeric value (0-1) for vertex transparency.

- vertex.color:

  Color for vertex borders.

- vertex.shape:

  Numeric code for vertex shape (see ggplot2 shapes).

- show.edges:

  Logical indicating whether to display edges.

- edge.size:

  Size specification for edges.

- edge.alpha:

  Transparency for edges. Can be "weight" or numeric.

- edge.color:

  Color specification for edges. Can be "weight" or color name.

- edge.line:

  Line type for edges ("solid", "dashed", etc.).

- show.text:

  Logical indicating whether to show vertex labels.

- text.size:

  Numeric size for vertex labels.

- text.color:

  Color for vertex labels.

- text.alpha:

  Transparency for vertex labels.

- text.vjust:

  Vertical adjustment for labels.

- show.borders:

  Logical indicating whether to show segment boundaries.

- border.size:

  Size for segment borders.

- border.fill:

  Fill color for segment boundaries.

- border.color:

  Color for segment border lines.

- border.alpha:

  Transparency for segment borders.

- border.padding:

  Padding around segment boundaries.

- border.text:

  Logical indicating whether to show segment labels.

- border.labels:

  Character vector of custom segment labels.

- border.text.size:

  Size for segment labels.

- border.text.color:

  Color for segment labels.

- border.text.vjust:

  Vertical adjustment for segment labels.

- border.text.hjust:

  Horizontal adjustment for segment labels.

- midpoints:

  Logical indicating whether to show edge midpoints.

- midpoint.arrow:

  Logical indicating whether to show arrows at midpoints.

- edge.text:

  Logical indicating whether to show edge labels.

- edge.text.size:

  Size for edge labels.

- edge.text.alpha:

  Transparency for edge labels.

- legend:

  Position for legend ("side", "bottom", "none", etc.).

## Value

A ggplot2 object.

## Details

Creates network visualizations of moneca clustering results using
ggplot2. This function provides extensive customization options but has
been superseded by
[`plot_moneca_ggraph`](https://gmontaletti.github.io/MONECA/reference/plot_moneca_ggraph.md)
for most use cases.

This function provides a highly customizable but complex interface for
creating moneca visualizations. It requires the eliter package for some
functionality. For most users,
[`plot_moneca_ggraph`](https://gmontaletti.github.io/MONECA/reference/plot_moneca_ggraph.md)
offers a more modern and user-friendly interface with better defaults.

**Note**: This function is maintained for backward compatibility but is
no longer actively developed. New features are added to the ggraph-based
plotting functions instead.

## See also

[`plot_moneca_ggraph`](https://gmontaletti.github.io/MONECA/reference/plot_moneca_ggraph.md)
for modern ggraph-based plotting,
[`moneca.plot`](https://gmontaletti.github.io/MONECA/reference/moneca.plot.md)
for base graphics plotting

## Examples

``` r
if (FALSE) { # \dontrun{
# Using synthetic data
mobility_data <- generate_mobility_data(n_classes = 5, seed = 123)
seg <- moneca(mobility_data, segment.levels = 2)
gg.moneca(seg)

# Custom styling
gg.moneca(seg,
         vertex.fill = "red",
         edge.color = "blue",
         show.borders = FALSE)
} # }
```
