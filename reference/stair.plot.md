# Legacy Multi-Level Stair Plot

**Deprecated**. Use
[`plot_stair_ggraph`](https://gmontaletti.github.io/MONECA/reference/plot_stair_ggraph.md)
instead. This function will be removed in moneca 1.2.0.

## Usage

``` r
stair.plot(
  segments,
  level = NULL,
  layout = NULL,
  edges = NULL,
  mode = "directed",
  vertex.size = "total",
  vertex.alpha = 1,
  vertex.color = "black",
  vertex.shape = 21,
  show.edges = TRUE,
  edge.size = 0.5,
  edge.alpha = "weight",
  edge.color = "black",
  edge.line = "solid",
  show.text = FALSE,
  text.size = 3,
  text.color = "black",
  text.alpha = 1,
  text.vjust = 1.5,
  show.borders = TRUE,
  border.size = 1,
  border.fill = NA,
  border.color = "black",
  border.alpha = 1,
  border.padding = 1,
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
  legend = "side",
  level.title = "Level"
)
```

## Arguments

- segments:

  A moneca object returned by
  [`moneca`](https://gmontaletti.github.io/MONECA/reference/moneca.md).

- level:

  Integer vector specifying which levels to include in the stair plot.

- layout:

  Layout matrix for consistent node positioning across plots.

- edges:

  Edge matrix or specification for network edges.

- mode:

  Character string specifying graph mode ("directed" or "undirected").

- vertex.size:

  Specification for vertex sizes ("total" or numeric).

- vertex.alpha:

  Numeric transparency for vertices (0-1).

- vertex.color:

  Color specification for vertex borders.

- vertex.shape:

  Numeric shape code for vertices.

- show.edges:

  Logical indicating whether to display edges.

- edge.size:

  Numeric size for edges.

- edge.alpha:

  Transparency for edges ("weight" or numeric).

- edge.color:

  Color specification for edges.

- edge.line:

  Line type for edges ("solid", "dashed", etc.).

- show.text:

  Logical indicating whether to show vertex labels.

- text.size:

  Numeric size for text labels.

- text.color:

  Color for text labels.

- text.alpha:

  Transparency for text labels.

- text.vjust:

  Vertical adjustment for text labels.

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

  Specification for segment labels.

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

  Position specification for legend.

- level.title:

  Specification for level titles.

## Value

A list of ggplot2 objects, one for each segmentation level.

## Details

Creates a series of plots showing how segmentation evolves across
hierarchical levels using the legacy ggplot2 system. For modern stair
plots, use
[`plot_stair_ggraph`](https://gmontaletti.github.io/MONECA/reference/plot_stair_ggraph.md).

This function creates multiple plots showing the progression of
segmentation across hierarchical levels. Each plot uses the same layout
to maintain consistency, making it easy to see how segments merge or
split across levels.

**Note**: This function is maintained for backward compatibility and
requires the eliter package. For new analyses, use
[`plot_stair_ggraph`](https://gmontaletti.github.io/MONECA/reference/plot_stair_ggraph.md)
which offers better performance and modern styling.

## See also

[`plot_stair_ggraph`](https://gmontaletti.github.io/MONECA/reference/plot_stair_ggraph.md)
for modern stair plots,
[`gg.moneca`](https://gmontaletti.github.io/MONECA/reference/gg.moneca.md)
for the underlying plotting function

## Examples

``` r
if (FALSE) { # \dontrun{
# Using synthetic data
mobility_data <- generate_mobility_data(n_classes = 6, seed = 123)
seg <- moneca(mobility_data, segment.levels = 3)
plots <- stair.plot(seg)
plots[[2]]  # Display second level

# Customized stair plot
custom_stairs <- stair.plot(seg,
                           level = c(2, 3, 4),
                           vertex.size = "total",
                           show.borders = TRUE,
                           border.text.size = 5)
} # }
```
