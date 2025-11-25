# Legacy Network Plot for MONECA Results

Creates a network visualization of MONECA segmentation results using
base graphics and igraph. For modern visualizations, use
[`plot_moneca_ggraph`](https://gmontaletti.github.io/MONECA/reference/plot_moneca_ggraph.md).

## Usage

``` r
moneca.plot(
  segments,
  layout = NULL,
  edges = NULL,
  mode = "directed",
  level = NULL,
  vertex.size = 5,
  vertex.frame.color = "black",
  edge.curved = FALSE,
  vertex.color = "grey50",
  vertex.label.color = "black",
  vertex.label.cex = 0.5,
  vertex.label.dist = 0.12,
  edge.arrow.size = 0.1,
  mark.col = NULL,
  mark.expand = 10,
  border.col = "black",
  edge.width = 1,
  edge.color = "black"
)
```

## Arguments

- segments:

  A moneca object returned by
  [`moneca`](https://gmontaletti.github.io/MONECA/reference/moneca.md).

- layout:

  A matrix of node coordinates, typically from
  [`layout.matrix`](https://gmontaletti.github.io/MONECA/reference/layout.matrix.md).

- edges:

  An adjacency matrix of edges, typically from
  [`segment.edges`](https://gmontaletti.github.io/MONECA/reference/segment.edges.md).

- mode:

  Character string specifying graph mode ("directed" or "undirected").

- level:

  Integer vector of hierarchical levels to visualize.

- vertex.size:

  Numeric value for vertex size. Default is 5.

- vertex.frame.color:

  Color for vertex borders. Default is "black".

- edge.curved:

  Logical for curved edges. Default is FALSE.

- vertex.color:

  Color for vertices. Default is "grey50".

- vertex.label.color:

  Color for vertex labels. Default is "black".

- vertex.label.cex:

  Size multiplier for labels. Default is 0.5.

- vertex.label.dist:

  Distance of labels from vertices. Default is 0.12.

- edge.arrow.size:

  Size of edge arrows. Default is 0.1.

- mark.col:

  Color for segment markers. Default is NULL.

- mark.expand:

  Expansion factor for segment boundaries. Default is 10.

- border.col:

  Color for segment borders. Default is "black".

- edge.width:

  Width of edges. Default is 1.

- edge.color:

  Color for edges. Can be a color name or matrix. Default is "black".

## Value

NULL (creates a plot as side effect).

## Details

This function provides backward compatibility with earlier versions of
MONECA. For new analyses, consider using
[`plot_moneca_ggraph`](https://gmontaletti.github.io/MONECA/reference/plot_moneca_ggraph.md)
which offers more modern styling and customization options.

## See also

[`plot_moneca_ggraph`](https://gmontaletti.github.io/MONECA/reference/plot_moneca_ggraph.md)
for modern plotting
