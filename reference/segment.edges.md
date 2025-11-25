# Extract Segment Edge Matrix

Creates an adjacency matrix representing edges between segments based on
mobility flows. This function is used for network visualization and
analysis.

## Usage

``` r
segment.edges(
  segments,
  cut.off = 1,
  mode = "directed",
  level = NULL,
  segment.reduction = NULL,
  method = "all",
  top = 3,
  diagonal = NULL,
  small.cell.reduction = 0
)
```

## Arguments

- segments:

  A moneca object returned by
  [`moneca`](https://gmontaletti.github.io/MONECA/reference/moneca.md).

- cut.off:

  Numeric threshold for minimum relative risk to include an edge.
  Default is 1.

- mode:

  Character string specifying the graph mode ("directed" or
  "undirected"). Default is "directed".

- level:

  Integer vector specifying which hierarchical levels to include.
  Default includes all levels.

- segment.reduction:

  Integer vector specifying levels for which to remove internal segment
  edges. Default includes all levels.

- method:

  Character string specifying edge filtering method:

  - "all" (default): Include all edges above threshold

  - "top.out": Keep only the top outgoing edges per node

  - "top.in": Keep only the top incoming edges per node

- top:

  Integer specifying how many top edges to keep when using "top.out" or
  "top.in" methods. Default is 3.

- diagonal:

  Controls diagonal values. If NULL (default), diagonal is zeroed.

- small.cell.reduction:

  Numeric threshold for small cell handling.

## Value

A square matrix representing edge weights between nodes/segments.

## See also

[`plot_moneca_ggraph`](https://gmontaletti.github.io/MONECA/reference/plot_moneca_ggraph.md),
[`moneca.plot`](https://gmontaletti.github.io/MONECA/reference/moneca.plot.md)
