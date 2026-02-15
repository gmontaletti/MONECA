# Legacy Ego Network Visualization

**Deprecated**. Use
[`plot_ego_ggraph`](https://gmontaletti.github.io/MONECA/reference/plot_ego_ggraph.md)
instead. This function will be removed in moneca 1.2.0.

## Usage

``` r
ego.plot(
  segments,
  mxa.b,
  id = 1,
  lay = NULL,
  edge.size = 0.8,
  border.padding = 1,
  title.line = TRUE,
  vertex.size = "totals",
  small.cell.reduction = 5,
  edge.weight = "discrete",
  color.scheme = "RdPu",
  ...
)
```

## Arguments

- segments:

  A moneca object returned by
  [`moneca`](https://gmontaletti.github.io/MONECA/reference/moneca.md).

- mxa.b:

  The original mobility matrix used in the moneca analysis.

- id:

  Integer or character specifying the focal node (ego) for the analysis.

- lay:

  Layout matrix for node positioning, typically from
  [`layout.matrix`](https://gmontaletti.github.io/MONECA/reference/layout.matrix.md).

- edge.size:

  Numeric value for edge thickness. Default is 0.8.

- border.padding:

  Numeric value for segment boundary padding. Default is 1.

- title.line:

  Logical indicating whether to add a title line. Default is TRUE.

- vertex.size:

  Specification for vertex sizes. If "totals", sizes are derived from
  row/column totals in the mobility matrix. Otherwise, uses the
  specified values.

- small.cell.reduction:

  Numeric threshold below which vertices receive different shapes to
  indicate low mobility volumes. Default is 5.

- edge.weight:

  Character string specifying edge weight display. "discrete" creates
  categorical edge weights, otherwise uses continuous weights.

- color.scheme:

  Character string specifying the RColorBrewer color scheme for the
  visualization. Default is "RdPu".

- ...:

  Additional arguments passed to
  [`gg.moneca`](https://gmontaletti.github.io/MONECA/reference/gg.moneca.md).

## Value

A ggplot2 object showing the ego network.

## Details

Creates ego network plots showing mobility patterns from a single focal
position using the legacy ggplot2 plotting system. For modern ego
network analysis, use
[`plot_ego_ggraph`](https://gmontaletti.github.io/MONECA/reference/plot_ego_ggraph.md).

This function creates a focused view of mobility patterns from a single
position in the social structure. It highlights both incoming and
outgoing mobility flows and uses different visual elements to represent:

- Edge colors/weights for relative risk levels

- Node sizes for total mobility volumes

- Node shapes for positions with low mobility

- Node colors for mobility share proportions

**Note**: This function is maintained for backward compatibility. For
new analyses, consider using
[`plot_ego_ggraph`](https://gmontaletti.github.io/MONECA/reference/plot_ego_ggraph.md)
which offers better performance and more modern styling options.

## See also

[`plot_ego_ggraph`](https://gmontaletti.github.io/MONECA/reference/plot_ego_ggraph.md)
for modern ego network visualization,
[`gg.moneca`](https://gmontaletti.github.io/MONECA/reference/gg.moneca.md)
for the underlying plotting function

## Examples

``` r
if (FALSE) { # \dontrun{
# Using synthetic data
mobility_data <- generate_mobility_data(n_classes = 6, seed = 123)
seg <- moneca(mobility_data, segment.levels = 2)
ego.plot(seg, mobility_data, id = 2)

# Customized ego plot
ego.plot(seg, mobility_data,
         id = 3,
         edge.size = 1.2,
         color.scheme = "Blues",
         small.cell.reduction = 10)
} # }
```
