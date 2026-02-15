# moneca: Mobility Network Clustering Analysis

moneca creates weighted networks from mobility tables and uses cliques
to identify discrete and nested clusters of social positions with high
internal mobility. The package provides comprehensive tools for
analyzing social mobility patterns through graph-theoretic approaches
with modern visualization capabilities.

## Details

The moneca package implements a sophisticated hierarchical clustering
algorithm that detects mobility patterns in social networks. It
identifies segments with high internal mobility by finding cliques in
weighted network representations of mobility data, creating nested
hierarchical structures that reveal the underlying organization of
social mobility.

**Key Features:**

- **Network-based clustering** using relative risk matrices

- **Hierarchical segmentation** with multiple nested levels

- **Modern visualization** with ggraph and ggplot2

- **Synthetic data generation** for testing and demonstrations

- **Quality metrics** for assessing segmentation performance

- **Multiple visualization types** (network, ego, stair plots)

- **Flexible input formats** with automatic data validation

**Core Functions:**

- [`moneca`](https://gmontaletti.github.io/MONECA/reference/moneca.md):
  Main clustering algorithm

- [`find.segments`](https://gmontaletti.github.io/MONECA/reference/find.segments.md):
  Core segmentation identification

- [`weight.matrix`](https://gmontaletti.github.io/MONECA/reference/weight.matrix.md):
  Relative risk matrix calculation

- [`segment.membership`](https://gmontaletti.github.io/MONECA/reference/segment.membership.md):
  Extract cluster memberships

**Modern Visualization (Recommended):**

- [`plot_moneca_ggraph`](https://gmontaletti.github.io/MONECA/reference/plot_moneca_ggraph.md):
  Network visualization with ggraph

- [`plot_ego_ggraph`](https://gmontaletti.github.io/MONECA/reference/plot_ego_ggraph.md):
  Ego network analysis

- [`plot_stair_ggraph`](https://gmontaletti.github.io/MONECA/reference/plot_stair_ggraph.md):
  Multi-level segmentation plots

**Legacy Visualization (Backward Compatibility):**

- [`gg.moneca`](https://gmontaletti.github.io/MONECA/reference/gg.moneca.md):
  Original ggplot2-based visualization

- [`moneca.plot`](https://gmontaletti.github.io/MONECA/reference/moneca.plot.md):
  Base R network plotting

- [`ego.plot`](https://gmontaletti.github.io/MONECA/reference/ego.plot.md):
  Legacy ego network plots

- [`stair.plot`](https://gmontaletti.github.io/MONECA/reference/stair.plot.md):
  Legacy stair plots

**Data Generation:**

- [`generate_mobility_data`](https://gmontaletti.github.io/MONECA/reference/generate_mobility_data.md):
  Create synthetic mobility tables

## Package Dependencies

**Required packages:**

- `igraph` (\>= 1.3.0): Network analysis with compatibility layer

- `ggplot2` (\>= 3.4.0): Graphics and plotting

- `RColorBrewer`: Color palettes for visualizations

- `scales`: Scale transformations and formatting

**Modern visualization packages:**

- `ggraph` (\>= 2.0.0): Advanced network visualization

- `tidygraph` (\>= 1.2.0): Tidy graph manipulation

- `dplyr` (\>= 1.0.0): Data manipulation

**Additional packages:**

- `grid`: Low-level graphics

- `toOrdinal`: Ordinal number formatting

## Color Vision Deficiency (CVD) Accessibility

All modern visualization functions use CVD-safe defaults:

- Default discrete palette is the 8-color Okabe-Ito palette
  (`color_palette = "okabe-ito"`), distinguishable under deuteranopia,
  protanopia, and tritanopia.

- Continuous gradients use perceptually uniform viridis scales.

- Diverging scales use a PuOr (purple-orange) scheme that remains
  discriminable under common CVD types.

- [`plot_moneca_ggraph()`](https://gmontaletti.github.io/MONECA/reference/plot_moneca_ggraph.md)
  supports `node_shape = "segment"` for shape-based redundant encoding
  alongside color.

- All hardcoded fill and reference-line colors use CVD-safe hex values.

Users who prefer other palettes can override with
`color_palette = "Set3"` or any RColorBrewer/viridis palette name.

## See also

Useful links:

- <https://gmontaletti.github.io/MONECA/>

- <https://github.com/gmontaletti/MONECA>

## Author

**Maintainer**: Giampaolo Montaletti <giampaolo.montaletti@gmail.com>
([ORCID](https://orcid.org/0009-0002-5327-1122))

Authors:

- Jonas Touboel <jt@soc.ku.dk> (Original algorithm and methodology)

Other contributors:

- Anton Grau Larsen <agl.ioa@cbs.dk> (Original package development)
  \[contributor\]
