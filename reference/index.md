# Package index

## Core Clustering Functions

Main functions for MONECA clustering analysis

- [`moneca()`](https://gmontaletti.github.io/MONECA/reference/moneca.md)
  : MONECA - Mobility Network Clustering Analysis
- [`moneca_fast()`](https://gmontaletti.github.io/MONECA/reference/moneca_fast.md)
  : MONECA Fast - Memory-Optimized Mobility Network Clustering Analysis
- [`weight.matrix()`](https://gmontaletti.github.io/MONECA/reference/weight.matrix.md)
  : Calculate Relative Risk Weight Matrix

## Segment Analysis

Functions for analyzing and extracting segment information

- [`segment.membership()`](https://gmontaletti.github.io/MONECA/reference/segment.membership.md)
  : Extract Segment Membership Information
- [`segment.membership.dataframe()`](https://gmontaletti.github.io/MONECA/reference/segment.membership.dataframe.md)
  : Generate Segment Membership Dataframe from MONECA Results
- [`segment.quality()`](https://gmontaletti.github.io/MONECA/reference/segment.quality.md)
  : Evaluate Segment Quality Metrics
- [`segment.colors()`](https://gmontaletti.github.io/MONECA/reference/segment.colors.md)
  : Generate Colors for Segments
- [`segment.edges()`](https://gmontaletti.github.io/MONECA/reference/segment.edges.md)
  : Extract Segment Edge Matrix
- [`vertex.mobility()`](https://gmontaletti.github.io/MONECA/reference/vertex.mobility.md)
  : Vertex mobility

## Auto-Tuning

Automatic parameter optimization

- [`auto_tune_small_cell_reduction()`](https://gmontaletti.github.io/MONECA/reference/auto_tune_small_cell_reduction.md)
  : Enhanced Automatic Small Cell Reduction Parameter Tuning with
  Multi-Objective Optimization

## Modern Visualization

Plotting functions using ggraph

- [`plot_moneca_ggraph()`](https://gmontaletti.github.io/MONECA/reference/plot_moneca_ggraph.md)
  : Modern Network Visualization for MONECA Results
- [`plot_ego_ggraph()`](https://gmontaletti.github.io/MONECA/reference/plot_ego_ggraph.md)
  : Ego Network Visualization with ggraph
- [`plot_stair_ggraph()`](https://gmontaletti.github.io/MONECA/reference/plot_stair_ggraph.md)
  : Multi-Level Segmentation Visualization (Stair Plot)
- [`plot_moneca_dendrogram()`](https://gmontaletti.github.io/MONECA/reference/plot_moneca_dendrogram.md)
  : Plot MONECA Results as Dendrogram
- [`plot_segment_quality()`](https://gmontaletti.github.io/MONECA/reference/plot_segment_quality.md)
  : Visualize Segment Quality Metrics

## Legacy Visualization

Original plotting functions (backward compatibility)

- [`moneca.plot()`](https://gmontaletti.github.io/MONECA/reference/moneca.plot.md)
  : Legacy Network Plot for MONECA Results
- [`gg.moneca()`](https://gmontaletti.github.io/MONECA/reference/gg.moneca.md)
  : Legacy ggplot2 Visualization for moneca Objects
- [`ego.plot()`](https://gmontaletti.github.io/MONECA/reference/ego.plot.md)
  : Legacy Ego Network Visualization
- [`stair.plot()`](https://gmontaletti.github.io/MONECA/reference/stair.plot.md)
  : Legacy Multi-Level Stair Plot
- [`layout.matrix()`](https://gmontaletti.github.io/MONECA/reference/layout.matrix.md)
  : Layout matrix

## Data Generation

Functions for generating synthetic mobility data

- [`generate_mobility_data()`](https://gmontaletti.github.io/MONECA/reference/generate_mobility_data.md)
  : Generate Realistic Synthetic Mobility Data

## Package Information

Package documentation and S3 methods

- [`moneca-package`](https://gmontaletti.github.io/MONECA/reference/MONECA-package.md)
  : moneca: Mobility Network Clustering Analysis
- [`print(`*`<moneca>`*`)`](https://gmontaletti.github.io/MONECA/reference/print.moneca.md)
  : Print Method for moneca Objects (Enhanced)
- [`print(`*`<first_level_summary>`*`)`](https://gmontaletti.github.io/MONECA/reference/print.first_level_summary.md)
  : Print first level summary
- [`print(`*`<moneca_tuning>`*`)`](https://gmontaletti.github.io/MONECA/reference/print.moneca_tuning.md)
  : Print Method for MONECA Tuning Results
