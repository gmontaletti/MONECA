# moneca: Mobility Network Clustering Analysis

**Author: Giampaolo Montaletti**  
ORCID: <https://orcid.org/0009-0002-5327-1122>  
Email: <giampaolo.montaletti@gmail.com>  
GitHub: <https://github.com/gmontaletti>

moneca (Mobility Network Clustering Analysis) is a comprehensive R
package for analyzing social mobility patterns through graph-theoretic
approaches. This package represents a complete rewrite and modernization
of the original MONECA package, featuring entirely new visualization
systems, synthetic data generation capabilities, comprehensive testing,
and modern R programming practices.

## Package History

**moneca** (lowercase, 2024-present) is the current package developed
and maintained by **Giampaolo Montaletti**. While it implements the core
clustering algorithm originally proposed by Touboel & Larsen, this
package features:

- **Entirely new codebase** for visualization using ggraph and tidygraph
- **Original synthetic data generation system** for reproducible
  research
- **Comprehensive test suite** written from scratch  
- **Modern documentation and vignettes** authored by Giampaolo
  Montaletti
- **Complete package infrastructure** redesign and modernization
- **New analysis functions** including enhanced membership analysis and
  quality metrics

**Original MONECA** (uppercase, 2017) by Jonas Touboel and Anton Grau
Larsen provided: - The core clustering algorithm and mathematical
methodology - Basic implementation of the segmentation approach -
Original research and theoretical foundation (Touboel & Larsen, 2017)

## Author and Maintainer

**Giampaolo Montaletti** - Package author, developer, and maintainer -
Complete package modernization and rewrite (2024) - Development of
modern visualization system - Creation of synthetic data generation
framework - Implementation of comprehensive testing suite - Authorship
of current documentation and vignettes

## Features

- **Network-based clustering** of mobility tables using graph theory
- **Hierarchical segmentation** at multiple analytical levels
- **Fast implementation** with moneca_fast() for large datasets
- **Auto-tuning** capabilities for automatic parameter optimization
- **Modern visualization** with ggplot2 and ggraph
- **Synthetic data generation** for testing and demonstrations
- **Comprehensive testing suite** and documentation
- **igraph compatibility layer** supporting versions 1.3.0+

## Installation

``` r
# Install development version from GitHub
install.packages("devtools")
library(devtools)
install_github("gmontaletti/moneca")

# Install dependencies
install.packages(c("ggplot2", "ggraph", "igraph", "dplyr", "tidygraph", 
                   "RColorBrewer", "scales", "toOrdinal"))
```

## Quick Start

``` r
library(moneca)

# Generate synthetic mobility data
mobility_data <- generate_mobility_data(
  n_classes = 6,
  n_total = 5000,
  immobility_strength = 0.7,
  class_clustering = 0.3,
  seed = 123
)

# Run MONECA analysis
seg <- moneca(mobility_data, segment.levels = 3)
print(seg)

# Modern network visualization
plot_moneca_ggraph(seg,
                  node_color = "segment",
                  title = "Social Mobility Network")

# Ego network analysis
plot_ego_ggraph(seg, mobility_data, ego_id = 3)

# Analyze segment membership
membership <- segment.membership(seg)
print(membership)
```

## Core Functions

The streamlined moneca API includes 21 key functions organized into
functional groups:

### Core Analysis Functions

- **moneca()** - Main clustering algorithm with hierarchical
  segmentation
- **moneca_fast()** - Fast implementation for large datasets (identical
  results)
- **weight.matrix()** - Convert mobility tables to relative risk
  matrices

### Data Generation

- **generate_mobility_data()** - Create synthetic mobility matrices for
  testing

### Modern Visualization (ggraph-based)

- **plot_moneca_ggraph()** - Network visualization with segment
  highlighting
- **plot_ego_ggraph()** - Ego network analysis and visualization
- **plot_stair_ggraph()** - Multi-level hierarchical segmentation plots
- **plot_moneca_dendrogram()** - Dendrogram visualization of
  hierarchical structure
- **plot_segment_quality()** - Quality assessment visualization

### Analysis Tools

- **segment.membership()** - Extract segment membership at each level
- **segment.membership.dataframe()** - Membership as data frame
- **segment.quality()** - Assess segmentation quality metrics
- **segment.edges()** - Extract edge weights between segments
- **segment.colors()** - Get color assignments for segments
- **vertex.mobility()** - Calculate vertex-level mobility metrics

### Auto-Tuning

- **auto_tune_small_cell_reduction()** - Automatic parameter
  optimization

### Legacy Visualization (backward compatibility)

- **moneca.plot()** - Base R network plotting
- **gg.moneca()** - Original ggplot2 visualization
- **ego.plot()** - Legacy ego network plot
- **stair.plot()** - Legacy stair plot
- **layout.matrix()** - Custom layout algorithm for network positioning

## Advanced Usage

### Fast Implementation

For large datasets, use the optimized implementation:

``` r
# Fast implementation (identical results to standard moneca)
seg_fast <- moneca_fast(mobility_data, segment.levels = 3)

# Works with all visualization and analysis functions
plot_moneca_ggraph(seg_fast, node_color = "segment")
```

### Auto-Tuning

Automatically optimize the small.cell.reduction parameter:

``` r
# Use auto-tuning for parameter optimization
seg_tuned <- moneca(mobility_data, segment.levels = 3, auto_tune = TRUE)

# Advanced: use auto_tune_small_cell_reduction() for more control
tuning_result <- auto_tune_small_cell_reduction(mobility_data, segment.levels = 3)
seg <- moneca(mobility_data,
              segment.levels = 3,
              small.cell.reduction = tuning_result$best_parameter)
```

### Enhanced Segment Naming

Improve visualization interpretability with custom labels:

``` r
# Create custom meaningful labels
custom_labels <- data.frame(
  name = c("Class 1", "Class 2", "Class 3"),
  segment_label = c("Executive", "Professional", "Technical"),
  stringsAsFactors = FALSE
)

# Use in plotting functions
plot_moneca_ggraph(seg,
                  segment_naming = custom_labels,
                  title = "Occupational Mobility Network")
```

## Documentation

- **Vignette**: See
  [`vignette("moneca-introduction")`](https://gmontaletti.github.io/MONECA/articles/moneca-introduction.md)
  for comprehensive examples
- **Function help**: Use `?function_name` for detailed documentation
- **Package overview**:
  [`?moneca`](https://gmontaletti.github.io/MONECA/reference/moneca.md)
  for general information

## Development and Testing

The package includes a comprehensive test suite and modern development
practices:

``` r
# Run tests
devtools::test()

# Check package
devtools::check()

# Build vignettes
devtools::build_vignettes()
```

## Methodology

MONECA implements a sophisticated algorithm for detecting mobility
patterns:

1.  **Mobility Matrix Processing**: Converts raw mobility data to
    relative risk matrices
2.  **Network Construction**: Creates weighted networks where edges
    represent mobility flows
3.  **Clique Detection**: Identifies groups with high internal
    connectivity using igraph
4.  **Hierarchical Clustering**: Iteratively aggregates positions into
    nested segments
5.  **Quality Assessment**: Provides metrics for evaluating segmentation
    quality

## Citation

If you use moneca in your research, please cite:

    # Current package:
    Montaletti, G. (2026). moneca: Mobility Network Clustering Analysis.
    R package version 1.1.0. https://github.com/gmontaletti/moneca

    # Original algorithm and methodology:
    Touboel, J., & Larsen, A. G. (2017). Mapping the Social Class Structure:
    From Occupational Mobility to Social Class Categories Using Network Analysis.
    Sociology, 51(6), 1257-1276.

## License

GPL-3

## Credits

**Current Package (moneca):** - **Giampaolo Montaletti** - Package
author, developer, and maintainer - Complete package rewrite and
modernization - All current documentation and vignettes  
- Modern visualization system (ggraph/tidygraph) - Synthetic data
generation framework - Comprehensive testing suite

**Original Algorithm (MONECA):** - **Jonas Touboel** - Original
clustering algorithm and mathematical methodology - **Anton Grau
Larsen** - Original MONECA package implementation

## Contributing

We welcome contributions! Please see our contributing guidelines and:

1.  Fork the repository
2.  Create a feature branch
3.  Make your changes with tests
4.  Submit a pull request

## Issues

Report bugs and feature requests at:
<https://github.com/gmontaletti/moneca/issues>
