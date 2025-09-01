# moneca: Mobility Network Clustering Analysis

moneca (Mobility Network Clustering Analysis) is an R package for analyzing social mobility patterns through graph-theoretic approaches. The package creates weighted networks from mobility tables and uses cliques to identify discrete and nested clusters of positions with high internal mobility.

## About this Fork

This is a maintained fork of the original [MONECA package](https://github.com/antongrau/MONECA) [![R-CMD-check](https://github.com/antongrau/MONECA/workflows/R-CMD-check/badge.svg)](https://github.com/antongrau/MONECA/actions) with comprehensive modernization and active maintenance.

**Original Authors & Credits:**
- **Jonas Touboel** - Original algorithm design and methodology
- **Anton Grau Larsen** - Original package development and implementation

**Current Maintainer:**
- **Giampaolo Montaletti** - Package modernization, maintenance, and new features

**Key Improvements in this Fork:**
- Modern ggraph-based visualizations with enhanced aesthetics
- Comprehensive synthetic data generation for reproducible examples
- Complete testing suite with testthat (30+ tests)
- Enhanced documentation and vignettes
- igraph compatibility layer supporting versions 1.3.0+
- Improved error handling and input validation
- Performance optimizations and code modernization 

## Features

- **Network-based clustering** of mobility tables using graph theory
- **Hierarchical segmentation** at multiple analytical levels  
- **Modern visualization** with ggplot2 and ggraph
- **Synthetic data generation** for testing and demonstrations
- **Comprehensive testing suite** and documentation
- **igraph compatibility layer** supporting versions 1.3.0+

## Installation

```r
# Install development version from GitHub
install.packages("devtools")
library(devtools)
install_github("gmontaletti/moneca")

# Install dependencies
install.packages(c("ggplot2", "ggraph", "igraph", "dplyr", "tidygraph", 
                   "RColorBrewer", "scales", "toOrdinal"))
```

## Quick Start

```r
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

## Analysis and Visualization

MONECA provides several tools for analyzing and visualizing mobility patterns:

```r
# Enhanced segment membership analysis
enhanced_membership <- segment.membership.enhanced(seg, node_naming = "semantic")
print(enhanced_membership)

# Quality assessment of segments
quality <- segment.quality(seg)
print(quality)

# Visualize segment quality
plot_segment_quality(seg, title = "Segment Quality Assessment")

# Multi-level stair plot
plot_stair_ggraph(seg, title = "Hierarchical Segmentation")
```

## Key Functions

### Core Analysis
- `moneca()` - Main clustering algorithm
- `weight.matrix()` - Convert mobility tables to relative risk matrices
- `find.segments()` - Identify cliques in weighted networks
- `segment.membership()` - Extract segment membership information

### Data Generation
- `generate_mobility_data()` - Create synthetic mobility matrices
- `generate_example_datasets()` - Pre-configured example datasets

### Modern Visualization
- `plot_moneca_ggraph()` - Network plots with ggraph
- `plot_ego_ggraph()` - Ego network visualization  
- `plot_stair_ggraph()` - Multi-level segmentation plots

### Analysis Tools
- `segment.membership.enhanced()` - Enhanced segment membership with naming
- `segment.quality()` - Assess segmentation quality across levels
- `plot_segment_quality()` - Visualize quality metrics

### Legacy Functions
- `moneca.plot()` - Base R plotting (still supported)
- `gg.moneca()` - Original ggplot2 plotting (still supported)

## Documentation

- **Vignette**: See `vignette("moneca-introduction")` for comprehensive examples
- **Function help**: Use `?function_name` for detailed documentation
- **Package overview**: `?moneca` for general information

## Development and Testing

The package includes a comprehensive test suite and modern development practices:

```r
# Run tests
devtools::test()

# Check package
devtools::check()

# Build vignettes
devtools::build_vignettes()
```

## Methodology

MONECA implements a sophisticated algorithm for detecting mobility patterns:

1. **Mobility Matrix Processing**: Converts raw mobility data to relative risk matrices
2. **Network Construction**: Creates weighted networks where edges represent mobility flows
3. **Clique Detection**: Identifies groups with high internal connectivity using igraph
4. **Hierarchical Clustering**: Iteratively aggregates positions into nested segments
5. **Quality Assessment**: Provides metrics for evaluating segmentation quality

## Citation

If you use MONECA in your research, please cite:

```
Touboel, J. & Larsen, A.G. (2024). moneca: Mobility Network Clustering Analysis. 
R package version 0.4.0. https://github.com/gmontaletti/moneca

# Original methodology:
Touboel, J., & Larsen, A. G. (2017). Mapping the Social Class Structure: 
From Occupational Mobility to Social Class Categories Using Network Analysis.
Sociology, 51(6), 1257-1276.
```

## License

GPL-3

## Authors

- **Jonas Touboel** - Original algorithm and methodology
- **Anton Grau Larsen** - Original package development
- **Giampaolo Montaletti** - Actual developer and mantainer

## Contributing

We welcome contributions! Please see our contributing guidelines and:

1. Fork the repository
2. Create a feature branch
3. Make your changes with tests
4. Submit a pull request

## Issues

Report bugs and feature requests at: https://github.com/gmontaletti/moneca/issues
