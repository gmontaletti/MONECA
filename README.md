# MONECA: Mobility Network Clustering Analysis

[![R-CMD-check](https://github.com/antongrau/MONECA/workflows/R-CMD-check/badge.svg)](https://github.com/antongrau/MONECA/actions)

MONECA (Mobility Network Clustering Analysis) is an R package for analyzing social mobility patterns through graph-theoretic approaches. The package creates weighted networks from mobility tables and uses cliques to identify discrete and nested clusters of positions with high internal mobility.

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
install_github("antongrau/MONECA")

# Install dependencies
install.packages(c("ggplot2", "ggraph", "igraph", "dplyr", "tidygraph", 
                   "RColorBrewer", "scales", "toOrdinal"))
```

## Quick Start

```r
library(MONECA)

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

# Comprehensive analysis
analysis <- moneca_comprehensive_analysis(seg)
print(analysis$summary_report)
print(analysis$level_plot)

# Ego network analysis
plot_ego_ggraph(seg, mobility_data, ego_id = 3)
```

## Comprehensive Analysis

The `moneca_comprehensive_analysis()` function provides a complete analytical workflow that:
- Extracts and compares matrices at different hierarchical levels
- Computes graph metrics across all levels (density, reciprocity, centrality, etc.)
- Generates publication-quality network visualizations
- Creates detailed centrality and mobility reports
- Produces structured data frame summaries for further analysis

```r
# Run comprehensive analysis
analysis <- moneca_comprehensive_analysis(seg, plot_level = 3)

# Access different components
print(analysis$matrices)           # Original and max-level matrices with names
print(analysis$metrics_comparison) # Graph metrics across all levels
print(analysis$centrality_report)  # Centrality measures for each segment
print(analysis$mobility_report)    # Mobility patterns and flow analysis
print(analysis$summary_report)     # Structured summary as data frame
print(analysis$level_plot)         # Network visualization

# Save outputs to directory
analysis_with_files <- moneca_comprehensive_analysis(seg, 
                                                     output_dir = "results/")
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

### Comprehensive Analysis
- `moneca_comprehensive_analysis()` - Complete analysis workflow with reports

### Legacy Functions
- `moneca.plot()` - Base R plotting (still supported)
- `gg.moneca()` - Original ggplot2 plotting (still supported)

## Documentation

- **Vignette**: See `vignette("moneca-introduction")` for comprehensive examples
- **Function help**: Use `?function_name` for detailed documentation
- **Package overview**: `?MONECA` for general information

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
Touboel, J. & Larsen, A.G. (2024). MONECA: Mobility Network Clustering Analysis. 
R package version 0.1. https://github.com/antongrau/MONECA
```

## License

GPL-3

## Authors

- **Jonas Touboel** - Original algorithm and methodology
- **Anton Grau Larsen** - Package maintenance and development

## Contributing

We welcome contributions! Please see our contributing guidelines and:

1. Fork the repository
2. Create a feature branch
3. Make your changes with tests
4. Submit a pull request

## Issues

Report bugs and feature requests at: https://github.com/antongrau/MONECA/issues
