# moneca: Mobility Network Clustering Analysis

**Author: Giampaolo Montaletti**  
ORCID: https://orcid.org/0009-0002-5327-1122  
Email: giampaolo.montaletti@gmail.com  
GitHub: https://github.com/gmontaletti

moneca (Mobility Network Clustering Analysis) is a comprehensive R package for analyzing social mobility patterns through graph-theoretic approaches. This package represents a complete rewrite and modernization of the original MONECA package, featuring entirely new visualization systems, synthetic data generation capabilities, comprehensive testing, and modern R programming practices.

## Package History

**moneca** (lowercase, 2024-present) is the current package developed and maintained by **Giampaolo Montaletti**. While it implements the core clustering algorithm originally proposed by Touboel & Larsen, this package features:

- **Entirely new codebase** for visualization using ggraph and tidygraph
- **Original synthetic data generation system** for reproducible research
- **Comprehensive test suite** written from scratch  
- **Modern documentation and vignettes** authored by Giampaolo Montaletti
- **Complete package infrastructure** redesign and modernization
- **New analysis functions** including enhanced membership analysis and quality metrics

**Original MONECA** (uppercase, 2017) by Jonas Touboel and Anton Grau Larsen provided:
- The core clustering algorithm and mathematical methodology
- Basic implementation of the segmentation approach
- Original research and theoretical foundation (Touboel & Larsen, 2017)

## Author and Maintainer

**Giampaolo Montaletti** - Package author, developer, and maintainer
- Complete package modernization and rewrite (2024)
- Development of modern visualization system
- Creation of synthetic data generation framework
- Implementation of comprehensive testing suite
- Authorship of current documentation and vignettes 

## Features

- **Network-based clustering** of mobility tables using graph theory
- **Hierarchical segmentation** at multiple analytical levels  
- **Advanced auto-tuning** with 6 optimization methods (stability, quality, performance, Pareto, cross-validation, Bayesian)
- **Modern visualization** with ggplot2 and ggraph
- **Synthetic data generation** for testing and demonstrations
- **Comprehensive testing suite** and documentation
- **igraph compatibility layer** supporting versions 1.3.0+
- **Performance optimization** with parallel processing and caching

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

# Run MONECA analysis with auto-tuning
seg <- moneca(mobility_data, segment.levels = 3, 
              auto_tune = TRUE, auto_tune_method = "stability")
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

## Auto-Tuning: Automatic Parameter Optimization

MONECA includes advanced auto-tuning capabilities that automatically optimize the `small.cell.reduction` parameter for improved clustering quality:

```r
# Stability-based auto-tuning (recommended for general use)
seg_stable <- moneca(mobility_data, segment.levels = 2,
                     auto_tune = TRUE, auto_tune_method = "stability")

# Quality-optimized auto-tuning (best clustering performance)  
seg_quality <- moneca(mobility_data, segment.levels = 2,
                      auto_tune = TRUE, auto_tune_method = "quality")

# Performance-balanced auto-tuning (speed vs quality trade-off)
seg_balanced <- moneca(mobility_data, segment.levels = 2,
                       auto_tune = TRUE, auto_tune_method = "performance",
                       performance_weight = 0.3)

# Multi-objective Pareto optimization
seg_pareto <- moneca(mobility_data, segment.levels = 2,
                     auto_tune = TRUE, auto_tune_method = "pareto")

# Cross-validation based tuning (conservative approach)
seg_cv <- moneca(mobility_data, segment.levels = 2,
                 auto_tune = TRUE, auto_tune_method = "cross_validation")

# View auto-tuning results
auto_result <- attr(seg_stable, "auto_tune_result")
print(paste("Optimal parameter:", auto_result$best_parameter))
print(paste("Score:", round(auto_result$best_score, 4)))
```

### Auto-Tuning Methods

| Method | Best For | Description |
|--------|----------|-------------|
| `stability` | General use | Bootstrap-based stability assessment |
| `quality` | Research/publication | Maximizes clustering quality metrics |
| `performance` | Large datasets | Balances quality and computational speed |
| `pareto` | Exploration | Multi-objective optimization |
| `cross_validation` | Small datasets | Conservative parameter selection |
| `bayesian` | Advanced users | Gaussian Process optimization* |

*Requires GPfit package

### Before/After Comparison

```r
# Compare manual vs auto-tuned results
manual_seg <- moneca(mobility_data, segment.levels = 2, small.cell.reduction = 5)
auto_seg <- moneca(mobility_data, segment.levels = 2, 
                   auto_tune = TRUE, auto_tune_method = "stability")

# Visualize comparison
library(gridExtra)
p1 <- plot_moneca_ggraph(manual_seg, title = "Manual Parameter")
p2 <- plot_moneca_ggraph(auto_seg, title = "Auto-Tuned")
grid.arrange(p1, p2, ncol = 2)
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

### Enhanced Segment Naming

MONECA now supports flexible segment naming to improve visualization interpretability:

```r
# Create custom meaningful labels
custom_labels <- data.frame(
  name = c("Class 1", "Class 2", "Class 3", "Class 4", "Class 5", "Class 6"),
  segment_label = c("Executive", "Professional", "Manager", 
                    "Technical", "Service", "Manual"),
  stringsAsFactors = FALSE
)

# Use in any plotting function
plot_moneca_ggraph(seg, 
                  segment_naming = custom_labels,
                  title = "Occupational Mobility Network")

# Also works with string strategies
plot_moneca_ggraph(seg, segment_naming = "auto")    # Intelligent pattern recognition
plot_moneca_ggraph(seg, segment_naming = "concat")  # Name concatenation
plot_moneca_ggraph(seg, segment_naming = "pattern") # Advanced pattern matching
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
- `plot_moneca_ggraph()` - Network plots with ggraph (supports enhanced segment naming)
- `plot_ego_ggraph()` - Ego network visualization (supports enhanced segment naming)
- `plot_stair_ggraph()` - Multi-level segmentation plots (supports enhanced segment naming)

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

If you use moneca in your research, please cite:

```
# Current package:
Montaletti, G. (2024). moneca: Mobility Network Clustering Analysis. 
R package version 0.4.0. https://github.com/gmontaletti/moneca

# Original algorithm and methodology:
Touboel, J., & Larsen, A. G. (2017). Mapping the Social Class Structure: 
From Occupational Mobility to Social Class Categories Using Network Analysis.
Sociology, 51(6), 1257-1276.
```

## License

GPL-3

## Credits

**Current Package (moneca):**
- **Giampaolo Montaletti** - Package author, developer, and maintainer
  - Complete package rewrite and modernization
  - All current documentation and vignettes  
  - Modern visualization system (ggraph/tidygraph)
  - Synthetic data generation framework
  - Comprehensive testing suite

**Original Algorithm (MONECA):**
- **Jonas Touboel** - Original clustering algorithm and mathematical methodology
- **Anton Grau Larsen** - Original MONECA package implementation

## Contributing

We welcome contributions! Please see our contributing guidelines and:

1. Fork the repository
2. Create a feature branch
3. Make your changes with tests
4. Submit a pull request

## Issues

Report bugs and feature requests at: https://github.com/gmontaletti/moneca/issues
