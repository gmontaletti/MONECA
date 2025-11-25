# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working
with code in this repository.

## About moneca

moneca (Mobility Network Clustering Analysis) is an R package that
creates weighted networks from mobility tables and uses cliques to
create discrete and nested clusters. The package analyzes social
mobility patterns through graph-theoretic approaches.

**Current Status (v0.9.2)**: The package has been streamlined with a
focused API of 21 exported functions, organized into core analysis, data
generation, modern visualization, analysis tools, and legacy
compatibility functions. Recent major revision removed archived
experimental features and consolidated the codebase around the proven
core algorithm and modern visualization system.

## Package Development Commands

### Building and Checking

``` r
# Build package
R CMD build .

# Check package (comprehensive validation)
R CMD check moneca_0.4.0.tar.gz

# Install from source
R CMD INSTALL .

# Alternative with devtools (recommended)
devtools::build()
devtools::check()
devtools::install()
```

### Testing and Documentation

``` r
# Run comprehensive test suite
devtools::test()

# Generate documentation with roxygen2
devtools::document()

# Build vignettes
devtools::build_vignettes()

# Run examples
devtools::run_examples()

# Load for interactive testing
devtools::load_all()

# Check test coverage
covr::package_coverage()
```

### Dependencies Installation

``` r
# Install all required dependencies
install.packages(c("ggplot2", "ggraph", "igraph", "tidygraph", "dplyr", 
                   "RColorBrewer", "scales", "toOrdinal", "grid"))

# Install development dependencies
install.packages(c("devtools", "testthat", "knitr", "rmarkdown", "covr"))

# Quick dependency check
devtools::install_deps()
```

## Code Architecture

### Core Algorithm Flow

1.  **moneca()**: Main clustering function that runs iterative
    segmentation
2.  **moneca_fast()**: Optimized implementation producing identical
    results to moneca()
3.  **weight.matrix()**: Converts mobility tables to relative risk
    matrices
4.  **find.segments()**: Identifies cliques in weighted networks
    (internal function)
5.  **segment.matrix()**: Aggregates mobility data by segments for next
    iteration (internal function)

### Key Data Structures

- **segment.list**: Nested list of clique memberships for each level
- **mat.list**: Mobility matrices for each segmentation level
- **small.cell.reduction**: Parameter controlling minimum cell sizes

### Streamlined API (21 Exported Functions)

The package exports exactly 21 functions organized into functional
groups:

#### Core Analysis (3)

- moneca() - Main algorithm
- moneca_fast() - Fast implementation
- weight.matrix() - Weight matrix conversion

#### Data Generation (1)

- generate_mobility_data() - Synthetic data generator

#### Modern Visualization (5)

- plot_moneca_ggraph() - Main network plots
- plot_ego_ggraph() - Ego network analysis
- plot_stair_ggraph() - Multi-level visualization
- plot_moneca_dendrogram() - Hierarchical tree plots
- plot_segment_quality() - Quality assessment plots

#### Analysis Tools (6)

- segment.membership() - Extract membership
- segment.membership.dataframe() - Membership as data frame
- segment.quality() - Quality metrics
- segment.edges() - Edge weights
- segment.colors() - Color assignments
- vertex.mobility() - Vertex-level metrics

#### Auto-Tuning (1)

- auto_tune_small_cell_reduction() - Parameter optimization

#### Legacy Visualization (5)

- moneca.plot() - Base R plotting
- gg.moneca() - Original ggplot2
- ego.plot() - Legacy ego network
- stair.plot() - Legacy stair plot
- layout.matrix() - Custom layout

### Modern Visualization System (R/modern_plotting.R)

- **plot_moneca_ggraph()**: Main network visualization with ggraph
  (RECOMMENDED)
- **plot_ego_ggraph()**: Ego network analysis plots with modern styling
- **plot_stair_ggraph()**: Multi-level segmentation visualization
- **Enhanced Segment Naming**: All modern plotting functions support
  flexible segment naming
- **Legacy functions**:
  [`gg.moneca()`](https://gmontaletti.github.io/MONECA/reference/gg.moneca.md),
  [`moneca.plot()`](https://gmontaletti.github.io/MONECA/reference/moneca.plot.md),
  [`ego.plot()`](https://gmontaletti.github.io/MONECA/reference/ego.plot.md)
  still available

### Synthetic Data Generation (R/synthetic_data.R)

- **generate_mobility_data()**: Creates realistic synthetic mobility
  matrices
- **generate_example_datasets()**: Pre-configured datasets for different
  mobility scenarios
- **Parameters**: immobility_strength, class_clustering, noise_level,
  custom class names

### Visualization Architecture

#### Modern ggraph System (Preferred)

- Uses tidygraph for graph manipulation
- ggraph for modern network layouts
- Supports: FR, KK, stress, and custom layouts
- Customizable node aesthetics (size, color, shape by segment/mobility)
- Advanced edge styling (width, color, transparency by weight)

#### Legacy Plotting (Backward Compatibility)

- **moneca.plot()**: Base R plotting using igraph
- **gg.moneca()**: Original ggplot2 plotting (depends on eliter package)
- **layout.matrix()**: Fruchterman-Reingold layout with segment-based
  attraction

### igraph Compatibility System

The package includes a sophisticated compatibility layer
(`R/igraph_compatibility.R`) that: - Automatically detects igraph
version at runtime - Maps old dot-notation functions to new underscore
functions - Provides custom `norm_coords` implementation for removed
functions - Ensures compatibility with igraph \>= 1.3.0

Key compatibility functions: -
[`moneca_graph_from_adjacency()`](https://gmontaletti.github.io/MONECA/reference/moneca_graph_from_adjacency.md)
→ `graph.adjacency()` or `graph_from_adjacency_matrix()` -
`moneca_get_edgelist()` → `get.edgelist()` or `as_edgelist()` -
`moneca_norm_coords()` → custom implementation when `norm_coords`
unavailable

### Modern Visualization System

The package now includes modern plotting functions using ggraph: -
[`plot_moneca_ggraph()`](https://gmontaletti.github.io/MONECA/reference/plot_moneca_ggraph.md):
Main network visualization with ggraph -
[`plot_ego_ggraph()`](https://gmontaletti.github.io/MONECA/reference/plot_ego_ggraph.md):
Ego network analysis plots  
-
[`plot_stair_ggraph()`](https://gmontaletti.github.io/MONECA/reference/plot_stair_ggraph.md):
Multi-level segmentation visualization - Legacy functions
([`gg.moneca()`](https://gmontaletti.github.io/MONECA/reference/gg.moneca.md),
[`moneca.plot()`](https://gmontaletti.github.io/MONECA/reference/moneca.plot.md))
still available for backward compatibility

### Enhanced Segment Naming System

All modern plotting functions support flexible segment naming through
the `segment_naming` parameter:

#### Input Types:

- **Character strings**: “auto”, “concat”, “pattern”, “custom” (existing
  functionality)
- **Data frame**: Must have columns “name” and “segment_label” (NEW
  FEATURE)
- **NULL**: Defaults to “auto” strategy

#### Implementation Details:

- **Data frame validation**: Ensures required columns exist
- **Partial naming support**: Missing names fall back to default
  strategies  
- **Cross-function consistency**: Works across all plotting functions
- **Error handling**: Clear error messages for invalid inputs

#### Usage Examples:

``` r
# String-based naming (existing)
plot_moneca_ggraph(seg, segment_naming = "auto")

# Custom data frame (new)
custom_labels <- data.frame(
  name = c("Class1", "Class2", "Class3"),
  segment_label = c("Executive", "Professional", "Technical"),
  stringsAsFactors = FALSE
)
plot_moneca_ggraph(seg, segment_naming = custom_labels)
```

### Synthetic Data Generation

- [`generate_mobility_data()`](https://gmontaletti.github.io/MONECA/reference/generate_mobility_data.md):
  Creates realistic synthetic mobility matrices
- [`generate_example_datasets()`](https://gmontaletti.github.io/MONECA/reference/generate_example_datasets.md):
  Pre-configured datasets for different mobility scenarios
- Supports customizable parameters: immobility strength, class
  clustering, noise levels

### Testing Framework (tests/testthat/)

- **Comprehensive test suite** using testthat (\>= 3.0.0)
- **test-synthetic-data.R**: Data generation validation, parameter
  handling, reproducibility
- **test-moneca-core.R**: Core algorithms, weight matrices, hierarchical
  segmentation
- **test-plotting.R**: Modern ggraph functions, parameter validation,
  error handling
- **test-igraph-compatibility.R**: Version compatibility, function
  mapping, edge cases
- **Run tests**: `devtools::test()` or `testthat::test_dir("tests")`
- **Coverage**: Use `covr::package_coverage()` to check test coverage

### Documentation Structure

- **Vignette**: `vignettes/moneca-introduction.Rmd` - Comprehensive
  tutorial
- **Function docs**: Roxygen2-generated documentation with modern
  examples
- **README.md**: Updated with modern workflow, installation, quick start
- **CLAUDE.md**: Development guidance (this file)

### Package Dependencies

- **Core Required**: ggplot2 (\>= 3.4.0), ggraph (\>= 2.0.0), igraph
  (\>= 1.3.0), tidygraph (\>= 1.2.0), dplyr (\>= 1.0.0)
- **Additional**: RColorBrewer, scales, toOrdinal, grid, stats,
  grDevices
- **Development**: knitr, rmarkdown, testthat (\>= 3.0.0)
- **Check deps**: `devtools::install_deps()` installs all required
  dependencies

### Development Best Practices

- **S3 classes** with custom print methods for MONECA objects
- **Backward compatibility** maintained for all existing functions
- **Modern examples** use synthetic data (no external dependencies)
- **Extensive parameter validation** with informative error messages
- **Reproducible analysis** with seed parameters in synthetic data
- **Modular architecture** with clear separation of concerns
- **Enhanced visualization features** like flexible segment naming
  improve usability

## Quick Start for Development

### Testing New Features

``` r
# Load package for development
devtools::load_all()

# Generate test data
test_data <- generate_mobility_data(n_classes = 5, seed = 123)

# Run analysis
seg <- moneca(test_data, segment.levels = 3)

# Test modern plotting
plot_moneca_ggraph(seg, node_color = "segment")

# Test enhanced segment naming
custom_names <- data.frame(
  name = rownames(test_data)[1:3],
  segment_label = c("Executive", "Professional", "Technical"),
  stringsAsFactors = FALSE
)
plot_moneca_ggraph(seg, segment_naming = custom_names)

# Run specific tests
testthat::test_file("tests/testthat/test-moneca-core.R")
```

### Common Development Tasks

``` r
# Update documentation after changes
devtools::document()

# Check package thoroughly
devtools::check()

# Install and test locally
devtools::install()

# Build vignettes after updates
devtools::build_vignettes()
```

### File Organization

- **R/moneca.R**: Package-level documentation
- **R/analytical_functions.R**: Core algorithm (moneca(),
  find.segments(), internal functions)
- **R/moneca_fast.R**: Fast implementation with optimized algorithms
- **R/modern_plotting.R**: Modern ggraph visualization functions
- **R/synthetic_data.R**: Data generation functions
- **R/descriptive_functions.R**: Analysis tools (segment.membership,
  segment.quality, etc.)
- **R/ggplotting.R**: Legacy ggplot2 functions (backward compatibility)
- **R/auto_tuning.R**: Auto-tuning functionality
- **R/igraph_compatibility.R**: Version compatibility layer
- **R/optimized_algorithms.R**: Performance-optimized internal
  algorithms
- **tests/testthat/**: Complete test suite
- **vignettes/**: Package tutorial and documentation
- **man/**: Auto-generated function documentation

### Migration Notes for Existing Users

- **Package name**: Changed from MONECA to moneca (lowercase)
- **Installation**: Use `install_github("gmontaletti/moneca")` instead
  of the original repository
- **Loading**: Use
  [`library(moneca)`](https://gmontaletti.github.io/MONECA/) instead of
  [`library(MONECA)`](https://rdrr.io/r/base/library.html)
- **Plotting**: Recommend migrating from
  [`gg.moneca()`](https://gmontaletti.github.io/MONECA/reference/gg.moneca.md)
  to
  [`plot_moneca_ggraph()`](https://gmontaletti.github.io/MONECA/reference/plot_moneca_ggraph.md)
- **Data**: Use
  [`generate_mobility_data()`](https://gmontaletti.github.io/MONECA/reference/generate_mobility_data.md)
  instead of external datasets for examples
- **Performance**: Use
  [`moneca_fast()`](https://gmontaletti.github.io/MONECA/reference/moneca_fast.md)
  for large datasets (produces identical results)
- **Testing**: All core functions continue to work unchanged
- **Dependencies**: Package now requires ggraph and tidygraph for modern
  features

### Development Guidelines

- Never change the original moneca() function - it is the reference
  implementation
- Use moneca_fast() for performance improvements while maintaining
  identical results
- The reference directory for cleanup/move file operations is in
  ../reference/moneca/
- When bumping version, update citation in README.md
- Maintain backward compatibility for all exported functions
- All new visualization should use ggraph-based modern_plotting.R
  functions

### Recent Major Revision (v0.9.2)

- Removed experimental/archived features (parallel processing, temporal
  analysis, advanced tuning variants)
- Consolidated to 21 well-tested exported functions
- Simplified vignette to focus on core workflow: moneca() →
  moneca_fast() → plot\_\*()
- Updated all documentation to reflect streamlined API
- Retained full backward compatibility for existing code
