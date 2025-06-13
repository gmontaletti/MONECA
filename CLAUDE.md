# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## About MONECA

MONECA (Mobility Network Clustering Analysis) is an R package that creates weighted networks from mobility tables and uses cliques to create discrete and nested clusters. The package analyzes social mobility patterns through graph-theoretic approaches. The package has been comprehensively modernized with ggraph visualizations, synthetic data generation, complete testing suite, and detailed documentation.

## Package Development Commands

### Building and Checking
```r
# Build package
R CMD build .

# Check package (comprehensive validation)
R CMD check MONECA_0.1.tar.gz

# Install from source
R CMD INSTALL .

# Alternative with devtools (recommended)
devtools::build()
devtools::check()
devtools::install()
```

### Testing and Documentation
```r
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
```r
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
1. **moneca()**: Main clustering function that runs iterative segmentation
2. **weight.matrix()**: Converts mobility tables to relative risk matrices
3. **find.segments()**: Identifies cliques in weighted networks using progress-based optimization
4. **segment.matrix()**: Aggregates mobility data by segments for next iteration

### Key Data Structures
- **segment.list**: Nested list of clique memberships for each level
- **mat.list**: Mobility matrices for each segmentation level  
- **small.cell.reduction**: Parameter controlling minimum cell sizes

### Modern Visualization System (R/modern_plotting.R)
- **plot_moneca_ggraph()**: Main network visualization with ggraph (RECOMMENDED)
- **plot_ego_ggraph()**: Ego network analysis plots with modern styling
- **plot_stair_ggraph()**: Multi-level segmentation visualization
- **Legacy functions**: `gg.moneca()`, `moneca.plot()`, `ego.plot()` still available

### Synthetic Data Generation (R/synthetic_data.R)
- **generate_mobility_data()**: Creates realistic synthetic mobility matrices
- **generate_example_datasets()**: Pre-configured datasets for different mobility scenarios
- **Parameters**: immobility_strength, class_clustering, noise_level, custom class names

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
- **layout.matrix()**: Fruchterman-Reingold layout with segment-based attraction

### igraph Compatibility System
The package includes a sophisticated compatibility layer (`R/igraph_compatibility.R`) that:
- Automatically detects igraph version at runtime
- Maps old dot-notation functions to new underscore functions
- Provides custom `norm_coords` implementation for removed functions
- Ensures compatibility with igraph >= 1.3.0

Key compatibility functions:
- `moneca_graph_from_adjacency()` → `graph.adjacency()` or `graph_from_adjacency_matrix()`
- `moneca_get_edgelist()` → `get.edgelist()` or `as_edgelist()`
- `moneca_norm_coords()` → custom implementation when `norm_coords` unavailable

### Modern Visualization System
The package now includes modern plotting functions using ggraph:
- `plot_moneca_ggraph()`: Main network visualization with ggraph
- `plot_ego_ggraph()`: Ego network analysis plots  
- `plot_stair_ggraph()`: Multi-level segmentation visualization
- Legacy functions (`gg.moneca()`, `moneca.plot()`) still available for backward compatibility

### Synthetic Data Generation
- `generate_mobility_data()`: Creates realistic synthetic mobility matrices
- `generate_example_datasets()`: Pre-configured datasets for different mobility scenarios
- Supports customizable parameters: immobility strength, class clustering, noise levels

### Testing Framework (tests/testthat/)
- **Comprehensive test suite** using testthat (>= 3.0.0)
- **test-synthetic-data.R**: Data generation validation, parameter handling, reproducibility
- **test-moneca-core.R**: Core algorithms, weight matrices, hierarchical segmentation
- **test-plotting.R**: Modern ggraph functions, parameter validation, error handling
- **test-igraph-compatibility.R**: Version compatibility, function mapping, edge cases
- **Run tests**: `devtools::test()` or `testthat::test_dir("tests")`
- **Coverage**: Use `covr::package_coverage()` to check test coverage

### Documentation Structure
- **Vignette**: `vignettes/moneca-introduction.Rmd` - Comprehensive tutorial
- **Function docs**: Roxygen2-generated documentation with modern examples
- **README.md**: Updated with modern workflow, installation, quick start
- **CLAUDE.md**: Development guidance (this file)

### Package Dependencies
- **Core Required**: ggplot2 (>= 3.4.0), ggraph (>= 2.0.0), igraph (>= 1.3.0), tidygraph (>= 1.2.0), dplyr (>= 1.0.0)
- **Additional**: RColorBrewer, scales, toOrdinal, grid, stats, grDevices
- **Development**: knitr, rmarkdown, testthat (>= 3.0.0)
- **Check deps**: `devtools::install_deps()` installs all required dependencies

### Development Best Practices
- **S3 classes** with custom print methods for MONECA objects
- **Backward compatibility** maintained for all existing functions
- **Modern examples** use synthetic data (no external dependencies)
- **Extensive parameter validation** with informative error messages
- **Reproducible analysis** with seed parameters in synthetic data
- **Modular architecture** with clear separation of concerns

## Quick Start for Development

### Testing New Features
```r
# Load package for development
devtools::load_all()

# Generate test data
test_data <- generate_mobility_data(n_classes = 5, seed = 123)

# Run analysis
seg <- moneca(test_data, segment.levels = 3)

# Test modern plotting
plot_moneca_ggraph(seg, node_color = "segment")

# Run specific tests
testthat::test_file("tests/testthat/test-moneca-core.R")
```

### Common Development Tasks
```r
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
- **R/MONECA.R**: Package-level documentation
- **R/analytical_functions.R**: Core algorithm (`moneca()`, `find.segments()`, etc.)
- **R/modern_plotting.R**: New ggraph visualization functions
- **R/synthetic_data.R**: Data generation functions
- **R/ggplotting.R**: Legacy ggplot2 functions (still functional)
- **R/igraph_compatibility.R**: Version compatibility layer
- **tests/testthat/**: Complete test suite
- **vignettes/**: Package tutorial and documentation
- **man/**: Auto-generated function documentation

### Migration Notes for Existing Users
- **Plotting**: Recommend migrating from `gg.moneca()` to `plot_moneca_ggraph()`
- **Data**: Use `generate_mobility_data()` instead of external datasets for examples
- **Testing**: All existing functions continue to work unchanged
- **Dependencies**: Package now requires ggraph and tidygraph for modern features