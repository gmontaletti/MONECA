# MONECA 0.3.2

## Breaking Changes

* **Removed experimental analysis functions**:
  - Removed `moneca_comprehensive_analysis()` - functionality integrated into enhanced print method
  - Removed `segment_mobility_analysis()` - use standard analysis functions instead
  - These functions were experimental and better served by existing core functions

## Improvements

* **Enhanced print.moneca() method**:
  - Now displays all previously hidden statistics by default
  - Clearer labeling and organization of output
  - Shows level-specific mobility concentration (not just average)
  - Includes detailed degree distributions with `show.degree.stats = TRUE` (default)
  - Better formatted output with clear section headers

* **New vignette**: "moneca-statistics" explaining all statistics in detail

## Documentation

* Comprehensive guide to interpreting MONECA statistics
* Clear explanations of each metric with examples
* Practical interpretation guidelines

# MONECA 0.3.1

## New Features

* **Segment-level mobility analysis**: New `segment_mobility_analysis()` function
* **Enhanced dendrogram visualization**: Improved `plot_moneca_dendrogram()` with better styling

# MONECA 0.3.0

## New Features

* **Perfect dendrogram visualization**: Zero crossing lines in hierarchical plots
* **Comprehensive analysis function**: New `moneca_comprehensive_analysis()`

# MONECA 0.2.9

## Improvements

* Enhanced documentation and visualizations
* Better error handling in analysis functions

# MONECA 0.2.8

## New Features

* **New algorithm comparison vignette**:
  - Comprehensive comparison of `moneca()` vs `moneca_fast()` algorithms
  - Detailed step-by-step workflow explanations for both algorithms
  - Performance benchmarking and practical usage guidelines
  - Parameter tuning recommendations for `moneca_fast()`
  - Neutral analysis of pros and cons for each approach

## Bug Fixes

* **Fixed ggplot2 guide warnings**:
  - Removed inappropriate `guide_colourbar()` warning for edge colors in `plot_ego_ggraph()`
  - Added `guide = "none"` to `scale_edge_color_viridis()` to suppress unwanted legend

* **Fixed hull positioning in `plot_moneca_ggraph()`**:
  - Completely restructured hull rendering to place hulls in background layer
  - Fixed coordinate extraction to properly align hulls with node positions
  - Eliminated duplicate hull rendering code
  - Improved layer ordering: hulls → edges → nodes → labels

## Improvements

* Cleaner visualization output with proper layer ordering
* Better hull positioning that actually contains the nodes
* More robust coordinate handling for custom layouts
* Enhanced documentation with detailed algorithm internals

# MONECA 0.2.7

## Bug Fixes

* **Fixed igraph clique warning**:
  - Added wrapper functions `moneca_cliques()` and `moneca_max_cliques()` that automatically convert directed graphs to undirected before clique calculations
  - Eliminated "Edge directions are ignored for maximal clique calculation" warning
  - Enhanced igraph compatibility layer with proper clique handling

## Improvements

* Cleaner console output with suppressed igraph warnings
* Better handling of directed vs undirected graphs in clique operations

# MONECA 0.2.6

## Code Modernization

* **Complete Danish to English translation**:
  - Translated all Danish comments to English throughout the codebase
  - Renamed Danish variable names to English equivalents:
    * `gruppe` → `group`
    * `grupper` → `groups` 
    * `niv.nu` → `level.current`
    * `niv.ned` → `level.below`
    * `bejler` → `candidate`
    * `klike` → `clique`
    * `potentiel.klike` → `potential.clique`
  - Updated function names: `bejler.test` → `clique.test`
  - Improved code readability and accessibility for international users

## Improvements

* Better code documentation with clear English comments
* More intuitive variable naming throughout the package
* Enhanced maintainability for future development

# MONECA 0.2.5

## Major Fixes

* **Fixed hull positioning in `plot_moneca_ggraph()`**:
  - Hulls now properly contain vertices using actual ggraph layout coordinates
  - Improved coordinate extraction to match node positions exactly
  - Fixed layer ordering issues that caused hulls to appear disconnected
  - Each segment hull now renders individually for better visual accuracy

## New Features

* **Enhanced `plot_stair_ggraph()` with first level visualization**:
  - Added `include_first_level = TRUE` parameter to show individual classes
  - Level 1 shows all individual classes without segmentation
  - Complete progression visualization from individual classes to final segments
  - Better understanding of segmentation evolution across hierarchical levels

## Improvements

* More robust hull rendering that works with both automatic and custom layouts
* Consistent visualization approach across all modern plotting functions
* Better documentation for stair plot parameters

# MONECA 0.2.4

## Bug Fixes

* Fixed hull positioning in `plot_moneca_ggraph()`:
  - Hulls now properly contain vertices instead of being plotted aside
  - Improved coordinate extraction to align with ggraph layout system
  - Better handling of both automatic layouts ("fr", "stress") and custom coordinate matrices
  - Fixed variable scope issues with hull label positioning

## Technical Improvements

* Enhanced layout coordinate handling for better hull-vertex alignment
* More robust segment boundary rendering across different layout types

# MONECA 0.2.3

## Bug Fixes

* Fixed layer ordering in modern plotting functions:
  - Hulls now render as background layer instead of covering nodes
  - Proper rendering order: hulls → edges → nodes → labels
  - Applies to both `plot_moneca_ggraph()` and `plot_stair_ggraph()`

* Updated ego plot labeling:
  - `plot_ego_ggraph()` now uses individual profession names instead of segment names
  - Better interpretability for ego network analysis

## Improvements

* Cleaned up redundant hull rendering code in `plot_stair_ggraph()`
* More consistent behavior across all modern plotting functions

# MONECA 0.2.2

## Improvements

* Enhanced modern plotting functions (`plot_moneca_ggraph()`, `plot_ego_ggraph()`, `plot_stair_ggraph()`):
  - Removed arrow heads from edges for cleaner network visualization
  - Made edge alpha proportional to edge weight for better visual hierarchy
  - Implemented rounded convex hulls when ggforce package is available
  - Replaced individual node labels with segment labels when showing hulls
  - Removed legends to maximize plot space
  - Added segment labels connected to hull boundaries
  - Reduced title size and spacing for more compact plots

## Visual Enhancements

* Segment visualization now uses meaningful group names instead of concatenated profession names
* Hull labels are positioned above segment boundaries with bold text
* Edge transparency now reflects connection strength
* Cleaner, more professional network visualizations

# MONECA 0.2.1

## New Features

* Added `moneca_fast()` - an optimized version of the core MONECA algorithm with significant performance improvements:
  - Uses maximal cliques instead of all cliques for better performance on dense networks
  - Implements vectorized operations in the segmentation algorithm
  - Adds support for sparse matrices via the Matrix package
  - Includes early stopping conditions based on edge density
  - Adds option to limit maximum clique size
  - Maintains full compatibility with existing MONECA output structure

## Improvements

* Performance optimizations reduce computation time, especially for larger mobility tables
* New parameters in `moneca_fast()`:
  - `use.sparse`: Enable sparse matrix operations for large datasets
  - `min.density`: Set minimum edge density threshold for early stopping
  - `max.clique.size`: Limit clique sizes to improve performance on very dense networks
  - `progress`: Control progress bar display

## Documentation

* Added comprehensive tests for `moneca_fast()`
* Added example demonstrating performance improvements in `inst/examples/moneca_fast_example.R`

# MONECA 0.2.0

## Major Update - Comprehensive Modernization

This release represents a major stabilization and modernization of the MONECA package, including modern visualization, synthetic data generation, complete testing, and enhanced documentation.

## New Features

### Modern Visualization System
* `plot_moneca_ggraph()` - Modern network visualization using ggraph
* `plot_ego_ggraph()` - Ego network analysis plots with contemporary styling  
* `plot_stair_ggraph()` - Multi-level segmentation visualization
* Support for various layouts: Fruchterman-Reingold, Kamada-Kawai, stress-based
* Customizable node aesthetics (size, color, shape by segment/mobility)
* Advanced edge styling (width, color, transparency by weight)

### Synthetic Data Generation
* `generate_mobility_data()` - Create realistic synthetic mobility matrices
* `generate_example_datasets()` - Pre-configured datasets for testing
* Customizable parameters: immobility strength, class clustering, noise levels
* Support for custom class names and reproducible generation with seeds

### Testing Framework
* Comprehensive test suite with 30+ tests across all major functions
* Tests for core algorithms, plotting functions, data generation
* igraph compatibility testing across versions
* Edge case handling and error validation

## Improvements

### igraph Compatibility
* Sophisticated compatibility layer for igraph >= 1.3.0
* Automatic function mapping between old and new APIs
* Custom implementation of removed functions (e.g., norm_coords)
* Runtime version detection and adaptation

### Code Quality
* Fixed critical bugs in matrix validation and edge calculations
* Improved error messages and input validation
* Better handling of edge cases and small datasets
* Memory usage optimizations

### Documentation
* New comprehensive vignette with modern examples
* All examples now use synthetic data (no external dependencies)
* Updated README with modern workflow
* Complete roxygen2 documentation for all functions

## Bug Fixes

* Fixed viewport error in plotting functions
* Resolved matrix validation issues in moneca.plot()
* Fixed edge calculation bugs in find.segments()
* Corrected vignette build errors with tidygraph functions
* Fixed "closure" object subsetting error

## Backward Compatibility

* All existing functions remain fully functional
* Legacy plotting functions (gg.moneca, moneca.plot) preserved
* Original API unchanged while adding new features

## Dependencies

* Now requires: ggraph (>= 2.0.0), tidygraph (>= 1.2.0), dplyr (>= 1.0.0)
* Maintains compatibility with R >= 3.5.0
* Full compatibility with latest igraph versions