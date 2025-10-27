# moneca 0.9.0

## Breaking Changes

* **Fixed clique detection inconsistencies across implementations**: All three implementations (`moneca()`, `moneca_fast()`, `moneca_parallel()`) now use **all cliques by default** for algorithmic correctness and reproducibility. Previously:
  - `moneca()` and `moneca_parallel()` were silently switching to maximal cliques for graphs with edge_density > 0.6 && n_vertices > 15
  - `moneca_fast()` was always using maximal cliques
  - This caused the three implementations to produce different clustering results

* **New explicit user control**: Added `use_maximal_cliques` parameter to all three functions:
  - Default: `FALSE` (use all cliques - algorithmic fidelity)
  - Set to `TRUE` for performance optimization on very dense graphs
  - No more automatic switching - user makes explicit choice

* **Removed automatic fallback logic** from igraph compatibility layer to prevent unintended behavior changes

## Impact

* **Scientific correctness**: All implementations now produce identical results by default
* **Reproducibility**: Results are consistent across `moneca()`, `moneca_fast()`, and `moneca_parallel()`
* **Performance**: Users with very dense graphs can explicitly enable `use_maximal_cliques=TRUE`
* **Migration**: If you were relying on the previous behavior, set `use_maximal_cliques=TRUE` explicitly

## New Features

* **Algorithm equivalence tests**: Added comprehensive test suite (`test-algorithm-equivalence.R`) validating that all implementations produce identical results

## Bug Fixes

* Fixed architectural issue where wrapper layer (igraph_compatibility.R) was overriding algorithm-level clique selection decisions
* Removed conflicting threshold checks (0.6/15 in wrapper vs 0.7/20 in algorithm layer)
* Changed `moneca_fast()` default for `min.density` from 0.01 to 0 (disabled) to ensure algorithmic fidelity. Users wanting early stopping optimization can explicitly set `min.density > 0`.

# moneca 0.8.1

## Bug Fixes

* **Fixed `calculate_gini()` Gini coefficient calculation**: The function now correctly includes zero values in the inequality calculation, aligning with the standard mathematical definition and documented behavior. Previously, the function incorrectly filtered out zeros before calculation, which led to underestimating inequality in distributions with isolates or highly centralized networks (e.g., star networks). This fix improves the accuracy of network structure metrics in cutoff sensitivity analysis.

# moneca 0.7.0

## Major New Features

### **Temporal Analysis Framework**

* **Added comprehensive temporal clustering analysis**:
  - `moneca_temporal()`: Moving window clustering for time series of mobility matrices
  - Maintains stable segment labels across time periods using Hungarian algorithm
  - Tracks segment evolution and transitions over time
  - Supports multiple aggregation methods (mean, sum, weighted) for temporal windows
  - Parallel processing support for efficient temporal analysis

* **Temporal stability assessment**:
  - `temporal_stability_analysis()`: Comprehensive stability metrics over time
  - `assess_clustering_stability_parallel()`: Multi-core stability evaluation
  - Tracks clustering consistency across different time windows
  - Provides stability scores and temporal robustness metrics

* **Advanced temporal visualization**:
  - `plot_temporal_segments()`: Network evolution visualization across time
  - `plot_alluvial_diagram()`: Flow-based visualization of segment transitions
  - `plot_segment_timeline()`: Temporal progression of individual segments
  - `plot_transition_heatmap()`: Heatmap visualization of segment transitions
  - `plot_stability_metrics()`: Temporal stability trend visualization

* **Temporal data management**:
  - `aggregate_mobility_window()`: Efficient temporal window aggregation
  - `match_segments_across_time()`: Intelligent segment matching between time periods
  - `compute_transition_matrix()`: Segment-to-segment transition analysis
  - `export_node_trajectories()`: Node-level temporal trajectory extraction

### **Enhanced Documentation**

* **Comprehensive temporal analysis examples**: 
  - Added `example_temporal_analysis.R` with practical workflows
  - Complete documentation for all temporal functions with roxygen2
  - Integration examples showing temporal analysis with auto-tuning
  - Performance optimization guides for large temporal datasets

* **New temporal-specific test suite**:
  - `test-temporal-clustering.R`: Core temporal functionality validation
  - `test-temporal-stability.R`: Stability analysis testing
  - Comprehensive edge case handling and error validation

## Performance Improvements

### **Temporal Processing Optimization**

* **Parallel temporal analysis**: Multi-core processing for temporal windows
* **Memory-efficient aggregation**: Optimized matrix operations for large time series
* **Progressive stability assessment**: Efficient bootstrap-based temporal stability
* **Caching system**: Avoid redundant computations across temporal windows

## API Enhancements

### **Temporal Analysis Integration**

* All temporal functions integrate seamlessly with existing moneca workflow
* Temporal results maintain full compatibility with standard moneca objects
* Enhanced print methods for temporal analysis results
* Comprehensive progress reporting for long-running temporal analyses

### **New S3 Classes**

* `moneca_temporal`: Primary temporal analysis results object
* `temporal_stability`: Stability analysis results with specialized methods
* Custom print methods for clear temporal result interpretation

## Breaking Changes

**None** - Full backward compatibility maintained. All temporal functionality is additive.

# moneca 0.6.0

## Major New Features

### **Comprehensive Auto-Tuning Framework**

* **Added advanced parameter optimization**: 
  - Automatic selection of optimal `small.cell.reduction` parameter values
  - Six sophisticated optimization methods available
  - Eliminates manual parameter selection and improves clustering quality
  - Fully integrated into all main MONECA functions

* **Auto-tuning methods implemented**:
  - `stability`: Bootstrap-based stability assessment (recommended default)
  - `quality`: Clustering quality optimization using silhouette and modularity
  - `performance`: Balanced approach optimizing quality vs computational speed
  - `pareto`: Multi-objective optimization with Pareto frontier analysis
  - `cross_validation`: K-fold cross-validation for conservative parameter selection
  - `bayesian`: Gaussian Process optimization (requires GPfit package)

* **Enhanced main functions with auto-tuning**:
  - `moneca()`: Core function with `auto_tune` and `auto_tune_method` parameters
  - `moneca_fast()`: Optimized version with auto-tuning capabilities
  - `moneca_fast_fixed()`: Fixed-algorithm version with auto-tuning
  - `moneca_parallel()`: Parallel processing version with auto-tuning
  - `weight.matrix()`: Weight matrix computation with parameter optimization
  - `weight.matrix.parallel()`: Parallel weight matrix with auto-tuning

## New Functions

### **Core Auto-Tuning Functions**

* `auto_tune_small_cell_reduction()`: Main auto-tuning function with comprehensive optimization strategies
* `auto_tune_small_cell_reduction_enhanced()`: Enhanced version with advanced features and performance improvements
* `generate_candidate_values()`: Intelligent candidate parameter generation using data-driven methods
* `assess_clustering_stability()`: Bootstrap-based stability assessment for parameter evaluation
* `compute_tuning_metrics()`: Comprehensive clustering quality metrics computation
* `compute_extended_quality_metrics()`: Extended quality assessment with additional measures
* `select_optimal_parameter()`: Parameter selection with multiple optimization strategies

### **Multi-Objective Optimization**

* `pareto_optimization()`: Multi-objective optimization with Pareto frontier analysis
* `compute_pareto_front()`: Pareto front computation for quality/performance trade-offs
* `bayesian_parameter_optimization()`: Bayesian optimization using Gaussian Processes
* `cross_validation_tuning()`: Cross-validation based parameter selection
* `adaptive_candidate_generation()`: Dynamic candidate generation based on search progress

### **Performance Optimization**

* `parallel_stability_assessment()`: Multi-core stability assessment for faster tuning
* `cached_weight_matrix_computation()`: Caching system for repeated weight matrix calculations
* `performance_aware_tuning()`: Speed-optimized tuning for large datasets
* `early_stopping_optimization()`: Early termination for efficient candidate screening

### **Visualization and Analysis**

* `plot_tuning_results()`: Comprehensive visualization of auto-tuning results
* `plot_pareto_front()`: Pareto frontier visualization for multi-objective results
* `parameter_sensitivity_plot()`: Sensitivity analysis visualization
* `tuning_convergence_plot()`: Convergence analysis for optimization methods

## Enhanced Documentation

### **New Vignettes**

* **Auto-Tuning Guide**: Comprehensive 50+ page guide covering:
  - Detailed explanation of all auto-tuning methods
  - Performance comparison and benchmarking
  - Best practices and method selection guidelines
  - Advanced use cases and troubleshooting
  - Real-world examples with before/after comparisons

* **Updated Introduction Vignette**: Added extensive auto-tuning section with:
  - Practical examples showing manual vs auto-tuned results
  - Method comparison visualizations
  - Parameter selection guidelines
  - Performance optimization tips

### **Enhanced Function Documentation**

* Complete roxygen2 documentation for all auto-tuning functions
* Comprehensive examples demonstrating all optimization methods
* Cross-references between related functions
* Parameter selection guidance and best practices

## API Enhancements

### **Backward Compatibility**

* All existing functions maintain full backward compatibility
* Auto-tuning is opt-in (`auto_tune = FALSE` by default)
* Manual parameter specification continues to work unchanged
* No breaking changes to existing workflows

### **New Parameters Across Main Functions**

* `auto_tune`: Enable/disable automatic parameter optimization
* `auto_tune_method`: Select optimization strategy
* `n_trials`: Number of trials for stability assessment
* `performance_weight`: Balance between quality and speed (0-1 scale)
* `candidate_values`: Custom parameter ranges for testing
* `seed`: Reproducibility control for optimization
* `verbose`: Progress reporting during auto-tuning
* `parallel_cores`: Multi-core processing for faster optimization

### **Auto-Tuning Result Attributes**

* All auto-tuned results include comprehensive metadata:
  - `auto_tune_result`: Complete optimization results
  - `best_parameter`: Selected optimal parameter value
  - `best_score`: Optimization score achieved
  - `method`: Auto-tuning method used
  - `evaluation_time`: Time spent on optimization
  - `all_results`: Complete candidate evaluation results

## Performance Improvements

### **Computational Efficiency**

* **Multi-core processing**: Parallel evaluation of parameter candidates
* **Caching system**: Avoid redundant weight matrix computations
* **Early stopping**: Intelligent termination for faster optimization
* **Memory optimization**: Efficient handling of large mobility matrices
* **Progress reporting**: Real-time feedback during long-running optimizations

### **Speed Optimizations**

* Performance-aware tuning method for time-constrained scenarios
* Adaptive candidate generation reducing unnecessary evaluations
* Optimized stability assessment using efficient resampling strategies
* Parallel processing support across all auto-tuning methods

## Quality Improvements

### **Enhanced Clustering Assessment**

* Multiple quality metrics: silhouette score, modularity, clustering coefficient
* Network density and connectivity analysis
* Stability assessment across data perturbations
* Cross-validation for robust parameter selection
* Extended quality metrics for comprehensive evaluation

### **Robust Optimization**

* Multiple independent trials for stability assessment
* Bootstrap resampling for robust quality estimation
* Cross-validation to prevent overfitting
* Multi-objective optimization considering multiple criteria simultaneously

## Technical Infrastructure

### **Dependencies and Compatibility**

* Added GPfit to Suggests for Bayesian optimization (optional)
* Enhanced parallel processing using the parallel package
* Maintained full igraph compatibility (>= 1.3.0)
* All auto-tuning functions work with existing MONECA data structures

### **Code Architecture**

* Modular auto-tuning framework with clean separation of concerns
* Consistent API across all optimization methods
* Comprehensive error handling and validation
* Extensive unit tests for all auto-tuning functionality

## Migration Guide

### **For New Users**

```r
# Recommended workflow with auto-tuning
library(moneca)
data <- generate_mobility_data(n_classes = 6, seed = 123)

# Use stability method for general analysis
result <- moneca(data, segment.levels = 2, 
                 auto_tune = TRUE, auto_tune_method = "stability")

# Check auto-tuning results
auto_info <- attr(result, "auto_tune_result")
print(paste("Optimal parameter:", auto_info$best_parameter))
```

### **For Existing Users**

* Existing code continues to work unchanged
* Add `auto_tune = TRUE` to enable optimization
* Compare results: `moneca(data, auto_tune = TRUE)` vs manual parameters
* Gradual migration: test auto-tuning on subset of analyses first

## Breaking Changes

**None** - Full backward compatibility maintained

# moneca 0.5.9

## New Features

* **Added moneca_fast_fixed() function**:
  - New optimized implementation with adaptive clustering strategies
  - Automatic algorithm selection based on graph density
  - Hybrid approach combining maximal clique enumeration and greedy processing
  - Maintains full API compatibility with existing moneca functions
  - All three implementations (original, fast, fast_fixed) remain active and available

* **Enhanced clique detection algorithms**:
  - Implemented find.segments.greedy() for high-density graphs (density > 0.8)
  - Optimized clique membership matrix processing for moderate-sized problems
  - Direct clique testing for large-scale problems to reduce memory usage
  - Early stopping conditions based on edge density thresholds

## Performance Improvements

* **Critical performance fixes for large-scale networks**:
  - Resolved exponential clique explosion issue that caused hanging on 270x270+ matrices
  - Performance improvement: reduces computation time from hours to seconds for large matrices
  - Memory-efficient processing with sparse matrix support
  - Enhanced progress reporting and error handling for large-scale analyses

* **Adaptive algorithm selection**:
  - Density > 0.8: Uses greedy edge processing (similar to original moneca approach)
  - Moderate density: Uses maximal clique enumeration with size limits
  - Sparse networks: Early termination with individual node segments
  - Automatic fallback mechanisms ensure robust performance across all network types

## Technical Improvements

* **Enhanced igraph compatibility**:
  - Robust version detection for igraph >= 1.3.0
  - Automatic function mapping between old and new igraph APIs
  - Custom wrapper functions for consistent behavior across versions

* **Backward compatibility maintained**:
  - All existing functions continue to work unchanged
  - Identical output structure and format across all implementations
  - Parameter compatibility ensures seamless migration between implementations

# moneca 0.5.1

## Bug Fixes

* **Fixed vignette build error in plot_stair_ggraph()**:
  - Resolved parameter conflict where 'show_segments' was being passed multiple times to plot_moneca_ggraph()
  - Fixed duplicate parameter passing that caused vignette build failures
  - Improved parameter handling in stair plot visualization function

# moneca 0.5.0

## New Features

* **Enhanced segment naming in plotting functions**:
  - `plot_moneca_ggraph()`, `plot_ego_ggraph()`, and `plot_stair_ggraph()` now support advanced segment naming options
  - Added support for custom dataframe-based segment labeling via `segment_naming` parameter
  - Can now provide a dataframe with `segment` and `label` columns for custom segment names
  - Maintains full backward compatibility with existing boolean and string options
  - Enhanced documentation with comprehensive examples of all naming options

## Improvements

* **Flexible segment labeling system**:
  - Support for dataframe input: `data.frame(segment = c(1, 2), label = c("Group A", "Group B"))`
  - Automatic matching of segment numbers to custom labels
  - Robust error handling for invalid dataframe formats
  - Clear validation messages for proper dataframe structure
  
* **Updated vignettes and documentation**:
  - All plotting function examples now demonstrate the new segment naming capabilities
  - Enhanced vignette with practical examples of custom segment labeling
  - Improved function documentation with detailed parameter descriptions

## Technical Changes

* Enhanced parameter validation for segment naming options
* Improved error messages for invalid segment naming inputs
* Better integration of custom labels with existing plotting workflows

# moneca 0.4.0

## Major Changes

* **Package renamed to moneca** (lowercase):
  - All references updated throughout codebase
  - Installation command now: `install_github("gmontaletti/moneca")`
  - Load with: `library(moneca)`

* **New maintainer**: Giampaolo Montaletti <giampaolo.montaletti@gmail.com>
  - This is now a maintained fork of the original MONECA package
  - Full attribution to original authors preserved
  - Active maintenance and development continues

* **Enhanced authorship attribution**:
  - **Jonas Touboel**: Original algorithm design and methodology
  - **Anton Grau Larsen**: Original package development and implementation  
  - **Giampaolo Montaletti**: Current maintainer, modernization, and new features

## Documentation Updates

* **README.md**: Added comprehensive "About this Fork" section with:
  - Clear explanation of fork relationship to original MONECA
  - Full credits to original authors
  - Summary of key improvements and modernizations
  - Updated installation instructions

* **DESCRIPTION**: Updated with proper Authors@R field and ORCID information

* **Package documentation**: Updated throughout to reflect new package name

## Technical Notes

* All existing functionality remains unchanged
* Backward compatibility maintained for all functions
* No breaking changes to user-facing APIs
* Package namespace updated to moneca throughout

# moneca 0.3.3 (former MONECA)

## Improvements

* **Enhanced network statistics with weighted degrees**:
  - `print.moneca()` now uses strength (weighted degree) instead of simple degree counts
  - Statistics now represent mobility **volume** rather than just connection counts
  - More accurate representation of node importance in weighted mobility networks
  - Updated documentation explains difference between degree and strength
  - Section renamed to "Detailed Weighted Degree Distributions (Strength)"

## Technical Changes

* Replaced `degree()` with `strength()` in network statistics calculations
* Updated output labels to reflect weighted degree metrics:
  - "Total Weighted Connections (Strength In + Out)"
  - "Outward Mobility Strength (Weighted Out-Degree)"
  - "Inward Mobility Strength (Weighted In-Degree)"
* Enhanced technical documentation with strength vs degree definitions

# moneca 0.3.2

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

* Comprehensive guide to interpreting moneca statistics
* Clear explanations of each metric with examples
* Practical interpretation guidelines

# moneca 0.3.1

## New Features

* **Segment-level mobility analysis**: New `segment_mobility_analysis()` function
* **Enhanced dendrogram visualization**: Improved `plot_moneca_dendrogram()` with better styling

# moneca 0.3.0

## New Features

* **Perfect dendrogram visualization**: Zero crossing lines in hierarchical plots
* **Comprehensive analysis function**: New `moneca_comprehensive_analysis()`

# moneca 0.2.9

## Improvements

* Enhanced documentation and visualizations
* Better error handling in analysis functions

# moneca 0.2.8

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

# moneca 0.2.7

## Bug Fixes

* **Fixed igraph clique warning**:
  - Added wrapper functions `moneca_cliques()` and `moneca_max_cliques()` that automatically convert directed graphs to undirected before clique calculations
  - Eliminated "Edge directions are ignored for maximal clique calculation" warning
  - Enhanced igraph compatibility layer with proper clique handling

## Improvements

* Cleaner console output with suppressed igraph warnings
* Better handling of directed vs undirected graphs in clique operations

# moneca 0.2.6

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

# moneca 0.2.5

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

# moneca 0.2.4

## Bug Fixes

* Fixed hull positioning in `plot_moneca_ggraph()`:
  - Hulls now properly contain vertices instead of being plotted aside
  - Improved coordinate extraction to align with ggraph layout system
  - Better handling of both automatic layouts ("fr", "stress") and custom coordinate matrices
  - Fixed variable scope issues with hull label positioning

## Technical Improvements

* Enhanced layout coordinate handling for better hull-vertex alignment
* More robust segment boundary rendering across different layout types

# moneca 0.2.3

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

# moneca 0.2.2

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

# moneca 0.2.1

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

# moneca 0.2.0

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