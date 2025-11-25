pkgname <- "moneca"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('moneca')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("assess_clustering_stability")
### * assess_clustering_stability

flush(stderr()); flush(stdout())

### Name: assess_clustering_stability
### Title: Assess Clustering Stability Using Bootstrap Resampling
### Aliases: assess_clustering_stability
### Keywords: internal

### ** Examples

## Not run: 
##D # Generate sample data
##D mobility_data <- generate_mobility_data(n_classes = 6, seed = 123)
##D 
##D # Assess stability for different parameters (uses default n_bootstrap = 50)
##D stability_0 <- assess_clustering_stability(mobility_data,
##D                                           small.cell.reduction = 0)
##D stability_5 <- assess_clustering_stability(mobility_data,
##D                                           small.cell.reduction = 5)
##D 
##D cat("Stability with parameter 0:", stability_0, "\n")
##D cat("Stability with parameter 5:", stability_5, "\n")
##D 
##D # For more thorough assessment, increase n_bootstrap
##D stability_thorough <- assess_clustering_stability(mobility_data,
##D                                                   small.cell.reduction = 2,
##D                                                   n_bootstrap = 100)
## End(Not run)




cleanEx()
nameEx("assess_clustering_stability_parallel")
### * assess_clustering_stability_parallel

flush(stderr()); flush(stdout())

### Name: assess_clustering_stability_parallel
### Title: Parallel Assessment of Clustering Stability Using Bootstrap
###   Resampling
### Aliases: assess_clustering_stability_parallel
### Keywords: internal

### ** Examples

## Not run: 
##D # Generate sample data
##D mobility_data <- generate_mobility_data(n_classes = 6, seed = 123)
##D 
##D # Parallel stability assessment (auto-detect cores, default n_bootstrap = 50)
##D stability_parallel <- assess_clustering_stability_parallel(
##D   mobility_data,
##D   small.cell.reduction = 5,
##D   parallel = TRUE
##D )
##D 
##D # Sequential for comparison
##D stability_seq <- assess_clustering_stability_parallel(
##D   mobility_data,
##D   small.cell.reduction = 5,
##D   parallel = FALSE
##D )
##D 
##D # For more thorough assessment, increase n_bootstrap
##D stability_thorough <- assess_clustering_stability_parallel(
##D   mobility_data,
##D   small.cell.reduction = 5,
##D   n_bootstrap = 100,
##D   parallel = TRUE
##D )
## End(Not run)




cleanEx()
nameEx("auto_tune_small_cell_reduction")
### * auto_tune_small_cell_reduction

flush(stderr()); flush(stdout())

### Name: auto_tune_small_cell_reduction
### Title: Enhanced Automatic Small Cell Reduction Parameter Tuning with
###   Multi-Objective Optimization
### Aliases: auto_tune_small_cell_reduction

### ** Examples

## Not run: 
##D # Generate sample mobility data
##D mobility_data <- generate_mobility_data(n_classes = 6, seed = 123)
##D 
##D # Quality-based tuning (default, fast and recommended)
##D tuning_result <- auto_tune_small_cell_reduction(
##D   mx = mobility_data,
##D   verbose = TRUE
##D )
##D 
##D # Use optimal parameter in moneca analysis
##D segments <- moneca(mobility_data,
##D                   small.cell.reduction = tuning_result$optimal_value)
##D 
##D # Stability-based tuning (slower but more thorough)
##D stability_result <- auto_tune_small_cell_reduction(
##D   mx = mobility_data,
##D   method = "stability",
##D   n_trials = 10,
##D   verbose = TRUE
##D )
##D 
##D # Quality-based tuning with custom candidates
##D custom_result <- auto_tune_small_cell_reduction(
##D   mx = mobility_data,
##D   candidate_values = c(0, 1, 2, 5, 10, 20),
##D   verbose = TRUE
##D )
##D 
##D # Performance-aware tuning
##D performance_result <- auto_tune_small_cell_reduction(
##D   mx = mobility_data,
##D   method = "performance",
##D   performance_weight = 0.5,  # Equal weight to speed and quality
##D   verbose = TRUE
##D )
## End(Not run)




cleanEx()
nameEx("compute_tuning_metrics")
### * compute_tuning_metrics

flush(stderr()); flush(stdout())

### Name: compute_tuning_metrics
### Title: Compute Multi-Objective Optimization Metrics
### Aliases: compute_tuning_metrics
### Keywords: internal

### ** Examples

## Not run: 
##D # Generate sample data and create weight matrix
##D mobility_data <- generate_mobility_data(n_classes = 5, seed = 123)
##D weight_matrix <- weight.matrix(mobility_data, small.cell.reduction = 2)
##D 
##D # Compute quality metrics
##D quality <- compute_tuning_metrics(weight_matrix = weight_matrix)
##D print(quality)
## End(Not run)




cleanEx()
nameEx("ego.plot")
### * ego.plot

flush(stderr()); flush(stdout())

### Name: ego.plot
### Title: Legacy Ego Network Visualization
### Aliases: ego.plot

### ** Examples

## Not run: 
##D # Using synthetic data
##D mobility_data <- generate_mobility_data(n_classes = 6, seed = 123)
##D seg <- moneca(mobility_data, segment.levels = 2)
##D ego.plot(seg, mobility_data, id = 2)
##D 
##D # Customized ego plot
##D ego.plot(seg, mobility_data, 
##D          id = 3,
##D          edge.size = 1.2,
##D          color.scheme = "Blues",
##D          small.cell.reduction = 10)
## End(Not run)




cleanEx()
nameEx("evaluate_performance_trade_offs")
### * evaluate_performance_trade_offs

flush(stderr()); flush(stdout())

### Name: evaluate_performance_trade_offs
### Title: Evaluate Performance Trade-offs
### Aliases: evaluate_performance_trade_offs
### Keywords: internal

### ** Examples

## Not run: 
##D # This function is typically called internally by auto_tune_small_cell_reduction
##D # Example usage in isolation:
##D 
##D quality_list <- list(
##D   list(overall = 0.8),
##D   list(overall = 0.6),
##D   list(overall = 0.9)
##D )
##D 
##D performance_list <- list(
##D   list(time = 1.2, valid = TRUE),
##D   list(time = 0.8, valid = TRUE), 
##D   list(time = 2.1, valid = TRUE)
##D )
##D 
##D trade_offs <- evaluate_performance_trade_offs(
##D   quality_metrics = quality_list,
##D   performance_metrics = performance_list,
##D   performance_weight = 0.3
##D )
## End(Not run)




cleanEx()
nameEx("generate_candidate_values")
### * generate_candidate_values

flush(stderr()); flush(stdout())

### Name: generate_candidate_values
### Title: Generate Data-Driven Candidate Values
### Aliases: generate_candidate_values
### Keywords: internal

### ** Examples

## Not run: 
##D # Generate sample data
##D mobility_data <- generate_mobility_data(n_classes = 5, seed = 123)
##D 
##D # Percentile-based candidates
##D candidates_pct <- generate_candidate_values(mobility_data, "percentile")
##D 
##D # Sparsity-informed candidates  
##D candidates_sparse <- generate_candidate_values(mobility_data, "sparsity")
##D 
##D # Hybrid approach (recommended)
##D candidates_hybrid <- generate_candidate_values(mobility_data, "hybrid")
##D 
##D print(candidates_hybrid)
## End(Not run)




cleanEx()
nameEx("generate_example_datasets")
### * generate_example_datasets

flush(stderr()); flush(stdout())

### Name: generate_example_datasets
### Title: Generate Predefined Example Datasets
### Aliases: generate_example_datasets
### Keywords: internal

### ** Examples

## Not run: 
##D # Load all example datasets
##D examples <- generate_example_datasets()
##D names(examples)
##D 
##D # Examine the simple example
##D print(examples$simple)
##D 
##D # Compare different mobility regimes
##D rigid_seg <- moneca(examples$rigid, segment.levels = 3)
##D fluid_seg <- moneca(examples$fluid, segment.levels = 3)
##D 
##D # Visualize different structures
##D plot_moneca_ggraph(rigid_seg, title = "Rigid Class Structure")
##D plot_moneca_ggraph(fluid_seg, title = "Fluid Mobility Regime")
## End(Not run)




cleanEx()
nameEx("generate_mobility_data")
### * generate_mobility_data

flush(stderr()); flush(stdout())

### Name: generate_mobility_data
### Title: Generate Realistic Synthetic Mobility Data
### Aliases: generate_mobility_data

### ** Examples

# Basic synthetic data with default parameters
basic_data <- generate_mobility_data()
print(basic_data[1:6, 1:6])  # Show first 5 classes plus totals

# Small example for quick testing
test_data <- generate_mobility_data(
  n_classes = 5,
  n_total = 1000,
  immobility_strength = 0.7,
  class_clustering = 0.2,
  seed = 42
)

# Professional class structure with custom names
professional_data <- generate_mobility_data(
  n_classes = 6,
  n_total = 5000,
  class_names = c("Upper", "Upper-Middle", "Middle", 
                  "Lower-Middle", "Working", "Lower"),
  immobility_strength = 0.5,
  class_clustering = 0.4,
  seed = 123
)

# Highly fluid mobility regime
fluid_data <- generate_mobility_data(
  n_classes = 8,
  immobility_strength = 0.3,
  class_clustering = 0.1,
  noise_level = 0.3
)

# Use in moneca analysis
seg <- moneca(test_data, segment.levels = 3)
## Not run: 
##D plot_moneca_ggraph(seg, title = "Synthetic Mobility Analysis")
## End(Not run)




cleanEx()
nameEx("gg.moneca")
### * gg.moneca

flush(stderr()); flush(stdout())

### Name: gg.moneca
### Title: Legacy ggplot2 Visualization for moneca Objects
### Aliases: gg.moneca

### ** Examples

## Not run: 
##D # Using synthetic data
##D mobility_data <- generate_mobility_data(n_classes = 5, seed = 123)
##D seg <- moneca(mobility_data, segment.levels = 2)
##D gg.moneca(seg)
##D 
##D # Custom styling
##D gg.moneca(seg, 
##D          vertex.fill = "red",
##D          edge.color = "blue",
##D          show.borders = FALSE)
## End(Not run)




cleanEx()
nameEx("moneca")
### * moneca

flush(stderr()); flush(stdout())

### Name: moneca
### Title: MONECA - Mobility Network Clustering Analysis
### Aliases: moneca

### ** Examples

# Generate synthetic mobility data
mobility_data <- generate_mobility_data(n_classes = 6, seed = 42)

# Run moneca analysis
seg <- moneca(mobility_data, segment.levels = 3)
print(seg)

# Run moneca with auto-tuning for optimal small.cell.reduction
seg_tuned <- moneca(mobility_data, segment.levels = 3, 
                    auto_tune = TRUE, tune_method = "stability", 
                    tune_verbose = TRUE)
print(seg_tuned)

# Examine segment membership
membership <- segment.membership(seg)
print(membership)




cleanEx()
nameEx("moneca_fast")
### * moneca_fast

flush(stderr()); flush(stdout())

### Name: moneca_fast
### Title: MONECA Fast - Memory-Optimized Mobility Network Clustering
###   Analysis
### Aliases: moneca_fast

### ** Examples

# Generate synthetic mobility data
mobility_data <- generate_mobility_data(n_classes = 6, seed = 42)

# Run moneca analysis (fast version)
seg <- moneca_fast(mobility_data, segment.levels = 3)
print(seg)

# Run with auto-tuning for optimal parameters
seg_tuned <- moneca_fast(mobility_data, segment.levels = 3, 
                         auto_tune = TRUE, tune_method = "stability")
print(seg_tuned)

# Examine segment membership
membership <- segment.membership(seg)
print(membership)

# Visualize with modern plotting
## Not run: 
##D plot_moneca_ggraph(seg, node_color = "segment", title = "MONECA Clustering")
## End(Not run)




cleanEx()
nameEx("new_moneca")
### * new_moneca

flush(stderr()); flush(stdout())

### Name: new_moneca
### Title: New MONECA - Fast Clustering with Maximal Cliques
### Aliases: new_moneca
### Keywords: internal

### ** Examples

## Not run: 
##D # Generate synthetic mobility data
##D mobility_data <- generate_mobility_data(n_classes = 6, seed = 42)
##D 
##D # Basic usage - uses maximal cliques
##D seg_new <- new_moneca(mobility_data, segment.levels = 3)
##D print(seg_new)
##D 
##D # Compare with standard moneca
##D seg_standard <- moneca(mobility_data, segment.levels = 3)
##D 
##D # Visualize results
##D plot_moneca_ggraph(seg_new, node_color = "segment")
## End(Not run)




cleanEx()
nameEx("plot_ego_ggraph")
### * plot_ego_ggraph

flush(stderr()); flush(stdout())

### Name: plot_ego_ggraph
### Title: Ego Network Visualization with ggraph
### Aliases: plot_ego_ggraph

### ** Examples

## Not run: 
##D # Generate synthetic data and run MONECA
##D mobility_data <- generate_mobility_data(n_classes = 6, seed = 123)
##D seg <- moneca(mobility_data, segment.levels = 3)
##D 
##D # Ego network for the middle category (index 3)
##D plot_ego_ggraph(seg, mobility_data, ego_id = 3,
##D                 title = "Mobility from Middle Class")
## End(Not run)




cleanEx()
nameEx("plot_moneca_dendrogram")
### * plot_moneca_dendrogram

flush(stderr()); flush(stdout())

### Name: plot_moneca_dendrogram
### Title: Plot MONECA Results as Dendrogram
### Aliases: plot_moneca_dendrogram

### ** Examples

## Not run: 
##D # Generate synthetic data and run MONECA
##D mobility_data <- generate_mobility_data(n_classes = 8, seed = 123)
##D seg <- moneca(mobility_data, segment.levels = 3)
##D 
##D # Basic dendrogram
##D plot_moneca_dendrogram(seg)
## End(Not run)




cleanEx()
nameEx("plot_moneca_ggraph")
### * plot_moneca_ggraph

flush(stderr()); flush(stdout())

### Name: plot_moneca_ggraph
### Title: Modern Network Visualization for MONECA Results
### Aliases: plot_moneca_ggraph

### ** Examples

## Not run: 
##D # Generate synthetic data and run MONECA
##D mobility_data <- generate_mobility_data(n_classes = 6, seed = 123)
##D seg <- moneca(mobility_data, segment.levels = 3)
##D 
##D # Basic network plot
##D plot_moneca_ggraph(seg)
##D 
##D # Customized plot with different aesthetics
##D plot_moneca_ggraph(seg,
##D   layout = "stress",
##D   node_color = "mobility",
##D   color_palette = "Spectral",
##D   title = "Social Mobility Network"
##D )
## End(Not run)




cleanEx()
nameEx("plot_segment_quality")
### * plot_segment_quality

flush(stderr()); flush(stdout())

### Name: plot_segment_quality
### Title: Visualize Segment Quality Metrics
### Aliases: plot_segment_quality

### ** Examples

## Not run: 
##D # Generate data and run MONECA
##D mobility_data <- generate_mobility_data(n_classes = 8, seed = 123)
##D seg <- moneca(mobility_data, segment.levels = 3)
##D 
##D # Overview of level 2 quality
##D plot_segment_quality(seg)
##D 
##D # Radar plot for segment comparison
##D plot_segment_quality(seg, plot_type = "radar", level = 2)
## End(Not run)




cleanEx()
nameEx("plot_stair_ggraph")
### * plot_stair_ggraph

flush(stderr()); flush(stdout())

### Name: plot_stair_ggraph
### Title: Multi-Level Segmentation Visualization (Stair Plot)
### Aliases: plot_stair_ggraph

### ** Examples

## Not run: 
##D # Generate synthetic data and run MONECA
##D mobility_data <- generate_mobility_data(n_classes = 6, seed = 123)
##D seg <- moneca(mobility_data, segment.levels = 4)
##D 
##D # Basic stair plot
##D stair_plots <- plot_stair_ggraph(seg)
##D 
##D # Customized stair plot with specific levels
##D custom_stair <- plot_stair_ggraph(seg,
##D                                  levels = c(2, 3),
##D                                  layout = "stress",
##D                                  ncol = 1)
## End(Not run)




cleanEx()
nameEx("print.moneca")
### * print.moneca

flush(stderr()); flush(stdout())

### Name: print.moneca
### Title: Print Method for moneca Objects (Enhanced)
### Aliases: print.moneca

### ** Examples

# Generate data and run analysis
mobility_data <- generate_mobility_data(n_classes = 6, seed = 42)
seg <- moneca(mobility_data, segment.levels = 3)

# Print comprehensive summary with all statistics
print(seg)

# Hide detailed degree distributions for cleaner output
print(seg, show.degree.stats = FALSE)

# Show more decimal places for precision
print(seg, digits = 2)




cleanEx()
nameEx("segment.membership")
### * segment.membership

flush(stderr()); flush(stdout())

### Name: segment.membership
### Title: Extract Segment Membership Information
### Aliases: segment.membership

### ** Examples

# Generate data and run analysis
mob_data <- generate_mobility_data(n_classes = 5, seed = 42)
seg <- moneca(mob_data, segment.levels = 3)

# Get membership information
membership <- segment.membership(seg)
print(membership)

# Get membership for specific levels only
membership_level2 <- segment.membership(seg, level = 2)




cleanEx()
nameEx("segment.membership.dataframe")
### * segment.membership.dataframe

flush(stderr()); flush(stdout())

### Name: segment.membership.dataframe
### Title: Generate Segment Membership Dataframe from MONECA Results
### Aliases: segment.membership.dataframe

### ** Examples

# Generate example data
mx <- generate_mobility_data(n_classes = 5, immobility_strength = 0.7)

# Run moneca analysis
seg <- moneca_fast(mx, segment.levels = 3)

# Generate membership dataframe
membership_df <- segment.membership.dataframe(seg)
print(membership_df)



cleanEx()
nameEx("segment.membership.enhanced")
### * segment.membership.enhanced

flush(stderr()); flush(stdout())

### Name: segment.membership.enhanced
### Title: Enhanced Segment Membership with Meaningful Names
### Aliases: segment.membership.enhanced
### Keywords: internal

### ** Examples

## Not run: 
##D # Generate data and run analysis
##D mob_data <- generate_mobility_data(n_classes = 6, seed = 42)
##D seg <- moneca(mob_data, segment.levels = 3)
##D 
##D # Get enhanced membership with automatic naming
##D enhanced <- segment.membership.enhanced(seg)
##D print(enhanced)
## End(Not run)




cleanEx()
nameEx("segment.quality")
### * segment.quality

flush(stderr()); flush(stdout())

### Name: segment.quality
### Title: Evaluate Segment Quality Metrics
### Aliases: segment.quality

### ** Examples

# Generate data and run MONECA
mobility_data <- generate_mobility_data(n_classes = 8, seed = 123)
seg <- moneca(mobility_data, segment.levels = 3)

# Get detailed quality metrics for all levels
quality_full <- segment.quality(seg)
print(quality_full)

# Get only final solution summary
quality_final <- segment.quality(seg, final.solution = TRUE)
print(quality_final)

# Use custom segment naming with final solution
custom_names <- data.frame(
  name = c("Class1", "Class2", "Class3"),
  segment_label = c("Executive", "Professional", "Technical"),
  stringsAsFactors = FALSE
)
quality_labeled <- segment.quality(seg, final.solution = TRUE, segment_naming = custom_names)
print(quality_labeled)

## Not run: 
##D # Visualize segment quality
##D plot_segment_quality(seg)
## End(Not run)




cleanEx()
nameEx("stair.plot")
### * stair.plot

flush(stderr()); flush(stdout())

### Name: stair.plot
### Title: Legacy Multi-Level Stair Plot
### Aliases: stair.plot

### ** Examples

## Not run: 
##D # Using synthetic data
##D mobility_data <- generate_mobility_data(n_classes = 6, seed = 123)
##D seg <- moneca(mobility_data, segment.levels = 3)
##D plots <- stair.plot(seg)
##D plots[[2]]  # Display second level
##D 
##D # Customized stair plot
##D custom_stairs <- stair.plot(seg,
##D                            level = c(2, 3, 4),
##D                            vertex.size = "total",
##D                            show.borders = TRUE,
##D                            border.text.size = 5)
## End(Not run)




cleanEx()
nameEx("weight.matrix")
### * weight.matrix

flush(stderr()); flush(stdout())

### Name: weight.matrix
### Title: Calculate Relative Risk Weight Matrix
### Aliases: weight.matrix

### ** Examples

# Create a simple mobility table
mob_table <- matrix(c(100, 20, 10, 130,
                      15, 80, 25, 120,  
                      5,  10, 50,  65,
                      120, 110, 85, 315), 
                    nrow = 4, byrow = TRUE)
rownames(mob_table) <- colnames(mob_table) <- c("A", "B", "C", "Total")

# Calculate relative risk matrix
rr_matrix <- weight.matrix(mob_table, cut.off = 1.5)

# Use automatic tuning for small.cell.reduction parameter
rr_matrix_tuned <- weight.matrix(mob_table, auto_tune = TRUE, 
                                 tune_method = "stability", tune_verbose = TRUE)

# Check tuning results
if (attr(rr_matrix_tuned, "auto_tuned", exact = TRUE)) {
  optimal_value <- attr(rr_matrix_tuned, "tuned_small_cell_reduction", exact = TRUE)
  message("Optimal small.cell.reduction: ", optimal_value)
}




### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
