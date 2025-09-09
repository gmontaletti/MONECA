#!/usr/bin/env Rscript

# Example: Temporal MONECA Analysis
# ==================================

# Load the package
library(devtools)
load_all()

cat("MONECA Temporal Analysis Example\n")
cat("================================\n\n")

# 1. Generate synthetic temporal mobility data
cat("1. Generating temporal mobility data...\n")
set.seed(42)

# Create mobility data for 8 time periods (e.g., quarters over 2 years)
time_periods <- 8
quarterly_data <- lapply(1:time_periods, function(quarter) {
  generate_mobility_data(
    n_classes = 6,
    seed = 1000 + quarter * 25,
    immobility_strength = 0.5 + (quarter %% 4) * 0.05,  # Seasonal variation
    class_names = c("Executive", "Professional", "Managerial", 
                   "Skilled", "Semiskilled", "Unskilled")
  )
})

names(quarterly_data) <- paste0("Q", 1:time_periods)

# 2. Run temporal clustering analysis
cat("2. Running temporal clustering analysis...\n")
temporal_result <- moneca_temporal(
  matrix_list = quarterly_data,
  window_size = 4,  # Use 4 quarters (1 year) as window
  segment.levels = 3,
  stability_method = "hungarian",
  aggregation_method = "mean",
  verbose = FALSE
)

cat(sprintf("   Created %d temporal windows\n", length(temporal_result$windows)))
cat(sprintf("   Identified %d unique segments across time\n",
           length(unique(unlist(lapply(temporal_result$stable_labels, 
                                     function(x) unique(x$stable_label)))))))

# 3. Analyze temporal stability
cat("3. Analyzing temporal stability...\n")
stability_analysis <- temporal_stability_analysis(
  temporal_result,
  stability_threshold = 0.7,
  verbose = FALSE
)

# Print summary
print(temporal_result)
cat("\n")
print(stability_analysis)

# 4. Export results for further analysis
cat("\n4. Exporting results...\n")

# Export node trajectories
trajectories <- export_node_trajectories(stability_analysis)
cat(sprintf("   Exported %d node trajectories\n", nrow(trajectories)))

# Show example trajectories
cat("\n   Sample trajectories:\n")
sample_nodes <- head(unique(trajectories$node), 3)
for (node in sample_nodes) {
  node_traj <- trajectories[trajectories$node == node, ]
  segments <- paste(unique(node_traj$segment), collapse = " -> ")
  volatility <- round(node_traj$volatility[1], 3)
  cat(sprintf("     %s: %s (volatility: %.3f)\n", node, segments, volatility))
}

# 5. Key insights
cat("\n5. Key findings:\n")
cat(sprintf("   - Overall stability: %.1f%%\n", 
           stability_analysis$overall_stability * 100))
cat(sprintf("   - Stable core nodes: %d/%d (%.1f%%)\n",
           length(stability_analysis$stable_core),
           stability_analysis$parameters$n_nodes,
           100 * length(stability_analysis$stable_core) / stability_analysis$parameters$n_nodes))
cat(sprintf("   - Volatile nodes: %d/%d (%.1f%%)\n",
           length(stability_analysis$volatile_nodes),
           stability_analysis$parameters$n_nodes,
           100 * length(stability_analysis$volatile_nodes) / stability_analysis$parameters$n_nodes))

if (length(stability_analysis$change_points) > 0) {
  cat(sprintf("   - Change points detected at windows: %s\n",
             paste(stability_analysis$change_points, collapse = ", ")))
} else {
  cat("   - No significant structural changes detected\n")
}

# 6. Demonstrate different stability methods
cat("\n6. Comparing stability methods...\n")
methods <- c("hungarian", "jaccard", "overlap")

method_comparison <- list()
for (method in methods) {
  cat(sprintf("   Testing %s method...", method))
  
  result <- moneca_temporal(
    matrix_list = quarterly_data[1:5],  # Use subset for speed
    window_size = 3,
    stability_method = method,
    verbose = FALSE
  )
  
  n_unique_segments <- length(unique(unlist(lapply(result$stable_labels,
                                                  function(x) unique(x$stable_label)))))
  method_comparison[[method]] <- n_unique_segments
  cat(sprintf(" %d segments\n", n_unique_segments))
}

cat("\n   Method comparison:\n")
for (method in names(method_comparison)) {
  cat(sprintf("     %s: %d segments\n", method, method_comparison[[method]]))
}

cat("\n✓ Temporal analysis complete!\n")
cat("\nNext steps:\n")
cat("- Use plot_temporal_segments() for visualization\n")
cat("- Use plot_temporal_stability() for stability metrics visualization\n")
cat("- Analyze specific transitions of interest\n") 
cat("- Compare stability across different time periods\n")
cat("- Export results for further statistical analysis\n")