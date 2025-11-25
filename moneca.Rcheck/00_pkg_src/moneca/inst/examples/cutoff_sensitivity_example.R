# Example: Cut-off Sensitivity Analysis with Strength-based Metrics
# This example demonstrates the new functionality for exploring the relationship
# between cut-off values and weighted density in moneca_fast

library(moneca)

# Generate synthetic mobility data
set.seed(123)
mx <- generate_mobility_data(n_classes = 8, 
                             immobility_strength = 0.6,
                             class_clustering = 0.3,
                             noise_level = 0.1)

cat("Generated mobility matrix:", nrow(mx) - 1, "x", ncol(mx) - 1, "\n")

# 1. Analyze the RR (Relative Risk) distribution
cat("\n1. Analyzing RR Distribution\n")
cat("=============================\n")
rr_dist <- analyze_rr_distribution(mx)
print(rr_dist)

# 2. Run comprehensive cut-off sensitivity analysis
cat("\n2. Cut-off Sensitivity Analysis\n")
cat("================================\n")
sensitivity <- cutoff_sensitivity_analysis(mx, 
                                           n_points = 20,
                                           verbose = TRUE)

# Show key results
cat("\nKey sensitivity results:\n")
print(head(sensitivity[, c("cutoff", "n_edges", "mean_strength", "strength_ratio", "modularity")], 10))

# 3. Find optimal cut-off using different criteria
cat("\n3. Finding Optimal Cut-off\n")
cat("===========================\n")

optimal_elbow <- find_optimal_cutoff(mx, criterion = "elbow", verbose = TRUE)
print(optimal_elbow)

optimal_balance <- find_optimal_cutoff(mx, criterion = "balance", verbose = FALSE)
cat("\nBalanced approach optimal cut-off:", optimal_balance$optimal_cutoff, "\n")

optimal_mod <- find_optimal_cutoff(mx, criterion = "modularity", verbose = FALSE)
cat("Modularity-based optimal cut-off:", optimal_mod$optimal_cutoff, "\n")

# 4. Compare moneca_fast results with old vs new density calculation
cat("\n4. Comparing Results\n")
cat("====================\n")

# Run with optimal cut-off
optimal_co <- optimal_elbow$optimal_cutoff
seg_optimal <- moneca_fast(mx, cut.off = optimal_co, segment.levels = 3, progress = FALSE)

cat("Results with optimal cut-off (", round(optimal_co, 3), "):\n", sep = "")
cat("  Number of segments at level 2:", length(seg_optimal$segment.list[[2]]), "\n")
cat("  Number of segments at level 3:", length(seg_optimal$segment.list[[3]]), "\n")

# Compare with default cut-off
seg_default <- moneca_fast(mx, cut.off = 1, segment.levels = 3, progress = FALSE)
cat("\nResults with default cut-off (1.0):\n")
cat("  Number of segments at level 2:", length(seg_default$segment.list[[2]]), "\n")
cat("  Number of segments at level 3:", length(seg_default$segment.list[[3]]), "\n")

# 5. Demonstrate strength vs edge density difference
cat("\n5. Strength vs Edge Density\n")
cat("============================\n")

# Get weight matrix at optimal cut-off
weight_mat <- weight.matrix(mx, cut.off = optimal_co)
weight_mat[is.na(weight_mat)] <- 0

# Create graph
g <- moneca_graph_from_adjacency(weight_mat, mode = "undirected", 
                                 weighted = TRUE, diag = FALSE)

# Calculate both densities
edge_density <- igraph::edge_density(g)
strength_density <- calculate_strength_density(g, method = "ratio")

cat("At optimal cut-off (", round(optimal_co, 3), "):\n", sep = "")
cat("  Edge density (unweighted):", round(edge_density, 4), "\n")
cat("  Strength density (weighted):", round(strength_density, 4), "\n")
cat("  Difference:", round(abs(edge_density - strength_density), 4), "\n")

# 6. Create visualizations (if ggplot2 is available)
if (requireNamespace("ggplot2", quietly = TRUE)) {
  cat("\n6. Creating Visualizations\n")
  cat("==========================\n")
  
  # Plot RR distribution
  p1 <- plot_rr_distribution(mx, cutoff_values = c(optimal_co, 1, 1.5))
  
  # Plot sensitivity analysis
  p2 <- plot_cutoff_analysis(sensitivity, optimal_cutoff = optimal_co)
  
  # Plot strength comparison
  p3 <- plot_strength_comparison(mx, cutoff_values = c(0.5, optimal_co, 1, 1.5))
  
  # Plot elbow curve
  p4 <- plot_elbow_curve(sensitivity, optimal_cutoff = optimal_co)
  
  cat("Created 4 visualization plots:\n")
  cat("  - RR distribution with cut-off thresholds\n")
  cat("  - Cut-off sensitivity analysis\n")
  cat("  - Strength comparison across cut-offs\n")
  cat("  - Elbow curve for optimal cut-off\n")
  
  # Note: In interactive use, you would display these plots:
  # print(p1)
  # print(p2)
  # print(p3)
  # print(p4)
  
} else {
  cat("\n6. Visualizations\n")
  cat("=================\n")
  cat("Install ggplot2 to see visualizations:\n")
  cat("  install.packages('ggplot2')\n")
}

# 7. Summary and recommendations
cat("\n7. Summary and Recommendations\n")
cat("===============================\n")
cat("Key findings from the analysis:\n\n")

cat("1. RR distribution range: [", round(rr_dist$range[1], 3), ", ", 
    round(rr_dist$range[2], 3), "]\n", sep = "")

cat("2. Optimal cut-off: ", round(optimal_co, 3), 
    " (vs default 1.0)\n", sep = "")

cat("3. At optimal cut-off:\n")
opt_idx <- which.min(abs(sensitivity$cutoff - optimal_co))
opt_metrics <- sensitivity[opt_idx, ]
cat("   - Retains ", round(opt_metrics$edge_retention * 100, 1), 
    "% of potential edges\n", sep = "")
cat("   - Strength concentration: ", round(opt_metrics$strength_ratio, 3), "\n", sep = "")
cat("   - Network modularity: ", round(opt_metrics$modularity, 3), "\n", sep = "")

if (optimal_co != 1) {
  if (optimal_co > 1) {
    cat("4. Recommendation: Consider using a higher cut-off (", 
        round(optimal_co, 2), ") for this dataset\n", sep = "")
    cat("   This will focus on stronger mobility connections and may improve segmentation.\n")
  } else {
    cat("4. Recommendation: Consider using a lower cut-off (", 
        round(optimal_co, 2), ") for this dataset\n", sep = "")
    cat("   This will capture more mobility patterns and provide richer segmentation.\n")
  }
} else {
  cat("4. The default cut-off (1.0) appears to be optimal for this dataset.\n")
}

cat("\nImportant: This analysis now uses strength-based density that properly\n")
cat("accounts for the weighted nature of the mobility network, rather than\n")
cat("simply counting edges. This provides more meaningful threshold decisions.\n")