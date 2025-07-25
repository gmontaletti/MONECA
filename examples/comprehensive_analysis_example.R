# Example: Comprehensive MONECA Analysis
# This script demonstrates the new moneca_comprehensive_analysis() function

library(MONECA)

# Generate synthetic mobility data with meaningful structure
set.seed(123)
mobility_data <- generate_mobility_data(
  n_classes = 8,
  n_total = 15000,
  class_names = c("Upper", "Upper-Middle", "Professional", "Middle", 
                  "Lower-Middle", "Working", "Lower", "Underclass"),
  immobility_strength = 0.5,
  class_clustering = 0.4
)

# Run MONECA analysis with 4 levels
seg <- moneca(mobility_data, segment.levels = 4)

# Perform comprehensive analysis
analysis <- moneca_comprehensive_analysis(seg, plot_level = 3)

# View the summary report (as data frame)
print(analysis$summary_report)

# Display the network plot
print(analysis$level_plot)

# Examine metrics comparison across levels
cat("\nGraph Metrics Comparison:\n")
print(analysis$metrics_comparison)

# Look at centrality metrics for the highest level
cat("\nTop Central Segments:\n")
print(head(analysis$centrality_report[, c("segment", "strength_total", "betweenness", "pagerank")]))

# Examine mobility patterns
cat("\nMobility Patterns:\n")
print(analysis$mobility_report[, c("segment", "mobility_rate", "net_flow")])

# Filter the summary report by category
cat("\nGraph Metrics from Summary:\n")
graph_metrics <- analysis$summary_report[analysis$summary_report$category == "Graph Metrics", ]
print(graph_metrics)

# Save results to a directory (optional)
# analysis_with_output <- moneca_comprehensive_analysis(seg, output_dir = "moneca_results")