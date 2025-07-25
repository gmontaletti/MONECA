# Example: Segment-Level Mobility Analysis with MONECA
# This script demonstrates the new segment_mobility_analysis() function

library(MONECA)

# Generate synthetic mobility data with meaningful structure
set.seed(123)
mobility_data <- generate_mobility_data(
  n_classes = 8,
  n_total = 15000,
  class_names = c("Upper_Class", "Professional", "Manager", "Skilled_Worker", 
                  "Service_Worker", "Manual_Worker", "Lower_Worker", "Unemployed"),
  immobility_strength = 0.6,
  class_clustering = 0.3
)

# Run MONECA analysis with 4 levels
seg <- moneca(mobility_data, segment.levels = 4)

# Get enhanced membership information
enhanced <- segment.membership.enhanced(seg)

# Display enhanced membership structure
cat("Enhanced Membership Structure:\n")
print(head(enhanced))

# Perform segment-level analysis with automatic level selection
analysis <- segment_mobility_analysis(seg, enhanced)

# Display analysis results
cat("\n=== SEGMENT-LEVEL ANALYSIS RESULTS ===\n")
print(analysis)

# Examine specific components
cat("\n=== SEGMENT MATRIX ===\n")
print(analysis$segment_matrix)

cat("\n=== MOBILITY METRICS ===\n")
print(analysis$mobility_metrics)

cat("\n=== CENTRALITY METRICS ===\n") 
print(analysis$centrality_metrics)

cat("\n=== METADATA ===\n")
print(analysis$metadata)

# Display the network plot
cat("\n=== SEGMENT PLOT ===\n")
print(analysis$segment_plot)

# Test different visualization options
cat("\n=== CUSTOM VISUALIZATION EXAMPLES ===\n")

# Example 1: Color by net flow, stress layout
analysis_netflow <- segment_mobility_analysis(seg, enhanced, 
                                             plot_layout = "stress",
                                             plot_node_color = "net_flow",
                                             node_size_metric = "mobility_rate")

cat("Net flow visualization created\n")

# Example 2: Specific level analysis
analysis_level2 <- segment_mobility_analysis(seg, enhanced, 
                                            level_selection = 2,
                                            plot_node_color = "centrality")

cat("Level 2 analysis completed\n")
cat("Level 2 has", analysis_level2$metadata$n_segments, "segments\n")

# Example 3: Maximum level analysis  
analysis_max <- segment_mobility_analysis(seg, enhanced, 
                                         level_selection = "max",
                                         plot_node_color = "segment")

cat("Maximum level analysis completed\n")
cat("Max level is", analysis_max$metadata$analysis_level, "with", 
    analysis_max$metadata$n_segments, "segments\n")

# Summary comparison
cat("\n=== LEVEL COMPARISON SUMMARY ===\n")
cat("Auto level:", analysis$metadata$analysis_level, "- Segments:", analysis$metadata$n_segments, "\n")
cat("Level 2:", analysis_level2$metadata$analysis_level, "- Segments:", analysis_level2$metadata$n_segments, "\n")
cat("Max level:", analysis_max$metadata$analysis_level, "- Segments:", analysis_max$metadata$n_segments, "\n")

cat("\nSegment analysis examples completed successfully!\n")