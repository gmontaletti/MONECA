## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path = "figures/",
  warning = FALSE,
  message = FALSE
)

# Check if required packages are available
required_packages <- c("ggplot2", "ggraph", "igraph", "dplyr", "tidygraph")
missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]

if (length(missing_packages) > 0) {
  knitr::opts_chunk$set(eval = FALSE)
  message("Some required packages are missing: ", paste(missing_packages, collapse = ", "))
  message("Code examples will not be evaluated.")
}

## ----installation, eval=FALSE-------------------------------------------------
# # Install from GitHub (when available)
# # devtools::install_github("antongrau/MONECA")
# 
# # Load the package
# library(MONECA)
# 
# # Load additional packages for visualization
# library(ggplot2)
# library(ggraph)

## ----load_package, echo=FALSE-------------------------------------------------
# For vignette building
library(MONECA)

## ----synthetic_basic----------------------------------------------------------
# Generate a simple 5-class mobility table
basic_data <- generate_mobility_data(
  n_classes = 5,
  n_total = 2000,
  immobility_strength = 0.7,
  class_clustering = 0.2,
  class_names = c("Upper", "Upper-Mid", "Middle", "Lower-Mid", "Working"),
  seed = 42
)

# View the structure
print(basic_data)

## ----synthetic_scenarios------------------------------------------------------
# Get multiple example datasets
examples <- generate_example_datasets()
names(examples)

# Examine the rigid class system
print("Rigid class system:")
print(examples$rigid)

# Examine the fluid (highly mobile) system
print("Fluid mobility system:")
print(examples$fluid)

## ----basic_analysis-----------------------------------------------------------
# Run MONECA on the basic dataset
seg <- moneca(basic_data, segment.levels = 3)

# View the results
print(seg)

## ----understand_results-------------------------------------------------------
# View segment membership
membership <- segment.membership(seg)
print(membership)

# View the aggregated matrix for level 2
print("Level 2 aggregated matrix:")
print(seg$mat.list[[2]])

## ----plot_basic, eval=TRUE----------------------------------------------------
# Create a basic network plot
p1 <- plot_moneca_ggraph(
  seg,
  title = "Basic MONECA Network",
  node_color = "segment",
  node_size = "total"
)

print(p1)

## ----plot_custom, eval=TRUE---------------------------------------------------
# Create a customized plot with different layout and styling
p2 <- plot_moneca_ggraph(
  seg,
  layout = "kk",  # Kamada-Kawai layout
  node_color = "segment",
  edge_width = "weight",
  color_palette = "Spectral",
  theme_style = "minimal",
  title = "Customized MONECA Visualization",
  show_labels = TRUE
)

print(p2)

## ----ego_plot, eval=TRUE------------------------------------------------------
# Create ego network for the middle class (position 3)
p3 <- plot_ego_ggraph(
  seg,
  basic_data,
  ego_id = 3,
  title = "Mobility from Middle Class",
  highlight_color = "red"
)

print(p3)

## ----stair_plot, eval=TRUE----------------------------------------------------
# Create stair plots showing different segmentation levels
stair_plots <- plot_stair_ggraph(seg, levels = 2:3)

# Display individual plots
print(stair_plots$`Level 2`)
print(stair_plots$`Level 3`)

## ----compare_regimes----------------------------------------------------------
# Analyze the rigid class system
rigid_seg <- moneca(examples$rigid, segment.levels = 3)

# Analyze the fluid mobility system  
fluid_seg <- moneca(examples$fluid, segment.levels = 3)

# Compare segment memberships
print("Rigid system segments:")
print(segment.membership(rigid_seg))

print("Fluid system segments:")
print(segment.membership(fluid_seg))

## ----compare_plots, eval=TRUE-------------------------------------------------
# Plot rigid system
p_rigid <- plot_moneca_ggraph(
  rigid_seg,
  title = "Rigid Class System",
  node_color = "segment",
  color_palette = "Reds"
)

# Plot fluid system
p_fluid <- plot_moneca_ggraph(
  fluid_seg,
  title = "Fluid Mobility System", 
  node_color = "segment",
  color_palette = "Blues"
)

print(p_rigid)
print(p_fluid)

## ----quality_assessment-------------------------------------------------------
# Assess the quality of segmentation
# (Note: this function may need to be implemented)
# quality_metrics <- segment.quality(seg)
# print(quality_metrics)

# Alternative: examine within vs. between segment mobility
level2_matrix <- seg$mat.list[[2]]
print("Level 2 mobility matrix (aggregated):")
print(level2_matrix)

# Calculate internal mobility rates
internal_mobility <- diag(level2_matrix[-nrow(level2_matrix), -ncol(level2_matrix)])
total_mobility <- level2_matrix[-nrow(level2_matrix), ncol(level2_matrix)]
immobility_rates <- internal_mobility / total_mobility

print("Immobility rates by segment:")
print(immobility_rates)

## ----edge_analysis------------------------------------------------------------
# Examine edge weights between segments
edges <- segment.edges(seg, level = 1)
print("Edge weights matrix:")
print(edges)

# Focus on strongest connections
strong_edges <- edges
strong_edges[strong_edges < 2] <- NA
print("Strong connections (relative risk > 2):")
print(strong_edges)

## ----data_format--------------------------------------------------------------
# Your data should be a matrix with:
# - Rows: origin positions
# - Columns: destination positions  
# - Last row: column totals
# - Last column: row totals
# - Bottom-right cell: grand total

# Example of properly formatted data structure:
example_structure <- matrix(c(
  100, 20, 10, 130,  # Origin 1: 100 stay, 20 to pos 2, 10 to pos 3, total 130
  15, 200, 25, 240,  # Origin 2: 15 to pos 1, 200 stay, 25 to pos 3, total 240  
  10, 30, 180, 220,  # Origin 3: 10 to pos 1, 30 to pos 2, 180 stay, total 220
  125, 250, 215, 590 # Totals: 125 end in pos 1, 250 in pos 2, 215 in pos 3, grand total 590
), nrow = 4, ncol = 4)

rownames(example_structure) <- c("Class_A", "Class_B", "Class_C", "Total")
colnames(example_structure) <- c("Class_A", "Class_B", "Class_C", "Total")

print("Properly formatted mobility matrix:")
print(example_structure)

## ----data_validation----------------------------------------------------------
# Check if your data is properly formatted
validate_mobility_matrix <- function(mat) {
  n <- nrow(mat) - 1
  
  # Check row totals
  calculated_row_totals <- rowSums(mat[1:n, 1:n])
  actual_row_totals <- mat[1:n, n + 1]
  
  # Check column totals  
  calculated_col_totals <- colSums(mat[1:n, 1:n])
  actual_col_totals <- mat[n + 1, 1:n]
  
  # Check grand total
  calculated_grand_total <- sum(mat[1:n, 1:n])
  actual_grand_total <- mat[n + 1, n + 1]
  
  list(
    row_totals_match = all.equal(calculated_row_totals, actual_row_totals),
    col_totals_match = all.equal(calculated_col_totals, actual_col_totals),
    grand_total_matches = all.equal(calculated_grand_total, actual_grand_total)
  )
}

# Validate our example
validation <- validate_mobility_matrix(example_structure)
print(validation)

## ----session_info-------------------------------------------------------------
# Session information for reproducibility
sessionInfo()

