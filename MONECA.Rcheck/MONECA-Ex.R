pkgname <- "MONECA"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('MONECA')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("ego.plot")
### * ego.plot

flush(stderr()); flush(stdout())

### Name: ego.plot
### Title: Legacy Ego Network Visualization
### Aliases: ego.plot

### ** Examples

## Not run: 
##D # Requires legacy data and eliter package
##D data(occupations)
##D ego.plot(mob.seg, mob.mat, id = 2)
##D 
##D # Customized ego plot
##D ego.plot(mob.seg, mob.mat, 
##D          id = 3,
##D          edge.size = 1.2,
##D          color.scheme = "Blues",
##D          small.cell.reduction = 10)
## End(Not run)




cleanEx()
nameEx("generate_example_datasets")
### * generate_example_datasets

flush(stderr()); flush(stdout())

### Name: generate_example_datasets
### Title: Generate Predefined Example Datasets
### Aliases: generate_example_datasets

### ** Examples

# Load all example datasets
examples <- generate_example_datasets()
names(examples)

# Examine the simple example
print(examples$simple)

# Compare different mobility regimes
rigid_seg <- moneca(examples$rigid, segment.levels = 3)
fluid_seg <- moneca(examples$fluid, segment.levels = 3)

# Visualize different structures
## Not run: 
##D plot_moneca_ggraph(rigid_seg, title = "Rigid Class Structure")
##D plot_moneca_ggraph(fluid_seg, title = "Fluid Mobility Regime")
##D 
##D # Create comparative stair plots
##D stair_rigid <- plot_stair_ggraph(rigid_seg)
##D stair_fluid <- plot_stair_ggraph(fluid_seg)
## End(Not run)

# Analyze segmentation quality across examples
for (name in names(examples)) {
  seg <- moneca(examples[[name]], segment.levels = 2)
  cat("Dataset:", name, "- Segments:", length(seg$segment.list[[2]]), "\n")
}




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

# Use in MONECA analysis
seg <- moneca(test_data, segment.levels = 3)
plot_moneca_ggraph(seg, title = "Synthetic Mobility Analysis")




cleanEx()
nameEx("gg.moneca")
### * gg.moneca

flush(stderr()); flush(stdout())

### Name: gg.moneca
### Title: Legacy ggplot2 Visualization for MONECA Objects
### Aliases: gg.moneca

### ** Examples

## Not run: 
##D # Requires eliter package
##D data(occupations)
##D gg.moneca(mob.seg)
##D 
##D # Custom styling
##D gg.moneca(mob.seg, 
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

# Run MONECA analysis
seg <- moneca(mobility_data, segment.levels = 3)
print(seg)

# Examine segment membership
membership <- segment.membership(seg)
print(membership)

# Visualize with modern plotting
## Not run: 
##D plot_moneca_ggraph(seg, node_color = "segment", title = "MONECA Clustering")
## End(Not run)




cleanEx()
nameEx("occupations")
### * occupations

flush(stderr()); flush(stdout())

### Name: occupations
### Title: Occupational mobility
### Aliases: occupations

### ** Examples

data(occupations)





cleanEx()
nameEx("plot_ego_ggraph")
### * plot_ego_ggraph

flush(stderr()); flush(stdout())

### Name: plot_ego_ggraph
### Title: Ego Network Visualization with ggraph
### Aliases: plot_ego_ggraph

### ** Examples

# Generate synthetic data and run MONECA
mobility_data <- generate_mobility_data(n_classes = 6, seed = 123)
seg <- moneca(mobility_data, segment.levels = 3)

# Ego network for the middle category (index 3)
plot_ego_ggraph(seg, mobility_data, ego_id = 3, 
                title = "Mobility from Middle Class")

# Ego network using category name (if available)
if (!is.null(rownames(mobility_data))) {
  plot_ego_ggraph(seg, mobility_data, ego_id = rownames(mobility_data)[1])
}

# Customized ego plot
plot_ego_ggraph(seg, mobility_data, 
                ego_id = 2,
                layout = "fr",
                highlight_color = "orange",
                flow_color = "plasma",
                edge_width_range = c(1, 5),
                title = "Professional Class Mobility")




cleanEx()
nameEx("plot_moneca_ggraph")
### * plot_moneca_ggraph

flush(stderr()); flush(stdout())

### Name: plot_moneca_ggraph
### Title: Modern Network Visualization for MONECA Results
### Aliases: plot_moneca_ggraph

### ** Examples

# Generate synthetic data and run MONECA
mobility_data <- generate_mobility_data(n_classes = 6, seed = 123)
seg <- moneca(mobility_data, segment.levels = 3)

# Basic network plot
plot_moneca_ggraph(seg)

# Customized plot with different aesthetics
plot_moneca_ggraph(seg,
  layout = "stress",
  node_color = "mobility",
  edge_width = "weight",
  color_palette = "Spectral",
  title = "Social Mobility Network",
  show_segments = FALSE
)

# Plot with custom node sizes and colors
custom_plot <- plot_moneca_ggraph(seg,
  node_size = c(8, 6, 10, 4, 7, 5),
  node_color = "red",
  edge_color = "darkblue",
  theme_style = "minimal"
)

# Further customize with ggplot2
custom_plot + 
  ggplot2::labs(subtitle = "Custom subtitle") +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16))




cleanEx()
nameEx("plot_stair_ggraph")
### * plot_stair_ggraph

flush(stderr()); flush(stdout())

### Name: plot_stair_ggraph
### Title: Multi-Level Segmentation Visualization (Stair Plot)
### Aliases: plot_stair_ggraph

### ** Examples

# Generate synthetic data and run MONECA
mobility_data <- generate_mobility_data(n_classes = 6, seed = 123)
seg <- moneca(mobility_data, segment.levels = 4)

# Basic stair plot
stair_plots <- plot_stair_ggraph(seg)

# Customized stair plot with specific levels
custom_stair <- plot_stair_ggraph(seg, 
                                 levels = c(2, 3),
                                 layout = "stress",
                                 ncol = 1,
                                 node_color = "mobility")

# Return individual plots for further customization
plot_list <- plot_stair_ggraph(seg, combine_plots = FALSE)
# Modify individual plots
plot_list[[1]] <- plot_list[[1]] + ggplot2::labs(subtitle = "Level 2")

## Not run: 
##D # Display the combined plot
##D print(stair_plots)
##D 
##D # Save individual plots
##D for (i in seq_along(plot_list)) {
##D   ggplot2::ggsave(paste0("level_", i, ".png"), plot_list[[i]])
##D }
## End(Not run)




cleanEx()
nameEx("print.moneca")
### * print.moneca

flush(stderr()); flush(stdout())

### Name: print.moneca
### Title: Print Method for MONECA Objects
### Aliases: print.moneca

### ** Examples

# Generate data and run analysis
mobility_data <- generate_mobility_data(n_classes = 6, seed = 42)
seg <- moneca(mobility_data, segment.levels = 3)

# Print comprehensive summary
print(seg)

# The summary includes mobility rates, network statistics, and
# segmentation quality measures for each hierarchical level




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
nameEx("stair.plot")
### * stair.plot

flush(stderr()); flush(stdout())

### Name: stair.plot
### Title: Legacy Multi-Level Stair Plot
### Aliases: stair.plot

### ** Examples

## Not run: 
##D # Requires legacy data and eliter package
##D data(occupations)
##D plots <- stair.plot(mob.seg)
##D plots[[2]]  # Display second level
##D 
##D # Customized stair plot
##D custom_stairs <- stair.plot(mob.seg,
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
