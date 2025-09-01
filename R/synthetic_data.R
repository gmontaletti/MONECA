#' Generate Realistic Synthetic Mobility Data
#'
#' Creates synthetic social mobility tables with configurable patterns that mimic
#' real-world mobility structures. This function is essential for testing moneca
#' algorithms, creating examples, and understanding how different mobility patterns
#' affect segmentation results.
#'
#' @param n_classes Integer specifying the number of social classes or occupational
#'   categories. Must be at least 2. Default is 10.
#' @param n_total Integer specifying the total sample size (number of individuals).
#'   Must be at least as large as n_classes. Default is 10000.
#' @param immobility_strength Numeric value (0-1) controlling the strength of
#'   diagonal immobility (tendency to remain in origin class). Higher values create
#'   more rigid class structures. Default is 0.6.
#' @param class_clustering Numeric value (0-1) controlling the tendency for mobility
#'   between adjacent classes in the social hierarchy. Higher values create more
#'   gradual, short-distance mobility. Default is 0.3.
#' @param noise_level Numeric value (0-1) controlling the amount of random mobility
#'   between non-adjacent classes. Higher values create more chaotic mobility patterns.
#'   Default is 0.1.
#' @param class_names Character vector of names for the social classes. If NULL,
#'   classes are named "Class 1", "Class 2", etc. Length must equal n_classes.
#' @param seed Integer for random seed to ensure reproducible results. Default is NULL
#'   (no seed set).
#'
#' @return A square matrix in moneca format with:
#'   \describe{
#'     \item{Core matrix}{Upper-left (n_classes x n_classes) contains mobility flows}
#'     \item{Row margins}{Last column contains origin class totals}
#'     \item{Column margins}{Last row contains destination class totals}
#'     \item{Grand total}{Bottom-right cell contains total sample size}
#'   }
#'   The matrix includes row and column names for easy interpretation.
#'
#' @details
#' This function generates mobility data using a hierarchical model that reflects
#' common patterns in social mobility research:
#' 
#' \enumerate{
#'   \item \strong{Diagonal Dominance}: Most people remain in their origin class
#'   \item \strong{Distance Effects}: Mobility is more likely between adjacent classes
#'   \item \strong{Marginal Heterogeneity}: Class sizes vary realistically
#'   \item \strong{Random Component}: Some unpredictable mobility occurs
#' }
#' 
#' The generated data can be used to test how different mobility regimes affect
#' moneca segmentation and to create realistic examples for demonstrations.
#' 
#' Parameter combinations:
#' \itemize{
#'   \item High immobility + low clustering = rigid class boundaries
#'   \item Low immobility + high clustering = fluid but structured mobility
#'   \item High noise = chaotic mobility patterns
#'   \item Balanced parameters = realistic social mobility
#' }
#'
#' @examples
#' # Basic synthetic data with default parameters
#' basic_data <- generate_mobility_data()
#' print(basic_data[1:6, 1:6])  # Show first 5 classes plus totals
#' 
#' # Small example for quick testing
#' test_data <- generate_mobility_data(
#'   n_classes = 5,
#'   n_total = 1000,
#'   immobility_strength = 0.7,
#'   class_clustering = 0.2,
#'   seed = 42
#' )
#' 
#' # Professional class structure with custom names
#' professional_data <- generate_mobility_data(
#'   n_classes = 6,
#'   n_total = 5000,
#'   class_names = c("Upper", "Upper-Middle", "Middle", 
#'                   "Lower-Middle", "Working", "Lower"),
#'   immobility_strength = 0.5,
#'   class_clustering = 0.4,
#'   seed = 123
#' )
#' 
#' # Highly fluid mobility regime
#' fluid_data <- generate_mobility_data(
#'   n_classes = 8,
#'   immobility_strength = 0.3,
#'   class_clustering = 0.1,
#'   noise_level = 0.3
#' )
#' 
#' # Use in moneca analysis
#' seg <- moneca(test_data, segment.levels = 3)
#' plot_moneca_ggraph(seg, title = "Synthetic Mobility Analysis")
#' 
#' @seealso 
#' \code{\link{moneca}} for analyzing the generated data,
#' \code{\link{generate_example_datasets}} for predefined examples,
#' \code{\link{plot_moneca_ggraph}} for visualizing results
#' 
#' @export
generate_mobility_data <- function(n_classes = 10,
                                 n_total = 10000,
                                 immobility_strength = 0.6,
                                 class_clustering = 0.3,
                                 noise_level = 0.1,
                                 class_names = NULL,
                                 seed = NULL) {
  
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  # Validate parameters
  if (n_classes < 2) stop("n_classes must be at least 2")
  if (n_total < n_classes) stop("n_total must be at least as large as n_classes")
  if (immobility_strength < 0 || immobility_strength > 1) {
    stop("immobility_strength must be between 0 and 1")
  }
  if (class_clustering < 0 || class_clustering > 1) {
    stop("class_clustering must be between 0 and 1")
  }
  if (noise_level < 0 || noise_level > 1) {
    stop("noise_level must be between 0 and 1")
  }
  
  # Create class names if not provided
  if (is.null(class_names)) {
    class_names <- paste("Class", 1:n_classes)
  } else if (length(class_names) != n_classes) {
    stop("Length of class_names must equal n_classes")
  }
  
  # Initialize mobility matrix
  mobility_matrix <- matrix(0, nrow = n_classes, ncol = n_classes)
  
  # Generate marginal distributions (class sizes)
  # Use a slightly skewed distribution to make it more realistic
  class_probs <- rgamma(n_classes, shape = 2, rate = 1)
  class_probs <- class_probs / sum(class_probs)
  
  # Origin class sizes
  origin_sizes <- as.integer(n_total * class_probs)
  # Adjust for rounding
  origin_sizes[1] <- origin_sizes[1] + (n_total - sum(origin_sizes))
  
  # Generate mobility for each origin class
  for (i in 1:n_classes) {
    n_from_class <- origin_sizes[i]
    
    # Base probability for staying in same class
    probs <- rep(noise_level / n_classes, n_classes)
    
    # Increase probability of staying in same class
    probs[i] <- probs[i] + immobility_strength
    
    # Add clustering effect (mobility to adjacent classes)
    if (class_clustering > 0) {
      # Adjacent classes get higher probabilities
      if (i > 1) probs[i-1] <- probs[i-1] + class_clustering / 2
      if (i < n_classes) probs[i+1] <- probs[i+1] + class_clustering / 2
      
      # Second-order adjacent classes get smaller boost
      if (i > 2) probs[i-2] <- probs[i-2] + class_clustering / 4
      if (i < n_classes - 1) probs[i+2] <- probs[i+2] + class_clustering / 4
    }
    
    # Normalize probabilities
    probs <- probs / sum(probs)
    
    # Generate mobility from this class
    destinations <- sample(1:n_classes, n_from_class, prob = probs, replace = TRUE)
    mobility_counts <- table(factor(destinations, levels = 1:n_classes))
    mobility_matrix[i, ] <- as.integer(mobility_counts)
  }
  
  # Add row and column totals (moneca format requirement)
  row_totals <- rowSums(mobility_matrix)
  col_totals <- colSums(mobility_matrix)
  total_total <- sum(mobility_matrix)
  
  # Create final matrix with margins
  final_matrix <- matrix(0, nrow = n_classes + 1, ncol = n_classes + 1)
  final_matrix[1:n_classes, 1:n_classes] <- mobility_matrix
  final_matrix[1:n_classes, n_classes + 1] <- row_totals
  final_matrix[n_classes + 1, 1:n_classes] <- col_totals
  final_matrix[n_classes + 1, n_classes + 1] <- total_total
  
  # Add names
  all_names <- c(class_names, "Total")
  rownames(final_matrix) <- all_names
  colnames(final_matrix) <- all_names
  
  return(final_matrix)
}

#' Generate Predefined Example Datasets
#'
#' Creates a collection of predefined mobility datasets with different structural
#' characteristics. These examples are useful for tutorials, testing different
#' moneca parameters, and demonstrating various mobility regime types.
#'
#' @return A named list containing mobility matrices with different characteristics:
#'   \describe{
#'     \item{simple}{Small 4-class example with clear segmentation}
#'     \item{complex}{Larger 8-class example with hierarchical structure}
#'     \item{rigid}{High immobility example showing strong class boundaries}
#'     \item{fluid}{Low immobility example with extensive mobility}
#'     \item{polarized}{Example with strong upper/lower class divide}
#'     \item{gradual}{Example with smooth class transitions}
#'   }
#'   Each matrix is in standard moneca format with row/column totals.
#'
#' @details
#' This function provides ready-to-use examples that demonstrate different types
#' of social mobility structures:
#' 
#' \itemize{
#'   \item \strong{Simple}: Good for learning moneca basics
#'   \item \strong{Complex}: Tests algorithm performance on larger structures
#'   \item \strong{Rigid}: Shows segmentation in low-mobility societies
#'   \item \strong{Fluid}: Tests segmentation in high-mobility contexts
#'   \item \strong{Polarized}: Demonstrates class divide patterns
#'   \item \strong{Gradual}: Shows continuous mobility gradients
#' }
#' 
#' These examples are particularly useful for:
#' \itemize{
#'   \item Tutorial and teaching materials
#'   \item Comparing moneca parameters across mobility types
#'   \item Testing visualization functions
#'   \item Benchmarking algorithm performance
#' }
#'
#' @examples
#' # Load all example datasets
#' examples <- generate_example_datasets()
#' names(examples)
#' 
#' # Examine the simple example
#' print(examples$simple)
#' 
#' # Compare different mobility regimes
#' rigid_seg <- moneca(examples$rigid, segment.levels = 3)
#' fluid_seg <- moneca(examples$fluid, segment.levels = 3)
#' 
#' # Visualize different structures
#' \dontrun{
#' plot_moneca_ggraph(rigid_seg, title = "Rigid Class Structure")
#' plot_moneca_ggraph(fluid_seg, title = "Fluid Mobility Regime")
#' 
#' # Create comparative stair plots
#' stair_rigid <- plot_stair_ggraph(rigid_seg)
#' stair_fluid <- plot_stair_ggraph(fluid_seg)
#' }
#' 
#' # Analyze segmentation quality across examples
#' for (name in names(examples)) {
#'   seg <- moneca(examples[[name]], segment.levels = 2)
#'   cat("Dataset:", name, "- Segments:", length(seg$segment.list[[2]]), "\n")
#' }
#' 
#' @seealso 
#' \code{\link{generate_mobility_data}} for custom synthetic data,
#' \code{\link{moneca}} for analysis,
#' \code{\link{plot_moneca_ggraph}} for visualization
#' 
#' @export
generate_example_datasets <- function() {
  
  list(
    # Simple 5-class system
    simple = generate_mobility_data(
      n_classes = 5,
      n_total = 2000,
      immobility_strength = 0.7,
      class_clustering = 0.2,
      class_names = c("Upper", "Upper-Mid", "Middle", "Lower-Mid", "Working"),
      seed = 42
    ),
    
    # Complex 8-class system with strong clustering
    complex = generate_mobility_data(
      n_classes = 8,
      n_total = 5000,
      immobility_strength = 0.5,
      class_clustering = 0.4,
      noise_level = 0.1,
      class_names = paste("Occupation", 1:8),
      seed = 123
    ),
    
    # Highly mobile society
    fluid = generate_mobility_data(
      n_classes = 6,
      n_total = 3000,
      immobility_strength = 0.3,
      class_clustering = 0.1,
      noise_level = 0.6,
      class_names = c("Elite", "Professional", "Managerial", "Skilled", "Semi-skilled", "Unskilled"),
      seed = 456
    ),
    
    # Rigid class system
    rigid = generate_mobility_data(
      n_classes = 4,
      n_total = 1500,
      immobility_strength = 0.9,
      class_clustering = 0.05,
      noise_level = 0.05,
      class_names = c("Aristocracy", "Bourgeoisie", "Petite-Bourgeoisie", "Proletariat"),
      seed = 789
    )
  )
}