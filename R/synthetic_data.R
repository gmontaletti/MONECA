#' Generate synthetic mobility data
#'
#' Creates synthetic social mobility data for demonstrations and testing.
#' The function generates a mobility table with realistic patterns including
#' diagonal dominance (immobility) and class-based clustering.
#'
#' @param n_classes Integer. Number of social classes/occupations (default: 10)
#' @param n_total Integer. Total number of individuals in the sample (default: 10000)
#' @param immobility_strength Numeric. Strength of diagonal immobility (0-1, default: 0.6)
#' @param class_clustering Numeric. Tendency for adjacent classes to exchange (0-1, default: 0.3)
#' @param noise_level Numeric. Amount of random mobility (0-1, default: 0.1)
#' @param class_names Character vector. Names for the classes (optional)
#' @param seed Integer. Random seed for reproducibility (optional)
#'
#' @return A mobility matrix with row and column totals in the format expected by MONECA
#'
#' @examples
#' # Basic synthetic data
#' mob_data <- generate_mobility_data()
#' print(mob_data)
#'
#' # Smaller example with custom parameters
#' small_data <- generate_mobility_data(
#'   n_classes = 5,
#'   n_total = 1000,
#'   immobility_strength = 0.7,
#'   class_clustering = 0.2
#' )
#'
#' # With custom class names
#' professional_data <- generate_mobility_data(
#'   n_classes = 6,
#'   class_names = c("Upper", "Upper-Middle", "Middle", "Lower-Middle", "Working", "Lower"),
#'   seed = 123
#' )
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
  
  # Add row and column totals (MONECA format requirement)
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

#' Generate example datasets
#'
#' Creates several example mobility datasets with different characteristics
#' for use in examples and vignettes.
#'
#' @return A list containing different example datasets
#' @export
#'
#' @examples
#' examples <- generate_example_datasets()
#' names(examples)
#' 
#' # Run MONECA on simple example
#' \dontrun{
#' seg <- moneca(examples$simple)
#' print(seg)
#' }
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