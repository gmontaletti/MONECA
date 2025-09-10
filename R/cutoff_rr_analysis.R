#' Cut-off and Relative Risk Analysis Functions
#'
#' Functions to analyze the relationship between cut-off values and relative risk
#' distributions in MONECA, using strength-based metrics for weighted graphs.
#'
#' @name cutoff_rr_analysis
#' @keywords internal
NULL

#' Analyze Relative Risk Distribution
#'
#' Analyzes the distribution of relative risk (RR) values in a mobility matrix
#' and suggests natural cut-off values based on the distribution.
#'
#' @param mx Mobility matrix with marginals in last row/column
#' @param small.cell.reduction Numeric value for small cell adjustment
#' @param symmetric Logical, whether to make the RR matrix symmetric
#'
#' @return List containing:
#'   - summary: Statistical summary of RR values
#'   - quantiles: Decile breakdown of RR distribution
#'   - density_curve: Density object for RR distribution
#'   - suggested_cutoffs: Named vector of suggested cut-off values
#'
#' @examples
#' # Generate synthetic mobility data
#' data <- generate_mobility_data(n_classes = 4, seed = 123)
#' 
#' # Analyze RR distribution
#' rr_analysis <- analyze_rr_distribution(data)
#' print(rr_analysis)
#' 
#' # Access suggested cut-offs
#' suggested_cutoffs <- rr_analysis$suggested_cutoffs
#' print(suggested_cutoffs)
#'
#' @export
analyze_rr_distribution <- function(mx, small.cell.reduction = 0, symmetric = TRUE) {
  # Calculate RR matrix without cut-off filtering
  rr_matrix <- weight.matrix(mx, cut.off = 0, 
                            small.cell.reduction = small.cell.reduction,
                            symmetric = symmetric)
  
  # Extract non-NA RR values
  rr_values <- rr_matrix[!is.na(rr_matrix) & rr_matrix > 0]
  
  if (length(rr_values) == 0) {
    stop("No valid RR values found in the matrix")
  }
  
  # Calculate distribution statistics
  result <- list(
    summary = summary(rr_values),
    quantiles = quantile(rr_values, probs = seq(0, 1, 0.1)),
    density_curve = density(rr_values),
    suggested_cutoffs = c(
      median = median(rr_values),
      mean = mean(rr_values),
      q75 = quantile(rr_values, 0.75, names = FALSE),
      q90 = quantile(rr_values, 0.90, names = FALSE),
      natural_break = find_natural_breaks(rr_values, n_breaks = 1)[1]
    ),
    n_values = length(rr_values),
    range = range(rr_values)
  )
  
  class(result) <- c("rr_distribution", "list")
  return(result)
}

#' Evaluate Strength Metrics at a Given Cut-off
#'
#' Calculates strength-based metrics for a mobility network after applying
#' a specific cut-off threshold to the relative risk matrix.
#'
#' @param mx Mobility matrix with marginals
#' @param cut.off Numeric threshold for RR values
#' @param small.cell.reduction Small cell adjustment parameter
#' @param symmetric Whether to symmetrize the RR matrix
#'
#' @return List of strength metrics including mean, max, CV, and Gini coefficient
#'
#' @examples
#' # Generate synthetic mobility data
#' data <- generate_mobility_data(n_classes = 4, seed = 123)
#' 
#' # Evaluate strength metrics at different cut-offs
#' metrics_1.0 <- evaluate_cutoff_strength(data, cut.off = 1.0)
#' metrics_1.5 <- evaluate_cutoff_strength(data, cut.off = 1.5)
#' 
#' # Compare edge counts and strength metrics
#' cat("Cut-off 1.0: ", metrics_1.0$n_edges, " edges\n")
#' cat("Cut-off 1.5: ", metrics_1.5$n_edges, " edges\n")
#'
#' @export
evaluate_cutoff_strength <- function(mx, cut.off = 1, 
                                    small.cell.reduction = 0, 
                                    symmetric = TRUE) {
  # Get RR matrix with cut.off applied
  rr_mat <- weight.matrix(mx, cut.off = cut.off, 
                         small.cell.reduction = small.cell.reduction,
                         symmetric = symmetric)
  
  # Replace NA with 0 for graph creation
  rr_mat[is.na(rr_mat)] <- 0
  
  # Create weighted graph
  g <- moneca_graph_from_adjacency(rr_mat, mode = "undirected", 
                                   weighted = TRUE, diag = FALSE)
  
  # Calculate strength metrics
  strengths <- igraph::strength(g, mode = "all")
  
  # Handle case with no edges
  if (all(strengths == 0)) {
    return(list(
      mean_strength = 0,
      max_strength = 0,
      min_strength = 0,
      total_strength = 0,
      strength_ratio = 0,
      strength_cv = 0,
      gini_coefficient = 0,
      n_edges = 0,
      n_isolates = length(strengths),
      avg_weight = 0,
      max_weight = 0
    ))
  }
  
  # Get edge weights
  edge_weights <- igraph::E(g)$weight
  
  # Calculate metrics
  result <- list(
    mean_strength = mean(strengths),
    max_strength = max(strengths),
    min_strength = min(strengths[strengths > 0]),
    total_strength = sum(strengths),
    strength_ratio = mean(strengths) / max(strengths),
    strength_cv = sd(strengths) / mean(strengths),
    gini_coefficient = calculate_gini(strengths),
    n_edges = igraph::ecount(g),
    n_isolates = sum(strengths == 0),
    avg_weight = mean(edge_weights),
    max_weight = max(edge_weights)
  )
  
  return(result)
}

#' Calculate Strength-Based Density
#'
#' Computes a weighted density metric based on node strengths rather than
#' simple edge counts. This provides a better measure for weighted networks.
#'
#' @param graph igraph object with edge weights
#' @param method Method for calculating density: "ratio", "normalized", or "cv"
#'
#' @return Numeric density value
#'
#' @export
calculate_strength_density <- function(graph, method = "ratio") {
  strengths <- igraph::strength(graph, mode = "all")
  
  if (length(strengths) == 0 || all(strengths == 0)) {
    return(0)
  }
  
  density <- switch(method,
    ratio = mean(strengths) / max(strengths),
    normalized = {
      # Normalize by maximum observed strength
      sum(strengths) / (length(strengths) * max(strengths))
    },
    cv = {
      # Use coefficient of variation (inverse for density)
      # Lower CV means more uniform = higher "density"
      1 / (1 + sd(strengths) / mean(strengths))
    },
    mean(strengths) / max(strengths)  # default
  )
  
  return(density)
}

#' Calculate Gini Coefficient
#'
#' Calculates the Gini coefficient for a vector of strength values,
#' measuring the inequality/concentration of the distribution.
#'
#' @param strengths Numeric vector of strength values
#'
#' @return Gini coefficient (0 = perfect equality, 1 = perfect inequality)
#'
#' @keywords internal
calculate_gini <- function(strengths) {
  if (length(strengths) == 0 || all(strengths == 0)) {
    return(0)
  }
  
  # Remove zeros and sort
  x <- sort(strengths[strengths > 0])
  n <- length(x)
  
  if (n <= 1) return(0)
  
  # Calculate Gini using the formula
  index <- 1:n
  gini <- 2 * sum(index * x) / (n * sum(x)) - (n + 1) / n
  
  return(gini)
}

#' Find Natural Breaks in RR Distribution
#'
#' Identifies natural breaks in the RR value distribution using the
#' Jenks natural breaks optimization method.
#'
#' @param rr_values Numeric vector of RR values
#' @param n_breaks Number of breaks to identify
#'
#' @return Numeric vector of break points
#'
#' @keywords internal
find_natural_breaks <- function(rr_values, n_breaks = 3) {
  if (length(rr_values) < n_breaks + 1) {
    return(quantile(rr_values, probs = seq(0, 1, length.out = n_breaks + 2)[-c(1, n_breaks + 2)]))
  }
  
  # Simple implementation using quantile-based clustering
  # For a more sophisticated approach, could use classInt::classIntervals
  # with style = "jenks"
  
  # Use k-means clustering on log-transformed values
  if (all(rr_values > 0)) {
    log_rr <- log(rr_values)
    km <- kmeans(log_rr, centers = n_breaks + 1, nstart = 25)
    
    # Find break points between clusters
    breaks <- numeric(n_breaks)
    sorted_centers <- sort(km$centers[,1])
    
    for (i in 1:n_breaks) {
      # Find values between clusters i and i+1
      cluster_i <- which(km$cluster == which(km$centers[,1] == sorted_centers[i]))
      cluster_next <- which(km$cluster == which(km$centers[,1] == sorted_centers[i+1]))
      
      if (length(cluster_i) > 0 && length(cluster_next) > 0) {
        breaks[i] <- exp((max(log_rr[cluster_i]) + min(log_rr[cluster_next])) / 2)
      } else {
        breaks[i] <- exp(sorted_centers[i])
      }
    }
    
    return(sort(breaks))
  } else {
    # Fallback to quantiles if values include zeros
    return(quantile(rr_values, probs = seq(0, 1, length.out = n_breaks + 2)[-c(1, n_breaks + 2)]))
  }
}

#' Find Elbow Point in Curve
#'
#' Identifies the elbow point in a curve using the perpendicular distance method.
#'
#' @param x Numeric vector of x values
#' @param y Numeric vector of y values
#'
#' @return x value at the elbow point
#'
#' @keywords internal
find_elbow_point <- function(x, y) {
  if (length(x) != length(y) || length(x) < 3) {
    stop("x and y must have the same length and at least 3 points")
  }
  
  # Normalize values to [0, 1] range
  x_norm <- (x - min(x)) / (max(x) - min(x))
  y_norm <- (y - min(y)) / (max(y) - min(y))
  
  # Calculate perpendicular distance from each point to the line
  # connecting first and last points
  n <- length(x_norm)
  distances <- numeric(n)
  
  # Line from first to last point
  x1 <- x_norm[1]; y1 <- y_norm[1]
  x2 <- x_norm[n]; y2 <- y_norm[n]
  
  for (i in 2:(n-1)) {
    # Perpendicular distance from point i to the line
    x0 <- x_norm[i]; y0 <- y_norm[i]
    
    # Distance formula
    num <- abs((y2 - y1) * x0 - (x2 - x1) * y0 + x2 * y1 - y2 * x1)
    den <- sqrt((y2 - y1)^2 + (x2 - x1)^2)
    
    distances[i] <- if (den > 0) num / den else 0
  }
  
  # Find point with maximum distance
  elbow_idx <- which.max(distances)
  
  return(x[elbow_idx])
}

#' Print RR Distribution Analysis
#'
#' @param x RR distribution object
#' @param ... Additional arguments
#'
#' @export
print.rr_distribution <- function(x, ...) {
  cat("Relative Risk Distribution Analysis\n")
  cat("=====================================\n")
  cat("Number of RR values:", x$n_values, "\n")
  cat("Range:", sprintf("%.3f to %.3f", x$range[1], x$range[2]), "\n\n")
  
  cat("Summary Statistics:\n")
  print(x$summary)
  
  cat("\nSuggested Cut-off Values:\n")
  for (name in names(x$suggested_cutoffs)) {
    cat(sprintf("  %s: %.3f\n", name, x$suggested_cutoffs[name]))
  }
  
  invisible(x)
}