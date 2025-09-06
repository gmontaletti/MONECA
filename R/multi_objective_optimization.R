# ============================================================================
# MULTI-OBJECTIVE OPTIMIZATION FUNCTIONS FOR AUTO-TUNING
# ============================================================================

#' Evaluate Candidates using Pareto Frontier Analysis
#'
#' Multi-objective optimization approach balancing multiple criteria.
#'
#' @param mx Mobility matrix.
#' @param candidate_values Candidates to evaluate.
#' @param cut.off Edge threshold.
#' @param objectives Vector of objectives to optimize.
#' @param cache Cache object.
#' @param parallel_setup Parallel processing setup.
#' @param verbose Whether to print progress.
#' @return List with Pareto optimization results.
#' @keywords internal
evaluate_pareto_candidates <- function(mx, candidate_values, cut.off, objectives,
                                      cache, parallel_setup, verbose) {
  
  # Evaluate all candidates first
  base_results <- evaluate_candidates_optimized(
    mx, candidate_values, cut.off, "quality", 10,
    cache, parallel_setup, FALSE, verbose
  )
  
  # Extract objective values
  n_candidates <- length(candidate_values)
  objective_matrix <- matrix(0, nrow = n_candidates, ncol = length(objectives))
  colnames(objective_matrix) <- objectives
  
  for (i in seq_along(candidate_values)) {
    if ("quality" %in% objectives) {
      objective_matrix[i, "quality"] <- base_results$quality_metrics[[i]]$overall
    }
    if ("performance" %in% objectives) {
      # Performance is inverse of time (higher is better)
      time_val <- base_results$performance_metrics[[i]]$time
      if (!is.na(time_val) && time_val > 0) {
        objective_matrix[i, "performance"] <- 1 / time_val
      }
    }
    if ("stability" %in% objectives) {
      objective_matrix[i, "stability"] <- base_results$stability_scores[i]
    }
  }
  
  # Identify Pareto frontier
  pareto_frontier <- find_pareto_frontier(objective_matrix)
  
  # Add Pareto information to results
  base_results$pareto_frontier <- list(
    frontier_indices = pareto_frontier,
    objective_matrix = objective_matrix,
    objectives = objectives
  )
  
  return(base_results)
}

#' Find Pareto Frontier in Multi-Objective Space
#'
#' Identifies non-dominated solutions in multi-objective optimization using
#' vectorized operations for improved performance.
#'
#' @param objective_matrix Matrix where rows are candidates and columns are objectives.
#' @return Vector of indices representing Pareto frontier.
#' @keywords internal
find_pareto_frontier <- function(objective_matrix) {
  n_candidates <- nrow(objective_matrix)
  
  if (n_candidates <= 1) {
    return(seq_len(n_candidates))
  }
  
  # Use vectorized approach for better performance
  is_pareto <- logical(n_candidates)
  
  # For each candidate, check if it's dominated by any other
  for (i in seq_len(n_candidates)) {
    current_solution <- objective_matrix[i, ]
    
    # Check domination against all other solutions
    # A solution dominates if it's >= in all objectives and > in at least one
    other_solutions <- objective_matrix[-i, , drop = FALSE]
    
    # Vectorized comparison
    better_or_equal <- sweep(other_solutions, 2, current_solution, ">=")
    strictly_better <- sweep(other_solutions, 2, current_solution, ">")
    
    # Check if any solution dominates current one
    dominates <- apply(better_or_equal, 1, all) & apply(strictly_better, 1, any)
    
    is_pareto[i] <- !any(dominates)
  }
  
  return(which(is_pareto))
}

#' Find Pareto Frontier with Fast Non-Dominated Sorting
#'
#' Efficient implementation using the fast non-dominated sorting algorithm
#' from NSGA-II for large numbers of candidates.
#'
#' @param objective_matrix Matrix where rows are candidates and columns are objectives.
#' @return Vector of indices representing Pareto frontier.
#' @keywords internal
find_pareto_frontier_fast <- function(objective_matrix) {
  n_candidates <- nrow(objective_matrix)
  n_objectives <- ncol(objective_matrix)
  
  if (n_candidates <= 1) {
    return(seq_len(n_candidates))
  }
  
  # Initialize domination structures
  dominated_solutions <- vector("list", n_candidates)
  domination_count <- numeric(n_candidates)
  
  # Calculate domination relationships
  for (i in seq_len(n_candidates)) {
    dominated_solutions[[i]] <- integer(0)
    domination_count[i] <- 0
    
    for (j in seq_len(n_candidates)) {
      if (i != j) {
        # Check if i dominates j
        if (dominates(objective_matrix[i, ], objective_matrix[j, ])) {
          dominated_solutions[[i]] <- c(dominated_solutions[[i]], j)
        } else if (dominates(objective_matrix[j, ], objective_matrix[i, ])) {
          domination_count[i] <- domination_count[i] + 1
        }
      }
    }
  }
  
  # Find first front (Pareto frontier)
  first_front <- which(domination_count == 0)
  
  return(first_front)
}

#' Check if Solution A Dominates Solution B
#'
#' Helper function to check Pareto dominance.
#'
#' @param solution_a First solution vector.
#' @param solution_b Second solution vector.
#' @return Logical indicating if A dominates B.
#' @keywords internal
dominates <- function(solution_a, solution_b) {
  # A dominates B if A is >= B in all objectives and > B in at least one
  better_or_equal <- all(solution_a >= solution_b)
  strictly_better <- any(solution_a > solution_b)
  
  return(better_or_equal && strictly_better)
}

#' Select Optimal Solution from Pareto Frontier
#'
#' Chooses best solution from Pareto frontier based on user preferences using
#' weighted achievement scalarizing function.
#'
#' @param pareto_solutions Pareto frontier information.
#' @param performance_weight Weight for performance vs other objectives.
#' @param quality_weight Weight for quality objective.
#' @param stability_weight Weight for stability objective.
#' @return Index of selected solution.
#' @keywords internal
select_from_pareto_frontier <- function(pareto_solutions, 
                                      performance_weight = 0.3,
                                      quality_weight = NULL,
                                      stability_weight = NULL) {
  if (is.null(pareto_solutions) || length(pareto_solutions$frontier_indices) == 0) {
    return(1)  # Fallback to first candidate
  }
  
  frontier_indices <- pareto_solutions$frontier_indices
  objective_matrix <- pareto_solutions$objective_matrix
  objectives <- pareto_solutions$objectives
  
  if (length(frontier_indices) == 1) {
    return(frontier_indices[1])
  }
  
  # Set default weights if not provided
  n_objectives <- length(objectives)
  if (is.null(quality_weight) && is.null(stability_weight)) {
    remaining_weight <- 1 - performance_weight
    quality_weight <- remaining_weight * 0.6  # Quality gets more weight
    stability_weight <- remaining_weight * 0.4
  }
  
  # Create weights vector
  weights <- numeric(n_objectives)
  names(weights) <- objectives
  
  if ("quality" %in% objectives) weights["quality"] <- quality_weight %||% 0.4
  if ("performance" %in% objectives) weights["performance"] <- performance_weight
  if ("stability" %in% objectives) weights["stability"] <- stability_weight %||% 0.3
  
  # Normalize weights
  weights <- weights / sum(weights)
  
  # Normalize objectives to [0,1] scale for fair comparison
  frontier_objectives <- objective_matrix[frontier_indices, , drop = FALSE]
  normalized_objectives <- frontier_objectives
  
  for (j in seq_len(ncol(frontier_objectives))) {
    col_range <- range(frontier_objectives[, j], na.rm = TRUE)
    if (col_range[2] > col_range[1]) {
      normalized_objectives[, j] <- (frontier_objectives[, j] - col_range[1]) / 
                                    (col_range[2] - col_range[1])
    } else {
      normalized_objectives[, j] <- 1  # All values are the same
    }
  }
  
  # Compute weighted achievement scores
  weighted_scores <- numeric(length(frontier_indices))
  for (i in seq_along(frontier_indices)) {
    score <- 0
    for (obj in objectives) {
      if (obj %in% names(weights)) {
        score <- score + weights[obj] * normalized_objectives[i, obj]
      }
    }
    weighted_scores[i] <- score
  }
  
  best_frontier_idx <- which.max(weighted_scores)
  return(frontier_indices[best_frontier_idx])
}

#' Compute Hypervolume Indicator for Pareto Frontier
#'
#' Calculates the hypervolume dominated by the Pareto frontier,
#' which is a quality measure for multi-objective optimization.
#'
#' @param pareto_solutions Pareto frontier information.
#' @param reference_point Reference point for hypervolume calculation.
#' @return Hypervolume value.
#' @keywords internal
compute_hypervolume <- function(pareto_solutions, reference_point = NULL) {
  if (is.null(pareto_solutions) || length(pareto_solutions$frontier_indices) == 0) {
    return(0)
  }
  
  frontier_indices <- pareto_solutions$frontier_indices
  objective_matrix <- pareto_solutions$objective_matrix
  
  frontier_objectives <- objective_matrix[frontier_indices, , drop = FALSE]
  
  # Set reference point if not provided (worst values - epsilon)
  if (is.null(reference_point)) {
    reference_point <- apply(objective_matrix, 2, min) - 0.1
  }
  
  # Simple hypervolume calculation for 2D case
  if (ncol(frontier_objectives) == 2) {
    # Sort by first objective
    sorted_idx <- order(frontier_objectives[, 1], decreasing = TRUE)
    sorted_objectives <- frontier_objectives[sorted_idx, ]
    
    hypervolume <- 0
    prev_y <- reference_point[2]
    
    for (i in seq_len(nrow(sorted_objectives))) {
      width <- sorted_objectives[i, 1] - reference_point[1]
      height <- sorted_objectives[i, 2] - prev_y
      hypervolume <- hypervolume + width * height
      prev_y <- sorted_objectives[i, 2]
    }
    
    return(max(0, hypervolume))
  } else {
    # For higher dimensions, use Monte Carlo approximation
    return(compute_hypervolume_mc(frontier_objectives, reference_point))
  }
}

#' Monte Carlo Hypervolume Estimation
#'
#' Estimates hypervolume using Monte Carlo sampling for higher dimensions.
#'
#' @param frontier_objectives Matrix of Pareto frontier points.
#' @param reference_point Reference point for hypervolume.
#' @param n_samples Number of Monte Carlo samples.
#' @return Estimated hypervolume.
#' @keywords internal
compute_hypervolume_mc <- function(frontier_objectives, reference_point, n_samples = 10000) {
  n_objectives <- ncol(frontier_objectives)
  
  # Define sampling bounds
  upper_bounds <- apply(frontier_objectives, 2, max)
  
  # Generate random samples
  dominated_count <- 0
  
  for (i in seq_len(n_samples)) {
    # Generate random point in objective space
    sample_point <- runif(n_objectives, reference_point, upper_bounds)
    
    # Check if sample point is dominated by any frontier point
    dominated <- FALSE
    for (j in seq_len(nrow(frontier_objectives))) {
      if (all(frontier_objectives[j, ] >= sample_point)) {
        dominated <- TRUE
        break
      }
    }
    
    if (dominated) {
      dominated_count <- dominated_count + 1
    }
  }
  
  # Estimate hypervolume
  total_volume <- prod(upper_bounds - reference_point)
  estimated_hypervolume <- (dominated_count / n_samples) * total_volume
  
  return(estimated_hypervolume)
}

#' Null-coalescing operator
#'
#' Provides default value if left side is NULL. This is a convenience operator
#' that returns the left operand if it is not NULL, otherwise returns the right operand.
#' This operator is commonly used for setting default values in function arguments.
#' 
#' @param x Value to check for NULL.
#' @param y Default value to return if \code{x} is NULL.
#' @return \code{x} if not NULL, otherwise \code{y}.
#' @keywords internal
#' @name grapes-or-or-grapes
#' 
#' @examples
#' \dontshow{
#' # Internal function examples
#' value1 <- NULL
#' value2 <- "default"
#' result <- value1 %||% value2  # returns "default"
#' 
#' value3 <- "actual"
#' result2 <- value3 %||% "default"  # returns "actual"
#' }
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}