#' Cut-off Sensitivity Analysis with Strength Metrics
#'
#' Functions for performing sensitivity analysis of cut-off parameters
#' using strength-based metrics for weighted mobility networks.
#'
#' @name cutoff_sensitivity
#' @keywords internal
NULL

#' Comprehensive Cut-off Sensitivity Analysis
#'
#' Performs a systematic sensitivity analysis of the cut-off parameter,
#' evaluating strength-based metrics across a range of cut-off values.
#'
#' @param mx Mobility matrix with marginals
#' @param cutoff_range Numeric vector of cut-off values to test. If NULL,
#'   automatically determined from RR distribution
#' @param n_points Number of points to evaluate if cutoff_range is NULL
#' @param small.cell.reduction Small cell adjustment parameter
#' @param symmetric Whether to symmetrize the RR matrix
#' @param verbose Logical, whether to show progress
#'
#' @return Data frame with metrics for each cut-off value
#'
#' @examples
#' # Generate synthetic mobility data
#' data <- generate_mobility_data(n_classes = 4, seed = 123)
#' 
#' # Run sensitivity analysis
#' sensitivity <- cutoff_sensitivity_analysis(data, n_points = 20, verbose = FALSE)
#' 
#' # View results
#' head(sensitivity)
#' 
#' # Plot key metrics (requires ggplot2)
#' if (require(ggplot2, quietly = TRUE)) {
#'   library(ggplot2)
#'   plot(sensitivity$cutoff, sensitivity$n_edges, type = "l", 
#'        xlab = "Cut-off", ylab = "Number of Edges")
#' }
#'
#' @export
cutoff_sensitivity_analysis <- function(mx, 
                                       cutoff_range = NULL,
                                       n_points = 50,
                                       small.cell.reduction = 0,
                                       symmetric = TRUE,
                                       verbose = TRUE) {
  
  # Auto-determine range if not provided
  if (is.null(cutoff_range)) {
    if (verbose) message("Analyzing RR distribution to determine cut-off range...")
    rr_dist <- analyze_rr_distribution(mx, small.cell.reduction, symmetric)
    
    # Use 5th to 95th percentile range
    lower <- max(0.1, as.numeric(rr_dist$quantiles["10%"]))
    upper <- min(5, as.numeric(rr_dist$quantiles["90%"]))
    
    cutoff_range <- seq(lower, upper, length.out = n_points)
    
    if (verbose) {
      message(sprintf("Cut-off range: %.2f to %.2f", lower, upper))
    }
  }
  
  # Initialize results storage
  results <- vector("list", length(cutoff_range))
  
  # Progress bar if verbose
  if (verbose) {
    pb <- txtProgressBar(min = 0, max = length(cutoff_range), style = 3)
  }
  
  # Evaluate each cut-off
  for (i in seq_along(cutoff_range)) {
    co <- cutoff_range[i]
    
    # Get metrics for this cut-off
    metrics <- evaluate_cutoff_strength(mx, co, small.cell.reduction, symmetric)
    metrics$cutoff <- co
    
    # Calculate segmentation metrics
    seg_metrics <- evaluate_segmentation_metrics(mx, co, small.cell.reduction)
    metrics <- c(metrics, seg_metrics)
    
    results[[i]] <- metrics
    
    if (verbose) setTxtProgressBar(pb, i)
  }
  
  if (verbose) close(pb)
  
  # Convert to data frame
  result_df <- do.call(rbind, lapply(results, function(x) {
    as.data.frame(x, stringsAsFactors = FALSE)
  }))
  
  # Add derived metrics
  result_df$edge_retention <- result_df$n_edges / max(result_df$n_edges)
  result_df$strength_retention <- result_df$total_strength / max(result_df$total_strength)
  result_df$isolation_rate <- result_df$n_isolates / (result_df$n_isolates[1] + 1)
  
  class(result_df) <- c("cutoff_sensitivity", "data.frame")
  return(result_df)
}

#' Evaluate Segmentation Metrics
#'
#' Evaluates how a specific cut-off affects the segmentation structure.
#'
#' @param mx Mobility matrix
#' @param cut.off Cut-off value
#' @param small.cell.reduction Small cell adjustment
#'
#' @return List of segmentation metrics
#'
#' @keywords internal
evaluate_segmentation_metrics <- function(mx, cut.off, small.cell.reduction = 0) {
  # Get weight matrix
  weight_mat <- weight.matrix(mx, cut.off = cut.off, 
                             small.cell.reduction = small.cell.reduction)
  
  # Create graph
  weight_mat[is.na(weight_mat)] <- 0
  g <- moneca_graph_from_adjacency(weight_mat, mode = "undirected", 
                                   weighted = TRUE, diag = FALSE)
  
  # Calculate segmentation-relevant metrics
  components <- igraph::components(g)
  
  # Modularity (if graph has edges)
  modularity_score <- if (igraph::ecount(g) > 0) {
    # Use fast greedy community detection
    communities <- igraph::cluster_fast_greedy(g)
    igraph::modularity(communities)
  } else {
    0
  }
  
  # Clustering coefficient
  clustering_coef <- if (igraph::ecount(g) > 0) {
    igraph::transitivity(g, type = "global")
  } else {
    0
  }
  
  list(
    n_components = components$no,
    largest_component = max(components$csize),
    modularity = modularity_score,
    clustering_coefficient = clustering_coef,
    connectivity = 1 - (components$no - 1) / (igraph::vcount(g) - 1)
  )
}

#' Find Optimal Cut-off Based on Strength Patterns
#'
#' Identifies the optimal cut-off value using various criteria based on
#' strength metrics and network structure.
#'
#' @param mx Mobility matrix
#' @param criterion Optimization criterion: "elbow", "stability", "balance", 
#'   "modularity", or "information"
#' @param cutoff_range Range of cut-off values to consider
#' @param n_bootstrap Number of bootstrap samples for stability criterion
#' @param verbose Whether to show progress
#'
#' @return List with optimal cut-off and supporting information
#'
#' @examples
#' # Generate synthetic mobility data
#' data <- generate_mobility_data(n_classes = 4, seed = 123)
#' 
#' # Find optimal cut-off using different criteria
#' optimal_elbow <- find_optimal_cutoff(data, criterion = "elbow", verbose = FALSE)
#' optimal_balance <- find_optimal_cutoff(data, criterion = "balance", verbose = FALSE)
#' 
#' # Compare results
#' print(optimal_elbow)
#' print(optimal_balance)
#' 
#' # Get just the cut-off values
#' cat("Elbow method suggests:", optimal_elbow$optimal_cutoff, "\n")
#' cat("Balance method suggests:", optimal_balance$optimal_cutoff, "\n")
#'
#' @export
find_optimal_cutoff <- function(mx, 
                               criterion = "elbow",
                               cutoff_range = NULL,
                               n_bootstrap = 30,
                               verbose = TRUE) {
  
  # Get sensitivity analysis
  if (verbose) message("Running sensitivity analysis...")
  sensitivity <- cutoff_sensitivity_analysis(mx, cutoff_range, verbose = verbose)
  
  # Find optimal based on criterion
  optimal_cutoff <- switch(criterion,
    elbow = {
      # Find elbow point in strength ratio curve
      find_elbow_point(sensitivity$cutoff, sensitivity$strength_ratio)
    },
    stability = {
      # Find most stable point using bootstrap
      if (verbose) message("Evaluating stability via bootstrap...")
      stability_scores <- evaluate_stability(mx, sensitivity$cutoff, n_bootstrap)
      sensitivity$cutoff[which.min(stability_scores)]
    },
    balance = {
      # Balance between edge retention and strength concentration
      score <- sensitivity$edge_retention * sensitivity$strength_ratio
      sensitivity$cutoff[which.max(score)]
    },
    modularity = {
      # Maximum modularity
      sensitivity$cutoff[which.max(sensitivity$modularity)]
    },
    information = {
      # Minimize information loss (maximize strength retention while reducing edges)
      efficiency <- sensitivity$strength_retention / (sensitivity$edge_retention + 0.01)
      sensitivity$cutoff[which.max(efficiency)]
    },
    {
      warning("Unknown criterion, using elbow method")
      find_elbow_point(sensitivity$cutoff, sensitivity$strength_ratio)
    }
  )
  
  # Get metrics at optimal point
  optimal_idx <- which.min(abs(sensitivity$cutoff - optimal_cutoff))
  optimal_metrics <- sensitivity[optimal_idx, ]
  
  # Prepare result
  result <- list(
    optimal_cutoff = optimal_cutoff,
    criterion = criterion,
    metrics = optimal_metrics,
    sensitivity_analysis = sensitivity,
    explanation = explain_optimal_choice(criterion, optimal_metrics)
  )
  
  class(result) <- c("optimal_cutoff", "list")
  return(result)
}

#' Evaluate Stability via Bootstrap
#'
#' Evaluates the stability of segmentation at different cut-off values
#' using bootstrap resampling.
#'
#' @param mx Mobility matrix
#' @param cutoff_values Vector of cut-off values to test
#' @param n_bootstrap Number of bootstrap samples
#'
#' @return Vector of stability scores (lower is more stable)
#'
#' @keywords internal
evaluate_stability <- function(mx, cutoff_values, n_bootstrap = 30) {
  n_cutoffs <- length(cutoff_values)
  stability_scores <- numeric(n_cutoffs)
  
  # Get original dimensions
  n <- nrow(mx) - 1  # Exclude marginal
  
  for (i in seq_along(cutoff_values)) {
    co <- cutoff_values[i]
    bootstrap_strengths <- matrix(0, n_bootstrap, n)
    
    for (b in 1:n_bootstrap) {
      # Bootstrap sample (resample cells of mobility matrix)
      boot_indices <- sample(1:n, n, replace = TRUE)
      mx_boot <- mx[c(boot_indices, n+1), c(boot_indices, n+1)]
      
      # Recalculate marginals
      mx_boot[n+1, ] <- colSums(mx_boot[1:n, ])
      mx_boot[, n+1] <- rowSums(mx_boot[, 1:n])
      
      # Get strength for this bootstrap sample
      weight_mat <- weight.matrix(mx_boot, cut.off = co)
      weight_mat[is.na(weight_mat)] <- 0
      g <- moneca_graph_from_adjacency(weight_mat, mode = "undirected", 
                                       weighted = TRUE, diag = FALSE)
      
      bootstrap_strengths[b, ] <- igraph::strength(g)[boot_indices]
    }
    
    # Calculate coefficient of variation across bootstraps
    stability_scores[i] <- mean(apply(bootstrap_strengths, 2, function(x) {
      if (all(x == 0)) 0 else sd(x) / (mean(x) + 1e-10)
    }))
  }
  
  return(stability_scores)
}

#' Explain Optimal Choice
#'
#' Provides a human-readable explanation of why a cut-off was chosen.
#'
#' @param criterion The criterion used
#' @param metrics The metrics at the optimal point
#'
#' @return Character string explanation
#'
#' @keywords internal
explain_optimal_choice <- function(criterion, metrics) {
  explanation <- switch(criterion,
    elbow = sprintf(
      "Elbow point: Retains %.1f%% of edges while maintaining %.1f%% strength concentration",
      metrics$edge_retention * 100,
      metrics$strength_ratio * 100
    ),
    stability = sprintf(
      "Most stable configuration: CV = %.3f, %d components",
      metrics$strength_cv,
      metrics$n_components
    ),
    balance = sprintf(
      "Balanced trade-off: %.1f%% edges, %.1f%% strength ratio, modularity = %.3f",
      metrics$edge_retention * 100,
      metrics$strength_ratio * 100,
      metrics$modularity
    ),
    modularity = sprintf(
      "Maximum modularity (%.3f): %d edges, %d components",
      metrics$modularity,
      metrics$n_edges,
      metrics$n_components
    ),
    information = sprintf(
      "Maximum information efficiency: %.1f%% strength with only %.1f%% edges",
      metrics$strength_retention * 100,
      metrics$edge_retention * 100
    ),
    "Optimal cut-off selected"
  )
  
  return(explanation)
}

#' Print Optimal Cut-off Result
#'
#' @param x Optimal cut-off object
#' @param ... Additional arguments
#'
#' @export
print.optimal_cutoff <- function(x, ...) {
  cat("Optimal Cut-off Analysis\n")
  cat("========================\n")
  cat("Optimal cut-off value:", sprintf("%.3f", x$optimal_cutoff), "\n")
  cat("Criterion used:", x$criterion, "\n")
  cat("Explanation:", x$explanation, "\n\n")
  
  cat("Metrics at optimal point:\n")
  metrics_display <- x$metrics[c("n_edges", "mean_strength", "strength_ratio", 
                                 "modularity", "n_components")]
  for (name in names(metrics_display)) {
    cat(sprintf("  %s: %.3f\n", name, metrics_display[[name]]))
  }
  
  invisible(x)
}

#' Print Sensitivity Analysis Summary
#'
#' @param x Sensitivity analysis object
#' @param n Number of rows to display
#' @param ... Additional arguments
#'
#' @export
print.cutoff_sensitivity <- function(x, n = 10, ...) {
  cat("Cut-off Sensitivity Analysis\n")
  cat("============================\n")
  cat("Cut-off range:", sprintf("%.2f to %.2f", min(x$cutoff), max(x$cutoff)), "\n")
  cat("Number of points evaluated:", nrow(x), "\n\n")
  
  # Show summary at key percentiles
  key_points <- round(seq(1, nrow(x), length.out = min(n, nrow(x))))
  
  cat("Key metrics across cut-off range:\n")
  display_cols <- c("cutoff", "n_edges", "mean_strength", "strength_ratio", "modularity")
  # Filter to only available columns
  available_cols <- intersect(display_cols, colnames(x))
  if (length(available_cols) > 0) {
    print(as.data.frame(x[key_points, available_cols, drop = FALSE]), row.names = FALSE, digits = 3)
  }
  
  invisible(x)
}