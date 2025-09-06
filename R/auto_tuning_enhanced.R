# ============================================================================
# ENHANCED AUTO-TUNING MAIN FUNCTION
# ============================================================================

#' Enhanced Automatic Small Cell Reduction Parameter Tuning with Multi-Objective Optimization
#'
#' Automatically selects optimal small.cell.reduction parameter for MONECA 
#' clustering using advanced optimization methods including Pareto frontier analysis,
#' cross-validation, Bayesian optimization, and performance-aware tuning.
#' This enhanced version provides significant speed improvements while maintaining
#' or improving tuning quality.
#'
#' @param mx Mobility matrix to analyze. Must be a square numeric matrix 
#'   representing mobility flows between categories.
#' @param cut.off Numeric threshold for edge weights in network construction. 
#'   Default is 1.
#' @param method Character string specifying tuning strategy. Options are:
#'   \itemize{
#'     \item "stability" (default): Bootstrap-based stability assessment
#'     \item "quality": Clustering quality optimization using silhouette and modularity
#'     \item "performance": Balance between quality and computational efficiency
#'     \item "pareto": Multi-objective optimization with Pareto frontier analysis
#'     \item "cross_validation": Cross-validation based parameter selection
#'     \item "bayesian": Bayesian optimization (requires DiceKriging package)
#'   }
#' @param n_trials Integer number of trials for stability assessment or CV folds. Default is 10.
#' @param candidate_values Numeric vector of candidate values to test. If NULL,
#'   values are generated automatically using data-driven methods.
#' @param performance_weight Numeric weight (0-1) for performance vs quality 
#'   trade-off. Higher values prioritize speed over quality. Default is 0.3.
#' @param min_density Minimum acceptable network density threshold. Default is 0.01.
#' @param max_candidates Maximum number of candidate values to evaluate for 
#'   computational efficiency. Default is 20.
#' @param seed Integer seed for reproducible results in bootstrap sampling. 
#'   Default is NULL.
#' @param verbose Logical indicating whether to show progress messages. 
#'   Default is FALSE.
#' @param use_cache Logical indicating whether to use caching for repeated computations.
#'   Default is TRUE.
#' @param parallel_cores Integer number of cores for parallel processing. Default is NULL
#'   (auto-detect).
#' @param early_stopping Logical indicating whether to use early stopping for
#'   candidate screening. Default is TRUE.
#' @param pareto_objectives Character vector of objectives for Pareto optimization.
#'   Default is c("quality", "performance").
#' @param cv_folds Integer number of cross-validation folds. Default is 5.
#' @param bayesian_iterations Integer number of Bayesian optimization iterations.
#'   Default is 20.
#'
#' @return A list containing:
#'   \item{optimal_value}{Recommended small.cell.reduction parameter value}
#'   \item{stability_scores}{Stability assessment results for each candidate}
#'   \item{quality_metrics}{Clustering quality metrics for each candidate}
#'   \item{performance_metrics}{Computational performance metrics}
#'   \item{candidates_tested}{Vector of candidate values evaluated}
#'   \item{tuning_method}{Method used for parameter selection}
#'   \item{selection_rationale}{Text explanation of parameter selection}
#'   \item{network_properties}{Network density and connectivity metrics}
#'   \item{computation_time}{Total time spent on parameter tuning}
#'   \item{pareto_solutions}{Pareto frontier analysis (if method = "pareto")}
#'   \item{cv_results}{Cross-validation results (if method = "cross_validation")}
#'   \item{bayesian_results}{Bayesian optimization results (if method = "bayesian")}
#'
#' @examples
#' \dontrun{
#' # Generate sample mobility data
#' mobility_data <- generate_mobility_data(n_classes = 6, seed = 123)
#' 
#' # Multi-objective Pareto optimization (NEW)
#' pareto_result <- auto_tune_small_cell_reduction_enhanced(
#'   mx = mobility_data, 
#'   method = "pareto",
#'   pareto_objectives = c("quality", "performance", "stability"),
#'   verbose = TRUE
#' )
#' 
#' # Cross-validation with confidence intervals (NEW)
#' cv_result <- auto_tune_small_cell_reduction_enhanced(
#'   mx = mobility_data,
#'   method = "cross_validation",
#'   cv_folds = 10,
#'   verbose = TRUE
#' )
#' 
#' # Bayesian optimization (NEW)
#' bayesian_result <- auto_tune_small_cell_reduction_enhanced(
#'   mx = mobility_data,
#'   method = "bayesian",
#'   bayesian_iterations = 30,
#'   verbose = TRUE
#' )
#' 
#' # Enhanced stability tuning with parallel processing
#' stability_result <- auto_tune_small_cell_reduction_enhanced(
#'   mx = mobility_data, 
#'   method = "stability",
#'   parallel_cores = 4,
#'   use_cache = TRUE,
#'   early_stopping = TRUE,
#'   verbose = TRUE
#' )
#' 
#' # Visualize results
#' plot_tuning_results(pareto_result, plot_type = "all")
#' parameter_sensitivity_plot(cv_result)
#' performance_quality_tradeoff_plot(stability_result)
#' }
#'
#' @seealso 
#' \code{\link{auto_tune_small_cell_reduction}} for the original function,
#' \code{\link{moneca}} for main clustering function,
#' \code{\link{plot_tuning_results}} for visualization,
#' \code{\link{parameter_sensitivity_plot}} for sensitivity analysis
#'
#' @export
auto_tune_small_cell_reduction_enhanced <- function(mx,
                                                   cut.off = 1,
                                                   method = "stability", 
                                                   n_trials = 10,
                                                   candidate_values = NULL,
                                                   performance_weight = 0.3,
                                                   min_density = 0.01,
                                                   max_candidates = 20,
                                                   seed = NULL,
                                                   verbose = FALSE,
                                                   use_cache = TRUE,
                                                   parallel_cores = NULL,
                                                   early_stopping = TRUE,
                                                   pareto_objectives = c("quality", "performance"),
                                                   cv_folds = 5,
                                                   bayesian_iterations = 20) {
  
  # Set seed for reproducibility
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  # Start timing
  start_time <- Sys.time()
  
  # Enhanced input validation
  if (!is.matrix(mx)) {
    mx <- as.matrix(mx)
  }
  
  if (nrow(mx) != ncol(mx)) {
    stop("Mobility matrix must be square")
  }
  
  if (!method %in% c("stability", "quality", "performance", "pareto", "cross_validation", "bayesian")) {
    stop("Method must be one of: 'stability', 'quality', 'performance', 'pareto', 'cross_validation', 'bayesian'")
  }
  
  if (performance_weight < 0 || performance_weight > 1) {
    stop("performance_weight must be between 0 and 1")
  }
  
  if (verbose) {
    cat("Starting enhanced auto-tuning for small.cell.reduction parameter...\n")
    cat("Method:", method, "\n")
    cat("Matrix size:", nrow(mx), "x", ncol(mx), "\n")
  }
  
  # Generate candidate values if not provided
  if (is.null(candidate_values)) {
    candidate_values <- generate_candidate_values(
      mx = mx, 
      method = "hybrid",
      max_candidates = max_candidates
    )
    if (verbose) {
      cat("Generated", length(candidate_values), "candidate values\n")
    }
  } else {
    # Limit candidates to max_candidates
    if (length(candidate_values) > max_candidates) {
      candidate_values <- candidate_values[1:max_candidates]
      if (verbose) {
        cat("Limited to", max_candidates, "candidate values\n")
      }
    }
  }
  
  # Ensure candidates are unique and sorted
  candidate_values <- sort(unique(candidate_values))
  
  # Initialize enhanced results storage with caching
  if (use_cache) {
    cache <- initialize_tuning_cache()
  } else {
    cache <- NULL
  }
  
  # Setup parallel processing if requested
  parallel_setup <- setup_parallel_processing(parallel_cores, verbose)
  
  results <- list()
  stability_scores <- numeric(length(candidate_values))
  quality_metrics <- list()
  performance_metrics <- list()
  network_properties <- list()
  pareto_solutions <- NULL
  cv_results <- NULL
  bayesian_results <- NULL
  
  # Enhanced candidate evaluation with method-specific optimizations
  if (verbose) {
    cat("Evaluating", length(candidate_values), "candidates using method:", method, "\n")
    if (use_cache) cat("Using computation cache for performance\n")
    if (!is.null(parallel_setup$cluster)) cat("Using parallel processing with", parallel_setup$n_cores, "cores\n")
  }
  
  # Method-specific evaluation strategies
  if (method == "pareto") {
    # Multi-objective Pareto optimization
    pareto_results <- evaluate_pareto_candidates(
      mx = mx,
      candidate_values = candidate_values,
      cut.off = cut.off,
      objectives = pareto_objectives,
      cache = cache,
      parallel_setup = parallel_setup,
      verbose = verbose
    )
    stability_scores <- pareto_results$stability_scores
    quality_metrics <- pareto_results$quality_metrics
    performance_metrics <- pareto_results$performance_metrics
    network_properties <- pareto_results$network_properties
    pareto_solutions <- pareto_results$pareto_frontier
  } else if (method == "cross_validation") {
    # Cross-validation based evaluation
    cv_results <- evaluate_cv_candidates(
      mx = mx,
      candidate_values = candidate_values,
      cut.off = cut.off,
      cv_folds = cv_folds,
      cache = cache,
      parallel_setup = parallel_setup,
      verbose = verbose
    )
    stability_scores <- cv_results$stability_scores
    quality_metrics <- cv_results$quality_metrics
    performance_metrics <- cv_results$performance_metrics
    network_properties <- cv_results$network_properties
  } else if (method == "bayesian") {
    # Bayesian optimization
    if (!requireNamespace("DiceKriging", quietly = TRUE)) {
      warning("DiceKriging package not available, falling back to stability method")
      method <- "stability"
    } else {
      bayesian_results <- evaluate_bayesian_candidates(
        mx = mx,
        candidate_range = range(candidate_values),
        cut.off = cut.off,
        n_iterations = bayesian_iterations,
        cache = cache,
        verbose = verbose
      )
      if (!is.null(bayesian_results)) {
        candidate_values <- bayesian_results$tested_candidates
        stability_scores <- bayesian_results$stability_scores
        quality_metrics <- bayesian_results$quality_metrics
        performance_metrics <- bayesian_results$performance_metrics
        network_properties <- bayesian_results$network_properties
      }
    }
  }
  
  # Standard evaluation for other methods or as fallback
  if (method %in% c("stability", "quality", "performance") || (method == "bayesian" && is.null(bayesian_results))) {
    evaluation_results <- evaluate_candidates_optimized(
      mx = mx,
      candidate_values = candidate_values,
      cut.off = cut.off,
      method = method,
      n_trials = n_trials,
      cache = cache,
      parallel_setup = parallel_setup,
      early_stopping = early_stopping,
      verbose = verbose
    )
    stability_scores <- evaluation_results$stability_scores
    quality_metrics <- evaluation_results$quality_metrics
    performance_metrics <- evaluation_results$performance_metrics
    network_properties <- evaluation_results$network_properties
  }
  
  # Enhanced parameter selection with method-specific strategies
  if (method == "stability") {
    # Choose parameter with highest stability score
    optimal_idx <- which.max(stability_scores)
    selection_rationale <- paste(
      "Selected based on highest stability score:",
      round(stability_scores[optimal_idx], 3)
    )
  } else if (method == "quality") {
    # Choose parameter with highest quality score
    quality_scores <- sapply(quality_metrics, function(x) x$overall)
    optimal_idx <- which.max(quality_scores)
    selection_rationale <- paste(
      "Selected based on highest quality score:",
      round(quality_scores[optimal_idx], 3)
    )
  } else if (method == "pareto") {
    # Select from Pareto frontier based on user preference
    optimal_idx <- select_from_pareto_frontier(
      pareto_solutions, 
      performance_weight = performance_weight
    )
    selection_rationale <- paste(
      "Selected from Pareto frontier with performance weight:",
      performance_weight
    )
  } else if (method == "cross_validation") {
    # Select based on cross-validation results
    cv_scores <- cv_results$cv_scores
    optimal_idx <- which.max(cv_scores)
    selection_rationale <- paste(
      "Selected based on", cv_folds, 
      "-fold cross-validation score:",
      round(cv_scores[optimal_idx], 3)
    )
  } else if (method == "bayesian") {
    # Bayesian optimization already provides the optimal candidate
    optimal_idx <- which.max(sapply(quality_metrics, function(x) x$overall))
    selection_rationale <- paste(
      "Selected via Bayesian optimization after", bayesian_iterations, "iterations"
    )
  } else { # performance method
    # Balance quality and performance
    trade_offs <- evaluate_performance_trade_offs(
      quality_metrics = quality_metrics,
      performance_metrics = performance_metrics,
      performance_weight = performance_weight
    )
    optimal_idx <- which.max(trade_offs)
    selection_rationale <- paste(
      "Selected based on performance-quality trade-off (weight =", 
      performance_weight, "), score:", round(trade_offs[optimal_idx], 3)
    )
  }
  
  optimal_value <- candidate_values[optimal_idx]
  end_time <- Sys.time()
  total_time <- as.numeric(end_time - start_time)
  
  if (verbose) {
    cat("\nOptimal small.cell.reduction:", optimal_value, "\n")
    cat("Total tuning time:", round(total_time, 2), "seconds\n")
    if (!is.null(cache)) {
      cat("Cache performance: hits =", cache$hits, ", misses =", cache$misses, "\n")
    }
  }
  
  # Cleanup parallel processing
  if (!is.null(parallel_setup$cluster)) {
    cleanup_parallel_processing(parallel_setup)
  }
  
  # Create comprehensive results object with enhanced information
  results <- list(
    optimal_value = optimal_value,
    stability_scores = stability_scores,
    quality_metrics = quality_metrics,
    performance_metrics = performance_metrics,
    candidates_tested = candidate_values,
    tuning_method = method,
    selection_rationale = selection_rationale,
    network_properties = network_properties,
    computation_time = total_time,
    pareto_solutions = pareto_solutions,
    cv_results = cv_results,
    bayesian_results = bayesian_results,
    parameters = list(
      cut.off = cut.off,
      n_trials = n_trials,
      performance_weight = performance_weight,
      min_density = min_density,
      max_candidates = max_candidates,
      use_cache = use_cache,
      parallel_cores = parallel_setup$n_cores,
      early_stopping = early_stopping,
      pareto_objectives = pareto_objectives,
      cv_folds = cv_folds,
      bayesian_iterations = bayesian_iterations
    )
  )
  
  class(results) <- c("moneca_tuning_enhanced", "moneca_tuning")
  return(results)
}

#' Print Method for Enhanced MONECA Tuning Results
#'
#' Provides formatted output for enhanced auto-tuning results.
#'
#' @param x Object of class "moneca_tuning_enhanced".
#' @param ... Additional arguments (unused).
#'
#' @export
print.moneca_tuning_enhanced <- function(x, ...) {
  cat("Enhanced MONECA Auto-Tuning Results\n")
  cat("===================================\n\n")
  
  cat("Optimal small.cell.reduction:", x$optimal_value, "\n")
  cat("Tuning method:", x$tuning_method, "\n")
  cat("Candidates tested:", length(x$candidates_tested), "\n")
  cat("Total computation time:", round(x$computation_time, 2), "seconds\n")
  
  # Show cache performance if available
  if (!is.null(x$parameters$use_cache) && x$parameters$use_cache) {
    cat("Parallel cores used:", x$parameters$parallel_cores, "\n")
  }
  
  cat("\nSelection rationale:", x$selection_rationale, "\n\n")
  
  # Method-specific details
  if (x$tuning_method == "pareto" && !is.null(x$pareto_solutions)) {
    cat("Pareto frontier analysis:\n")
    cat("  Objectives:", paste(x$pareto_solutions$objectives, collapse = ", "), "\n")
    cat("  Pareto optimal solutions:", length(x$pareto_solutions$frontier_indices), "\n")
  } else if (x$tuning_method == "cross_validation" && !is.null(x$cv_results)) {
    cat("Cross-validation details:\n")
    cat("  CV folds:", x$parameters$cv_folds, "\n")
    if (!is.null(x$cv_results$cv_confidence_intervals)) {
      optimal_idx <- which(x$candidates_tested == x$optimal_value)
      ci <- x$cv_results$cv_confidence_intervals[optimal_idx, ]
      cat("  95% Confidence interval: [", round(ci[1], 3), ", ", round(ci[2], 3), "]\n")
    }
  } else if (x$tuning_method == "bayesian" && !is.null(x$bayesian_results)) {
    cat("Bayesian optimization details:\n")
    cat("  Iterations:", x$parameters$bayesian_iterations, "\n")
    cat("  Acquisition function:", x$bayesian_results$acquisition_function, "\n")
  }
  
  # Show top candidates
  if (x$tuning_method == "stability" && length(x$stability_scores) > 0) {
    max_idx <- which.max(x$stability_scores)
    cat("\nStability scores (top 3):\n")
    top_indices <- order(x$stability_scores, decreasing = TRUE)[1:min(3, length(x$stability_scores))]
    for (i in top_indices) {
      marker <- if (i == max_idx) " (*)" else ""
      cat(sprintf("  Parameter %g: %.3f%s\n", x$candidates_tested[i], x$stability_scores[i], marker))
    }
    cat("\n")
  }
  
  # Quality metrics summary
  if (length(x$quality_metrics) > 0) {
    quality_scores <- sapply(x$quality_metrics, function(q) q$overall)
    max_quality_idx <- which.max(quality_scores)
    cat("Quality scores (top 3):\n")
    top_indices <- order(quality_scores, decreasing = TRUE)[1:min(3, length(quality_scores))]
    for (i in top_indices) {
      marker <- if (i == max_quality_idx) " (*)" else ""
      cat(sprintf("  Parameter %g: %.3f%s\n", x$candidates_tested[i], quality_scores[i], marker))
    }
  }
  
  cat("\nUse plot_tuning_results() to visualize these results.\n")
}