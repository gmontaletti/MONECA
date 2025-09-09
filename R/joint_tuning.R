#' Joint Parameter Tuning Framework for MONECA
#'
#' This module provides comprehensive joint calibration for both small.cell.reduction 
#' and cut.off parameters in MONECA algorithms. It implements advanced optimization 
#' methods that consider the mathematical relationships and interactions between 
#' these two parameters.
#'
#' @name joint_tuning
#' @keywords internal
NULL

#' Joint Auto-Tuning for small.cell.reduction and cut.off Parameters
#'
#' Automatically selects optimal values for both small.cell.reduction and cut.off 
#' parameters simultaneously, considering their mathematical relationships and 
#' compound effects on clustering results.
#'
#' @param mx Mobility matrix to analyze. Must be a square numeric matrix.
#' @param method Character string specifying optimization strategy:
#'   \itemize{
#'     \item "grid" (default): 2D grid search with stability assessment
#'     \item "bayesian": Bayesian optimization in joint parameter space
#'     \item "pareto": Multi-objective Pareto frontier optimization
#'     \item "adaptive": Adaptive refinement starting with coarse grid
#'   }
#' @param scr_range Numeric vector of length 2 specifying the range for 
#'   small.cell.reduction parameter. Default is c(0, NULL) where NULL 
#'   auto-determines the upper bound.
#' @param cutoff_range Numeric vector of length 2 specifying the range for 
#'   cut.off parameter. Default is c(0.5, 3).
#' @param n_grid_points Integer number of grid points per dimension for grid 
#'   search. Default is 10.
#' @param n_bootstrap Integer number of bootstrap samples for stability 
#'   assessment. Default is 50.
#' @param objectives Character vector of optimization objectives. Options include:
#'   "stability", "quality", "sparsity", "modularity". 
#'   Default is c("stability", "quality").
#' @param weights Numeric vector of weights for combining multiple objectives.
#'   Must sum to 1. Default is equal weights.
#' @param seed Integer seed for reproducibility. Default is NULL.
#' @param verbose Logical indicating whether to show progress. Default is TRUE.
#' @param parallel Character or logical indicating parallel processing preference.
#'   Can be "auto" (default, intelligent switching), TRUE/FALSE (force parallel/sequential),
#'   or "parallel"/"sequential" for explicit control. When "auto", the function
#'   analyzes problem characteristics and system resources to make optimal decision.
#' @param plot_surface Logical indicating whether to plot optimization surface.
#'   Default is FALSE.
#'
#' @return A list of class "moneca_joint_tuning" containing:
#'   \item{optimal_scr}{Optimal small.cell.reduction value}
#'   \item{optimal_cutoff}{Optimal cut.off value}
#'   \item{optimization_surface}{Matrix of objective values for parameter grid}
#'   \item{parameter_grid}{Data frame of tested parameter combinations}
#'   \item{scores}{Detailed scores for each parameter combination}
#'   \item{method}{Optimization method used}
#'   \item{objectives}{Objectives optimized}
#'   \item{selection_rationale}{Explanation of parameter selection}
#'   \item{mathematical_relationship}{Estimated interaction effects}
#'   \item{computation_time}{Total optimization time}
#'
#' @details
#' The function considers the mathematical relationship between parameters:
#' 
#' \strong{Sequential Filtering:}
#' 1. small.cell.reduction filters raw counts: mx[mx < scr] = 0
#' 2. Relative risk calculation: RR = Observed / Expected
#' 3. cut.off filters relative risks: RR[RR < cutoff] = NA
#' 
#' \strong{Compound Effects:}
#' Network density is affected multiplicatively by both parameters.
#' The function models this interaction to find optimal combinations.
#'
#' @examples
#' \dontrun{
#' # Generate sample data
#' mobility_data <- generate_mobility_data(n_classes = 6, seed = 123)
#' 
#' # Grid search optimization
#' joint_result <- auto_tune_joint_parameters(
#'   mx = mobility_data,
#'   method = "grid",
#'   n_grid_points = 15,
#'   verbose = TRUE,
#'   plot_surface = TRUE
#' )
#' 
#' # Use optimal parameters
#' segments <- moneca(
#'   mobility_data,
#'   small.cell.reduction = joint_result$optimal_scr,
#'   cut.off = joint_result$optimal_cutoff
#' )
#' 
#' # Adaptive refinement for efficiency
#' adaptive_result <- auto_tune_joint_parameters(
#'   mx = mobility_data,
#'   method = "adaptive",
#'   verbose = TRUE
#' )
#' }
#'
#' @seealso 
#' \code{\link{auto_tune_small_cell_reduction}},
#' \code{\link{analyze_parameter_interaction}},
#' \code{\link{visualize_parameter_space}}
#'
#' @export
auto_tune_joint_parameters <- function(mx,
                                      method = "grid",
                                      scr_range = c(0, NULL),
                                      cutoff_range = c(0.5, 3),
                                      n_grid_points = 10,
                                      n_bootstrap = 50,
                                      objectives = c("stability", "quality"),
                                      weights = NULL,
                                      seed = NULL,
                                      verbose = TRUE,
                                      parallel = "auto",
                                      plot_surface = FALSE) {
  
  # Set seed for reproducibility
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  start_time <- Sys.time()
  
  # Input validation
  if (!is.matrix(mx)) {
    mx <- as.matrix(mx)
  }
  
  if (nrow(mx) != ncol(mx)) {
    stop("Mobility matrix must be square")
  }
  
  if (!method %in% c("grid", "bayesian", "pareto", "adaptive")) {
    stop("Method must be one of: 'grid', 'bayesian', 'pareto', 'adaptive'")
  }
  
  # Input validation for small.cell.reduction range
  if (any(scr_range != floor(scr_range))) {
    if (verbose) {
      message("small.cell.reduction range contains non-integer values. ",
              "Rounding to nearest integers since parameter operates on count data.")
    }
    scr_range <- ensure_integer_scr_values(scr_range, warn_if_rounded = FALSE)
  }
  
  # Auto-determine parameter ranges if needed
  if (is.null(scr_range[2]) || length(scr_range) < 2) {
    suggested_ranges <- suggest_parameter_ranges(mx, method = "moderate")
    scr_range <- suggested_ranges$scr_range
  }
  
  # Ensure SCR range has integer bounds
  scr_range <- ensure_integer_scr_values(scr_range, warn_if_rounded = verbose)
  
  # Ensure ranges are valid
  if (length(scr_range) < 2 || scr_range[2] <= scr_range[1]) {
    scr_range <- c(0L, max(10L, scr_range[1] + 5L))
  }
  
  if (is.na(cutoff_range[2]) || cutoff_range[2] <= cutoff_range[1]) {
    cutoff_range[2] <- cutoff_range[1] + 1
  }
  
  # Set default weights if not provided
  if (is.null(weights)) {
    weights <- rep(1/length(objectives), length(objectives))
  } else {
    if (length(weights) != length(objectives)) {
      stop("Length of weights must match length of objectives")
    }
    if (abs(sum(weights) - 1) > 1e-6) {
      stop("Weights must sum to 1")
    }
  }
  
  # Smart parallel processing decision
  n_combinations <- n_grid_points^2  # Initial estimate
  matrix_size <- nrow(mx)
  
  # Make intelligent parallel processing decision
  parallel_decision <- should_use_parallel(
    n_combinations = n_combinations,
    matrix_size = matrix_size,
    n_bootstrap = n_bootstrap,
    user_preference = parallel,
    verbose = verbose && method %in% c("grid", "adaptive")  # Show details for compute-intensive methods
  )
  
  use_parallel <- parallel_decision$use_parallel
  n_cores <- parallel_decision$n_cores
  
  if (verbose) {
    cat("Starting joint parameter optimization...\n")
    cat("Method:", method, "\n")
    cat("small.cell.reduction range:", scr_range[1], "-", scr_range[2], "\n")
    cat("cut.off range:", cutoff_range[1], "-", cutoff_range[2], "\n")
    cat("Objectives:", paste(objectives, collapse = ", "), "\n")
    cat("Parallel processing:", if (use_parallel) paste("YES (", n_cores, "cores)") else "NO", "\n\n")
  }
  
  # Execute optimization based on method
  result <- switch(method,
    "grid" = optimize_grid_search(
      mx = mx,
      scr_range = scr_range,
      cutoff_range = cutoff_range,
      n_grid_points = n_grid_points,
      n_bootstrap = n_bootstrap,
      objectives = objectives,
      weights = weights,
      verbose = verbose,
      parallel = use_parallel
    ),
    "adaptive" = optimize_adaptive_refinement(
      mx = mx,
      scr_range = scr_range,
      cutoff_range = cutoff_range,
      objectives = objectives,
      weights = weights,
      verbose = verbose,
      parallel = use_parallel
    ),
    "pareto" = optimize_pareto_frontier(
      mx = mx,
      scr_range = scr_range,
      cutoff_range = cutoff_range,
      n_grid_points = n_grid_points,
      objectives = objectives,
      verbose = verbose,
      parallel = use_parallel
    ),
    "bayesian" = optimize_bayesian_joint(
      mx = mx,
      scr_range = scr_range,
      cutoff_range = cutoff_range,
      objectives = objectives,
      weights = weights,
      verbose = verbose,
      parallel = use_parallel
    )
  )
  
  # Analyze mathematical relationships
  result$mathematical_relationship <- analyze_parameter_interaction(
    mx = mx,
    scr = result$optimal_scr,
    cutoff = result$optimal_cutoff
  )
  
  # Add metadata
  result$method <- method
  result$objectives <- objectives
  result$weights <- weights
  result$parallel_info <- parallel_decision
  
  end_time <- Sys.time()
  result$computation_time <- as.numeric(end_time - start_time)
  
  if (verbose) {
    cat("\nOptimization complete!\n")
    cat("Optimal small.cell.reduction:", result$optimal_scr, "\n")
    cat("Optimal cut.off:", result$optimal_cutoff, "\n")
    cat("Total time:", round(result$computation_time, 2), "seconds\n")
  }
  
  # Plot optimization surface if requested
  if (plot_surface) {
    plot_optimization_surface(result)
  }
  
  class(result) <- "moneca_joint_tuning"
  return(result)
}

#' Grid Search Optimization for Joint Parameters
#'
#' Performs systematic grid search over 2D parameter space.
#'
#' @inheritParams auto_tune_joint_parameters
#' @keywords internal
optimize_grid_search <- function(mx, scr_range, cutoff_range, n_grid_points,
                                n_bootstrap, objectives, weights, 
                                verbose, parallel) {
  
  # Create parameter grid with integer constraints for SCR
  scr_values <- generate_integer_scr_grid(scr_range, n_grid_points)
  cutoff_values <- seq(cutoff_range[1], cutoff_range[2], length.out = n_grid_points)
  
  param_grid <- expand.grid(
    scr = scr_values,
    cutoff = cutoff_values
  )
  
  n_combinations <- nrow(param_grid)
  
  if (verbose) {
    cat("Evaluating", n_combinations, "parameter combinations...\n")
  }
  
  # Initialize score matrix (dimensions based on actual grid sizes)
  scores <- matrix(NA, nrow = length(scr_values), ncol = length(cutoff_values))
  detailed_scores <- list()
  
  # Use the already-determined parallel decision
  # Note: 'parallel' parameter now contains the smart decision result
  use_parallel <- parallel
  
  # Set up parallel processing if needed
  cluster_setup <- NULL
  if (use_parallel) {
    cluster_setup <- setup_parallel_cluster(verbose = verbose)
    on.exit({
      if (!is.null(cluster_setup)) cleanup_parallel_cluster(cluster_setup, verbose = verbose)
    })
  }
  
  # Function to process a single parameter combination
  process_combination <- function(i) {
    scr <- param_grid$scr[i]
    cutoff <- param_grid$cutoff[i]
    
    # Compute objective scores
    obj_scores <- evaluate_parameter_combination(
      mx = mx,
      scr = scr,
      cutoff = cutoff,
      objectives = objectives,
      n_bootstrap = n_bootstrap,
      parallel = FALSE  # Individual evaluations use sequential to avoid nested parallelism
    )
    
    # Combine scores with weights
    combined_score <- sum(obj_scores * weights)
    
    # Return all necessary information
    return(list(
      i = i,
      scr = scr,
      cutoff = cutoff,
      obj_scores = obj_scores,
      combined_score = combined_score
    ))
  }
  
  # Use optimized batch evaluation system
  optimization_result <- evaluate_parameter_combinations_optimized(
    mx = mx,
    param_combinations = param_grid,
    objectives = objectives,
    weights = weights,
    n_bootstrap = n_bootstrap,
    use_cache = TRUE,
    smart_ordering = TRUE,
    early_termination = n_combinations > 20,  # Enable for larger searches
    termination_threshold = 0.6,  # Evaluate at least 60% before considering termination
    parallel = use_parallel,
    verbose = verbose
  )
  
  # Extract results from optimized evaluation
  detailed_scores <- optimization_result$detailed_scores
  best_idx <- optimization_result$best_idx
  optimal_params <- param_grid[best_idx, ]
  
  # Populate score matrix for visualization
  for (i in seq_len(nrow(param_grid))) {
    if (!is.null(detailed_scores[[i]])) {
      scr <- param_grid$scr[i]
      cutoff <- param_grid$cutoff[i]
      
      # Store in matrix (for visualization)
      row_idx <- which(scr_values == scr)
      col_idx <- which(cutoff_values == cutoff)
      
      if (length(row_idx) > 0 && length(col_idx) > 0) {
        scores[row_idx, col_idx] <- detailed_scores[[i]]["combined"]
      }
    }
  }
  
  # Create result object with performance information
  result <- list(
    optimal_scr = optimal_params$scr,
    optimal_cutoff = optimal_params$cutoff,
    optimization_surface = scores,
    parameter_grid = param_grid,
    scores = detailed_scores,
    scr_values = scr_values,
    cutoff_values = cutoff_values,
    performance_stats = optimization_result$performance_stats,
    selection_rationale = sprintf(
      "Selected based on %s optimization with combined score: %.3f (%s evaluation)",
      paste(objectives, collapse = "+"),
      detailed_scores[[best_idx]]["combined"],
      if (!is.null(optimization_result$performance_stats$early_terminated) && 
          optimization_result$performance_stats$early_terminated) "early-terminated" else "complete"
    )
  )
  
  return(result)
}

#' Adaptive Refinement Optimization
#'
#' Starts with coarse grid and refines promising regions.
#'
#' @inheritParams auto_tune_joint_parameters
#' @keywords internal
optimize_adaptive_refinement <- function(mx, scr_range, cutoff_range,
                                        objectives, weights, verbose, parallel) {
  
  if (verbose) {
    cat("Starting adaptive refinement optimization...\n")
  }
  
  # Set up parallel processing if needed
  cluster_setup <- NULL
  if (parallel) {
    cluster_setup <- setup_parallel_cluster(verbose = verbose)
    on.exit({
      if (!is.null(cluster_setup)) cleanup_parallel_cluster(cluster_setup, verbose = verbose)
    })
  }
  
  # Phase 1: Coarse grid (5x5)
  if (verbose) cat("Phase 1: Coarse grid search (5x5)...\n")
  
  coarse_result <- optimize_grid_search(
    mx = mx,
    scr_range = scr_range,
    cutoff_range = cutoff_range,
    n_grid_points = 5,
    n_bootstrap = 25,  # Slightly increased for better stability estimation
    objectives = objectives,
    weights = weights,
    verbose = FALSE,
    parallel = parallel
  )
  
  # Identify promising region (top 20% of scores)
  scores_flat <- as.vector(coarse_result$optimization_surface)
  threshold <- quantile(scores_flat, 0.8, na.rm = TRUE)
  
  # Find bounds of promising region
  promising_indices <- which(coarse_result$optimization_surface >= threshold, 
                           arr.ind = TRUE)
  
  scr_indices <- promising_indices[, 1]
  cutoff_indices <- promising_indices[, 2]
  
  # New refined ranges (ensure integer bounds for SCR)
  scr_min_idx <- max(1, min(scr_indices) - 1)
  scr_max_idx <- min(length(coarse_result$scr_values), max(scr_indices) + 1)
  scr_range_refined <- c(
    coarse_result$scr_values[scr_min_idx],
    coarse_result$scr_values[scr_max_idx]
  )
  
  cutoff_min_idx <- max(1, min(cutoff_indices) - 1)
  cutoff_max_idx <- min(length(coarse_result$cutoff_values), max(cutoff_indices) + 1)
  cutoff_range_refined <- c(
    coarse_result$cutoff_values[cutoff_min_idx],
    coarse_result$cutoff_values[cutoff_max_idx]
  )
  
  # Phase 2: Fine grid in promising region (10x10)
  if (verbose) {
    cat("Phase 2: Fine grid search in promising region...\n")
    cat("  Refined SCR range:", scr_range_refined[1], "-", scr_range_refined[2], "\n")
    cat("  Refined cutoff range:", cutoff_range_refined[1], "-", cutoff_range_refined[2], "\n")
  }
  
  fine_result <- optimize_grid_search(
    mx = mx,
    scr_range = scr_range_refined,
    cutoff_range = cutoff_range_refined,
    n_grid_points = 12,  # Increased for better resolution
    n_bootstrap = 60,  # More bootstraps for final selection
    objectives = objectives,
    weights = weights,
    verbose = FALSE,
    parallel = parallel
  )
  
  # Combine results
  result <- fine_result
  result$coarse_surface <- coarse_result$optimization_surface
  result$refinement_history <- list(
    coarse = coarse_result,
    fine = fine_result
  )
  
  return(result)
}

# ============================================================================
# OPTIMIZED EVALUATION SYSTEM WITH CACHING AND BATCH PROCESSING
# ============================================================================

# Global cache for weight matrices (session-level cache)
.weight_matrix_cache <- new.env(parent = emptyenv())
.moneca_fast_cache <- new.env(parent = emptyenv())

#' Clear Evaluation Caches
#'
#' Clears all cached weight matrices and moneca_fast results to free memory.
#' This is useful when working with large datasets or after completing
#' parameter optimization to reclaim memory.
#'
#' @param verbose Logical indicating whether to show cache statistics.
#'
#' @examples
#' \dontrun{
#' # After running joint parameter tuning
#' result <- auto_tune_joint_parameters(mobility_data)
#' 
#' # Clear caches to free memory
#' clear_evaluation_caches(verbose = TRUE)
#' }
#'
#' @export
clear_evaluation_caches <- function(verbose = FALSE) {
  if (verbose) {
    wm_count <- length(ls(.weight_matrix_cache))
    mf_count <- length(ls(.moneca_fast_cache))
    message(sprintf("Clearing caches: %d weight matrices, %d moneca_fast results", 
                   wm_count, mf_count))
  }
  
  rm(list = ls(.weight_matrix_cache), envir = .weight_matrix_cache)
  rm(list = ls(.moneca_fast_cache), envir = .moneca_fast_cache)
  gc(verbose = FALSE)
  
  if (verbose) {
    message("Caches cleared successfully")
  }
}

#' Get Cache Statistics
#'
#' Returns information about the current state of evaluation caches.
#'
#' @return List containing cache statistics.
#'
#' @examples
#' \dontrun{
#' cache_info <- get_cache_stats()
#' print(cache_info)
#' }
#'
#' @export
get_cache_stats <- function() {
  wm_count <- length(ls(.weight_matrix_cache))
  mf_count <- length(ls(.moneca_fast_cache))
  
  # Estimate memory usage (rough approximation)
  wm_memory <- wm_count * 1024  # Rough estimate in bytes
  mf_memory <- mf_count * 512   # Rough estimate in bytes
  
  list(
    weight_matrix_cache = list(
      entries = wm_count,
      estimated_memory_kb = wm_memory / 1024
    ),
    moneca_fast_cache = list(
      entries = mf_count,
      estimated_memory_kb = mf_memory / 1024
    ),
    total_entries = wm_count + mf_count,
    total_estimated_memory_kb = (wm_memory + mf_memory) / 1024
  )
}

#' Generate cache key for parameter combinations
#' @keywords internal
generate_cache_key <- function(mx_hash, scr, cutoff, additional = NULL) {
  # Create deterministic hash based on matrix content and parameters
  key_components <- paste(c(mx_hash, scr, cutoff, additional), collapse = "_")
  # Use a simple but effective hash function
  return(paste0("key_", abs(as.integer(sum(utf8ToInt(key_components)) %% 2^31))))
}

#' Generate simple hash for matrices
#' @keywords internal
simple_matrix_hash <- function(mx) {
  # Create a simple but effective hash of the matrix
  mx_vec <- as.vector(mx)
  mx_vec <- mx_vec[!is.na(mx_vec)]
  hash_components <- c(length(mx_vec), sum(mx_vec), mean(mx_vec), sd(mx_vec))
  return(paste(round(hash_components, 6), collapse = "_"))
}

#' Batch Weight Matrix Computation
#'
#' Computes weight matrices for multiple parameter combinations efficiently,
#' using caching, vectorized operations, and parallel processing when enabled.
#'
#' @param mx Mobility matrix.
#' @param param_combinations Data frame with scr and cutoff columns.
#' @param use_cache Logical indicating whether to use caching.
#' @param parallel Logical indicating whether to use parallel processing.
#' @param verbose Logical indicating whether to show progress.
#'
#' @return List of weight matrices with same length as param_combinations.
#' @keywords internal
batch_compute_weight_matrices <- function(mx, param_combinations, 
                                         use_cache = TRUE, parallel = FALSE, verbose = FALSE) {
  
  n_combinations <- nrow(param_combinations)
  weight_matrices <- vector("list", n_combinations)
  
  # Generate hash for the mobility matrix (for cache keys)
  mx_hash <- simple_matrix_hash(mx)
  
  # Track cache hits for performance monitoring
  cache_hits <- 0
  
  if (verbose) {
    message(sprintf("Computing %d weight matrices (caching: %s, parallel: %s)...", 
                   n_combinations, use_cache, parallel))
  }
  
  # Determine which combinations need computation (not in cache)
  combinations_to_compute <- integer(0)
  cache_keys <- character(n_combinations)
  
  for (i in seq_len(n_combinations)) {
    scr <- param_combinations$scr[i]
    cutoff <- param_combinations$cutoff[i]
    cache_key <- generate_cache_key(mx_hash, scr, cutoff)
    cache_keys[i] <- cache_key
    
    # Check cache first
    if (use_cache && exists(cache_key, envir = .weight_matrix_cache)) {
      weight_matrices[[i]] <- get(cache_key, envir = .weight_matrix_cache)
      cache_hits <- cache_hits + 1
    } else {
      combinations_to_compute <- c(combinations_to_compute, i)
    }
  }
  
  # Compute missing weight matrices
  if (length(combinations_to_compute) > 0) {
    if (parallel && requireNamespace("foreach", quietly = TRUE) && 
        requireNamespace("doParallel", quietly = TRUE)) {
      
      # Use parallel processing with foreach %dopar%
      compute_results <- foreach::foreach(
        idx = combinations_to_compute,
        .combine = 'c',
        .multicombine = TRUE,
        .maxcombine = length(combinations_to_compute),
        .errorhandling = 'pass',
        .packages = c("moneca"),  # Ensure package is loaded on workers
        .export = c("weight.matrix")  # Export necessary functions
      ) %dopar% {
        scr <- param_combinations$scr[idx]
        cutoff <- param_combinations$cutoff[idx]
        
        # Compute weight matrix
        result <- tryCatch({
          wm <- weight.matrix(
            mx = mx,
            cut.off = cutoff,
            small.cell.reduction = scr
          )
          list(index = idx, matrix = wm, error = NULL)
        }, error = function(e) {
          list(index = idx, matrix = NULL, error = e$message)
        })
        
        return(result)
      }
      
      # Process parallel results
      for (result in compute_results) {
        if (is.list(result) && !is.null(result$index)) {
          idx <- result$index
          weight_matrices[[idx]] <- result$matrix
          
          # Store in cache if enabled and computation was successful
          if (use_cache && !is.null(result$matrix)) {
            assign(cache_keys[idx], result$matrix, envir = .weight_matrix_cache)
          }
        }
      }
      
    } else {
      # Sequential processing fallback
      for (i in combinations_to_compute) {
        scr <- param_combinations$scr[i]
        cutoff <- param_combinations$cutoff[i]
        
        # Compute weight matrix
        tryCatch({
          wm <- weight.matrix(
            mx = mx,
            cut.off = cutoff,
            small.cell.reduction = scr
          )
          
          weight_matrices[[i]] <- wm
          
          # Store in cache if enabled
          if (use_cache) {
            assign(cache_keys[i], wm, envir = .weight_matrix_cache)
          }
        }, error = function(e) {
          weight_matrices[[i]] <- NULL
        })
      }
    }
  }
  
  if (verbose && use_cache) {
    message(sprintf("Cache performance: %d/%d hits (%.1f%% hit rate)", 
                   cache_hits, n_combinations, 
                   100 * cache_hits / n_combinations))
  }
  
  if (verbose && parallel && length(combinations_to_compute) > 0) {
    message(sprintf("Parallel computation: %d matrices computed using %s", 
                   length(combinations_to_compute),
                   if (requireNamespace("doParallel", quietly = TRUE)) "doParallel" else "sequential fallback"))
  }
  
  return(weight_matrices)
}

#' Vectorized Quality Metrics Computation
#'
#' Computes quality metrics for multiple weight matrices in batch,
#' optimizing for vectorized operations and parallel processing where possible.
#'
#' @param weight_matrices List of weight matrices.
#' @param objectives Character vector of objectives to compute.
#' @param parallel Logical indicating whether to use parallel processing.
#' @param verbose Logical indicating whether to show progress.
#'
#' @return Matrix with objectives as columns and combinations as rows.
#' @keywords internal
batch_compute_quality_metrics <- function(weight_matrices, objectives, 
                                          parallel = FALSE, verbose = FALSE) {
  
  n_matrices <- length(weight_matrices)
  n_objectives <- length(objectives)
  
  # Initialize results matrix
  results <- matrix(0, nrow = n_matrices, ncol = n_objectives)
  colnames(results) <- objectives
  
  if (verbose) {
    message(sprintf("Computing quality metrics for %d weight matrices (parallel: %s)...", 
                   n_matrices, parallel))
  }
  
  # Vectorized computation for simple objectives
  if ("sparsity" %in% objectives) {
    sparsity_idx <- which(objectives == "sparsity")
    
    # Vectorized sparsity computation
    densities <- vapply(weight_matrices, function(wm) {
      if (is.null(wm)) return(0)
      sum(!is.na(wm)) / length(wm)
    }, numeric(1))
    
    # Optimal around 0.1-0.3 density
    results[, sparsity_idx] <- exp(-((densities - 0.2)^2) / 0.05)
  }
  
  # Process other objectives that need individual computation
  remaining_objectives <- setdiff(objectives, "sparsity")
  
  if (length(remaining_objectives) > 0) {
    # Find valid matrices for computation
    valid_indices <- which(vapply(weight_matrices, function(wm) {
      !is.null(wm) && !all(is.na(wm)) && sum(!is.na(wm)) >= 2
    }, logical(1)))
    
    if (length(valid_indices) > 0) {
      if (parallel && requireNamespace("foreach", quietly = TRUE) && 
          requireNamespace("doParallel", quietly = TRUE) && length(valid_indices) > 1) {
        
        # Use parallel processing for remaining objectives
        parallel_results <- foreach::foreach(
          i = valid_indices,
          .combine = 'rbind',
          .multicombine = TRUE,
          .errorhandling = 'pass',
          .packages = c("moneca"),
          .export = c("compute_clustering_quality_metrics", "compute_network_modularity")
        ) %dopar% {
          wm <- weight_matrices[[i]]
          obj_results <- numeric(length(remaining_objectives))
          names(obj_results) <- remaining_objectives
          
          for (obj in remaining_objectives) {
            tryCatch({
              if (obj == "quality") {
                quality_metrics <- compute_clustering_quality_metrics(wm)
                obj_results[obj] <- quality_metrics$overall
              } else if (obj == "modularity") {
                obj_results[obj] <- compute_network_modularity(wm)
              }
            }, error = function(e) {
              obj_results[obj] <- 0
            })
          }
          
          # Return index and results
          c(index = i, obj_results)
        }
        
        # Process parallel results
        if (is.matrix(parallel_results)) {
          for (row_idx in seq_len(nrow(parallel_results))) {
            matrix_idx <- parallel_results[row_idx, "index"]
            for (obj in remaining_objectives) {
              obj_idx <- which(objectives == obj)
              results[matrix_idx, obj_idx] <- parallel_results[row_idx, obj]
            }
          }
        }
        
      } else {
        # Sequential processing fallback
        for (i in valid_indices) {
          wm <- weight_matrices[[i]]
          
          for (obj in remaining_objectives) {
            obj_idx <- which(objectives == obj)
            
            tryCatch({
              if (obj == "quality") {
                quality_metrics <- compute_clustering_quality_metrics(wm)
                results[i, obj_idx] <- quality_metrics$overall
              } else if (obj == "modularity") {
                results[i, obj_idx] <- compute_network_modularity(wm)
              }
            }, error = function(e) {
              results[i, obj_idx] <- 0
            })
          }
        }
      }
    }
  }
  
  if (verbose && parallel && length(remaining_objectives) > 0) {
    n_parallel_computed <- length(which(vapply(weight_matrices, function(wm) {
      !is.null(wm) && !all(is.na(wm)) && sum(!is.na(wm)) >= 2
    }, logical(1))))
    
    message(sprintf("Parallel computation: %d matrices processed for %s objectives using %s", 
                   n_parallel_computed,
                   paste(remaining_objectives, collapse = ", "),
                   if (requireNamespace("doParallel", quietly = TRUE)) "doParallel" else "sequential fallback"))
  }
  
  return(results)
}

#' Batch Stability Assessment with Caching
#'
#' Performs stability assessment for multiple parameter combinations,
#' using caching to avoid redundant computations.
#'
#' @param mx Mobility matrix.
#' @param param_combinations Data frame with scr and cutoff columns.
#' @param n_bootstrap Number of bootstrap samples.
#' @param use_cache Logical indicating whether to use caching.
#' @param parallel Logical indicating whether to use parallel processing.
#' @param verbose Logical indicating whether to show progress.
#'
#' @return Numeric vector of stability scores.
#' @keywords internal
batch_assess_stability <- function(mx, param_combinations, n_bootstrap = 50,
                                  use_cache = TRUE, parallel = FALSE, 
                                  verbose = FALSE) {
  
  n_combinations <- nrow(param_combinations)
  stability_scores <- numeric(n_combinations)
  
  # Generate hash for the mobility matrix and bootstrap count
  mx_hash <- paste(simple_matrix_hash(mx), n_bootstrap, sep = "_")
  
  cache_hits <- 0
  
  if (verbose) {
    message(sprintf("Computing stability for %d parameter combinations (parallel: %s)...", 
                   n_combinations, parallel))
  }
  
  # Determine which combinations need computation (not in cache)
  combinations_to_compute <- integer(0)
  cache_keys <- character(n_combinations)
  
  for (i in seq_len(n_combinations)) {
    scr <- param_combinations$scr[i]
    cutoff <- param_combinations$cutoff[i]
    cache_key <- generate_cache_key(mx_hash, scr, cutoff, "stability")
    cache_keys[i] <- cache_key
    
    # Check cache first
    if (use_cache && exists(cache_key, envir = .moneca_fast_cache)) {
      stability_scores[i] <- get(cache_key, envir = .moneca_fast_cache)
      cache_hits <- cache_hits + 1
    } else {
      combinations_to_compute <- c(combinations_to_compute, i)
    }
  }
  
  # Compute missing stability scores
  if (length(combinations_to_compute) > 0) {
    if (parallel && requireNamespace("foreach", quietly = TRUE) && 
        requireNamespace("doParallel", quietly = TRUE) && length(combinations_to_compute) > 1) {
      
      # Use parallel processing with foreach %dopar%
      compute_results <- foreach::foreach(
        idx = combinations_to_compute,
        .combine = 'c',
        .multicombine = TRUE,
        .maxcombine = length(combinations_to_compute),
        .errorhandling = 'pass',
        .packages = c("moneca"),
        .export = c("assess_clustering_stability", "assess_clustering_stability_parallel")
      ) %dopar% {
        scr <- param_combinations$scr[idx]
        cutoff <- param_combinations$cutoff[idx]
        
        # Compute stability using the appropriate function
        result <- tryCatch({
          # Use the parallel stability function if available, otherwise sequential
          if (exists("assess_clustering_stability_parallel", mode = "function")) {
            score <- assess_clustering_stability_parallel(
              mx = mx,
              small.cell.reduction = scr,
              cut.off = cutoff,
              n_bootstrap = n_bootstrap,
              segment.levels = 2,  # Faster with fewer levels
              parallel = FALSE,  # Avoid nested parallelism
              verbose = FALSE
            )
          } else {
            score <- assess_clustering_stability(
              mx = mx,
              small.cell.reduction = scr,
              cut.off = cutoff,
              n_bootstrap = n_bootstrap,
              segment.levels = 2
            )
          }
          list(index = idx, score = score, error = NULL)
        }, error = function(e) {
          list(index = idx, score = 0, error = e$message)
        })
        
        return(result)
      }
      
      # Process parallel results
      for (result in compute_results) {
        if (is.list(result) && !is.null(result$index)) {
          idx <- result$index
          stability_scores[idx] <- result$score
          
          # Store in cache if enabled and computation was successful
          if (use_cache && result$score > 0) {
            assign(cache_keys[idx], result$score, envir = .moneca_fast_cache)
          }
        }
      }
      
    } else {
      # Sequential processing fallback
      for (i in combinations_to_compute) {
        scr <- param_combinations$scr[i]
        cutoff <- param_combinations$cutoff[i]
        
        # Compute stability
        tryCatch({
          if (parallel && exists("assess_clustering_stability_parallel", mode = "function")) {
            score <- assess_clustering_stability_parallel(
              mx = mx,
              small.cell.reduction = scr,
              cut.off = cutoff,
              n_bootstrap = n_bootstrap,
              segment.levels = 2,  # Faster with fewer levels
              parallel = TRUE,
              verbose = FALSE
            )
          } else {
            score <- assess_clustering_stability(
              mx = mx,
              small.cell.reduction = scr,
              cut.off = cutoff,
              n_bootstrap = n_bootstrap,
              segment.levels = 2
            )
          }
          
          stability_scores[i] <- score
          
          # Store in cache if enabled
          if (use_cache) {
            assign(cache_keys[i], score, envir = .moneca_fast_cache)
          }
        }, error = function(e) {
          stability_scores[i] <- 0
        })
      }
    }
  }
  
  if (verbose && use_cache) {
    message(sprintf("Stability cache performance: %d/%d hits (%.1f%% hit rate)", 
                   cache_hits, n_combinations, 
                   100 * cache_hits / n_combinations))
  }
  
  if (verbose && parallel && length(combinations_to_compute) > 0) {
    message(sprintf("Parallel computation: %d stability assessments computed using %s", 
                   length(combinations_to_compute),
                   if (requireNamespace("doParallel", quietly = TRUE)) "doParallel" else "sequential fallback"))
  }
  
  return(stability_scores)
}

#' Smart Parameter Combination Ordering
#'
#' Orders parameter combinations for evaluation to prioritize promising
#' combinations and enable early termination of poor performers.
#'
#' @param param_combinations Data frame with scr and cutoff columns.
#' @param mx Mobility matrix for heuristic scoring.
#' @param objectives Character vector of objectives.
#'
#' @return Integer vector of row indices in optimal evaluation order.
#' @keywords internal
smart_evaluation_ordering <- function(param_combinations, mx, objectives) {
  
  n_combinations <- nrow(param_combinations)
  
  # Heuristic scoring based on parameter ranges and data characteristics
  # This is a fast approximation to guide evaluation order
  
  # Data characteristics
  mx_off_diag <- mx
  diag(mx_off_diag) <- 0
  non_zero_values <- mx_off_diag[mx_off_diag > 0]
  
  if (length(non_zero_values) == 0) {
    return(seq_len(n_combinations))
  }
  
  median_count <- median(non_zero_values)
  
  # Compute heuristic scores
  heuristic_scores <- numeric(n_combinations)
  
  for (i in seq_len(n_combinations)) {
    scr <- param_combinations$scr[i]
    cutoff <- param_combinations$cutoff[i]
    
    # Penalize extreme values
    scr_penalty <- if (scr > median_count * 2) {
      exp(-(scr - median_count)^2 / (2 * median_count^2))
    } else {
      1
    }
    
    cutoff_penalty <- exp(-((cutoff - 1.5)^2) / 0.5)  # Prefer cutoff around 1.5
    
    # Combine penalties (higher score = more promising)
    heuristic_scores[i] <- scr_penalty * cutoff_penalty
  }
  
  # Return indices ordered by decreasing heuristic score (most promising first)
  return(order(heuristic_scores, decreasing = TRUE))
}

#' Optimized Parameter Combination Evaluation
#'
#' Evaluates parameter combinations using batch processing, caching,
#' and smart ordering for optimal performance.
#'
#' @param mx Mobility matrix.
#' @param param_combinations Data frame with scr and cutoff columns.
#' @param objectives Character vector of objectives to evaluate.
#' @param weights Numeric vector of objective weights.
#' @param n_bootstrap Number of bootstrap samples for stability.
#' @param use_cache Logical indicating whether to use caching.
#' @param smart_ordering Logical indicating whether to use smart evaluation ordering.
#' @param early_termination Logical indicating whether to enable early termination.
#' @param termination_threshold Fraction of combinations after which to consider early termination.
#' @param parallel Logical indicating whether to use parallel processing.
#' @param verbose Logical indicating whether to show progress.
#'
#' @return List containing detailed scores and optimal combination info.
#' @keywords internal
evaluate_parameter_combinations_optimized <- function(mx, param_combinations, 
                                                      objectives, weights, 
                                                      n_bootstrap = 50,
                                                      use_cache = TRUE,
                                                      smart_ordering = TRUE,
                                                      early_termination = TRUE,
                                                      termination_threshold = 0.5,
                                                      parallel = FALSE, 
                                                      verbose = FALSE) {
  
  start_time <- Sys.time()
  n_combinations <- nrow(param_combinations)
  
  # Performance monitoring
  perf_stats <- list(
    n_combinations = n_combinations,
    cache_enabled = use_cache,
    smart_ordering = smart_ordering,
    early_termination = early_termination,
    early_terminated = FALSE,  # Initialize to FALSE
    combinations_evaluated = n_combinations  # Will be updated if early termination occurs
  )
  
  if (verbose) {
    message(sprintf("Optimized evaluation of %d parameter combinations...", n_combinations))
    message(sprintf("Settings: cache=%s, smart_ordering=%s, early_termination=%s", 
                   use_cache, smart_ordering, early_termination))
  }
  
  # Determine evaluation order
  if (smart_ordering) {
    evaluation_order <- smart_evaluation_ordering(param_combinations, mx, objectives)
    if (verbose) {
      message("Using smart evaluation ordering based on heuristic scores")
    }
  } else {
    evaluation_order <- seq_len(n_combinations)
  }
  
  # Initialize results storage
  detailed_scores <- vector("list", n_combinations)
  combined_scores <- numeric(n_combinations)
  
  # Split objectives into batch-processable and individual ones
  batch_objectives <- intersect(objectives, c("quality", "sparsity", "modularity"))
  stability_needed <- "stability" %in% objectives
  
  # Batch process weight matrices for batch-processable objectives
  if (length(batch_objectives) > 0) {
    if (verbose) {
      message("Batch computing weight matrices for quality metrics...")
    }
    
    weight_matrices <- batch_compute_weight_matrices(
      mx = mx,
      param_combinations = param_combinations,
      use_cache = use_cache,
      parallel = parallel,
      verbose = verbose
    )
    
    # Batch compute quality metrics
    if (verbose) {
      message("Batch computing quality metrics...")
    }
    
    quality_results <- batch_compute_quality_metrics(
      weight_matrices = weight_matrices,
      objectives = batch_objectives,
      parallel = parallel,
      verbose = verbose
    )
  } else {
    quality_results <- NULL
  }
  
  # Batch process stability if needed
  stability_results <- NULL
  if (stability_needed) {
    if (verbose) {
      message("Batch computing stability assessments...")
    }
    
    stability_results <- batch_assess_stability(
      mx = mx,
      param_combinations = param_combinations,
      n_bootstrap = n_bootstrap,
      use_cache = use_cache,
      parallel = parallel,
      verbose = verbose
    )
  }
  
  # Combine results and compute final scores
  if (verbose) {
    message("Combining results and computing final scores...")
  }
  
  best_score <- -Inf
  best_idx <- 1
  scores_computed <- 0
  
  for (i in evaluation_order) {
    scores_computed <- scores_computed + 1
    
    # Extract scores for this combination
    obj_scores <- numeric(length(objectives))
    names(obj_scores) <- objectives
    
    # Fill in batch-computed scores
    if (!is.null(quality_results)) {
      for (obj in batch_objectives) {
        if (obj %in% colnames(quality_results)) {
          obj_scores[obj] <- quality_results[i, obj]
        }
      }
    }
    
    # Fill in stability scores
    if (stability_needed && !is.null(stability_results)) {
      obj_scores["stability"] <- stability_results[i]
    }
    
    # Compute combined score
    combined_score <- sum(obj_scores * weights)
    combined_scores[i] <- combined_score
    
    # Store detailed results
    detailed_scores[[i]] <- c(
      scr = param_combinations$scr[i],
      cutoff = param_combinations$cutoff[i],
      obj_scores,
      combined = combined_score
    )
    
    # Update best score
    if (combined_score > best_score) {
      best_score <- combined_score
      best_idx <- i
    }
    
    # Early termination check
    if (early_termination && scores_computed >= n_combinations * termination_threshold) {
      # Check if we've found a clearly superior solution
      current_scores <- combined_scores[evaluation_order[1:scores_computed]]
      current_scores <- current_scores[current_scores != 0]  # Remove uncomputed scores
      
      if (length(current_scores) > 10) {
        # If best score is significantly better than 75th percentile, consider early termination
        percentile_75 <- quantile(current_scores, 0.75, na.rm = TRUE)
        if (best_score > percentile_75 * 1.2) {
          if (verbose) {
            message(sprintf("Early termination: evaluated %d/%d combinations (%.1f%%)", 
                           scores_computed, n_combinations, 
                           100 * scores_computed / n_combinations))
          }
          perf_stats$early_terminated <- TRUE
          perf_stats$combinations_evaluated <- scores_computed
          break
        }
      }
    }
  }
  
  # Fill in remaining scores if early termination didn't occur
  if (scores_computed < n_combinations) {
    remaining_indices <- evaluation_order[(scores_computed + 1):n_combinations]
    for (i in remaining_indices) {
      detailed_scores[[i]] <- c(
        scr = param_combinations$scr[i],
        cutoff = param_combinations$cutoff[i],
        rep(0, length(objectives)),  # Zero scores for unevaluated
        combined = 0
      )
      names(detailed_scores[[i]]) <- c("scr", "cutoff", objectives, "combined")
    }
  }
  
  end_time <- Sys.time()
  computation_time <- as.numeric(end_time - start_time)
  
  # Performance statistics
  perf_stats$computation_time <- computation_time
  perf_stats$combinations_evaluated <- scores_computed
  perf_stats$evaluation_rate <- scores_computed / computation_time
  
  # Update early termination flag if it occurred
  if (scores_computed < n_combinations) {
    perf_stats$early_terminated <- TRUE
  }
  
  if (verbose) {
    message(sprintf("Evaluation complete: %d combinations in %.2f seconds (%.1f combinations/sec)", 
                   scores_computed, computation_time, perf_stats$evaluation_rate))
  }
  
  return(list(
    detailed_scores = detailed_scores,
    combined_scores = combined_scores,
    best_idx = best_idx,
    best_score = best_score,
    performance_stats = perf_stats,
    evaluation_order = evaluation_order
  ))
}

#' Legacy Evaluate Parameter Combination (maintained for compatibility)
#'
#' Computes objective scores for a specific parameter combination.
#' This function is maintained for backward compatibility but the optimized
#' batch evaluation function is recommended for better performance.
#'
#' @param mx Mobility matrix.
#' @param scr small.cell.reduction value.
#' @param cutoff cut.off value.
#' @param objectives Character vector of objectives to evaluate.
#' @param n_bootstrap Number of bootstrap samples for stability.
#' @param parallel Logical indicating whether to use parallel processing.
#' @param verbose Logical indicating whether to show messages.
#'
#' @return Numeric vector of objective scores.
#' @keywords internal
evaluate_parameter_combination <- function(mx, scr, cutoff, 
                                         objectives, n_bootstrap = 50, 
                                         parallel = FALSE, verbose = FALSE) {
  
  # Convert to batch format for compatibility
  param_combinations <- data.frame(scr = scr, cutoff = cutoff)
  weights <- rep(1, length(objectives))
  
  # Use optimized evaluation
  result <- evaluate_parameter_combinations_optimized(
    mx = mx,
    param_combinations = param_combinations,
    objectives = objectives,
    weights = weights,
    n_bootstrap = n_bootstrap,
    use_cache = TRUE,
    smart_ordering = FALSE,  # Single combination
    early_termination = FALSE,
    parallel = parallel,
    verbose = verbose
  )
  
  # Extract scores for the single combination
  return(result$detailed_scores[[1]][objectives])
}

#' Analyze Mathematical Relationship Between Parameters
#'
#' Analyzes how small.cell.reduction and cut.off interact mathematically
#' to affect network properties and clustering outcomes.
#'
#' @param mx Mobility matrix.
#' @param scr small.cell.reduction value.
#' @param cutoff cut.off value.
#' @param n_samples Number of parameter samples for interaction analysis.
#'
#' @return List containing:
#'   \item{interaction_strength}{Measure of parameter interaction (0-1)}
#'   \item{marginal_effects}{Individual parameter effects}
#'   \item{joint_effect}{Combined effect on network density}
#'   \item{optimal_ratio}{Suggested ratio of scr to cutoff}
#'   \item{sensitivity}{Parameter sensitivity analysis}
#'
#' @details
#' The function analyzes:
#' - How changes in each parameter affect network properties
#' - Whether parameters have synergistic or antagonistic effects
#' - The optimal balance between the two filtering stages
#' - Sensitivity of outcomes to parameter perturbations
#'
#' @examples
#' \dontrun{
#' mobility_data <- generate_mobility_data(n_classes = 5)
#' interaction <- analyze_parameter_interaction(
#'   mx = mobility_data,
#'   scr = 2,
#'   cutoff = 1.5
#' )
#' print(interaction)
#' }
#'
#' @export
analyze_parameter_interaction <- function(mx, scr, cutoff, n_samples = 20) {
  
  # Create small perturbations around current values
  scr_range_continuous <- c(max(0, scr * 0.5), scr * 1.5)
  cutoff_range <- c(max(0.5, cutoff * 0.5), cutoff * 1.5)
  
  # Generate integer samples for SCR
  scr_samples <- generate_integer_scr_grid(scr_range_continuous, n_samples, 
                                          include_boundaries = TRUE)
  cutoff_samples <- seq(cutoff_range[1], cutoff_range[2], length.out = n_samples)
  
  # Update n_samples to actual grid sizes for matrix creation
  n_scr_samples <- length(scr_samples)
  n_cutoff_samples <- length(cutoff_samples)
  
  # Compute network properties for parameter grid (vectorized)
  # VECTORIZATION STRATEGY: Replace nested loops with vectorized batch operations
  # Uses more efficient parameter combination handling and pre-allocated results
  
  # Pre-allocate density matrix for better memory management
  density_matrix <- matrix(NA_real_, nrow = n_scr_samples, ncol = n_cutoff_samples)
  
  # VECTORIZED BATCH APPROACH: Process parameters in row-wise chunks
  # This is more memory efficient than expanding full grid for large parameter spaces
  
  # Vectorized computation using outer-like approach with better memory management
  for (i in seq_len(n_scr_samples)) {
    scr_val <- scr_samples[i]
    
    # Vectorize cutoff processing for this SCR value
    cutoff_densities <- vapply(cutoff_samples, function(cutoff_val) {
      tryCatch({
        wm <- weight.matrix(
          mx = mx,
          cut.off = cutoff_val,
          small.cell.reduction = scr_val
        )
        sum(!is.na(wm)) / length(wm)
      }, error = function(e) {
        NA_real_
      })
    }, FUN.VALUE = numeric(1), USE.NAMES = FALSE)
    
    # Assign entire row at once (vectorized assignment)
    density_matrix[i, ] <- cutoff_densities
  }
  
  # Analyze marginal effects
  marginal_scr <- rowMeans(density_matrix, na.rm = TRUE)
  marginal_cutoff <- colMeans(density_matrix, na.rm = TRUE)
  
  # Compute interaction strength (deviation from additivity)
  expected_additive <- outer(marginal_scr, marginal_cutoff, "+") / 2
  actual <- density_matrix
  interaction_strength <- 1 - cor(as.vector(expected_additive), 
                                 as.vector(actual), 
                                 use = "complete.obs")
  
  # Sensitivity analysis
  scr_sensitivity <- sd(marginal_scr, na.rm = TRUE) / mean(marginal_scr, na.rm = TRUE)
  cutoff_sensitivity <- sd(marginal_cutoff, na.rm = TRUE) / mean(marginal_cutoff, na.rm = TRUE)
  
  # Find optimal ratio
  center_scr_idx <- max(1, n_scr_samples %/% 2)
  center_cutoff_idx <- max(1, n_cutoff_samples %/% 2)
  
  # Handle case where matrix might be empty or have NA values
  if (nrow(density_matrix) > 0 && ncol(density_matrix) > 0 && 
      center_scr_idx <= nrow(density_matrix) && 
      center_cutoff_idx <= ncol(density_matrix)) {
    optimal_density <- density_matrix[center_scr_idx, center_cutoff_idx]
  } else {
    optimal_density <- NA
  }
  
  # Search for same density with different ratios (vectorized)
  # VECTORIZATION STRATEGY: Replace nested loop with which() + array indices
  # Use logical indexing and vectorized operations instead of explicit loops
  ratio_analysis <- numeric(0)
  if (!is.na(optimal_density) && n_scr_samples > 0 && n_cutoff_samples > 0) {
    # Find all matrix positions where density matches optimal (replaces nested loop)
    density_matches <- which(
      !is.na(density_matrix) & 
      abs(density_matrix - optimal_density) < 0.01, 
      arr.ind = TRUE
    )
    
    if (nrow(density_matches) > 0) {
      # Extract corresponding parameter values using vectorized indexing
      scr_match_values <- scr_samples[density_matches[, "row"]]
      cutoff_match_values <- cutoff_samples[density_matches[, "col"]]
      
      # Filter and compute ratios using vectorized operations
      valid_cutoffs <- cutoff_match_values > 0
      if (any(valid_cutoffs)) {
        ratio_analysis <- scr_match_values[valid_cutoffs] / cutoff_match_values[valid_cutoffs]
      }
    }
  }
  
  optimal_ratio <- if (length(ratio_analysis) > 0) {
    median(ratio_analysis)
  } else {
    scr / cutoff
  }
  
  result <- list(
    interaction_strength = interaction_strength,
    marginal_effects = list(
      scr = marginal_scr,
      cutoff = marginal_cutoff
    ),
    joint_effect = if (!is.na(optimal_density)) optimal_density else 0,
    optimal_ratio = optimal_ratio,
    sensitivity = list(
      scr = scr_sensitivity,
      cutoff = cutoff_sensitivity,
      relative = scr_sensitivity / cutoff_sensitivity
    ),
    density_surface = density_matrix
  )
  
  class(result) <- "parameter_interaction"
  return(result)
}

#' Visualize Parameter Optimization Surface
#'
#' Creates visualization of the 2D parameter optimization surface.
#'
#' @param joint_tuning_result Result from auto_tune_joint_parameters.
#' @param type Character string specifying plot type: "heatmap", "contour", 
#'   or "3d". Default is "heatmap".
#' @param show_optimal Logical indicating whether to mark optimal point.
#'
#' @return A ggplot2 object (for heatmap/contour) or plotly object (for 3d).
#'
#' @examples
#' \dontrun{
#' result <- auto_tune_joint_parameters(mobility_data)
#' plot_optimization_surface(result, type = "contour")
#' }
#'
#' @export
plot_optimization_surface <- function(joint_tuning_result, 
                                     type = "heatmap",
                                     show_optimal = TRUE) {
  
  if (!inherits(joint_tuning_result, "moneca_joint_tuning")) {
    stop("Input must be a moneca_joint_tuning object")
  }
  
  # Prepare data for plotting
  surface_data <- expand.grid(
    scr = joint_tuning_result$scr_values,
    cutoff = joint_tuning_result$cutoff_values
  )
  surface_data$score <- as.vector(t(joint_tuning_result$optimization_surface))
  
  # Create plot based on type
  if (type == "heatmap") {
    p <- ggplot(surface_data, aes(x = scr, y = cutoff, fill = score)) +
      geom_tile() +
      scale_fill_viridis_c(name = "Score") +
      labs(
        x = "small.cell.reduction",
        y = "cut.off",
        title = "Parameter Optimization Surface"
      ) +
      theme_minimal()
    
    if (show_optimal) {
      p <- p + 
        geom_point(
          aes(x = joint_tuning_result$optimal_scr,
              y = joint_tuning_result$optimal_cutoff),
          color = "red", size = 4, shape = 4, stroke = 2
        ) +
        annotate(
          "text",
          x = joint_tuning_result$optimal_scr,
          y = joint_tuning_result$optimal_cutoff,
          label = "Optimal",
          vjust = -1, color = "red"
        )
    }
    
  } else if (type == "contour") {
    p <- ggplot(surface_data, aes(x = scr, y = cutoff, z = score)) +
      geom_contour_filled() +
      geom_contour(color = "white", alpha = 0.5) +
      labs(
        x = "small.cell.reduction",
        y = "cut.off",
        title = "Parameter Optimization Contours"
      ) +
      theme_minimal()
    
    if (show_optimal) {
      p <- p + 
        geom_point(
          aes(x = joint_tuning_result$optimal_scr,
              y = joint_tuning_result$optimal_cutoff),
          color = "red", size = 4, shape = 4, stroke = 2
        )
    }
    
  } else if (type == "3d") {
    # Check if plotly is available
    if (!requireNamespace("plotly", quietly = TRUE)) {
      stop("Package 'plotly' is required for 3D plots. Install with: install.packages('plotly')")
    }
    
    p <- plotly::plot_ly(
      x = joint_tuning_result$scr_values,
      y = joint_tuning_result$cutoff_values,
      z = joint_tuning_result$optimization_surface,
      type = "surface",
      colorscale = "Viridis"
    ) %>%
      plotly::layout(
        scene = list(
          xaxis = list(title = "small.cell.reduction"),
          yaxis = list(title = "cut.off"),
          zaxis = list(title = "Score")
        ),
        title = "3D Parameter Optimization Surface"
      )
    
    if (show_optimal) {
      p <- p %>%
        plotly::add_trace(
          x = joint_tuning_result$optimal_scr,
          y = joint_tuning_result$optimal_cutoff,
          z = max(joint_tuning_result$optimization_surface, na.rm = TRUE),
          type = "scatter3d",
          mode = "markers",
          marker = list(color = "red", size = 10),
          name = "Optimal"
        )
    }
  } else {
    stop("Type must be one of: 'heatmap', 'contour', '3d'")
  }
  
  return(p)
}

#' Suggest Parameter Ranges Based on Data Characteristics
#'
#' Analyzes mobility matrix to suggest appropriate ranges for parameter tuning.
#'
#' @param mx Mobility matrix.
#' @param method Character string: "conservative", "moderate", or "aggressive".
#'
#' @return List containing suggested ranges for both parameters.
#'
#' @export
suggest_parameter_ranges <- function(mx, method = "moderate") {
  
  # Use integer-aware bounds for SCR
  scr_bounds <- suggest_integer_scr_bounds(mx, method)
  scr_range <- scr_bounds
  
  # Keep existing logic for cutoff range
  mx_off_diag <- mx
  diag(mx_off_diag) <- 0
  non_zero_values <- mx_off_diag[mx_off_diag > 0]
  
  if (length(non_zero_values) == 0) {
    warning("No non-zero off-diagonal values found")
    return(list(
      scr_range = scr_range,
      cutoff_range = c(0.5, 2)
    ))
  }
  
  # Compute quantiles for data characteristics
  q <- quantile(non_zero_values, probs = c(0.1, 0.25, 0.5, 0.75, 0.9))
  
  # Suggest cutoff ranges based on method (unchanged logic)
  if (method == "conservative") {
    cutoff_range <- c(0.8, 1.5)
  } else if (method == "moderate") {
    cutoff_range <- c(0.5, 2.0)
  } else if (method == "aggressive") {
    cutoff_range <- c(0.3, 3.0)
  } else {
    stop("Method must be 'conservative', 'moderate', or 'aggressive'")
  }
  
  # Compute expected relative risks for cutoff calibration
  row_sums <- rowSums(mx)
  col_sums <- colSums(mx)
  total <- sum(mx)
  
  # Sample some expected values
  sample_expected <- outer(row_sums[1:min(5, nrow(mx))], 
                          col_sums[1:min(5, ncol(mx))]) / total
  median_expected <- median(sample_expected[sample_expected > 0])
  
  # Adjust cutoff range based on expected values
  if (median_expected > 0) {
    rr_factor <- 1 / median_expected
    cutoff_range <- cutoff_range * sqrt(rr_factor)  # Square root for moderation
  }
  
  return(list(
    scr_range = scr_range,
    cutoff_range = cutoff_range,
    data_characteristics = list(
      matrix_size = nrow(mx),
      sparsity = 1 - length(non_zero_values) / length(mx_off_diag),
      value_distribution = q,
      suggested_method = method
    )
  ))
}

#' Print Method for Joint Tuning Results
#'
#' @param x Object of class "moneca_joint_tuning".
#' @param ... Additional arguments (unused).
#'
#' @export
print.moneca_joint_tuning <- function(x, ...) {
  cat("MONECA Joint Parameter Tuning Results\n")
  cat("=====================================\n\n")
  
  cat("Optimal Parameters:\n")
  cat("  small.cell.reduction:", x$optimal_scr, "\n")
  cat("  cut.off:", x$optimal_cutoff, "\n\n")
  
  cat("Optimization Details:\n")
  cat("  Method:", x$method, "\n")
  cat("  Objectives:", paste(x$objectives, collapse = ", "), "\n")
  cat("  Computation time:", round(x$computation_time, 2), "seconds\n\n")
  
  cat("Selection rationale:", x$selection_rationale, "\n\n")
  
  # Performance information
  if (!is.null(x$performance_stats)) {
    cat("Performance Statistics:\n")
    stats <- x$performance_stats
    cat(sprintf("  Combinations evaluated: %d/%d (%.1f%%)\n", 
               stats$combinations_evaluated, stats$n_combinations,
               100 * stats$combinations_evaluated / stats$n_combinations))
    cat(sprintf("  Evaluation rate: %.1f combinations/second\n", 
               stats$evaluation_rate))
    
    if (!is.null(stats$early_terminated) && stats$early_terminated) {
      cat("  Early termination: YES (optimization converged)\n")
    }
    
    if (stats$cache_enabled) {
      cat("  Caching: ENABLED\n")
    }
    cat("\n")
  }
  
  if (!is.null(x$mathematical_relationship)) {
    cat("Parameter Interaction Analysis:\n")
    cat("  Interaction strength:", 
        round(x$mathematical_relationship$interaction_strength, 3), "\n")
    cat("  Optimal ratio (scr/cutoff):", 
        round(x$mathematical_relationship$optimal_ratio, 2), "\n")
    cat("  Relative sensitivity:", 
        round(x$mathematical_relationship$sensitivity$relative, 2), "\n")
  }
}

#' Print Method for Parameter Interaction Analysis
#'
#' @param x Object of class "parameter_interaction".
#' @param ... Additional arguments (unused).
#'
#' @export
print.parameter_interaction <- function(x, ...) {
  cat("Parameter Interaction Analysis\n")
  cat("==============================\n\n")
  
  cat("Interaction strength:", round(x$interaction_strength, 3), "\n")
  cat("  (0 = independent, 1 = strongly interacting)\n\n")
  
  cat("Joint effect on network density:", round(x$joint_effect, 3), "\n")
  cat("Optimal parameter ratio (scr/cutoff):", round(x$optimal_ratio, 2), "\n\n")
  
  cat("Sensitivity Analysis:\n")
  cat("  small.cell.reduction:", round(x$sensitivity$scr, 3), "\n")
  cat("  cut.off:", round(x$sensitivity$cutoff, 3), "\n")
  cat("  Relative sensitivity:", round(x$sensitivity$relative, 2), "\n")
  
  if (x$sensitivity$relative > 1.5) {
    cat("\nNote: small.cell.reduction is significantly more sensitive\n")
  } else if (x$sensitivity$relative < 0.67) {
    cat("\nNote: cut.off is significantly more sensitive\n")
  } else {
    cat("\nNote: Both parameters have similar sensitivity\n")
  }
}

# ============================================================================
# UTILITY FUNCTIONS FOR INTEGER CONSTRAINTS
# ============================================================================

#' Ensure Integer Values for small.cell.reduction Parameter
#'
#' Converts continuous small.cell.reduction values to integers, since the
#' parameter operates on integer mobility counts. Non-integer values are
#' mathematically equivalent to their floor values.
#'
#' @param scr_values Numeric vector of small.cell.reduction values.
#' @param warn_if_rounded Logical indicating whether to warn when rounding occurs.
#' @param min_value Minimum allowed integer value (default 0).
#' @param max_value Maximum allowed integer value (default NULL for no limit).
#'
#' @return Integer vector with unique values, sorted in ascending order.
#' @keywords internal
ensure_integer_scr_values <- function(scr_values, warn_if_rounded = TRUE, 
                                     min_value = 0, max_value = NULL) {
  
  if (length(scr_values) == 0) {
    return(integer(0))
  }
  
  # Check for non-integer values
  non_integer_present <- any(scr_values != floor(scr_values))
  
  if (non_integer_present && warn_if_rounded) {
    warning("small.cell.reduction values must be integers (operates on count data). ",
            "Non-integer values will be rounded down to nearest integer.")
  }
  
  # Convert to integers (floor to be conservative)
  integer_values <- as.integer(floor(scr_values))
  
  # Apply bounds
  if (!is.null(min_value)) {
    integer_values <- pmax(integer_values, min_value)
  }
  
  if (!is.null(max_value)) {
    integer_values <- pmin(integer_values, max_value)
  }
  
  # Remove duplicates and sort
  unique_values <- sort(unique(integer_values))
  
  return(unique_values)
}

#' Generate Integer Grid for small.cell.reduction Parameter
#'
#' Creates a grid of integer values for small.cell.reduction parameter,
#' ensuring efficient coverage of the meaningful parameter space.
#'
#' @param scr_range Numeric vector of length 2 specifying SCR range.
#' @param n_points Desired number of grid points.
#' @param include_boundaries Logical indicating whether to always include
#'   range boundaries. Default TRUE.
#'
#' @return Integer vector of unique SCR values.
#' @keywords internal
generate_integer_scr_grid <- function(scr_range, n_points, 
                                     include_boundaries = TRUE) {
  
  # Ensure range is integers
  scr_min <- max(0, floor(scr_range[1]))
  scr_max <- ceiling(scr_range[2])
  
  # If range is very small, just return all integers in range
  range_size <- scr_max - scr_min + 1
  if (range_size <= n_points) {
    return(seq(scr_min, scr_max, by = 1))
  }
  
  # Generate approximately n_points integer values
  if (include_boundaries) {
    # Always include min and max
    if (n_points <= 2) {
      return(c(scr_min, scr_max))
    }
    
    # Generate interior points
    interior_points <- n_points - 2
    if (interior_points > 0) {
      step_size <- (scr_max - scr_min) / (interior_points + 1)
      interior_values <- scr_min + step_size * seq_len(interior_points)
      interior_integers <- unique(round(interior_values))
      
      # Combine all values
      all_values <- c(scr_min, interior_integers, scr_max)
    } else {
      all_values <- c(scr_min, scr_max)
    }
  } else {
    # Evenly spaced across the range
    step_size <- (scr_max - scr_min) / (n_points - 1)
    continuous_values <- scr_min + step_size * seq(0, n_points - 1)
    all_values <- round(continuous_values)
  }
  
  # Ensure unique integers within bounds
  unique_values <- unique(all_values)
  unique_values <- unique_values[unique_values >= scr_min & 
                               unique_values <= scr_max]
  
  return(sort(as.integer(unique_values)))
}

#' Suggest Realistic Integer Bounds for small.cell.reduction
#'
#' Analyzes mobility matrix to suggest meaningful integer bounds for
#' small.cell.reduction parameter based on actual count distribution.
#'
#' @param mx Mobility matrix.
#' @param method Character string: "conservative", "moderate", or "aggressive".
#' @param max_reasonable Maximum reasonable SCR value as multiple of median.
#'
#' @return Integer vector of length 2 with suggested bounds.
#' @keywords internal
suggest_integer_scr_bounds <- function(mx, method = "moderate", 
                                      max_reasonable = 10) {
  
  # Remove diagonal for analysis
  mx_off_diag <- mx
  diag(mx_off_diag) <- 0
  
  # Get integer counts
  integer_counts <- mx_off_diag[mx_off_diag > 0]
  integer_counts <- as.integer(floor(integer_counts))
  
  if (length(integer_counts) == 0) {
    warning("No non-zero off-diagonal values found")
    return(c(0L, 10L))
  }
  
  # Get unique integer values that actually exist in data
  unique_counts <- sort(unique(integer_counts))
  
  # Compute statistics on integer counts
  q <- quantile(integer_counts, probs = c(0.1, 0.25, 0.5, 0.75, 0.9))
  median_count <- median(integer_counts)
  
  # Determine upper bound based on method
  if (method == "conservative") {
    # Don't go beyond 25th percentile
    upper_bound <- max(1L, as.integer(floor(q["25%"])))
  } else if (method == "moderate") {
    # Go up to median or a reasonable multiple
    upper_bound <- max(1L, as.integer(floor(min(q["50%"], median_count * 2))))
  } else if (method == "aggressive") {
    # Go up to 75th percentile or reasonable multiple
    upper_bound <- max(1L, as.integer(floor(min(q["75%"], median_count * max_reasonable))))
  } else {
    stop("Method must be 'conservative', 'moderate', or 'aggressive'")
  }
  
  # Ensure we don't exceed the maximum count in the data
  max_data_count <- max(integer_counts)
  upper_bound <- min(upper_bound, max_data_count)
  
  return(c(0L, upper_bound))
}

# Helper function for Pareto frontier optimization
optimize_pareto_frontier <- function(mx, scr_range, cutoff_range, n_grid_points,
                                    objectives, verbose, parallel) {
  
  if (verbose) {
    cat("Computing Pareto frontier for multi-objective optimization...\n")
  }
  
  # Set up parallel processing if needed
  cluster_setup <- NULL
  if (parallel) {
    cluster_setup <- setup_parallel_cluster(verbose = verbose)
    on.exit({
      if (!is.null(cluster_setup)) cleanup_parallel_cluster(cluster_setup, verbose = verbose)
    })
  }
  
  # First get all objective scores
  grid_result <- optimize_grid_search(
    mx = mx,
    scr_range = scr_range,
    cutoff_range = cutoff_range,
    n_grid_points = n_grid_points,
    n_bootstrap = 30,
    objectives = objectives,
    weights = rep(1/length(objectives), length(objectives)),
    verbose = FALSE,
    parallel = parallel
  )
  
  # Extract objective scores for Pareto analysis
  scores_matrix <- do.call(rbind, lapply(grid_result$scores, function(x) {
    x[objectives]
  }))
  
  # Find Pareto optimal solutions
  pareto_indices <- find_pareto_optimal(scores_matrix)
  
  # Select knee point from Pareto frontier
  if (length(pareto_indices) > 1) {
    # Find knee point (maximum distance from line connecting endpoints)
    knee_idx <- find_knee_point(scores_matrix[pareto_indices, ])
    optimal_idx <- pareto_indices[knee_idx]
  } else {
    optimal_idx <- pareto_indices[1]
  }
  
  optimal_params <- grid_result$parameter_grid[optimal_idx, ]
  
  result <- list(
    optimal_scr = optimal_params$scr,
    optimal_cutoff = optimal_params$cutoff,
    optimization_surface = grid_result$optimization_surface,
    parameter_grid = grid_result$parameter_grid,
    scores = grid_result$scores,
    scr_values = grid_result$scr_values,
    cutoff_values = grid_result$cutoff_values,
    pareto_indices = pareto_indices,
    selection_rationale = sprintf(
      "Selected knee point from Pareto frontier (%d Pareto-optimal solutions)",
      length(pareto_indices)
    )
  )
  
  return(result)
}

# Helper function for Bayesian optimization
optimize_bayesian_joint <- function(mx, scr_range, cutoff_range,
                                   objectives, weights, verbose, parallel = FALSE) {
  
  # Check if required package is available
  if (!requireNamespace("DiceKriging", quietly = TRUE)) {
    stop("Bayesian optimization requires 'DiceKriging' package. ",
         "Install with: install.packages('DiceKriging')")
  }
  
  if (verbose) {
    cat("Starting Bayesian optimization...\n")
  }
  
  # Set up parallel processing if needed (though Bayesian optimization runs sequentially)
  cluster_setup <- NULL
  if (parallel) {
    cluster_setup <- setup_parallel_cluster(verbose = verbose)
    on.exit({
      if (!is.null(cluster_setup)) cleanup_parallel_cluster(cluster_setup, verbose = verbose)
    })
  }
  
  # Initial sampling (Latin Hypercube)
  n_initial <- 10
  initial_design <- lhs_sample(
    n = n_initial,
    bounds = list(scr = scr_range, cutoff = cutoff_range)
  )
  
  # Evaluate initial points
  initial_scores <- numeric(n_initial)
  for (i in seq_len(n_initial)) {
    obj_scores <- evaluate_parameter_combination(
      mx = mx,
      scr = initial_design[i, 1],
      cutoff = initial_design[i, 2],
      objectives = objectives,
      n_bootstrap = 30,
      parallel = FALSE,  # Sequential for Bayesian optimization
      verbose = FALSE
    )
    initial_scores[i] <- sum(obj_scores * weights)
  }
  
  # Fit Gaussian Process model
  gp_model <- DiceKriging::km(
    design = initial_design,
    response = initial_scores,
    covtype = "matern5_2"
  )
  
  # Optimization loop
  n_iterations <- 20
  for (iter in seq_len(n_iterations)) {
    # Find next point to evaluate (maximize expected improvement)
    next_point <- optimize_acquisition(
      gp_model = gp_model,
      bounds = list(scr = scr_range, cutoff = cutoff_range)
    )
    
    # Evaluate new point
    obj_scores <- evaluate_parameter_combination(
      mx = mx,
      scr = next_point[1],
      cutoff = next_point[2],
      objectives = objectives,
      n_bootstrap = 30,
      parallel = FALSE,  # Sequential for Bayesian optimization
      verbose = FALSE
    )
    score <- sum(obj_scores * weights)
    
    # Update GP model
    gp_model <- DiceKriging::update(
      object = gp_model,
      newX = matrix(next_point, nrow = 1),
      newy = score
    )
    
    if (verbose && iter %% 5 == 0) {
      cat("  Iteration", iter, "- Best score:", 
          max(c(initial_scores, score)), "\n")
    }
  }
  
  # Find optimal from all evaluated points
  all_points <- rbind(initial_design, gp_model@X[-seq_len(n_initial), ])
  all_scores <- c(initial_scores, gp_model@y[-seq_len(n_initial)])
  
  optimal_idx <- which.max(all_scores)
  
  result <- list(
    optimal_scr = all_points[optimal_idx, 1],
    optimal_cutoff = all_points[optimal_idx, 2],
    optimization_surface = NULL,  # Not available for Bayesian
    parameter_grid = data.frame(
      scr = all_points[, 1],
      cutoff = all_points[, 2]
    ),
    scores = all_scores,
    selection_rationale = sprintf(
      "Selected via Bayesian optimization (GP with Matérn kernel), score: %.3f",
      all_scores[optimal_idx]
    )
  )
  
  return(result)
}

# Utility functions
find_pareto_optimal <- function(scores) {
  n <- nrow(scores)
  is_dominated <- rep(FALSE, n)
  
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      if (i != j && !is_dominated[i]) {
        # Check if j dominates i
        if (all(scores[j, ] >= scores[i, ]) && 
            any(scores[j, ] > scores[i, ])) {
          is_dominated[i] <- TRUE
          break
        }
      }
    }
  }
  
  return(which(!is_dominated))
}

find_knee_point <- function(pareto_scores) {
  if (nrow(pareto_scores) <= 2) return(1)
  
  # Normalize scores
  normalized <- scale(pareto_scores)
  
  # Find point with maximum distance from line connecting endpoints
  start_point <- normalized[1, ]
  end_point <- normalized[nrow(normalized), ]
  
  distances <- numeric(nrow(normalized))
  for (i in seq_len(nrow(normalized))) {
    distances[i] <- point_line_distance(normalized[i, ], start_point, end_point)
  }
  
  return(which.max(distances))
}

point_line_distance <- function(point, line_start, line_end) {
  # Distance from point to line in n-dimensional space
  v <- line_end - line_start
  w <- point - line_start
  
  c1 <- sum(w * v)
  c2 <- sum(v * v)
  
  if (c2 == 0) return(sqrt(sum(w * w)))
  
  b <- c1 / c2
  pb <- line_start + b * v
  
  return(sqrt(sum((point - pb)^2)))
}

lhs_sample <- function(n, bounds) {
  # Simple Latin Hypercube Sampling
  n_params <- length(bounds)
  design <- matrix(NA, nrow = n, ncol = n_params)
  
  for (i in seq_len(n_params)) {
    perm <- sample(n)
    design[, i] <- bounds[[i]][1] + 
                  (bounds[[i]][2] - bounds[[i]][1]) * (perm - 0.5) / n
  }
  
  colnames(design) <- names(bounds)
  return(design)
}

optimize_acquisition <- function(gp_model, bounds) {
  # Simple optimization of expected improvement
  # (Simplified version - could be improved with proper optimization)
  
  candidate_points <- lhs_sample(100, bounds)
  ei_values <- numeric(nrow(candidate_points))
  
  for (i in seq_len(nrow(candidate_points))) {
    pred <- DiceKriging::predict.km(
      object = gp_model,
      newdata = matrix(candidate_points[i, ], nrow = 1),
      type = "UK"
    )
    
    # Expected improvement calculation
    current_best <- max(gp_model@y)
    improvement <- pred$mean - current_best
    
    if (pred$sd > 0) {
      z <- improvement / pred$sd
      ei_values[i] <- improvement * pnorm(z) + pred$sd * dnorm(z)
    } else {
      ei_values[i] <- 0
    }
  }
  
  best_idx <- which.max(ei_values)
  return(candidate_points[best_idx, ])
}

# ============================================================================
# PARALLEL PROCESSING CLUSTER MANAGEMENT
# ============================================================================

#' Set Up Parallel Cluster for Joint Parameter Tuning
#'
#' Creates and registers a parallel cluster for use in joint parameter optimization.
#' Handles cluster creation, registration, and exports necessary functions.
#'
#' @param n_cores Integer number of cores to use. If NULL, uses available cores - 1.
#' @param verbose Logical indicating whether to show setup messages.
#'
#' @return List containing cluster object and setup information, or NULL if setup fails.
#' @keywords internal
setup_parallel_cluster <- function(n_cores = NULL, verbose = TRUE) {
  
  if (!requireNamespace("parallel", quietly = TRUE) ||
      !requireNamespace("doParallel", quietly = TRUE)) {
    if (verbose) {
      message("Parallel processing packages not available. Using sequential processing.")
    }
    return(NULL)
  }
  
  # Determine number of cores
  if (is.null(n_cores)) {
    n_cores <- max(1, parallel::detectCores() - 1)
  }
  n_cores <- max(1, min(n_cores, parallel::detectCores()))
  
  tryCatch({
    if (verbose) {
      message(sprintf("Setting up parallel cluster with %d cores...", n_cores))
    }
    
    # Create cluster
    cluster <- parallel::makeCluster(n_cores)
    
    # Register with doParallel
    doParallel::registerDoParallel(cluster)
    
    # Export necessary packages and functions to workers
    parallel::clusterEvalQ(cluster, {
      library(moneca)
      NULL
    })
    
    # Test cluster with a simple operation
    test_result <- foreach::foreach(i = 1:n_cores, .combine = 'c') %dopar% {
      i^2
    }
    
    if (length(test_result) != n_cores) {
      stop("Cluster test failed")
    }
    
    if (verbose) {
      message("Parallel cluster setup successful")
    }
    
    return(list(
      cluster = cluster,
      n_cores = n_cores,
      setup_successful = TRUE
    ))
    
  }, error = function(e) {
    if (verbose) {
      message(sprintf("Failed to set up parallel cluster: %s. Using sequential processing.", e$message))
    }
    return(NULL)
  })
}

#' Clean Up Parallel Cluster
#'
#' Stops and cleans up a parallel cluster created by setup_parallel_cluster().
#'
#' @param cluster_setup Cluster setup object returned by setup_parallel_cluster().
#' @param verbose Logical indicating whether to show cleanup messages.
#' @keywords internal
cleanup_parallel_cluster <- function(cluster_setup, verbose = FALSE) {
  
  if (is.null(cluster_setup) || !cluster_setup$setup_successful) {
    return(invisible())
  }
  
  tryCatch({
    if (!is.null(cluster_setup$cluster)) {
      parallel::stopCluster(cluster_setup$cluster)
      if (verbose) {
        message("Parallel cluster cleaned up successfully")
      }
    }
  }, error = function(e) {
    if (verbose) {
      warning(sprintf("Error cleaning up parallel cluster: %s", e$message))
    }
  })
  
  invisible()
}

#' Execute Function with Parallel Processing Safety
#'
#' Wrapper function that ensures proper cluster management and error handling
#' for parallel processing operations.
#'
#' @param fun Function to execute.
#' @param args List of arguments to pass to the function.
#' @param parallel Logical indicating whether to use parallel processing.
#' @param verbose Logical indicating whether to show messages.
#' @param fallback_sequential Logical indicating whether to fall back to sequential processing on error.
#'
#' @return Result of function execution.
#' @keywords internal
execute_with_parallel_safety <- function(fun, args, parallel = FALSE, 
                                        verbose = FALSE, fallback_sequential = TRUE) {
  
  if (!parallel) {
    # Execute sequentially
    return(do.call(fun, args))
  }
  
  # Set up parallel processing
  cluster_setup <- NULL
  tryCatch({
    cluster_setup <- setup_parallel_cluster(verbose = verbose)
    
    if (!is.null(cluster_setup)) {
      # Execute with parallel processing
      result <- do.call(fun, args)
      return(result)
    } else if (fallback_sequential) {
      if (verbose) {
        message("Parallel setup failed. Falling back to sequential processing.")
      }
      args$parallel <- FALSE  # Update args to use sequential
      return(do.call(fun, args))
    } else {
      stop("Parallel setup failed and sequential fallback is disabled")
    }
    
  }, error = function(e) {
    if (verbose) {
      warning(sprintf("Parallel execution failed: %s", e$message))
    }
    
    if (fallback_sequential) {
      if (verbose) {
        message("Falling back to sequential processing.")
      }
      args$parallel <- FALSE
      return(do.call(fun, args))
    } else {
      stop(sprintf("Parallel execution failed: %s", e$message))
    }
    
  }, finally = {
    if (!is.null(cluster_setup)) {
      cleanup_parallel_cluster(cluster_setup, verbose = verbose)
    }
  })
}

# ============================================================================
# MEMORY MANAGEMENT AND PERFORMANCE UTILITIES
# ============================================================================

#' Memory-efficient cache management
#' @keywords internal
manage_cache_memory <- function(max_cache_size = 100) {
  # Clean weight matrix cache if it gets too large
  if (length(ls(.weight_matrix_cache)) > max_cache_size) {
    # Remove oldest entries (simple FIFO approach)
    cache_keys <- ls(.weight_matrix_cache)
    keys_to_remove <- cache_keys[1:(length(cache_keys) - max_cache_size %/% 2)]
    rm(list = keys_to_remove, envir = .weight_matrix_cache)
  }
  
  # Clean moneca_fast cache if it gets too large
  if (length(ls(.moneca_fast_cache)) > max_cache_size) {
    cache_keys <- ls(.moneca_fast_cache)
    keys_to_remove <- cache_keys[1:(length(cache_keys) - max_cache_size %/% 2)]
    rm(list = keys_to_remove, envir = .moneca_fast_cache)
  }
  
  # Force garbage collection
  gc(verbose = FALSE)
}

#' Performance monitoring utilities
#' @keywords internal
log_performance_stats <- function(stats, verbose = TRUE) {
  if (!verbose) return(invisible())
  
  message(sprintf("Performance Summary:"))
  message(sprintf("  - Total combinations: %d", stats$n_combinations))
  message(sprintf("  - Combinations evaluated: %d (%.1f%%)", 
                 stats$combinations_evaluated,
                 100 * stats$combinations_evaluated / stats$n_combinations))
  message(sprintf("  - Computation time: %.2f seconds", stats$computation_time))
  message(sprintf("  - Evaluation rate: %.1f combinations/second", stats$evaluation_rate))
  
  if (!is.null(stats$early_terminated) && stats$early_terminated) {
    message(sprintf("  - Early termination: YES (saved %.1f%% of evaluations)", 
                   100 * (1 - stats$combinations_evaluated / stats$n_combinations)))
  }
  
  if (stats$cache_enabled) {
    message(sprintf("  - Caching: ENABLED (memory optimized)"))
  }
}

# Fast version of moneca for parameter tuning with caching
moneca_fast <- function(mx, segment.levels = 2, cut.off = 1, 
                       small.cell.reduction = 0, progress = FALSE,
                       use_cache = TRUE) {
  # Optimized version of moneca for faster evaluation during tuning
  # Includes caching and memory optimization
  
  # Generate cache key if caching is enabled
  if (use_cache) {
    mx_hash <- simple_matrix_hash(mx)
    cache_key <- generate_cache_key(mx_hash, small.cell.reduction, cut.off, 
                                   paste("moneca_fast", segment.levels, sep = "_"))
    
    # Check cache first
    if (exists(cache_key, envir = .moneca_fast_cache)) {
      return(get(cache_key, envir = .moneca_fast_cache))
    }
  }
  
  segment.list <- list()
  mat.list <- list()
  
  # First level - use optimized weight matrix computation
  tryCatch({
    mx.edges <- weight.matrix(
      mx = mx,
      cut.off = cut.off,
      small.cell.reduction = small.cell.reduction,
      symmetric = FALSE
    )
    
    if (all(is.na(mx.edges))) {
      result <- list(segment.list = list(), mat.list = list())
      if (use_cache) {
        assign(cache_key, result, envir = .moneca_fast_cache)
      }
      return(result)
    }
    
    segments <- find.segments(mx.edges, cut.off = 0)
    
    if (length(segments) == 0) {
      result <- list(segment.list = list(), mat.list = list())
      if (use_cache) {
        assign(cache_key, result, envir = .moneca_fast_cache)
      }
      return(result)
    }
    
    segment.list[[1]] <- segments
    mat.list[[1]] <- mx
    
    # Additional levels if requested (optimized for speed)
    if (segment.levels > 1 && length(segments) > 1) {
      current_mx <- mx
      current_segments <- segments
      
      for (level in 2:min(segment.levels, 3)) {  # Cap at 3 for speed
        mx_seg <- segment.matrix(current_mx, current_segments)
        
        # Early termination conditions
        if (nrow(mx_seg) < 3 || ncol(mx_seg) < 3) break
        
        # Check for sufficient data
        if (sum(mx_seg) < 10) break  # Insufficient data for meaningful segmentation
        
        mx.edges <- weight.matrix(
          mx = mx_seg,
          cut.off = cut.off,
          small.cell.reduction = small.cell.reduction,
          symmetric = FALSE
        )
        
        if (all(is.na(mx.edges))) break
        
        new_segments <- find.segments(mx.edges, cut.off = 0)
        
        # Stop if no meaningful segmentation
        if (length(new_segments) <= 1) break
        
        segment.list[[level]] <- new_segments
        mat.list[[level]] <- mx_seg
        
        current_segments <- new_segments
        current_mx <- mx_seg
      }
    }
    
    result <- list(
      segment.list = segment.list,
      mat.list = mat.list
    )
    
    # Cache the result if caching is enabled
    if (use_cache) {
      assign(cache_key, result, envir = .moneca_fast_cache)
      
      # Periodic cache management
      if (runif(1) < 0.1) {  # 10% chance to clean cache
        manage_cache_memory()
      }
    }
    
    return(result)
    
  }, error = function(e) {
    # Return empty result on error
    result <- list(segment.list = list(), mat.list = list())
    if (use_cache) {
      assign(cache_key, result, envir = .moneca_fast_cache)
    }
    return(result)
  })
}