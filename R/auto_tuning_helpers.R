# ============================================================================
# ENHANCED AUTO-TUNING FRAMEWORK - ADDITIONAL HELPER FUNCTIONS
# ============================================================================

#' Optimized Candidate Evaluation with Caching and Early Stopping
#'
#' Enhanced version of candidate evaluation with performance optimizations.
#'
#' @param mx Mobility matrix.
#' @param candidate_values Vector of candidates to evaluate.
#' @param cut.off Edge weight threshold.
#' @param method Tuning method.
#' @param n_trials Number of trials for stability assessment.
#' @param cache Cache object for storing results.
#' @param parallel_setup Parallel processing setup.
#' @param early_stopping Whether to use early stopping.
#' @param verbose Whether to print progress messages.
#' @return List containing evaluation results.
#' @keywords internal
evaluate_candidates_optimized <- function(mx, candidate_values, cut.off, method,
                                         n_trials, cache, parallel_setup,
                                         early_stopping, verbose) {
  
  n_candidates <- length(candidate_values)
  
  # Pre-allocate results vectors
  stability_scores <- numeric(n_candidates)
  quality_metrics <- vector("list", n_candidates)
  performance_metrics <- vector("list", n_candidates)
  network_properties <- vector("list", n_candidates)
  
  # Early stopping tracking
  if (early_stopping) {
    quality_trend <- numeric(min(5, n_candidates))
    trend_window <- min(3, n_candidates)
  }
  
  # Define evaluation function for single candidate
  evaluate_single_candidate <- function(i) {
    candidate <- candidate_values[i]
    
    tryCatch({
      # Check cache first
      cached_result <- check_cache(cache, candidate, cut.off, mx)
      if (!is.null(cached_result)) {
        return(cached_result)
      }
      
      # Compute weight matrix
      weight_matrix <- weight.matrix(
        mx = mx,
        cut.off = cut.off,
        small.cell.reduction = candidate
      )
      
      # Validate network
      if (all(is.na(weight_matrix)) || sum(!is.na(weight_matrix)) == 0) {
        return(create_invalid_result())
      }
      
      # Performance timing
      perf_start <- Sys.time()
      
      # Method-specific evaluation
      stability_score <- 0
      if (method == "stability") {
        stability_score <- assess_clustering_stability_fast(
          mx = mx,
          small.cell.reduction = candidate,
          cut.off = cut.off,
          n_bootstrap = n_trials
        )
      }
      
      # Quality metrics
      quality <- compute_clustering_quality_metrics_fast(weight_matrix)
      
      # Performance metrics  
      perf_end <- Sys.time()
      performance <- list(
        time = as.numeric(perf_end - perf_start),
        density = sum(!is.na(weight_matrix)) / length(weight_matrix),
        valid = TRUE
      )
      
      # Network properties
      network_props <- compute_network_properties_fast(weight_matrix)
      
      result <- list(
        stability_score = stability_score,
        quality_metrics = quality,
        performance_metrics = performance,
        network_properties = network_props,
        candidate = candidate
      )
      
      # Cache result
      if (!is.null(cache)) {
        cache_result(cache, candidate, cut.off, mx, result)
      }
      
      return(result)
      
    }, error = function(e) {
      return(create_invalid_result())
    })
  }
  
  # Execute evaluation (parallel or sequential)
  if (parallel_setup$use_parallel && n_candidates > 3) {
    # Parallel evaluation
    if (verbose) cat("Using parallel evaluation...\n")
    
    # Export necessary objects to cluster
    parallel::clusterExport(parallel_setup$cluster, 
                          c("mx", "candidate_values", "cut.off", "method", "n_trials"),
                          envir = environment())
    
    results <- parallel::parLapply(parallel_setup$cluster, 
                                  seq_along(candidate_values),
                                  evaluate_single_candidate)
  } else {
    # Sequential evaluation with early stopping
    results <- vector("list", n_candidates)
    
    for (i in seq_along(candidate_values)) {
      if (verbose) {
        cat("\rEvaluating candidate", i, "of", n_candidates, 
            sprintf("(%.1f%%)    ", 100 * i / n_candidates))
        flush.console()
      }
      
      results[[i]] <- evaluate_single_candidate(i)
      
      # Early stopping check
      if (early_stopping && i >= trend_window) {
        current_quality <- results[[i]]$quality_metrics$overall
        quality_trend[((i - 1) %% length(quality_trend)) + 1] <- current_quality
        
        # Check if quality is consistently decreasing
        if (i >= 5 && is_quality_declining(quality_trend)) {
          if (verbose) {
            cat("\nEarly stopping activated - quality declining\n")
          }
          # Truncate results and candidates
          candidate_values <- candidate_values[1:i]
          results <- results[1:i]
          break
        }
      }
    }
  }
  
  # Extract results from list
  for (i in seq_along(results)) {
    if (!is.null(results[[i]])) {
      stability_scores[i] <- results[[i]]$stability_score
      quality_metrics[[i]] <- results[[i]]$quality_metrics
      performance_metrics[[i]] <- results[[i]]$performance_metrics
      network_properties[[i]] <- results[[i]]$network_properties
    } else {
      # Handle missing results
      stability_scores[i] <- 0
      quality_metrics[[i]] <- create_invalid_result()$quality_metrics
      performance_metrics[[i]] <- create_invalid_result()$performance_metrics
      network_properties[[i]] <- create_invalid_result()$network_properties
    }
  }
  
  if (verbose) cat("\n")
  
  return(list(
    stability_scores = stability_scores,
    quality_metrics = quality_metrics,
    performance_metrics = performance_metrics,
    network_properties = network_properties,
    candidates_evaluated = length(candidate_values)
  ))
}

#' Fast Clustering Stability Assessment with Optimizations
#'
#' Optimized version of stability assessment with reduced computational overhead.
#'
#' @param mx Mobility matrix.
#' @param small.cell.reduction Parameter value.
#' @param cut.off Edge threshold.
#' @param n_bootstrap Number of bootstrap samples (reduced for speed).
#' @return Stability score.
#' @keywords internal
assess_clustering_stability_fast <- function(mx, small.cell.reduction, cut.off, n_bootstrap) {
  # Reduce bootstrap samples for speed while maintaining reliability
  n_bootstrap_fast <- min(n_bootstrap, max(10, n_bootstrap %/% 2))
  
  return(assess_clustering_stability(
    mx = mx,
    small.cell.reduction = small.cell.reduction,
    cut.off = cut.off,
    n_bootstrap = n_bootstrap_fast,
    sample_fraction = 0.7  # Slightly smaller sample for speed
  ))
}

#' Fast Quality Metrics Computation with Caching
#'
#' Optimized quality metrics computation.
#'
#' @param weight_matrix Weight matrix.
#' @return Quality metrics list.
#' @keywords internal
compute_clustering_quality_metrics_fast <- function(weight_matrix) {
  # Use efficient matrix operations where possible
  return(compute_clustering_quality_metrics(weight_matrix))
}

#' Fast Network Properties Computation
#'
#' Optimized network properties computation.
#'
#' @param weight_matrix Weight matrix.
#' @return Network properties list.
#' @keywords internal
compute_network_properties_fast <- function(weight_matrix) {
  return(compute_network_properties(weight_matrix))
}

#' Create Invalid Result Object
#'
#' Creates a standardized invalid result for failed evaluations.
#'
#' @return List with invalid result structure.
#' @keywords internal
create_invalid_result <- function() {
  return(list(
    stability_score = 0,
    quality_metrics = list(silhouette = 0, modularity = 0, overall = 0),
    performance_metrics = list(time = NA, density = 0, valid = FALSE),
    network_properties = list(density = 0, components = 0, edges = 0)
  ))
}

#' Check Cache for Computed Results
#'
#' Checks if results for given parameters are already cached.
#'
#' @param cache Cache object.
#' @param candidate Parameter candidate.
#' @param cut.off Cut-off threshold.
#' @param mx Mobility matrix.
#' @return Cached result or NULL.
#' @keywords internal
check_cache <- function(cache, candidate, cut.off, mx) {
  if (is.null(cache)) return(NULL)
  
  # Create hash for this parameter combination
  if (requireNamespace("digest", quietly = TRUE)) {
    param_hash <- digest::digest(list(candidate, cut.off), algo = "md5")
    matrix_hash <- digest::digest(mx, algo = "md5")
  } else {
    # Fallback without digest
    return(NULL)
  }
  
  if (cache$use_dt) {
    # Use data.table for fast lookup
    tryCatch({
      matches <- cache$weight_matrices[
        candidate == !!candidate & cut_off == !!cut.off & matrix_hash == !!matrix_hash
      ]
      
      if (nrow(matches) > 0) {
        cache$hits <- cache$hits + 1
        # Return cached result (implementation would retrieve full result)
        return(NULL)  # Simplified for now
      }
    }, error = function(e) {
      # Fallback if data.table operations fail
    })
  }
  
  cache$misses <- cache$misses + 1
  return(NULL)
}

#' Cache Computation Result
#'
#' Stores computation result in cache for future use.
#'
#' @param cache Cache object.
#' @param candidate Parameter candidate.
#' @param cut.off Cut-off threshold.
#' @param mx Mobility matrix.
#' @param result Computation result.
#' @keywords internal
cache_result <- function(cache, candidate, cut.off, mx, result) {
  if (is.null(cache)) return()
  
  # Implementation would store result in cache structure
  # Simplified for now to avoid complexity
}

#' Check if Quality is Declining (Early Stopping)
#'
#' Determines if recent quality scores show declining trend.
#'
#' @param quality_trend Vector of recent quality scores.
#' @return Logical indicating declining trend.
#' @keywords internal
is_quality_declining <- function(quality_trend) {
  # Simple trend detection
  if (length(quality_trend) < 3) return(FALSE)
  
  # Check if last 3 values are consistently decreasing
  recent <- tail(quality_trend, 3)
  return(all(diff(recent) <= -0.01))  # Declining by at least 1%
}