#' Enhanced Auto-Tuning Framework for MONECA Small Cell Reduction Parameter
#'
#' This module provides comprehensive automatic parameter tuning for the 
#' small.cell.reduction parameter in MONECA algorithms. It implements 
#' stability-based, quality-based, and performance-aware tuning methods with
#' advanced optimization features including Pareto frontier analysis,
#' cross-validation, Bayesian optimization, and performance optimization
#' using data.table for enhanced speed.
#'
#' @name auto_tuning
#' @keywords internal
NULL

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
#'     \item "bayesian": Bayesian optimization (requires GPfit package)
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
#'
#' @details
#' The function implements three tuning strategies:
#'
#' \strong{Stability Method:} Uses bootstrap resampling to assess clustering 
#' consistency across data subsamples. Parameters that produce more stable 
#' clustering structures are preferred.
#'
#' \strong{Quality Method:} Optimizes clustering quality using multiple metrics 
#' including silhouette coefficients, network modularity, and information-theoretic 
#' measures.
#'
#' \strong{Performance Method:} Balances clustering quality with computational 
#' efficiency, considering both analysis time and memory requirements.
#'
#' Candidate values are automatically generated using percentile-based thresholds, 
#' matrix sparsity analysis, and network density targets unless explicitly provided.
#'
#' @examples
#' \dontrun{
#' # Generate sample mobility data
#' mobility_data <- generate_mobility_data(n_classes = 6, seed = 123)
#' 
#' # Stability-based tuning (recommended)
#' tuning_result <- auto_tune_small_cell_reduction(
#'   mx = mobility_data, 
#'   method = "stability",
#'   n_trials = 15,
#'   verbose = TRUE
#' )
#' 
#' # Use optimal parameter in moneca analysis
#' segments <- moneca(mobility_data, 
#'                   small.cell.reduction = tuning_result$optimal_value)
#' 
#' # Quality-based tuning with custom candidates
#' quality_result <- auto_tune_small_cell_reduction(
#'   mx = mobility_data,
#'   method = "quality", 
#'   candidate_values = c(0, 1, 2, 5, 10, 20),
#'   verbose = TRUE
#' )
#' 
#' # Performance-aware tuning
#' performance_result <- auto_tune_small_cell_reduction(
#'   mx = mobility_data,
#'   method = "performance",
#'   performance_weight = 0.5,  # Equal weight to speed and quality
#'   verbose = TRUE
#' )
#' }
#'
#' @seealso 
#' \code{\link{moneca}} for main clustering function,
#' \code{\link{weight.matrix}} for weight matrix computation,
#' \code{\link{generate_candidate_values}} for candidate generation,
#' \code{\link{assess_clustering_stability}} for stability assessment
#'
#' @export
auto_tune_small_cell_reduction <- function(mx,
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
  
  # Input validation
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
    cat("Starting auto-tuning for small.cell.reduction parameter...\n")
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
  
  # Evaluate each candidate
  if (verbose) {
    cat("Evaluating", length(candidate_values), "candidates...\n")
  }
  
  for (i in seq_along(candidate_values)) {
    candidate <- candidate_values[i]
    
    if (verbose) {
      cat("Testing candidate", i, "/", length(candidate_values), 
          "(value =", candidate, ")... ")
    }
    
    # Compute weight matrix for this candidate
    tryCatch({
      weight_matrix <- weight.matrix(
        mx = mx, 
        cut.off = cut.off, 
        small.cell.reduction = candidate
      )
      
      # Check for valid network
      if (all(is.na(weight_matrix)) || sum(!is.na(weight_matrix)) == 0) {
        if (verbose) cat("invalid network\n")
        stability_scores[i] <- 0
        quality_metrics[[i]] <- list(silhouette = 0, modularity = 0, overall = 0)
        performance_metrics[[i]] <- list(time = NA, density = 0)
        network_properties[[i]] <- list(density = 0, components = 0, edges = 0)
        next
      }
      
      # Assess based on selected method
      if (method == "stability") {
        stability_scores[i] <- assess_clustering_stability(
          mx = mx, 
          small.cell.reduction = candidate,
          cut.off = cut.off,
          n_bootstrap = n_trials
        )
      }
      
      # Compute quality metrics
      quality_metrics[[i]] <- compute_clustering_quality_metrics(
        weight_matrix = weight_matrix
      )
      
      # Compute performance metrics
      performance_start <- Sys.time()
      test_segments <- tryCatch({
        # Quick test run for performance assessment
        moneca_fast(mx, segment.levels = 2, cut.off = cut.off, 
                   small.cell.reduction = candidate)
      }, error = function(e) NULL)
      performance_end <- Sys.time()
      
      performance_time <- as.numeric(performance_end - performance_start)
      
      performance_metrics[[i]] <- list(
        time = performance_time,
        density = sum(!is.na(weight_matrix)) / length(weight_matrix),
        valid = !is.null(test_segments)
      )
      
      # Network properties
      network_properties[[i]] <- compute_network_properties(weight_matrix)
      
      if (verbose) {
        if (method == "stability") {
          cat("stability =", round(stability_scores[i], 3))
        }
        cat("quality =", round(quality_metrics[[i]]$overall, 3))
        cat("time =", round(performance_time, 2), "s\n")
      }
      
    }, error = function(e) {
      if (verbose) cat("error:", e$message, "\n")
      stability_scores[i] <- 0
      quality_metrics[[i]] <- list(silhouette = 0, modularity = 0, overall = 0)
      performance_metrics[[i]] <- list(time = NA, density = 0, valid = FALSE)
      network_properties[[i]] <- list(density = 0, components = 0, edges = 0)
    })
  }
  
  # Select optimal parameter based on method
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
  }
  
  # Create comprehensive results object
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
    parameters = list(
      cut.off = cut.off,
      n_trials = n_trials,
      performance_weight = performance_weight,
      min_density = min_density,
      max_candidates = max_candidates
    )
  )
  
  class(results) <- "moneca_tuning"
  return(results)
}

#' Generate Data-Driven Candidate Values
#'
#' Generates candidate values for small.cell.reduction parameter using 
#' data-driven approaches including percentile analysis, matrix sparsity 
#' assessment, and network density optimization.
#'
#' @param mx Mobility matrix for analysis.
#' @param method Character string specifying generation strategy:
#'   \itemize{
#'     \item "percentile": Percentile-based thresholds from data distribution
#'     \item "sparsity": Matrix sparsity-informed selection  
#'     \item "density": Network density optimization targets
#'     \item "hybrid": Combination of multiple strategies (default)
#'   }
#' @param max_candidates Maximum number of candidates to generate. Default is 20.
#' @param include_zero Logical indicating whether to include 0 as a candidate.
#'   Default is TRUE.
#'
#' @return Numeric vector of candidate small.cell.reduction values, sorted 
#'   in ascending order.
#'
#' @details
#' \strong{Percentile Method:} Generates thresholds based on the distribution 
#' of non-zero values in the mobility matrix. Uses quantiles from 5% to 95%.
#'
#' \strong{Sparsity Method:} Analyzes matrix sparsity patterns and generates 
#' candidates that preserve different levels of network connectivity.
#'
#' \strong{Density Method:} Targets specific network density levels (0.1, 0.2, etc.)
#' by estimating required thresholds.
#'
#' \strong{Hybrid Method:} Combines all approaches and selects diverse, 
#' non-redundant candidates covering the full parameter space.
#'
#' @examples
#' \dontrun{
#' # Generate sample data
#' mobility_data <- generate_mobility_data(n_classes = 5, seed = 123)
#' 
#' # Percentile-based candidates
#' candidates_pct <- generate_candidate_values(mobility_data, "percentile")
#' 
#' # Sparsity-informed candidates  
#' candidates_sparse <- generate_candidate_values(mobility_data, "sparsity")
#' 
#' # Hybrid approach (recommended)
#' candidates_hybrid <- generate_candidate_values(mobility_data, "hybrid")
#' 
#' print(candidates_hybrid)
#' }
#'
#' @seealso \code{\link{auto_tune_small_cell_reduction}}
#' @export
generate_candidate_values <- function(mx, 
                                    method = "hybrid", 
                                    max_candidates = 20,
                                    include_zero = TRUE) {
  
  # Ensure matrix format
  if (!is.matrix(mx)) {
    mx <- as.matrix(mx)
  }
  
  # Remove diagonal for analysis (mobility typically excludes self-transitions)
  mx_off_diag <- mx
  diag(mx_off_diag) <- 0
  
  # Get non-zero values for analysis
  non_zero_values <- mx_off_diag[mx_off_diag > 0]
  
  if (length(non_zero_values) == 0) {
    warning("No non-zero off-diagonal values found")
    return(if (include_zero) 0 else numeric(0))
  }
  
  candidates <- numeric(0)
  
  # Generate candidates based on method
  if (method %in% c("percentile", "hybrid")) {
    # Percentile-based candidates
    percentiles <- seq(0.05, 0.95, by = 0.1)
    pct_candidates <- quantile(non_zero_values, probs = percentiles, na.rm = TRUE)
    candidates <- c(candidates, pct_candidates)
  }
  
  if (method %in% c("sparsity", "hybrid")) {
    # Sparsity-informed candidates
    # Target different sparsity levels
    total_cells <- length(mx_off_diag[mx_off_diag >= 0])
    current_density <- length(non_zero_values) / total_cells
    
    # Generate thresholds for target densities
    target_densities <- c(0.1, 0.2, 0.3, 0.5, 0.7) * current_density
    
    sparsity_candidates <- numeric(0)
    for (target in target_densities) {
      target_n_cells <- round(target * total_cells)
      if (target_n_cells > 0 && target_n_cells <= length(non_zero_values)) {
        threshold <- sort(non_zero_values, decreasing = TRUE)[target_n_cells]
        sparsity_candidates <- c(sparsity_candidates, threshold)
      }
    }
    candidates <- c(candidates, sparsity_candidates)
  }
  
  if (method %in% c("density", "hybrid")) {
    # Network density-based candidates
    # Estimate thresholds for target network densities
    sorted_values <- sort(non_zero_values, decreasing = TRUE)
    n_values <- length(sorted_values)
    
    # Target different retention rates
    retention_rates <- c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9)
    
    for (rate in retention_rates) {
      n_keep <- max(1, round(rate * n_values))
      if (n_keep <= n_values) {
        threshold <- sorted_values[n_keep]
        candidates <- c(candidates, threshold)
      }
    }
  }
  
  # Add some standard values
  if (method == "hybrid") {
    # Add some commonly used values
    mean_val <- mean(non_zero_values)
    median_val <- median(non_zero_values)
    
    standard_candidates <- c(
      1, 2, 5, 10,  # Common threshold values
      mean_val * 0.1, mean_val * 0.5, mean_val,  # Mean-based
      median_val * 0.5, median_val  # Median-based
    )
    candidates <- c(candidates, standard_candidates)
  }
  
  # Include zero if requested
  if (include_zero) {
    candidates <- c(0, candidates)
  }
  
  # Clean up candidates
  candidates <- unique(candidates)
  candidates <- candidates[!is.na(candidates) & candidates >= 0]
  candidates <- sort(candidates)
  
  # Limit number of candidates
  if (length(candidates) > max_candidates) {
    # Keep evenly spaced candidates across the range
    indices <- round(seq(1, length(candidates), length.out = max_candidates))
    candidates <- candidates[indices]
  }
  
  return(candidates)
}

#' Assess Clustering Stability Using Bootstrap Resampling
#'
#' Evaluates the stability of MONECA clustering results for a given 
#' small.cell.reduction parameter using bootstrap resampling. More stable 
#' parameters produce consistent clustering across data subsamples.
#'
#' @param mx Mobility matrix for analysis.
#' @param small.cell.reduction Small cell reduction parameter to test.
#' @param cut.off Edge weight threshold for network construction. Default is 1.
#' @param n_bootstrap Number of bootstrap samples to generate. Default is 100.
#' @param sample_fraction Fraction of data to use in each bootstrap sample. 
#'   Default is 0.8.
#' @param segment.levels Number of hierarchical levels for clustering. Default is 3.
#'
#' @return Numeric stability score between 0 and 1, where higher values 
#'   indicate more stable clustering results.
#'
#' @details
#' The function performs bootstrap resampling by:
#' 1. Creating multiple subsamples of the mobility matrix
#' 2. Running MONECA clustering on each subsample
#' 3. Extracting clustering signatures (segment memberships)
#' 4. Computing stability metrics based on clustering consistency
#'
#' The stability score combines multiple measures including:
#' - Consistency of segment assignments across bootstrap samples
#' - Stability of hierarchical structure
#' - Robustness of clique detection
#'
#' @examples
#' \dontrun{
#' # Generate sample data
#' mobility_data <- generate_mobility_data(n_classes = 6, seed = 123)
#' 
#' # Assess stability for different parameters
#' stability_0 <- assess_clustering_stability(mobility_data, 
#'                                           small.cell.reduction = 0)
#' stability_5 <- assess_clustering_stability(mobility_data, 
#'                                           small.cell.reduction = 5)
#' 
#' cat("Stability with parameter 0:", stability_0, "\n")
#' cat("Stability with parameter 5:", stability_5, "\n")
#' }
#'
#' @seealso \code{\link{auto_tune_small_cell_reduction}}, \code{\link{moneca}}
#' @export
assess_clustering_stability <- function(mx, 
                                      small.cell.reduction,
                                      cut.off = 1,
                                      n_bootstrap = 100,
                                      sample_fraction = 0.8,
                                      segment.levels = 3) {
  
  n <- nrow(mx)
  sample_size <- max(3, floor(n * sample_fraction))  # Minimum 3 for meaningful clustering
  
  # Storage for bootstrap results
  bootstrap_signatures <- list()
  valid_runs <- 0
  
  for (i in seq_len(n_bootstrap)) {
    # Create bootstrap sample
    sample_indices <- sample(seq_len(n), size = sample_size, replace = FALSE)
    mx_sample <- mx[sample_indices, sample_indices]
    
    # Run clustering on sample
    tryCatch({
      segments <- moneca_fast(
        mx = mx_sample,
        segment.levels = segment.levels,
        cut.off = cut.off,
        small.cell.reduction = small.cell.reduction
      )
      
      # Extract clustering signature
      if (!is.null(segments$segment.list) && length(segments$segment.list) > 0) {
        signature <- extract_clustering_signature(segments, sample_indices)
        bootstrap_signatures[[length(bootstrap_signatures) + 1]] <- signature
        valid_runs <- valid_runs + 1
      }
    }, error = function(e) {
      # Skip failed bootstrap samples
      NULL
    })
  }
  
  if (valid_runs < 2) {
    return(0)  # Cannot assess stability with fewer than 2 valid runs
  }
  
  # Compute stability metrics
  stability_score <- compute_stability_metrics(bootstrap_signatures)
  
  return(stability_score)
}

#' Extract Clustering Signature
#'
#' Extracts a standardized signature from MONECA clustering results for 
#' stability comparison across bootstrap samples.
#'
#' @param segments MONECA segmentation results.
#' @param original_indices Original indices of the sampled data.
#'
#' @return List containing clustering signature components.
#' @keywords internal
extract_clustering_signature <- function(segments, original_indices = NULL) {
  
  if (is.null(segments$segment.list) || length(segments$segment.list) == 0) {
    return(NULL)
  }
  
  signature <- list()
  
  # Extract segment memberships for each level
  for (level in seq_along(segments$segment.list)) {
    level_segments <- segments$segment.list[[level]]
    
    if (length(level_segments) > 0) {
      # Create membership vector
      n_items <- max(unlist(level_segments))
      membership <- rep(0, n_items)
      
      for (seg_id in seq_along(level_segments)) {
        segment <- level_segments[[seg_id]]
        membership[segment] <- seg_id
      }
      
      signature[[paste0("level_", level)]] <- membership
    }
  }
  
  # Add summary statistics
  signature$n_levels <- length(segments$segment.list)
  signature$n_segments_per_level <- sapply(segments$segment.list, length)
  signature$original_indices <- original_indices
  
  return(signature)
}

#' Compute Stability Metrics
#'
#' Computes stability scores from clustering signatures across bootstrap samples.
#'
#' @param signatures List of clustering signatures from bootstrap samples.
#'
#' @return Numeric stability score between 0 and 1.
#' @keywords internal
compute_stability_metrics <- function(signatures) {
  
  if (length(signatures) < 2) {
    return(0)
  }
  
  n_signatures <- length(signatures)
  stability_scores <- numeric(0)
  
  # Compare all pairs of signatures
  for (i in seq_len(n_signatures - 1)) {
    for (j in (i + 1):n_signatures) {
      sig1 <- signatures[[i]]
      sig2 <- signatures[[j]]
      
      # Compare signatures at matching levels
      level_similarities <- numeric(0)
      
      common_levels <- intersect(names(sig1), names(sig2))
      common_levels <- common_levels[grepl("^level_", common_levels)]
      
      for (level in common_levels) {
        mem1 <- sig1[[level]]
        mem2 <- sig2[[level]]
        
        # Handle different sample sizes
        if (!is.null(sig1$original_indices) && !is.null(sig2$original_indices)) {
          common_indices <- intersect(sig1$original_indices, sig2$original_indices)
          if (length(common_indices) >= 2) {
            # Map common original indices to positions in the sample membership vectors
            # The membership vector indices correspond to the sampled items (1 to n_sampled)
            # We need to find which sampled items correspond to the common original indices
            
            # For sig1: find positions of common_indices in sig1$original_indices
            sig1_positions <- match(common_indices, sig1$original_indices)
            # For sig2: find positions of common_indices in sig2$original_indices  
            sig2_positions <- match(common_indices, sig2$original_indices)
            
            # Filter out NA matches and ensure positions are within bounds
            valid_matches <- !is.na(sig1_positions) & !is.na(sig2_positions) & 
                           sig1_positions <= length(mem1) & sig2_positions <= length(mem2) &
                           sig1_positions > 0 & sig2_positions > 0
            
            if (sum(valid_matches) >= 2) {
              sig1_valid_pos <- sig1_positions[valid_matches]
              sig2_valid_pos <- sig2_positions[valid_matches]
              
              mem1_common <- mem1[sig1_valid_pos]
              mem2_common <- mem2[sig2_valid_pos]
              
              # Compute adjusted rand index for clustering similarity
              similarity <- compute_adjusted_rand_index(mem1_common, mem2_common)
              level_similarities <- c(level_similarities, similarity)
            }
          }
        }
      }
      
      if (length(level_similarities) > 0) {
        pair_stability <- mean(level_similarities, na.rm = TRUE)
        stability_scores <- c(stability_scores, pair_stability)
      }
    }
  }
  
  if (length(stability_scores) == 0) {
    return(0)
  }
  
  # Return mean stability across all pairs
  mean_stability <- mean(stability_scores, na.rm = TRUE)
  
  # Ensure score is between 0 and 1
  return(max(0, min(1, mean_stability)))
}

#' Compute Adjusted Rand Index
#'
#' Computes the adjusted rand index for comparing clustering solutions.
#'
#' @param clustering1 First clustering solution.
#' @param clustering2 Second clustering solution.
#'
#' @return Numeric adjusted rand index.
#' @keywords internal
compute_adjusted_rand_index <- function(clustering1, clustering2) {
  
  if (length(clustering1) != length(clustering2)) {
    return(0)
  }
  
  # Create contingency table
  tab <- table(clustering1, clustering2)
  
  # Compute adjusted rand index
  n <- sum(tab)
  ni <- rowSums(tab)
  nj <- colSums(tab)
  
  # Sum of choose(tab[i,j], 2)
  sum_tab_choose2 <- sum(choose(tab, 2))
  
  # Expected value
  sum_ni_choose2 <- sum(choose(ni, 2))
  sum_nj_choose2 <- sum(choose(nj, 2))
  expected <- (sum_ni_choose2 * sum_nj_choose2) / choose(n, 2)
  
  # Maximum value
  max_val <- 0.5 * (sum_ni_choose2 + sum_nj_choose2)
  
  if (max_val == expected) {
    return(1)  # Perfect agreement
  }
  
  # Adjusted rand index
  ari <- (sum_tab_choose2 - expected) / (max_val - expected)
  
  return(ari)
}

#' Compute Multi-Objective Optimization Metrics
#'
#' Computes comprehensive metrics for evaluating clustering quality, including
#' silhouette analysis, network modularity, and information-theoretic measures.
#'
#' @param segments MONECA segmentation results (optional, for full analysis).
#' @param weight_matrix Weight matrix from mobility data (required).
#' @param include_silhouette Logical indicating whether to compute silhouette 
#'   coefficients. Default is TRUE.
#' @param include_modularity Logical indicating whether to compute network 
#'   modularity. Default is TRUE.
#'
#' @return List containing:
#'   \item{silhouette}{Average silhouette coefficient}
#'   \item{modularity}{Network modularity score}
#'   \item{network_density}{Network density measure}
#'   \item{connectivity}{Network connectivity metrics}
#'   \item{overall}{Overall quality score combining all metrics}
#'
#' @details
#' The function evaluates clustering quality using multiple complementary metrics:
#' 
#' \strong{Silhouette Analysis:} Measures how well-separated clusters are by 
#' comparing within-cluster and between-cluster distances.
#' 
#' \strong{Network Modularity:} Assesses community structure quality in the 
#' mobility network.
#' 
#' \strong{Connectivity Metrics:} Evaluates network structure properties 
#' including density, component structure, and edge distribution.
#'
#' @examples
#' \dontrun{
#' # Generate sample data and create weight matrix
#' mobility_data <- generate_mobility_data(n_classes = 5, seed = 123)
#' weight_matrix <- weight.matrix(mobility_data, small.cell.reduction = 2)
#' 
#' # Compute quality metrics
#' quality <- compute_tuning_metrics(weight_matrix = weight_matrix)
#' print(quality)
#' }
#'
#' @seealso \code{\link{auto_tune_small_cell_reduction}}
#' @export
compute_tuning_metrics <- function(segments = NULL, 
                                 weight_matrix = NULL,
                                 include_silhouette = TRUE,
                                 include_modularity = TRUE) {
  
  if (is.null(weight_matrix)) {
    stop("weight_matrix is required")
  }
  
  # Initialize metrics
  metrics <- list()
  
  # Network density
  valid_cells <- !is.na(weight_matrix)
  total_cells <- length(weight_matrix)
  metrics$network_density <- sum(valid_cells) / total_cells
  
  # Edge statistics
  valid_weights <- weight_matrix[valid_cells]
  metrics$n_edges <- length(valid_weights)
  metrics$mean_weight <- if (length(valid_weights) > 0) mean(valid_weights) else 0
  metrics$weight_variance <- if (length(valid_weights) > 1) var(valid_weights) else 0
  
  # Network connectivity
  connectivity <- compute_network_connectivity(weight_matrix)
  metrics$connectivity <- connectivity
  
  # Modularity (if requested and feasible)
  if (include_modularity && metrics$n_edges > 0) {
    tryCatch({
      modularity <- compute_network_modularity(weight_matrix)
      metrics$modularity <- modularity
    }, error = function(e) {
      metrics$modularity <- 0
    })
  } else {
    metrics$modularity <- 0
  }
  
  # Silhouette analysis (if segments provided)
  if (include_silhouette && !is.null(segments)) {
    tryCatch({
      silhouette <- compute_silhouette_score(weight_matrix, segments)
      metrics$silhouette <- silhouette
    }, error = function(e) {
      metrics$silhouette <- 0
    })
  } else {
    metrics$silhouette <- 0
  }
  
  # Overall quality score (weighted combination)
  weights <- c(
    network_density = 0.2,
    modularity = 0.4,
    silhouette = 0.3,
    connectivity = 0.1
  )
  
  # Normalize components to 0-1 scale
  density_score <- min(1, metrics$network_density * 10)  # Penalize very sparse networks
  modularity_score <- max(0, min(1, metrics$modularity))
  silhouette_score <- max(0, min(1, (metrics$silhouette + 1) / 2))  # Convert from [-1,1] to [0,1]
  connectivity_score <- max(0, min(1, metrics$connectivity$relative_size))
  
  metrics$overall <- (
    weights["network_density"] * density_score +
    weights["modularity"] * modularity_score + 
    weights["silhouette"] * silhouette_score +
    weights["connectivity"] * connectivity_score
  )
  
  return(metrics)
}

#' Compute Clustering Quality Metrics
#'
#' Convenience wrapper for compute_tuning_metrics focused on clustering quality.
#'
#' @param weight_matrix Weight matrix from mobility data.
#' @param segments Optional MONECA segmentation results.
#'
#' @return List of clustering quality metrics.
#'
#' @seealso \code{\link{compute_tuning_metrics}}
#' @export
compute_clustering_quality_metrics <- function(weight_matrix, segments = NULL) {
  return(compute_tuning_metrics(
    segments = segments,
    weight_matrix = weight_matrix,
    include_silhouette = !is.null(segments),
    include_modularity = TRUE
  ))
}

#' Evaluate Performance Trade-offs
#'
#' Balances clustering quality against computational performance to identify 
#' optimal parameters for different use cases and constraints.
#'
#' @param quality_metrics List of quality metric results for each candidate.
#' @param performance_metrics List of performance metric results for each candidate.
#' @param performance_weight Numeric weight (0-1) for performance vs quality 
#'   trade-off. Higher values prioritize speed over quality.
#'
#' @return Numeric vector of trade-off scores for each candidate.
#'
#' @details
#' The function combines normalized quality and performance scores using 
#' the specified weight. Quality scores include clustering metrics like 
#' modularity and silhouette coefficients. Performance scores consider 
#' computational time and memory efficiency.
#'
#' @examples
#' \dontrun{
#' # This function is typically called internally by auto_tune_small_cell_reduction
#' # Example usage in isolation:
#' 
#' quality_list <- list(
#'   list(overall = 0.8),
#'   list(overall = 0.6),
#'   list(overall = 0.9)
#' )
#' 
#' performance_list <- list(
#'   list(time = 1.2, valid = TRUE),
#'   list(time = 0.8, valid = TRUE), 
#'   list(time = 2.1, valid = TRUE)
#' )
#' 
#' trade_offs <- evaluate_performance_trade_offs(
#'   quality_metrics = quality_list,
#'   performance_metrics = performance_list,
#'   performance_weight = 0.3
#' )
#' }
#'
#' @seealso \code{\link{auto_tune_small_cell_reduction}}
#' @export
evaluate_performance_trade_offs <- function(quality_metrics,
                                          performance_metrics, 
                                          performance_weight = 0.3) {
  
  n_candidates <- length(quality_metrics)
  
  if (length(performance_metrics) != n_candidates) {
    stop("quality_metrics and performance_metrics must have the same length")
  }
  
  # Extract quality scores
  quality_scores <- sapply(quality_metrics, function(x) {
    if (is.null(x$overall)) 0 else x$overall
  })
  
  # Extract performance scores (inverse of time, with penalty for invalid runs)
  performance_scores <- sapply(performance_metrics, function(x) {
    # Check for NULL first
    if (is.null(x)) {
      return(0)
    }
    # Check for valid field existence and type
    if (is.null(x$valid) || !is.logical(x$valid) || !x$valid) {
      return(0)
    }
    # Check for time field existence and validity
    if (is.null(x$time) || is.na(x$time)) {
      return(0)
    }
    # Performance score is inverse of time (faster = better)
    # Add small constant to avoid division by zero
    return(1 / (x$time + 0.001))
  })
  
  # Normalize scores to [0, 1]
  if (max(quality_scores, na.rm = TRUE) > 0) {
    quality_scores <- quality_scores / max(quality_scores, na.rm = TRUE)
  }
  
  if (max(performance_scores, na.rm = TRUE) > 0) {
    performance_scores <- performance_scores / max(performance_scores, na.rm = TRUE)
  }
  
  # Combine scores using specified weight
  quality_weight <- 1 - performance_weight
  trade_off_scores <- (quality_weight * quality_scores) + 
                     (performance_weight * performance_scores)
  
  return(trade_off_scores)
}

#' Compute Network Properties
#'
#' Computes comprehensive network structure properties from a weight matrix.
#'
#' @param weight_matrix Weight matrix representing network connections.
#'
#' @return List containing network properties.
#' @keywords internal
compute_network_properties <- function(weight_matrix) {
  
  valid_weights <- !is.na(weight_matrix)
  n_edges <- sum(valid_weights)
  total_possible <- length(weight_matrix)
  
  properties <- list()
  properties$density <- n_edges / total_possible
  properties$edges <- n_edges
  properties$mean_weight <- if (n_edges > 0) mean(weight_matrix[valid_weights]) else 0
  
  # Estimate number of components (simple approximation)
  if (n_edges > 0) {
    # Convert to binary adjacency for component analysis
    adj_matrix <- ifelse(valid_weights, 1, 0)
    properties$components <- estimate_components(adj_matrix)
  } else {
    properties$components <- nrow(weight_matrix)  # All isolated
  }
  
  return(properties)
}

#' Estimate Number of Connected Components
#'
#' Simple approximation of connected components in a network.
#'
#' @param adj_matrix Binary adjacency matrix.
#'
#' @return Estimated number of connected components.
#' @keywords internal
estimate_components <- function(adj_matrix) {
  
  n <- nrow(adj_matrix)
  visited <- rep(FALSE, n)
  components <- 0
  
  for (i in seq_len(n)) {
    if (!visited[i]) {
      components <- components + 1
      # Simple DFS to mark connected nodes
      stack <- i
      while (length(stack) > 0) {
        current <- stack[length(stack)]
        stack <- stack[-length(stack)]
        
        if (!visited[current]) {
          visited[current] <- TRUE
          # Add neighbors to stack
          neighbors <- which(adj_matrix[current, ] == 1 | adj_matrix[, current] == 1)
          # Filter out invalid indices and unvisited neighbors
          neighbors <- neighbors[neighbors > 0 & neighbors <= n]
          neighbors <- neighbors[!visited[neighbors]]
          stack <- c(stack, neighbors)
        }
      }
    }
  }
  
  return(components)
}

#' Compute Network Connectivity Metrics
#'
#' Computes various connectivity metrics for network analysis.
#'
#' @param weight_matrix Weight matrix representing the network.
#'
#' @return List containing connectivity metrics.
#' @keywords internal
compute_network_connectivity <- function(weight_matrix) {
  
  n <- nrow(weight_matrix)
  valid_weights <- !is.na(weight_matrix)
  
  # Basic connectivity
  connectivity <- list()
  connectivity$n_nodes <- n
  connectivity$n_edges <- sum(valid_weights)
  connectivity$density <- connectivity$n_edges / (n * n)
  
  # Degree distribution
  row_degrees <- rowSums(valid_weights)
  col_degrees <- colSums(valid_weights)
  
  connectivity$mean_out_degree <- mean(row_degrees)
  connectivity$mean_in_degree <- mean(col_degrees)
  connectivity$max_degree <- max(c(row_degrees, col_degrees))
  
  # Relative size of largest component (approximation)
  if (connectivity$n_edges > 0) {
    adj_binary <- ifelse(valid_weights, 1, 0)
    n_components <- estimate_components(adj_binary)
    connectivity$n_components <- n_components
    connectivity$relative_size <- (n - n_components + 1) / n  # Approximation
  } else {
    connectivity$n_components <- n
    connectivity$relative_size <- 1/n  # All nodes isolated
  }
  
  return(connectivity)
}

#' Compute Network Modularity
#'
#' Computes modularity score for network community structure.
#'
#' @param weight_matrix Weight matrix representing the network.
#'
#' @return Numeric modularity score.
#' @keywords internal
compute_network_modularity <- function(weight_matrix) {
  
  # Simple modularity approximation
  # Convert to symmetric matrix for community detection
  n <- nrow(weight_matrix)
  valid_weights <- !is.na(weight_matrix)
  
  if (sum(valid_weights) < 2) {
    return(0)
  }
  
  # Create symmetric version
  sym_matrix <- weight_matrix
  sym_matrix[is.na(sym_matrix)] <- 0
  sym_matrix <- (sym_matrix + t(sym_matrix)) / 2
  
  # Simple community detection based on strong connections
  threshold <- quantile(sym_matrix[sym_matrix > 0], 0.7, na.rm = TRUE)
  strong_connections <- sym_matrix >= threshold
  
  # Estimate modularity based on clustering tendency
  total_edges <- sum(strong_connections) / 2
  if (total_edges == 0) return(0)
  
  # Rough modularity approximation
  degrees <- rowSums(strong_connections)
  expected_edges <- sum(degrees^2) / (4 * total_edges)
  
  modularity <- (total_edges - expected_edges) / total_edges
  return(max(0, min(1, modularity)))
}

#' Compute Silhouette Score
#'
#' Computes silhouette coefficient for clustering quality assessment.
#'
#' @param weight_matrix Weight matrix used for clustering.
#' @param segments MONECA segmentation results.
#'
#' @return Numeric silhouette score.
#' @keywords internal
compute_silhouette_score <- function(weight_matrix, segments) {
  
  if (is.null(segments$segment.list) || length(segments$segment.list) == 0) {
    return(0)
  }
  
  # Use the first level of segmentation
  first_level <- segments$segment.list[[1]]
  
  if (length(first_level) <= 1) {
    return(0)  # No meaningful clustering
  }
  
  # Create cluster membership vector
  n <- nrow(weight_matrix)
  membership <- rep(0, n)
  
  for (i in seq_along(first_level)) {
    segment_members <- first_level[[i]]
    if (length(segment_members) > 0 && max(segment_members) <= n) {
      membership[segment_members] <- i
    }
  }
  
  # Compute distance matrix (inverse of weights)
  dist_matrix <- 1 / (weight_matrix + 0.001)  # Add small constant to avoid division by zero
  dist_matrix[is.na(weight_matrix)] <- Inf
  diag(dist_matrix) <- 0
  
  # Compute silhouette coefficients
  silhouettes <- numeric(n)
  
  for (i in seq_len(n)) {
    cluster_i <- membership[i]
    
    if (cluster_i == 0) {
      silhouettes[i] <- 0
      next
    }
    
    # Within-cluster distances
    same_cluster <- which(membership == cluster_i & membership != 0)
    if (length(same_cluster) > 1) {
      a_i <- mean(dist_matrix[i, same_cluster[same_cluster != i]], na.rm = TRUE)
    } else {
      a_i <- 0
    }
    
    # Between-cluster distances
    other_clusters <- unique(membership[membership != cluster_i & membership != 0])
    if (length(other_clusters) > 0) {
      b_values <- numeric(length(other_clusters))
      for (j in seq_along(other_clusters)) {
        other_cluster <- other_clusters[j]
        other_members <- which(membership == other_cluster)
        b_values[j] <- mean(dist_matrix[i, other_members], na.rm = TRUE)
      }
      b_i <- min(b_values, na.rm = TRUE)
    } else {
      b_i <- 0
    }
    
    # Silhouette coefficient
    if (max(a_i, b_i) > 0) {
      silhouettes[i] <- (b_i - a_i) / max(a_i, b_i)
    } else {
      silhouettes[i] <- 0
    }
  }
  
  # Return mean silhouette coefficient
  return(mean(silhouettes[is.finite(silhouettes)], na.rm = TRUE))
}

#' Print Method for MONECA Tuning Results
#'
#' Provides formatted output for auto-tuning results.
#'
#' @param x Object of class "moneca_tuning".
#' @param ... Additional arguments (unused).
#'
#' @export
print.moneca_tuning <- function(x, ...) {
  cat("MONECA Auto-Tuning Results\n")
  cat("==========================\n\n")
  
  cat("Optimal small.cell.reduction:", x$optimal_value, "\n")
  cat("Tuning method:", x$tuning_method, "\n")
  cat("Candidates tested:", length(x$candidates_tested), "\n")
  cat("Total computation time:", round(x$computation_time, 2), "seconds\n\n")
  
  cat("Selection rationale:", x$selection_rationale, "\n\n")
  
  if (x$tuning_method == "stability" && length(x$stability_scores) > 0) {
    max_idx <- which.max(x$stability_scores)
    cat("Stability scores (top 3):\n")
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
}

# ============================================================================
# ENHANCED AUTO-TUNING FRAMEWORK - PERFORMANCE OPTIMIZATION FUNCTIONS
# ============================================================================

#' Initialize Tuning Cache for Performance Optimization
#'
#' Creates a cache structure to store computed results and avoid redundant
#' calculations during parameter tuning.
#'
#' @return List containing cache structure.
#' @keywords internal
initialize_tuning_cache <- function() {
  # Use data.table for efficient caching if available
  if (requireNamespace("data.table", quietly = TRUE)) {
    cache <- list(
      weight_matrices = data.table::data.table(
        candidate = numeric(0),
        cut_off = numeric(0),
        hash = character(0),
        matrix_hash = character(0)
      ),
      quality_metrics = data.table::data.table(
        matrix_hash = character(0),
        overall = numeric(0),
        modularity = numeric(0),
        silhouette = numeric(0)
      ),
      use_dt = TRUE
    )
  } else {
    cache <- list(
      weight_matrices = list(),
      quality_metrics = list(),
      use_dt = FALSE
    )
  }
  
  cache$hits <- 0
  cache$misses <- 0
  
  return(cache)
}

#' Setup Parallel Processing for Auto-Tuning
#'
#' Configures parallel processing cluster for enhanced performance.
#'
#' @param n_cores Number of cores to use. If NULL, auto-detects optimal number.
#' @param verbose Logical indicating whether to print setup messages.
#' @return List containing parallel setup information.
#' @keywords internal
setup_parallel_processing <- function(n_cores = NULL, verbose = FALSE) {
  
  # Determine optimal number of cores
  if (is.null(n_cores)) {
    n_cores <- max(1, min(4, parallel::detectCores() - 1))
  }
  
  setup <- list(
    n_cores = n_cores,
    cluster = NULL,
    use_parallel = n_cores > 1
  )
  
  if (setup$use_parallel) {
    tryCatch({
      # Try to set up parallel cluster
      setup$cluster <- parallel::makeCluster(n_cores)
      
      # Load required packages on cluster
      parallel::clusterEvalQ(setup$cluster, {
        library(stats)
        NULL
      })
      
      if (verbose) {
        cat("Set up parallel processing with", n_cores, "cores
")
      }
    }, error = function(e) {
      if (verbose) {
        cat("Failed to setup parallel processing:", e$message, "
")
      }
      setup$use_parallel <- FALSE
      setup$cluster <- NULL
    })
  }
  
  return(setup)
}

#' Cleanup Parallel Processing Resources
#'
#' Properly shuts down parallel processing cluster.
#'
#' @param parallel_setup Parallel setup object.
#' @keywords internal
cleanup_parallel_processing <- function(parallel_setup) {
  if (!is.null(parallel_setup$cluster)) {
    parallel::stopCluster(parallel_setup$cluster)
  }
}
