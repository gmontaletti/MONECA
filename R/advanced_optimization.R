# ============================================================================
# ADVANCED OPTIMIZATION METHODS FOR AUTO-TUNING
# ============================================================================

# ============================================================================
# CROSS-VALIDATION FUNCTIONS
# ============================================================================

#' Evaluate Candidates using Cross-Validation
#'
#' Cross-validation approach for robust parameter selection with uncertainty
#' quantification and confidence intervals.
#'
#' @param mx Mobility matrix.
#' @param candidate_values Candidates to evaluate.
#' @param cut.off Edge threshold.
#' @param cv_folds Number of CV folds.
#' @param cache Cache object.
#' @param parallel_setup Parallel processing setup.
#' @param verbose Whether to print progress.
#' @return List with CV results including confidence intervals.
#' @keywords internal
evaluate_cv_candidates <- function(mx, candidate_values, cut.off, cv_folds,
                                  cache, parallel_setup, verbose) {
  
  n_candidates <- length(candidate_values)
  cv_scores <- numeric(n_candidates)
  cv_std_errors <- numeric(n_candidates)
  cv_confidence_intervals <- matrix(0, nrow = n_candidates, ncol = 2)
  colnames(cv_confidence_intervals) <- c("lower", "upper")
  
  # Create CV folds with stratification if possible
  n <- nrow(mx)
  fold_indices <- create_cv_folds_stratified(mx, cv_folds)
  
  if (verbose) {
    cat("Performing", cv_folds, "-fold cross-validation with", n_candidates, "candidates...\n")
  }
  
  # Define CV evaluation function for parallel processing
  evaluate_cv_candidate <- function(i) {
    candidate <- candidate_values[i]
    fold_scores <- numeric(cv_folds)
    
    for (fold in seq_len(cv_folds)) {
      # Split data
      test_indices <- fold_indices[[fold]]
      train_indices <- setdiff(seq_len(n), test_indices)
      
      if (length(train_indices) < 3 || length(test_indices) < 2) {
        fold_scores[fold] <- 0
        next
      }
      
      # Train on fold
      mx_train <- mx[train_indices, train_indices]
      
      tryCatch({
        # Compute clustering quality on training set
        weight_matrix <- weight.matrix(
          mx = mx_train,
          cut.off = cut.off,
          small.cell.reduction = candidate
        )
        
        if (!all(is.na(weight_matrix)) && sum(!is.na(weight_matrix)) > 0) {
          quality <- compute_clustering_quality_metrics_fast(weight_matrix)
          fold_scores[fold] <- quality$overall
        } else {
          fold_scores[fold] <- 0
        }
      }, error = function(e) {
        fold_scores[fold] <- 0
      })
    }
    
    # Compute statistics
    mean_score <- mean(fold_scores, na.rm = TRUE)
    std_error <- sd(fold_scores, na.rm = TRUE) / sqrt(cv_folds)
    
    # 95% confidence interval
    t_value <- qt(0.975, df = cv_folds - 1)
    ci_lower <- mean_score - t_value * std_error
    ci_upper <- mean_score + t_value * std_error
    
    return(list(
      mean_score = mean_score,
      std_error = std_error,
      ci_lower = ci_lower,
      ci_upper = ci_upper,
      fold_scores = fold_scores
    ))
  }
  
  # Execute CV evaluation (parallel or sequential)
  if (parallel_setup$use_parallel && n_candidates > 2) {
    # Parallel CV evaluation
    if (verbose) cat("Using parallel cross-validation...\n")
    
    parallel::clusterExport(parallel_setup$cluster, 
                          c("mx", "candidate_values", "cut.off", "cv_folds", "fold_indices"),
                          envir = environment())
    
    cv_results <- parallel::parLapply(parallel_setup$cluster, 
                                     seq_along(candidate_values),
                                     evaluate_cv_candidate)
  } else {
    # Sequential CV evaluation
    cv_results <- vector("list", n_candidates)
    
    for (i in seq_along(candidate_values)) {
      if (verbose) {
        cat("\rCandidate", i, "of", n_candidates, "- evaluating CV folds...")
        flush.console()
      }
      
      cv_results[[i]] <- evaluate_cv_candidate(i)
    }
  }
  
  # Extract results
  for (i in seq_len(n_candidates)) {
    cv_scores[i] <- cv_results[[i]]$mean_score
    cv_std_errors[i] <- cv_results[[i]]$std_error
    cv_confidence_intervals[i, ] <- c(cv_results[[i]]$ci_lower, cv_results[[i]]$ci_upper)
  }
  
  if (verbose) cat("\n")
  
  # Also get full evaluation for the CV results structure
  base_results <- evaluate_candidates_optimized(
    mx, candidate_values, cut.off, "quality", 5,
    cache, parallel_setup, FALSE, FALSE
  )
  
  # Add CV-specific results
  base_results$cv_scores <- cv_scores
  base_results$cv_std_errors <- cv_std_errors
  base_results$cv_confidence_intervals <- cv_confidence_intervals
  base_results$cv_detailed_results <- cv_results
  
  return(base_results)
}

#' Create Stratified Cross-Validation Folds
#'
#' Creates balanced folds for cross-validation with optional stratification
#' based on matrix properties.
#'
#' @param mx Mobility matrix for stratification analysis.
#' @param k Number of folds.
#' @return List of fold indices.
#' @keywords internal
create_cv_folds_stratified <- function(mx, k) {
  n <- nrow(mx)
  
  # Simple stratification based on row sums (mobility levels)
  if (n > 10) {
    row_sums <- rowSums(mx, na.rm = TRUE)
    
    # Create strata based on quantiles
    strata <- cut(row_sums, breaks = quantile(row_sums, probs = seq(0, 1, length.out = min(5, k + 1))), 
                  include.lowest = TRUE, labels = FALSE)
    strata[is.na(strata)] <- 1
    
    # Create balanced folds within strata
    folds <- create_stratified_folds(seq_len(n), strata, k)
  } else {
    # Use simple random folds for small datasets
    folds <- create_cv_folds(n, k)
  }
  
  return(folds)
}

#' Create Stratified Folds
#'
#' Helper function to create stratified folds.
#'
#' @param indices Vector of indices.
#' @param strata Strata labels.
#' @param k Number of folds.
#' @return List of fold indices.
#' @keywords internal
create_stratified_folds <- function(indices, strata, k) {
  folds <- vector("list", k)
  
  # Initialize folds
  for (i in seq_len(k)) {
    folds[[i]] <- integer(0)
  }
  
  # Assign indices to folds within each stratum
  unique_strata <- unique(strata)
  
  for (stratum in unique_strata) {
    stratum_indices <- indices[strata == stratum]
    n_stratum <- length(stratum_indices)
    
    # Shuffle indices within stratum
    stratum_indices <- sample(stratum_indices)
    
    # Assign to folds
    for (i in seq_len(n_stratum)) {
      fold_id <- ((i - 1) %% k) + 1
      folds[[fold_id]] <- c(folds[[fold_id]], stratum_indices[i])
    }
  }
  
  return(folds)
}

#' Create Cross-Validation Folds
#'
#' Creates balanced folds for cross-validation (fallback method).
#'
#' @param n Number of observations.
#' @param k Number of folds.
#' @return List of fold indices.
#' @keywords internal
create_cv_folds <- function(n, k) {
  # Create balanced folds
  fold_size <- n %/% k
  remainder <- n %% k
  
  indices <- sample(seq_len(n))
  folds <- list()
  
  start_idx <- 1
  for (i in seq_len(k)) {
    end_idx <- start_idx + fold_size - 1
    if (i <= remainder) {
      end_idx <- end_idx + 1
    }
    
    folds[[i]] <- indices[start_idx:min(end_idx, n)]
    start_idx <- end_idx + 1
  }
  
  return(folds)
}

# ============================================================================
# BAYESIAN OPTIMIZATION FUNCTIONS
# ============================================================================

#' Evaluate Candidates using Bayesian Optimization
#'
#' Advanced optimization using Gaussian Process models with acquisition functions.
#'
#' @param mx Mobility matrix.
#' @param candidate_range Range of candidate values.
#' @param cut.off Edge threshold.
#' @param n_iterations Number of optimization iterations.
#' @param cache Cache object.
#' @param verbose Whether to print progress.
#' @param acquisition_function Acquisition function ("ei", "ucb", "pi").
#' @return List with Bayesian optimization results.
#' @keywords internal
evaluate_bayesian_candidates <- function(mx, candidate_range, cut.off, 
                                        n_iterations, cache, verbose,
                                        acquisition_function = "ei") {
  
  if (!requireNamespace("DiceKriging", quietly = TRUE)) {
    warning("DiceKriging package not available for Bayesian optimization")
    return(NULL)
  }
  
  # Initialize with space-filling design
  n_initial <- min(5, max(3, n_iterations %/% 3))
  initial_candidates <- create_latin_hypercube_sample(candidate_range, n_initial)
  
  all_candidates <- initial_candidates
  all_scores <- numeric(n_initial)
  
  # Evaluate initial points
  if (verbose) {
    cat("Bayesian optimization: evaluating", n_initial, "initial points...\n")
  }
  
  for (i in seq_along(initial_candidates)) {
    all_scores[i] <- evaluate_single_objective(mx, initial_candidates[i], cut.off)
    if (verbose) {
      cat("  Initial point", i, ": candidate =", round(initial_candidates[i], 3), 
          ", score =", round(all_scores[i], 3), "\n")
    }
  }
  
  # Bayesian optimization iterations
  best_score <- max(all_scores)
  
  for (iter in seq_len(n_iterations - n_initial)) {
    if (verbose) {
      cat("\rBayesian iteration", iter, "of", n_iterations - n_initial, 
          "- best score:", round(best_score, 3))
      flush.console()
    }
    
    # Fit Gaussian Process model
    tryCatch({
      # Prepare design matrix
      design_matrix <- matrix(all_candidates, ncol = 1)
      
      # Fit GP model
      gp_model <- DiceKriging::km(
        design = design_matrix,
        response = all_scores,
        covtype = "matern5_2",  # More flexible than matern3_2
        nugget.estim = TRUE      # Handle noise
      )
      
      # Find next candidate using acquisition function
      next_candidate <- optimize_acquisition_function(
        gp_model, candidate_range, all_scores, acquisition_function
      )
      
      # Ensure we don't evaluate the same point twice
      min_distance <- min(abs(all_candidates - next_candidate))
      if (min_distance < 1e-6) {
        # Add small random perturbation
        next_candidate <- next_candidate + runif(1, -0.1, 0.1) * 
                         (candidate_range[2] - candidate_range[1])
        next_candidate <- max(candidate_range[1], min(candidate_range[2], next_candidate))
      }
      
      # Evaluate next candidate
      next_score <- evaluate_single_objective(mx, next_candidate, cut.off)
      
      # Update data
      all_candidates <- c(all_candidates, next_candidate)
      all_scores <- c(all_scores, next_score)
      
      if (next_score > best_score) {
        best_score <- next_score
      }
      
    }, error = function(e) {
      if (verbose) {
        cat("\nGP model fitting failed:", e$message, "- using random sampling\n")
      }
      # Fallback to random sampling if GP fails
      next_candidate <- runif(1, candidate_range[1], candidate_range[2])
      next_score <- evaluate_single_objective(mx, next_candidate, cut.off)
      
      all_candidates <- c(all_candidates, next_candidate)
      all_scores <- c(all_scores, next_score)
    })
  }
  
  if (verbose) cat("\n")
  
  # Get full evaluation for best candidates
  n_best <- min(10, length(all_scores))
  best_indices <- order(all_scores, decreasing = TRUE)[1:n_best]
  best_candidates <- all_candidates[best_indices]
  
  # Remove duplicates and sort
  unique_candidates <- unique(round(best_candidates, 6))
  unique_candidates <- sort(unique_candidates)
  
  # Full evaluation of best candidates
  base_results <- evaluate_candidates_optimized(
    mx, unique_candidates, cut.off, "quality", 5,
    cache, list(use_parallel = FALSE), FALSE, FALSE
  )
  
  base_results$tested_candidates <- unique_candidates
  base_results$bayesian_scores <- all_scores[best_indices][1:length(unique_candidates)]
  base_results$bayesian_iterations <- n_iterations
  base_results$acquisition_function <- acquisition_function
  
  return(base_results)
}

#' Create Latin Hypercube Sample
#'
#' Creates space-filling initial design for Bayesian optimization.
#'
#' @param bounds Vector with min and max values.
#' @param n_samples Number of samples to generate.
#' @return Vector of sampled points.
#' @keywords internal
create_latin_hypercube_sample <- function(bounds, n_samples) {
  # Simple 1D Latin hypercube sampling
  intervals <- seq(0, 1, length.out = n_samples + 1)
  
  # Sample one point from each interval
  samples <- numeric(n_samples)
  for (i in seq_len(n_samples)) {
    samples[i] <- runif(1, intervals[i], intervals[i + 1])
  }
  
  # Shuffle and scale to bounds
  samples <- sample(samples)
  scaled_samples <- bounds[1] + samples * (bounds[2] - bounds[1])
  
  return(scaled_samples)
}

#' Optimize Acquisition Function
#'
#' Finds next candidate that optimizes the acquisition function.
#'
#' @param gp_model Gaussian Process model.
#' @param candidate_range Range of candidates.
#' @param observed_scores Current observed scores.
#' @param acquisition_function Type of acquisition function.
#' @return Next candidate to evaluate.
#' @keywords internal
optimize_acquisition_function <- function(gp_model, candidate_range, observed_scores, 
                                         acquisition_function = "ei") {
  
  # Create fine grid for optimization
  grid_points <- seq(candidate_range[1], candidate_range[2], length.out = 200)
  
  # Evaluate acquisition function on grid
  if (acquisition_function == "ei") {
    acq_values <- compute_expected_improvement(gp_model, grid_points, observed_scores)
  } else if (acquisition_function == "ucb") {
    acq_values <- compute_upper_confidence_bound(gp_model, grid_points)
  } else if (acquisition_function == "pi") {
    acq_values <- compute_probability_improvement(gp_model, grid_points, observed_scores)
  } else {
    # Default to Expected Improvement
    acq_values <- compute_expected_improvement(gp_model, grid_points, observed_scores)
  }
  
  # Find maximum
  best_idx <- which.max(acq_values)
  
  # Add small random noise to avoid getting stuck
  if (length(best_idx) > 0) {
    noise <- runif(1, -0.001, 0.001) * (candidate_range[2] - candidate_range[1])
    return(grid_points[best_idx] + noise)
  } else {
    # Fallback to random point
    return(runif(1, candidate_range[1], candidate_range[2]))
  }
}

#' Compute Expected Improvement
#'
#' Calculates Expected Improvement acquisition function values.
#'
#' @param gp_model Gaussian Process model.
#' @param grid_points Points to evaluate.
#' @param observed_scores Current best scores.
#' @return Vector of EI values.
#' @keywords internal
compute_expected_improvement <- function(gp_model, grid_points, observed_scores) {
  n_points <- length(grid_points)
  ei_values <- numeric(n_points)
  
  best_so_far <- max(observed_scores)
  
  for (i in seq_len(n_points)) {
    pred <- predict(gp_model, matrix(grid_points[i], ncol = 1), type = "UK")
    
    mean_pred <- pred$mean
    sd_pred <- sqrt(pred$sd2)
    
    if (sd_pred > 1e-10) {
      z <- (mean_pred - best_so_far) / sd_pred
      ei_values[i] <- (mean_pred - best_so_far) * pnorm(z) + sd_pred * dnorm(z)
    } else {
      ei_values[i] <- 0
    }
  }
  
  return(ei_values)
}

#' Compute Upper Confidence Bound
#'
#' Calculates Upper Confidence Bound acquisition function values.
#'
#' @param gp_model Gaussian Process model.
#' @param grid_points Points to evaluate.
#' @param kappa Exploration parameter.
#' @return Vector of UCB values.
#' @keywords internal
compute_upper_confidence_bound <- function(gp_model, grid_points, kappa = 2.0) {
  n_points <- length(grid_points)
  ucb_values <- numeric(n_points)
  
  for (i in seq_len(n_points)) {
    pred <- predict(gp_model, matrix(grid_points[i], ncol = 1), type = "UK")
    
    mean_pred <- pred$mean
    sd_pred <- sqrt(pred$sd2)
    
    ucb_values[i] <- mean_pred + kappa * sd_pred
  }
  
  return(ucb_values)
}

#' Compute Probability of Improvement
#'
#' Calculates Probability of Improvement acquisition function values.
#'
#' @param gp_model Gaussian Process model.
#' @param grid_points Points to evaluate.
#' @param observed_scores Current observed scores.
#' @return Vector of PI values.
#' @keywords internal
compute_probability_improvement <- function(gp_model, grid_points, observed_scores) {
  n_points <- length(grid_points)
  pi_values <- numeric(n_points)
  
  best_so_far <- max(observed_scores)
  
  for (i in seq_len(n_points)) {
    pred <- predict(gp_model, matrix(grid_points[i], ncol = 1), type = "UK")
    
    mean_pred <- pred$mean
    sd_pred <- sqrt(pred$sd2)
    
    if (sd_pred > 1e-10) {
      z <- (mean_pred - best_so_far) / sd_pred
      pi_values[i] <- pnorm(z)
    } else {
      pi_values[i] <- 0
    }
  }
  
  return(pi_values)
}

#' Evaluate Single Objective for Bayesian Optimization
#'
#' Quick evaluation of single objective with error handling.
#'
#' @param mx Mobility matrix.
#' @param candidate Parameter candidate.
#' @param cut.off Edge threshold.
#' @return Objective score.
#' @keywords internal
evaluate_single_objective <- function(mx, candidate, cut.off) {
  tryCatch({
    weight_matrix <- weight.matrix(
      mx = mx,
      cut.off = cut.off,
      small.cell.reduction = candidate
    )
    
    if (all(is.na(weight_matrix)) || sum(!is.na(weight_matrix)) == 0) {
      return(0)
    }
    
    quality <- compute_clustering_quality_metrics_fast(weight_matrix)
    return(quality$overall)
  }, error = function(e) {
    return(0)
  })
}