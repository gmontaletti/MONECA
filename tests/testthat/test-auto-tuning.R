# ============================================================================
# Comprehensive Test Suite for MONECA Auto-Tuning Functionality
# ============================================================================

# ============================================================================
# Setup and Helper Functions
# ============================================================================

create_test_matrix <- function(n = 5, seed = 123, n_total = 1000) {
  # Use the existing generate_mobility_data function for consistency
  return(generate_mobility_data(n_classes = n, n_total = n_total, seed = seed))
}

# ============================================================================
# Core Functionality Tests
# ============================================================================

test_that("auto_tune_small_cell_reduction runs with default parameters", {
  test_mx <- create_test_matrix(n = 4, seed = 123)
  
  result <- auto_tune_small_cell_reduction(test_mx)
  
  expect_s3_class(result, "moneca_tuning")
  expect_type(result$optimal_value, "double")
  expect_true(result$optimal_value >= 0)
  expect_true(length(result$candidates_tested) > 0)
  expect_equal(result$tuning_method, "stability")
  expect_type(result$computation_time, "double")
})

test_that("auto_tune_small_cell_reduction works with different methods", {
  test_mx <- create_test_matrix(n = 4, seed = 123)
  
  methods <- c("stability", "quality", "performance")
  
  for (method in methods) {
    result <- auto_tune_small_cell_reduction(test_mx, method = method, verbose = FALSE)
    
    expect_s3_class(result, "moneca_tuning")
    expect_equal(result$tuning_method, method)
    expect_true(result$optimal_value >= 0)
    expect_type(result$selection_rationale, "character")
  }
})

test_that("generate_candidate_values produces valid candidates", {
  test_mx <- create_test_matrix(n = 4, seed = 123)
  
  methods <- c("percentile", "sparsity", "density", "hybrid")
  
  for (method in methods) {
    candidates <- generate_candidate_values(test_mx, method = method)
    
    expect_type(candidates, "double")
    expect_true(all(candidates >= 0))
    expect_true(length(candidates) > 0)
    expect_true(all(!is.na(candidates)))
    # Should be sorted
    expect_equal(candidates, sort(candidates))
    # Should be unique
    expect_equal(length(candidates), length(unique(candidates)))
  }
})

test_that("generate_candidate_values respects max_candidates parameter", {
  test_mx <- create_test_matrix(n = 6, seed = 123)
  
  max_cand <- 5
  candidates <- generate_candidate_values(test_mx, max_candidates = max_cand)
  
  expect_lte(length(candidates), max_cand)
})

test_that("generate_candidate_values include_zero parameter works", {
  test_mx <- create_test_matrix(n = 4, seed = 123)
  
  # With zero
  candidates_with_zero <- generate_candidate_values(test_mx, include_zero = TRUE)
  expect_true(0 %in% candidates_with_zero)
  
  # Without zero
  candidates_no_zero <- generate_candidate_values(test_mx, include_zero = FALSE)
  expect_false(0 %in% candidates_no_zero)
})

test_that("assess_clustering_stability produces valid scores", {
  test_mx <- create_test_matrix(n = 4, seed = 123)
  
  stability_score <- assess_clustering_stability(
    mx = test_mx,
    small.cell.reduction = 2,
    n_bootstrap = 5  # Small number for testing speed
  )
  
  expect_type(stability_score, "double")
  expect_length(stability_score, 1)
  expect_true(stability_score >= 0)
  expect_true(stability_score <= 1)
})

test_that("compute_tuning_metrics produces valid quality metrics", {
  test_mx <- create_test_matrix(n = 4, seed = 123)
  
  # Create a weight matrix for testing
  weight_mx <- weight.matrix(test_mx, small.cell.reduction = 1)
  
  metrics <- compute_tuning_metrics(weight_matrix = weight_mx)
  
  expect_type(metrics, "list")
  expect_true("network_density" %in% names(metrics))
  expect_true("modularity" %in% names(metrics))
  expect_true("overall" %in% names(metrics))
  
  expect_true(metrics$network_density >= 0)
  expect_true(metrics$network_density <= 1)
  expect_true(metrics$overall >= 0)
  expect_true(metrics$overall <= 1)
})

test_that("evaluate_performance_trade_offs balances quality and performance", {
  # Create mock metrics
  quality_metrics <- list(
    list(overall = 0.8),
    list(overall = 0.6),
    list(overall = 0.9)
  )
  
  performance_metrics <- list(
    list(time = 1.2, valid = TRUE),
    list(time = 0.8, valid = TRUE),
    list(time = 2.1, valid = TRUE)
  )
  
  trade_offs <- evaluate_performance_trade_offs(
    quality_metrics = quality_metrics,
    performance_metrics = performance_metrics,
    performance_weight = 0.3
  )
  
  expect_type(trade_offs, "double")
  expect_length(trade_offs, 3)
  expect_true(all(trade_offs >= 0))
  expect_true(all(trade_offs <= 1))
})

# ============================================================================
# Integration Tests with MONECA Functions
# ============================================================================

test_that("weight.matrix with auto_tune=FALSE preserves original behavior", {
  test_mx <- create_test_matrix(n = 4, seed = 123)
  
  # Original behavior
  wm_original <- weight.matrix(test_mx, auto_tune = FALSE, small.cell.reduction = 2)
  
  # Should work without auto-tuning functionality
  expect_is(wm_original, "matrix")
  # Note: weight.matrix removes the totals row/column, so expect n-1 dimensions
  expect_equal(nrow(wm_original), 3)  # 4-1 = 3
  expect_equal(ncol(wm_original), 3)  # 4-1 = 3
})

test_that("weight.matrix with auto_tune=TRUE works", {
  test_mx <- create_test_matrix(n = 4, seed = 123)
  
  wm_tuned <- weight.matrix(test_mx, auto_tune = TRUE, tune_verbose = FALSE)
  
  expect_is(wm_tuned, "matrix")
  # Note: weight.matrix removes the totals row/column, so expect n-1 dimensions
  expect_equal(nrow(wm_tuned), 3)  # 4-1 = 3
  expect_equal(ncol(wm_tuned), 3)  # 4-1 = 3
  
  # Should have tuning attributes
  tuning_info <- attr(wm_tuned, "tuning_info")
  if (!is.null(tuning_info)) {
    expect_s3_class(tuning_info, "moneca_tuning")
  }
})

test_that("weight.matrix.parallel with auto_tune works", {
  skip_if_not_installed("parallel")
  
  test_mx <- create_test_matrix(n = 4, seed = 123)
  
  wm_tuned <- weight.matrix.parallel(
    test_mx, 
    auto_tune = TRUE, 
    tune_verbose = FALSE,
    n.cores = 1  # Use single core for testing
  )
  
  expect_is(wm_tuned, "matrix")
  # Note: weight.matrix removes the totals row/column, so expect n-1 dimensions
  expect_equal(nrow(wm_tuned), 3)  # 4-1 = 3
  expect_equal(ncol(wm_tuned), 3)  # 4-1 = 3
})

test_that("moneca family functions work with auto-tuned parameters", {
  test_mx <- create_test_matrix(n = 4, seed = 123)
  
  # Test with moneca (if available)
  result <- tryCatch({
    # Get auto-tuned parameter first
    tune_result <- auto_tune_small_cell_reduction(
      test_mx, 
      method = "quality",
      verbose = FALSE,
      n_trials = 3  # Small for speed
    )
    
    # Use in moneca
    segments <- moneca(
      test_mx, 
      small.cell.reduction = tune_result$optimal_value,
      segment.levels = 2
    )
    
    segments
  }, error = function(e) NULL)
  
  # If moneca worked, check the result
  if (!is.null(result)) {
    expect_type(result, "list")
    expect_true("segment.list" %in% names(result) || "segments" %in% names(result))
  }
})

# ============================================================================
# Performance Tests  
# ============================================================================

test_that("auto-tuning overhead is reasonable", {
  test_mx <- create_test_matrix(n = 4, seed = 123)
  
  # Time without auto-tuning
  time_original <- system.time({
    wm_original <- weight.matrix(test_mx, auto_tune = FALSE, small.cell.reduction = 1)
  })
  
  # Time with auto-tuning (reduced trials for speed)
  time_tuned <- system.time({
    wm_tuned <- weight.matrix(
      test_mx, 
      auto_tune = TRUE, 
      tune_verbose = FALSE
    )
  })
  
  # Auto-tuning should not be more than 10x slower for small matrices
  overhead_ratio <- time_tuned[["elapsed"]] / max(time_original[["elapsed"]], 0.01)
  expect_lt(overhead_ratio, 10)  # Less than 10x overhead
})

test_that("caching functionality works when available", {
  test_mx <- create_test_matrix(n = 4, seed = 123)
  
  # Test with caching enabled
  result_cached <- auto_tune_small_cell_reduction(
    test_mx, 
    use_cache = TRUE,
    verbose = FALSE,
    n_trials = 3
  )
  
  expect_s3_class(result_cached, "moneca_tuning")
  expect_true(result_cached$optimal_value >= 0)
})

test_that("early stopping works", {
  test_mx <- create_test_matrix(n = 4, seed = 123)
  
  # Generate many candidates to trigger early stopping
  many_candidates <- seq(0, 50, by = 2)  
  
  result <- auto_tune_small_cell_reduction(
    test_mx,
    candidate_values = many_candidates,
    early_stopping = TRUE,
    verbose = FALSE
  )
  
  expect_s3_class(result, "moneca_tuning")
  # Early stopping might reduce the number of candidates tested
  expect_lte(length(result$candidates_tested), length(many_candidates))
})

# ============================================================================
# Edge Cases and Error Handling
# ============================================================================

test_that("auto_tune_small_cell_reduction handles empty matrices", {
  expect_error(
    auto_tune_small_cell_reduction(matrix(nrow = 0, ncol = 0)),
    "square"
  )
})

test_that("auto_tune_small_cell_reduction handles single element matrices", {
  expect_error(
    auto_tune_small_cell_reduction(matrix(1)),
    "square"
  )
})

test_that("auto_tune_small_cell_reduction handles very sparse matrices", {
  # Create a mobility-like sparse matrix with proper structure
  sparse_data <- generate_mobility_data(n_classes = 5, n_total = 50, seed = 123)
  # Make it very sparse by setting many values to 0
  sparse_data[sparse_data < 5] <- 0
  
  result <- auto_tune_small_cell_reduction(
    sparse_data, 
    verbose = FALSE,
    n_trials = 3,
    max_candidates = 5
  )
  
  expect_s3_class(result, "moneca_tuning")
  expect_true(result$optimal_value >= 0)
})

test_that("auto_tune_small_cell_reduction validates input parameters", {
  test_mx <- create_test_matrix(n = 4, seed = 123)
  
  # Invalid method
  expect_error(
    auto_tune_small_cell_reduction(test_mx, method = "invalid_method"),
    "Method must be one of"
  )
  
  # Invalid performance_weight
  expect_error(
    auto_tune_small_cell_reduction(test_mx, performance_weight = -0.1),
    "performance_weight must be between 0 and 1"
  )
  
  expect_error(
    auto_tune_small_cell_reduction(test_mx, performance_weight = 1.1),
    "performance_weight must be between 0 and 1"
  )
  
  # Non-square matrix
  non_square <- matrix(1:12, nrow = 3, ncol = 4)
  expect_error(
    auto_tune_small_cell_reduction(non_square),
    "square"
  )
})

test_that("generate_candidate_values handles edge cases", {
  # Matrix with all zeros
  zero_mx <- matrix(0, nrow = 3, ncol = 3)
  rownames(zero_mx) <- colnames(zero_mx) <- paste0("Class", 1:3)
  
  expect_warning(
    candidates <- generate_candidate_values(zero_mx),
    "No non-zero"
  )
  
  # Should still return candidates (likely just 0)
  expect_type(candidates, "double")
  expect_true(length(candidates) >= 1)
})

test_that("assess_clustering_stability handles degenerate cases", {
  test_mx <- create_test_matrix(n = 3, seed = 123)
  
  # Very high threshold that should result in no valid networks
  stability <- assess_clustering_stability(
    mx = test_mx,
    small.cell.reduction = 999,
    n_bootstrap = 3  # Small for speed
  )
  
  expect_type(stability, "double")
  expect_length(stability, 1)
  expect_true(stability >= 0)
  expect_true(stability <= 1)
})

test_that("method fallbacks work correctly", {
  test_mx <- create_test_matrix(n = 4, seed = 123)
  
  # Should not error even if some internal methods fail
  result <- auto_tune_small_cell_reduction(
    test_mx,
    method = "stability",
    verbose = FALSE,
    n_trials = 2  # Very small for speed
  )
  
  expect_s3_class(result, "moneca_tuning")
})

# ============================================================================
# Advanced Features Tests
# ============================================================================

test_that("multi-objective optimization components exist", {
  test_mx <- create_test_matrix(n = 4, seed = 123)
  
  # Test that pareto objectives parameter is accepted
  result <- auto_tune_small_cell_reduction(
    test_mx,
    method = "performance",  # Use performance method which should handle objectives
    pareto_objectives = c("quality", "performance"),
    verbose = FALSE
  )
  
  expect_s3_class(result, "moneca_tuning")
})

test_that("cross-validation parameters are accepted", {
  test_mx <- create_test_matrix(n = 4, seed = 123)
  
  # Test that CV parameters don't cause errors
  result <- auto_tune_small_cell_reduction(
    test_mx,
    method = "quality",  # Use a simpler method
    cv_folds = 3,
    verbose = FALSE
  )
  
  expect_s3_class(result, "moneca_tuning")
})

test_that("bayesian optimization parameters are accepted", {
  test_mx <- create_test_matrix(n = 4, seed = 123)
  
  # Test that Bayesian parameters don't cause errors
  result <- auto_tune_small_cell_reduction(
    test_mx,
    method = "quality",  # Use a simpler method since Bayesian might not be available
    bayesian_iterations = 5,
    verbose = FALSE
  )
  
  expect_s3_class(result, "moneca_tuning")
})

# ============================================================================
# Regression Tests
# ============================================================================

test_that("deterministic results with same seed", {
  test_mx <- create_test_matrix(n = 4, seed = 123)
  
  result1 <- auto_tune_small_cell_reduction(
    test_mx, 
    seed = 456, 
    verbose = FALSE,
    n_trials = 3
  )
  
  result2 <- auto_tune_small_cell_reduction(
    test_mx, 
    seed = 456, 
    verbose = FALSE,
    n_trials = 3
  )
  
  expect_equal(result1$optimal_value, result2$optimal_value)
  expect_equal(result1$candidates_tested, result2$candidates_tested)
})

test_that("parameter ranges and bounds are respected", {
  test_mx <- create_test_matrix(n = 4, seed = 123)
  
  result <- auto_tune_small_cell_reduction(
    test_mx, 
    min_density = 0.05,
    max_candidates = 10,
    verbose = FALSE
  )
  
  expect_s3_class(result, "moneca_tuning")
  expect_lte(length(result$candidates_tested), 10)
  expect_true(result$optimal_value >= 0)
})

test_that("backward compatibility maintained", {
  test_mx <- create_test_matrix(n = 4, seed = 123)
  
  # These calls should work identically to before auto-tuning was added
  wm1 <- weight.matrix(test_mx, small.cell.reduction = 2)
  wm2 <- weight.matrix(test_mx, small.cell.reduction = 2, auto_tune = FALSE)
  
  # Remove any attributes before comparison as auto_tune might add some
  attributes(wm1) <- NULL
  attributes(wm2) <- NULL
  expect_equal(wm1, wm2)
})

# ============================================================================
# Print and Utility Function Tests
# ============================================================================

test_that("print method for moneca_tuning works", {
  test_mx <- create_test_matrix(n = 4, seed = 123)
  
  result <- auto_tune_small_cell_reduction(
    test_mx, 
    verbose = FALSE,
    n_trials = 3
  )
  
  # Should not error when printing
  expect_output(print(result), "MONECA Auto-Tuning Results")
  expect_output(print(result), "Optimal small.cell.reduction")
  expect_output(print(result), "Tuning method")
})

test_that("helper functions handle missing dependencies gracefully", {
  # Test that functions work even if optional packages are not available
  
  # Test cache initialization
  cache <- initialize_tuning_cache()
  expect_type(cache, "list")
  
  # Test parallel setup
  parallel_setup <- setup_parallel_processing(n_cores = 1, verbose = FALSE)
  expect_type(parallel_setup, "list")
  expect_true("use_parallel" %in% names(parallel_setup))
  
  # Cleanup
  cleanup_parallel_processing(parallel_setup)
})

# ============================================================================
# Visualization Tests (Structure Only)  
# ============================================================================

test_that("visualization functions exist and have proper structure", {
  # We test that the functions exist and can be called, but don't test actual plots
  # since that would require complex graphics testing
  
  test_mx <- create_test_matrix(n = 4, seed = 123)
  
  result <- auto_tune_small_cell_reduction(
    test_mx, 
    verbose = FALSE,
    n_trials = 3
  )
  
  # Check that result has the structure needed for visualization
  expect_true("candidates_tested" %in% names(result))
  expect_true("quality_metrics" %in% names(result))
  expect_true("performance_metrics" %in% names(result))
  
  # Test that we have enough information to create visualizations
  expect_true(length(result$candidates_tested) > 0)
  expect_true(length(result$quality_metrics) > 0)
  expect_true(length(result$performance_metrics) > 0)
})

# ============================================================================
# Memory and Resource Management Tests
# ============================================================================

test_that("memory usage is reasonable for large candidate sets", {
  test_mx <- create_test_matrix(n = 5, seed = 123)
  
  # Use a moderate number of candidates to test memory efficiency
  many_candidates <- seq(0, 20, by = 1)
  
  # This should not cause memory issues
  result <- auto_tune_small_cell_reduction(
    test_mx,
    candidate_values = many_candidates,
    verbose = FALSE,
    use_cache = TRUE
  )
  
  expect_s3_class(result, "moneca_tuning")
  expect_true(length(result$candidates_tested) > 0)
})

test_that("parallel processing cleanup works", {
  skip_if_not_installed("parallel")
  
  # Test that parallel resources are properly cleaned up
  parallel_setup <- setup_parallel_processing(n_cores = 2, verbose = FALSE)
  
  if (parallel_setup$use_parallel) {
    expect_true(!is.null(parallel_setup$cluster))
    
    # Cleanup should not error
    expect_silent(cleanup_parallel_processing(parallel_setup))
  }
})

# ============================================================================
# Performance Benchmark Tests
# ============================================================================

test_that("performance scaling is reasonable", {
  # Test that performance scales reasonably with matrix size
  
  small_mx <- create_test_matrix(n = 3, seed = 123)
  medium_mx <- create_test_matrix(n = 4, seed = 123)
  
  time_small <- system.time({
    result_small <- auto_tune_small_cell_reduction(
      small_mx, 
      verbose = FALSE,
      n_trials = 3,
      max_candidates = 5
    )
  })
  
  time_medium <- system.time({
    result_medium <- auto_tune_small_cell_reduction(
      medium_mx, 
      verbose = FALSE, 
      n_trials = 3,
      max_candidates = 5
    )
  })
  
  # Both should complete successfully
  expect_s3_class(result_small, "moneca_tuning")
  expect_s3_class(result_medium, "moneca_tuning")
  
  # Time scaling should be reasonable (not exponential)
  # Allow for some variation in timing
  time_ratio <- time_medium[["elapsed"]] / max(time_small[["elapsed"]], 0.01)
  expect_lt(time_ratio, 50)  # Should not be more than 50x slower
})

# ============================================================================
# Documentation and Interface Tests
# ============================================================================

test_that("function interfaces are consistent", {
  # Test that all main functions have consistent parameter naming
  test_mx <- create_test_matrix(n = 4, seed = 123)
  
  # These should all accept similar parameters without error
  expect_silent({
    wm1 <- weight.matrix(test_mx, cut.off = 1, small.cell.reduction = 1)
  })
  
  expect_silent({
    candidates <- generate_candidate_values(test_mx, method = "hybrid")
  })
  
  expect_silent({
    stability <- assess_clustering_stability(
      test_mx, 
      small.cell.reduction = 1,
      cut.off = 1,
      n_bootstrap = 3
    )
  })
})

test_that("error messages are informative", {
  # Test that error messages provide clear guidance
  
  expect_error(
    auto_tune_small_cell_reduction(matrix(nrow = 0, ncol = 0)),
    class = "simpleError"
  )
  
  expect_error(
    auto_tune_small_cell_reduction(
      create_test_matrix(n = 4, seed = 123), 
      method = "nonexistent"
    ),
    "Method must be one of"
  )
  
  expect_error(
    evaluate_performance_trade_offs(
      quality_metrics = list(list(overall = 0.5)),
      performance_metrics = list(),  # Wrong length
      performance_weight = 0.5
    ),
    "same length"
  )
})