# ============================================================================
# Simplified Test Suite for MONECA Auto-Tuning Functionality  
# ============================================================================

test_that("auto_tune_small_cell_reduction basic functionality works", {
  # Create test matrix using generate_mobility_data for compatibility
  test_mx <- generate_mobility_data(n_classes = 4, n_total = 1000, seed = 123)
  
  result <- auto_tune_small_cell_reduction(
    test_mx, 
    method = "quality",
    n_trials = 3,
    max_candidates = 5,
    verbose = FALSE
  )
  
  expect_s3_class(result, "moneca_tuning")
  expect_type(result$optimal_value, "double")
  expect_true(result$optimal_value >= 0)
  expect_equal(result$tuning_method, "quality")
  expect_true(length(result$candidates_tested) > 0)
  expect_type(result$computation_time, "double")
})

test_that("generate_candidate_values produces valid output", {
  test_mx <- generate_mobility_data(n_classes = 4, n_total = 1000, seed = 123)
  
  candidates <- generate_candidate_values(test_mx, method = "hybrid", max_candidates = 8)
  
  expect_type(candidates, "double")
  expect_true(all(candidates >= 0))
  expect_true(length(candidates) > 0)
  expect_lte(length(candidates), 8)
  expect_equal(candidates, sort(candidates))  # Should be sorted
})

test_that("assess_clustering_stability returns valid score", {
  test_mx <- generate_mobility_data(n_classes = 4, n_total = 1000, seed = 123)
  
  stability_score <- assess_clustering_stability(
    mx = test_mx,
    small.cell.reduction = 2,
    n_bootstrap = 5  # Small for speed
  )
  
  expect_type(stability_score, "double")
  expect_length(stability_score, 1)
  expect_gte(stability_score, 0)
  expect_lte(stability_score, 1)
})

test_that("compute_tuning_metrics works with weight matrix", {
  test_mx <- generate_mobility_data(n_classes = 4, n_total = 1000, seed = 123)
  weight_mx <- weight.matrix(test_mx, small.cell.reduction = 1)
  
  metrics <- compute_tuning_metrics(weight_matrix = weight_mx)
  
  expect_type(metrics, "list")
  expect_true("network_density" %in% names(metrics))
  expect_true("overall" %in% names(metrics))
  expect_gte(metrics$network_density, 0)
  expect_lte(metrics$network_density, 1)
  expect_gte(metrics$overall, 0)
  expect_lte(metrics$overall, 1)
})

test_that("weight.matrix with auto_tune works", {
  test_mx <- generate_mobility_data(n_classes = 4, n_total = 1000, seed = 123)
  
  # Without auto-tuning (control)
  wm_control <- weight.matrix(test_mx, auto_tune = FALSE, small.cell.reduction = 2)
  
  # With auto-tuning
  wm_tuned <- weight.matrix(test_mx, auto_tune = TRUE, tune_verbose = FALSE)
  
  expect_is(wm_control, "matrix")
  expect_is(wm_tuned, "matrix")
  expect_equal(dim(wm_control), dim(wm_tuned))  # Should have same dimensions
})

test_that("different tuning methods work", {
  test_mx <- generate_mobility_data(n_classes = 4, n_total = 800, seed = 123)
  
  methods <- c("stability", "quality", "performance")
  
  for (method in methods) {
    result <- auto_tune_small_cell_reduction(
      test_mx, 
      method = method,
      n_trials = 3,  # Small for speed
      max_candidates = 5,
      verbose = FALSE
    )
    
    expect_s3_class(result, "moneca_tuning")
    expect_equal(result$tuning_method, method)
    expect_gte(result$optimal_value, 0)
  }
})

test_that("input validation works correctly", {
  test_mx <- generate_mobility_data(n_classes = 4, n_total = 1000, seed = 123)
  
  # Invalid method
  expect_error(
    auto_tune_small_cell_reduction(test_mx, method = "invalid"),
    "Method must be one of"
  )
  
  # Invalid performance weight
  expect_error(
    auto_tune_small_cell_reduction(test_mx, performance_weight = -0.1),
    "performance_weight must be between 0 and 1"
  )
  
  # Non-square matrix
  non_square <- matrix(1:12, nrow = 3, ncol = 4)
  expect_error(
    auto_tune_small_cell_reduction(non_square),
    "square"
  )
})

test_that("print method for moneca_tuning works", {
  test_mx <- generate_mobility_data(n_classes = 4, n_total = 1000, seed = 123)
  
  result <- auto_tune_small_cell_reduction(
    test_mx, 
    method = "quality",
    n_trials = 3,
    max_candidates = 5,
    verbose = FALSE
  )
  
  expect_output(print(result), "MONECA Auto-Tuning Results")
  expect_output(print(result), "Optimal small.cell.reduction")
})