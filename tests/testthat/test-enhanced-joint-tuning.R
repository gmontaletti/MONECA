# Fast test suite for enhanced auto_tune_joint_parameters function
# Tests for Progress Tracking System and Custom Grid Vector Support

# Setup test data helper - smaller for speed
create_test_data <- function(n_classes = 3, seed = 123) {
  set.seed(seed)
  generate_mobility_data(n_classes = n_classes, seed = seed)
}

# =============================================================================
# PROGRESS TRACKING SYSTEM TESTS (using internal functions)
# =============================================================================

test_that("ProgressTracker class creates correctly", {
  tracker <- moneca:::ProgressTracker("Test Task", 10, verbose = FALSE)
  
  # Check class
  expect_s3_class(tracker, "ProgressTracker")
  
  # Check structure
  expect_equal(tracker$task_name, "Test Task")
  expect_equal(tracker$total_items, 10)
  expect_false(tracker$verbose)
  
  # Check files exist
  expect_true(file.exists(tracker$progress_file))
  expect_true(file.exists(tracker$timing_file))
  
  # Cleanup
  moneca:::cleanup_progress(tracker)
})

test_that("progress tracking functions work", {
  tracker <- moneca:::ProgressTracker("Update Test", 5, verbose = FALSE)
  
  # Initial progress should be 0
  expect_equal(moneca:::get_progress(tracker), 0)
  
  # Update progress
  moneca:::update_progress(tracker, increment = 1)
  expect_equal(moneca:::get_progress(tracker), 1)
  
  # Display progress should not error
  expect_silent(moneca:::display_progress(tracker, 2))
  
  # Cleanup
  moneca:::cleanup_progress(tracker)
})

test_that("progress tracking gracefully handles errors", {
  tracker <- moneca:::ProgressTracker("Error Test", 3, verbose = FALSE)
  
  # Remove progress file to trigger error condition
  unlink(tracker$progress_file)
  
  # These should not error even when files are missing
  expect_silent(moneca:::update_progress(tracker, 1))
  expect_equal(moneca:::get_progress(tracker), 0)  # Should return 0 when cannot read
  
  moneca:::cleanup_progress(tracker)
})

# =============================================================================
# CUSTOM GRID VECTOR TESTS  
# =============================================================================

test_that("custom scr_values parameter works correctly", {
  test_mx <- create_test_data(n_classes = 3)
  custom_scr <- c(0, 1, 2)
  
  result <- auto_tune_joint_parameters(
    mx = test_mx,
    method = "grid",
    scr_values = custom_scr,
    cutoff_range = c(0.8, 1.2),
    n_grid_points = 2,  # Small grid for speed
    n_bootstrap = 3,    # Minimal bootstraps
    verbose = FALSE
  )
  
  expect_s3_class(result, "moneca_joint_tuning")
  expect_true(result$optimal_scr %in% custom_scr)
  expect_equal(result$scr_values, custom_scr)
  
  # Check that optimization surface has correct dimensions
  expect_equal(nrow(result$optimization_surface), length(custom_scr))
})

test_that("custom cutoff_values parameter works correctly", {
  test_mx <- create_test_data(n_classes = 3) 
  custom_cutoff <- c(0.8, 1.0, 1.2)
  
  result <- auto_tune_joint_parameters(
    mx = test_mx,
    method = "grid",
    scr_range = c(0, 2),
    cutoff_values = custom_cutoff,
    n_grid_points = 2,
    n_bootstrap = 3,
    verbose = FALSE
  )
  
  expect_s3_class(result, "moneca_joint_tuning")
  expect_true(result$optimal_cutoff %in% custom_cutoff)
  expect_equal(result$cutoff_values, custom_cutoff)
  
  # Check that optimization surface has correct dimensions
  expect_equal(ncol(result$optimization_surface), length(custom_cutoff))
})

test_that("both custom vectors work together", {
  test_mx <- create_test_data(n_classes = 3)
  custom_scr <- c(0, 1)
  custom_cutoff <- c(0.8, 1.2)
  
  result <- auto_tune_joint_parameters(
    mx = test_mx,
    method = "grid",
    scr_values = custom_scr,
    cutoff_values = custom_cutoff,
    n_bootstrap = 3,
    verbose = FALSE
  )
  
  expect_s3_class(result, "moneca_joint_tuning")
  expect_true(result$optimal_scr %in% custom_scr)
  expect_true(result$optimal_cutoff %in% custom_cutoff)
  expect_equal(result$scr_values, custom_scr)
  expect_equal(result$cutoff_values, custom_cutoff)
  
  # Should have 2x2 = 4 parameter combinations
  expect_equal(nrow(result$parameter_grid), 4)
  expect_equal(dim(result$optimization_surface), c(2, 2))
})

test_that("mixed usage works - custom SCR with cutoff range", {
  test_mx <- create_test_data(n_classes = 3)
  custom_scr <- c(0, 2)
  
  result <- auto_tune_joint_parameters(
    mx = test_mx,
    method = "grid", 
    scr_values = custom_scr,
    cutoff_range = c(0.8, 1.2),
    n_grid_points = 2,
    n_bootstrap = 3,
    verbose = FALSE
  )
  
  expect_s3_class(result, "moneca_joint_tuning")
  expect_equal(result$scr_values, custom_scr)
  expect_equal(length(result$cutoff_values), 2)  # From n_grid_points
  expect_true(all(result$cutoff_values >= 0.8 & result$cutoff_values <= 1.2))
})

test_that("custom values validation works correctly", {
  test_mx <- create_test_data(n_classes = 3)
  
  # Test non-numeric scr_values
  expect_error(
    auto_tune_joint_parameters(
      mx = test_mx,
      scr_values = c("a", "b"),
      verbose = FALSE
    ),
    "scr_values must be a numeric vector"
  )
  
  # Test negative scr_values  
  expect_error(
    auto_tune_joint_parameters(
      mx = test_mx,
      scr_values = c(-1, 0, 1),
      verbose = FALSE
    ),
    "scr_values must contain non-negative values"
  )
  
  # Test non-numeric cutoff_values
  expect_error(
    auto_tune_joint_parameters(
      mx = test_mx,
      cutoff_values = c("x", "y"),
      verbose = FALSE
    ),
    "cutoff_values must be a numeric vector"
  )
  
  # Test non-positive cutoff_values
  expect_error(
    auto_tune_joint_parameters(
      mx = test_mx,
      cutoff_values = c(0, 1),
      verbose = FALSE
    ),
    "cutoff_values must contain positive values"
  )
})

test_that("custom values are converted and sorted correctly", {
  test_mx <- create_test_data(n_classes = 3)
  
  # Test non-integer SCR values get converted
  expect_message(
    result <- auto_tune_joint_parameters(
      mx = test_mx,
      method = "grid",
      scr_values = c(2.7, 1.3, 0.5),
      cutoff_values = c(1.0),
      n_bootstrap = 3,
      verbose = TRUE
    ),
    "Rounding to nearest integers"
  )
  
  # Should be converted and sorted: 0.5->1, 1.3->1, 2.7->3 -> unique: 1, 3
  expected_scr <- c(1L, 3L)  # Duplicates removed and sorted
  expect_equal(result$scr_values, expected_scr)
  
  # Test cutoff values are sorted and duplicates removed
  result2 <- auto_tune_joint_parameters(
    mx = test_mx,
    method = "grid", 
    scr_values = c(0, 1),
    cutoff_values = c(2.0, 1.0, 1.5, 1.0),  # Has duplicate
    n_bootstrap = 3,
    verbose = FALSE
  )
  
  expected_cutoff <- c(1.0, 1.5, 2.0)  # Sorted and unique
  expect_equal(result2$cutoff_values, expected_cutoff)
})

# =============================================================================
# BACKWARD COMPATIBILITY TESTS
# =============================================================================

test_that("existing range-based calls work unchanged", {
  test_mx <- create_test_data(n_classes = 3)
  
  # Old-style call should work exactly as before
  result_old_style <- auto_tune_joint_parameters(
    mx = test_mx,
    method = "grid",
    scr_range = c(0, 2),
    cutoff_range = c(0.8, 1.2),
    n_grid_points = 2,
    n_bootstrap = 3,
    verbose = FALSE
  )
  
  expect_s3_class(result_old_style, "moneca_joint_tuning")
  expect_equal(length(result_old_style$scr_values), 2)
  expect_equal(length(result_old_style$cutoff_values), 2)
})

test_that("progress tracking works with both parallel and sequential", {
  test_mx <- create_test_data(n_classes = 3)
  
  # Sequential execution
  expect_no_error({
    result_seq <- auto_tune_joint_parameters(
      mx = test_mx,
      method = "grid",
      n_grid_points = 2,
      n_bootstrap = 3,
      parallel = FALSE,
      verbose = FALSE
    )
  })
  
  expect_s3_class(result_seq, "moneca_joint_tuning")
})

# =============================================================================
# INTEGRATION TESTS
# =============================================================================

test_that("progress tracking and custom vectors work together", {
  test_mx <- create_test_data(n_classes = 3)
  custom_scr <- c(0, 1)
  custom_cutoff <- c(0.8, 1.2)
  
  # This should use both features together
  expect_no_error({
    result <- auto_tune_joint_parameters(
      mx = test_mx,
      method = "grid",
      scr_values = custom_scr,
      cutoff_values = custom_cutoff,
      n_bootstrap = 3,
      verbose = FALSE
    )
  })
  
  expect_s3_class(result, "moneca_joint_tuning")
  expect_equal(result$scr_values, custom_scr)
  expect_equal(result$cutoff_values, custom_cutoff)
  expect_equal(nrow(result$parameter_grid), 4)  # 2 SCR * 2 cutoff
})

test_that("adaptive method works with custom vectors", {
  test_mx <- create_test_data(n_classes = 3)
  
  # Adaptive should work with custom vectors in initial phase
  result <- auto_tune_joint_parameters(
    mx = test_mx,
    method = "adaptive",
    scr_values = c(0, 1),
    cutoff_values = c(0.8, 1.2),
    verbose = FALSE
  )
  
  expect_s3_class(result, "moneca_joint_tuning")
  expect_true(!is.null(result$refinement_history))
})

test_that("error handling works correctly", {
  test_mx <- create_test_data(n_classes = 3)
  
  # Single values should work
  result <- auto_tune_joint_parameters(
    mx = test_mx,
    method = "grid",
    scr_values = c(1),
    cutoff_values = c(1.0),
    n_bootstrap = 3,
    verbose = FALSE
  )
  
  expect_s3_class(result, "moneca_joint_tuning")
  expect_equal(result$optimal_scr, 1)
  expect_equal(result$optimal_cutoff, 1.0)
  expect_equal(dim(result$optimization_surface), c(1, 1))
})

test_that("verbose output works correctly with new features", {
  test_mx <- create_test_data(n_classes = 3)
  
  # Should output information about custom vectors
  expect_output(
    auto_tune_joint_parameters(
      mx = test_mx,
      method = "grid",
      scr_values = c(0, 1),
      cutoff_values = c(1.0, 1.2),
      n_bootstrap = 3,
      verbose = TRUE
    ),
    "Using custom SCR values"
  )
  
  expect_output(
    auto_tune_joint_parameters(
      mx = test_mx,
      method = "grid",
      scr_values = c(0, 1),
      cutoff_values = c(1.0, 1.2),
      n_bootstrap = 3,
      verbose = TRUE
    ),
    "Using custom cutoff values"
  )
})

test_that("plot_optimization_surface works with custom vectors", {
  # Create result with custom vectors
  test_mx <- create_test_data(n_classes = 3)
  result <- auto_tune_joint_parameters(
    mx = test_mx,
    method = "grid",
    scr_values = c(0, 1),
    cutoff_values = c(0.8, 1.2),
    n_bootstrap = 3,
    verbose = FALSE
  )
  
  # Plotting should work
  p <- plot_optimization_surface(result, type = "heatmap")
  expect_s3_class(p, "ggplot")
  
  p2 <- plot_optimization_surface(result, type = "contour")
  expect_s3_class(p2, "ggplot")
})

test_that("integration with moneca function works", {
  test_mx <- create_test_data(n_classes = 3)
  
  # Get optimal parameters using custom vectors
  result <- auto_tune_joint_parameters(
    mx = test_mx,
    method = "grid",
    scr_values = c(0, 1),
    cutoff_values = c(0.8, 1.2),
    n_bootstrap = 3,
    verbose = FALSE
  )
  
  # Use results with moneca function
  expect_no_error({
    segments <- moneca(
      test_mx,
      small.cell.reduction = result$optimal_scr,
      cut.off = result$optimal_cutoff,
      segment.levels = 2
    )
  })
})

# Final comprehensive integration test
test_that("all enhanced features work together in realistic scenario", {
  test_mx <- create_test_data(n_classes = 4)  # Slightly larger for realism
  
  # Full integration test with both new features
  result <- auto_tune_joint_parameters(
    mx = test_mx,
    method = "grid",
    scr_values = c(0, 1, 2),           # Custom SCR values
    cutoff_values = c(0.8, 1.0, 1.2), # Custom cutoff values  
    n_bootstrap = 5,
    objectives = c("stability", "quality"),
    seed = 123,
    verbose = FALSE,                   # Progress tracking enabled but quiet
    parallel = "auto"
  )
  
  expect_s3_class(result, "moneca_joint_tuning")
  expect_equal(result$scr_values, c(0, 1, 2))
  expect_equal(result$cutoff_values, c(0.8, 1.0, 1.2))
  expect_equal(nrow(result$parameter_grid), 9)  # 3 * 3 combinations
  expect_equal(dim(result$optimization_surface), c(3, 3))
  
  # Should be able to use result with moneca
  expect_no_error({
    segments <- moneca(
      test_mx,
      small.cell.reduction = result$optimal_scr,
      cut.off = result$optimal_cutoff,
      segment.levels = 2
    )
  })
})