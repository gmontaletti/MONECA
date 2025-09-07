# Tests for temporal stability analysis functions

# Helper function to create temporal results for testing
create_test_temporal_result <- function() {
  test_matrices <- lapply(1:5, function(i) {
    generate_mobility_data(n_classes = 4, seed = 100 + i)
  })
  
  result <- moneca_temporal(
    matrix_list = test_matrices,
    window_size = 2,
    segment.levels = 2,
    verbose = FALSE
  )
  
  return(result)
}

test_that("temporal_stability_analysis basic functionality", {
  
  temporal_result <- create_test_temporal_result()
  
  stability <- temporal_stability_analysis(
    temporal_result,
    verbose = FALSE
  )
  
  # Check return class and structure
  expect_s3_class(stability, "temporal_stability")
  expect_true(is.list(stability))
  
  # Check required components
  required_components <- c("stability_scores", "node_trajectories", "change_points",
                          "stable_core", "volatile_nodes", "segment_lifetimes",
                          "transition_summary", "volatility_scores", "parameters")
  expect_true(all(required_components %in% names(stability)))
  
  # Check stability_scores structure
  expect_true(is.data.frame(stability$stability_scores))
  expected_cols <- c("segment", "first_appearance", "last_appearance", 
                     "persistence", "avg_size", "size_cv", "stability", "core_size")
  expect_true(all(expected_cols %in% names(stability$stability_scores)))
  
  # Check parameters
  expect_equal(stability$parameters$stability_threshold, 0.7)
  expect_equal(stability$parameters$min_segment_size, 2)
})

test_that("temporal_stability_analysis input validation", {
  
  # Test invalid input type
  expect_error(
    temporal_stability_analysis("not_temporal_result"),
    "Input must be a moneca_temporal object"
  )
  
  # Test with invalid threshold
  temporal_result <- create_test_temporal_result()
  
  expect_no_error({
    stability <- temporal_stability_analysis(
      temporal_result,
      stability_threshold = 0.5,
      verbose = FALSE
    )
  })
  
  expect_equal(stability$parameters$stability_threshold, 0.5)
})

test_that("node trajectory computation works correctly", {
  
  temporal_result <- create_test_temporal_result()
  
  stability <- temporal_stability_analysis(
    temporal_result,
    compute_node_metrics = TRUE,
    verbose = FALSE
  )
  
  # Check trajectory matrix properties
  expect_true(is.matrix(stability$node_trajectories))
  expect_true(nrow(stability$node_trajectories) > 0)
  expect_equal(ncol(stability$node_trajectories), length(temporal_result$stable_labels))
  
  # Check volatility scores
  expect_true(is.numeric(stability$volatility_scores))
  expect_equal(length(stability$volatility_scores), nrow(stability$node_trajectories))
  expect_true(all(stability$volatility_scores >= 0))
  expect_true(all(stability$volatility_scores <= 1))
  
  # Check stable core and volatile nodes
  expect_true(is.character(stability$stable_core))
  expect_true(is.character(stability$volatile_nodes))
  
  # Stable and volatile should be disjoint
  expect_equal(length(intersect(stability$stable_core, stability$volatile_nodes)), 0)
})

test_that("segment lifetime computation works", {
  
  temporal_result <- create_test_temporal_result()
  
  stability <- temporal_stability_analysis(
    temporal_result,
    verbose = FALSE
  )
  
  lifetimes <- stability$segment_lifetimes
  
  # Check structure
  expect_true(is.data.frame(lifetimes))
  expected_cols <- c("segment", "birth_window", "death_window", "lifetime", "continuous")
  expect_true(all(expected_cols %in% names(lifetimes)))
  
  # Check logical consistency
  valid_rows <- !is.na(lifetimes$birth_window) & !is.na(lifetimes$death_window)
  if (sum(valid_rows) > 0) {
    expect_true(all(lifetimes$death_window[valid_rows] >= lifetimes$birth_window[valid_rows]))
    expect_true(all(lifetimes$lifetime[valid_rows] >= 1))
  }
})

test_that("change point detection works", {
  
  temporal_result <- create_test_temporal_result()
  
  stability <- temporal_stability_analysis(
    temporal_result,
    verbose = FALSE
  )
  
  # Check change points properties
  expect_true(is.numeric(stability$change_points))
  expect_true(all(stability$change_points >= 2))  # Can't have change point at window 1
  expect_true(all(stability$change_points <= length(temporal_result$stable_labels)))
  
  # Check change magnitudes
  expect_true(is.numeric(stability$change_magnitudes))
  expect_equal(length(stability$change_magnitudes), 
               length(temporal_result$stable_labels) - 1)
  expect_true(all(stability$change_magnitudes >= 0))
  expect_true(all(stability$change_magnitudes <= 1))
})

test_that("transition summary computation works", {
  
  temporal_result <- create_test_temporal_result()
  
  stability <- temporal_stability_analysis(
    temporal_result,
    verbose = FALSE
  )
  
  trans_summary <- stability$transition_summary
  
  # Check required components
  expected_components <- c("diagonal_weight", "off_diagonal_weight", 
                          "max_transition", "n_active_transitions", "entropy")
  expect_true(all(expected_components %in% names(trans_summary)))
  
  # Check logical consistency
  expect_true(trans_summary$diagonal_weight >= 0)
  expect_true(trans_summary$off_diagonal_weight >= 0)
  expect_true(trans_summary$max_transition >= 0)
  expect_true(trans_summary$max_transition <= 1)
  expect_true(trans_summary$n_active_transitions >= 0)
  expect_true(trans_summary$entropy >= 0)
})

test_that("print method works for temporal_stability", {
  
  temporal_result <- create_test_temporal_result()
  
  stability <- temporal_stability_analysis(
    temporal_result,
    verbose = FALSE
  )
  
  # Test that print doesn't error
  expect_output(print(stability), "Temporal Stability Analysis Results")
  expect_output(print(stability), "Summary Statistics:")
  expect_output(print(stability), "Node Stability:")
  expect_output(print(stability), "Segment Stability")
})

test_that("export_node_trajectories works correctly", {
  
  temporal_result <- create_test_temporal_result()
  
  stability <- temporal_stability_analysis(
    temporal_result,
    compute_node_metrics = TRUE,
    verbose = FALSE
  )
  
  # Test basic export
  trajectories <- export_node_trajectories(stability)
  
  expect_true(is.data.frame(trajectories))
  expected_cols <- c("node", "window", "segment", "volatility")
  expect_true(all(expected_cols %in% names(trajectories)))
  
  # Check dimensions
  n_nodes <- nrow(stability$node_trajectories)
  n_windows <- ncol(stability$node_trajectories)
  expect_equal(nrow(trajectories), n_nodes * n_windows)
  
  # Test without volatility
  trajectories_no_vol <- export_node_trajectories(stability, include_volatility = FALSE)
  expect_false("volatility" %in% names(trajectories_no_vol))
  
  # Test error with missing trajectories
  stability_no_traj <- stability
  stability_no_traj$node_trajectories <- NULL
  
  expect_error(
    export_node_trajectories(stability_no_traj),
    "No node trajectories found"
  )
})

test_that("temporal_stability_analysis handles edge cases", {
  
  # Create minimal temporal result
  minimal_matrices <- lapply(1:2, function(i) {
    generate_mobility_data(n_classes = 2, seed = 200 + i)
  })
  
  minimal_temporal <- moneca_temporal(
    matrix_list = minimal_matrices,
    window_size = 2,
    verbose = FALSE
  )
  
  stability <- temporal_stability_analysis(
    minimal_temporal,
    verbose = FALSE
  )
  
  expect_s3_class(stability, "temporal_stability")
  expect_equal(stability$parameters$n_windows, 1)
  
  # With only one window, some metrics should be empty or have special values
  expect_equal(length(stability$change_points), 0)
  expect_equal(length(stability$change_magnitudes), 0)
})

test_that("stability metrics are computed correctly", {
  
  temporal_result <- create_test_temporal_result()
  
  stability <- temporal_stability_analysis(
    temporal_result,
    verbose = FALSE
  )
  
  # Check that persistence values are between 0 and 1
  valid_persistence <- !is.na(stability$stability_scores$persistence)
  if (sum(valid_persistence) > 0) {
    expect_true(all(stability$stability_scores$persistence[valid_persistence] >= 0))
    expect_true(all(stability$stability_scores$persistence[valid_persistence] <= 1))
  }
  
  # Check that stability values are between 0 and 1
  valid_stability <- !is.na(stability$stability_scores$stability)
  if (sum(valid_stability) > 0) {
    expect_true(all(stability$stability_scores$stability[valid_stability] >= 0))
    expect_true(all(stability$stability_scores$stability[valid_stability] <= 1))
  }
  
  # Check that core size is non-negative integer
  valid_core_size <- !is.na(stability$stability_scores$core_size)
  if (sum(valid_core_size) > 0) {
    expect_true(all(stability$stability_scores$core_size[valid_core_size] >= 0))
    expect_true(all(stability$stability_scores$core_size[valid_core_size] == 
                   floor(stability$stability_scores$core_size[valid_core_size])))
  }
})

test_that("temporal_stability_analysis works without node metrics", {
  
  temporal_result <- create_test_temporal_result()
  
  stability <- temporal_stability_analysis(
    temporal_result,
    compute_node_metrics = FALSE,
    verbose = FALSE
  )
  
  # Should still have basic components
  expect_s3_class(stability, "temporal_stability")
  
  # But node-specific components should be NULL or empty
  expect_null(stability$node_trajectories)
  expect_null(stability$volatility_scores)
  expect_equal(length(stability$stable_core), 0)
  expect_equal(length(stability$volatile_nodes), 0)
})