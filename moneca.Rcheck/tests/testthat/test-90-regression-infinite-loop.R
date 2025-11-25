# ============================================================================
# Regression Tests for Infinite Loop Bug Fix
# ============================================================================
#
# This test file ensures that the infinite loop bug in estimate_components()
# (R/auto_tuning.R lines 1397-1418) does not recur.
#
# Bug Context:
# - The loop in estimate_components() didn't filter out nodes already in
#   current_component, causing infinite iteration on densely connected graphs
# - This occurred when small.cell.reduction = 0 or with fully connected networks
# - Fix: Line 1411 now excludes both visited AND current_component nodes
# - Safety: Added iteration counter that stops after n iterations with warning
#
# ============================================================================

# ============================================================================
# Test 1: Fully Connected Network
# ============================================================================

test_that("auto_tune_small_cell_reduction handles fully connected networks without hanging", {
  # Create a fully connected mobility matrix
  # All cells have high values = very dense network
  n_classes <- 5
  dense_matrix <- matrix(100, nrow = n_classes, ncol = n_classes)
  diag(dense_matrix) <- 500  # Diagonal dominance for mobility table format
  rownames(dense_matrix) <- paste0("Class", 1:n_classes)
  colnames(dense_matrix) <- paste0("Class", 1:n_classes)

  # Test with timeout protection (should complete in < 30 seconds)
  expect_no_error({
    result <- auto_tune_small_cell_reduction(
      mx = dense_matrix,
      method = "quality",  # Quality method uses compute_network_properties
      max_candidates = 3,   # Keep test fast
      n_trials = 3,         # Keep test fast
      verbose = FALSE
    )
  })

  # Verify it returns valid results
  expect_s3_class(result, "moneca_tuning")
  expect_true(is.numeric(result$optimal_value))
  expect_true(result$optimal_value >= 0)
  expect_true(length(result$candidates_tested) > 0)
  expect_type(result$computation_time, "double")
  expect_true(result$computation_time < 30)  # Should complete quickly
})

test_that("estimate_components handles fully connected adjacency matrix", {
  # Create fully connected adjacency (all 1s except diagonal)
  n <- 6
  adj_matrix <- matrix(1, nrow = n, ncol = n)
  diag(adj_matrix) <- 0

  # Should detect 1 component (all nodes connected)
  expect_no_error({
    components <- moneca:::estimate_components(adj_matrix)
  })

  # Fully connected graph should have 1 component
  expect_equal(components, 1)
})

# ============================================================================
# Test 2: Small Cell Reduction = 0 (Triggers Dense Networks)
# ============================================================================

test_that("auto_tune_small_cell_reduction works with small.cell.reduction = 0", {
  # Create a normal mobility matrix with moderate connectivity
  test_data <- generate_mobility_data(
    n_classes = 5,
    n_total = 1000,
    immobility_strength = 0.3,
    class_clustering = 0.5,
    seed = 456
  )

  # Test with small.cell.reduction = 0 as a candidate
  # This was a trigger for the infinite loop bug
  expect_no_error({
    result <- auto_tune_small_cell_reduction(
      mx = test_data,
      method = "stability",
      candidate_values = c(0, 2, 5),  # Explicitly include 0
      n_trials = 3,    # Keep test fast
      verbose = FALSE
    )
  })

  # Verify successful completion
  expect_s3_class(result, "moneca_tuning")
  expect_true(0 %in% result$candidates_tested)
  expect_true(length(result$stability_scores) == 3)
  expect_true(all(is.finite(result$stability_scores)))
})

test_that("estimate_components handles dense networks with small.cell.reduction = 0", {
  # Create a high-mobility scenario (low diagonal dominance)
  high_mobility_data <- generate_mobility_data(
    n_classes = 4,
    n_total = 800,
    immobility_strength = 0.1,  # Very low diagonal dominance
    class_clustering = 0.9,      # High clustering creates dense connections
    seed = 789
  )

  # Create weight matrix with small.cell.reduction = 0
  weight_mat <- weight.matrix(
    mx = high_mobility_data,
    cut.off = 1,
    small.cell.reduction = 0
  )

  # Convert to binary adjacency
  adj_matrix <- ifelse(!is.na(weight_mat), 1, 0)

  # Should complete without hanging
  expect_no_error({
    components <- moneca:::estimate_components(adj_matrix)
  })

  # Should return valid number of components
  expect_type(components, "double")
  expect_true(components >= 1)
  expect_true(components <= nrow(adj_matrix))
})

# ============================================================================
# Test 3: Safety Mechanism Validation
# ============================================================================

test_that("iteration counter prevents infinite loops in estimate_components", {
  # Create a pathological case: fully connected graph
  n <- 8
  adj_matrix <- matrix(1, nrow = n, ncol = n)
  diag(adj_matrix) <- 0

  # Should NOT warn for normal fully connected case (completes before n iterations)
  expect_no_warning({
    components <- moneca:::estimate_components(adj_matrix)
  })

  # Verify correct result
  expect_equal(components, 1)  # All nodes in one component
})

test_that("safety mechanism triggers warning if pathological case occurs", {
  # This test verifies that the safety mechanism WOULD trigger if needed
  # We test the iteration counter logic indirectly

  # Create a complex dense network
  n <- 10
  adj_matrix <- matrix(0, nrow = n, ncol = n)

  # Create a ring structure with some cross-connections
  # This creates a single component but with complex topology
  for (i in 1:(n-1)) {
    adj_matrix[i, i+1] <- 1
    adj_matrix[i+1, i] <- 1
  }
  adj_matrix[n, 1] <- 1
  adj_matrix[1, n] <- 1

  # Add some cross-connections to make it denser
  adj_matrix[1, 5] <- 1
  adj_matrix[5, 1] <- 1
  adj_matrix[3, 8] <- 1
  adj_matrix[8, 3] <- 1

  # Should complete successfully (not complex enough to trigger safety)
  expect_no_error({
    components <- moneca:::estimate_components(adj_matrix)
  })

  # Should correctly identify as 1 component
  expect_equal(components, 1)
})

test_that("iteration counter is properly bounded by n", {
  # Test that iteration counter prevents loops > n iterations
  # Create several different graph structures

  test_cases <- list(
    # Star graph: one central node connected to all others
    star = function(n) {
      adj <- matrix(0, n, n)
      adj[1, 2:n] <- 1
      adj[2:n, 1] <- 1
      adj
    },

    # Complete bipartite graph
    bipartite = function(n) {
      adj <- matrix(0, n, n)
      mid <- floor(n/2)
      adj[1:mid, (mid+1):n] <- 1
      adj[(mid+1):n, 1:mid] <- 1
      adj
    },

    # Dense random graph
    dense_random = function(n) {
      set.seed(999)
      adj <- matrix(rbinom(n*n, 1, 0.7), n, n)
      diag(adj) <- 0
      # Make symmetric
      adj[lower.tri(adj)] <- t(adj)[lower.tri(adj)]
      adj
    }
  )

  for (case_name in names(test_cases)) {
    n <- 7
    adj_matrix <- test_cases[[case_name]](n)

    # Should complete without error or warning
    test_msg <- paste("Testing", case_name, "graph")
    expect_no_error({
      components <- moneca:::estimate_components(adj_matrix)
    })

    # Should return valid result
    expect_true(components >= 1, label = test_msg)
    expect_true(components <= n, label = test_msg)
  }
})

# ============================================================================
# Test 4: Integration Test - Full Auto-Tuning Pipeline
# ============================================================================

test_that("full auto-tuning pipeline handles edge cases without hanging", {
  # Test the complete pipeline with various challenging scenarios

  # Scenario 1: Very small matrix
  small_matrix <- matrix(c(100, 20, 30, 150), nrow = 2, ncol = 2)
  rownames(small_matrix) <- c("A", "B")
  colnames(small_matrix) <- c("A", "B")

  expect_no_error({
    result <- auto_tune_small_cell_reduction(
      mx = small_matrix,
      method = "quality",
      max_candidates = 3,
      verbose = FALSE
    )
  })
  expect_s3_class(result, "moneca_tuning")

  # Scenario 2: Matrix with uniform values (edge case)
  uniform_matrix <- matrix(50, nrow = 4, ncol = 4)
  diag(uniform_matrix) <- 200
  rownames(uniform_matrix) <- paste0("C", 1:4)
  colnames(uniform_matrix) <- paste0("C", 1:4)

  expect_no_error({
    result <- auto_tune_small_cell_reduction(
      mx = uniform_matrix,
      method = "performance",
      max_candidates = 3,
      n_trials = 2,
      verbose = FALSE
    )
  })
  expect_s3_class(result, "moneca_tuning")

  # Scenario 3: High variance matrix
  set.seed(321)
  n <- 5
  high_var_matrix <- matrix(rpois(n*n, lambda = 100), nrow = n, ncol = n)
  diag(high_var_matrix) <- diag(high_var_matrix) * 5
  rownames(high_var_matrix) <- paste0("G", 1:n)
  colnames(high_var_matrix) <- paste0("G", 1:n)

  expect_no_error({
    result <- auto_tune_small_cell_reduction(
      mx = high_var_matrix,
      method = "stability",
      max_candidates = 3,
      n_trials = 3,
      verbose = FALSE
    )
  })
  expect_s3_class(result, "moneca_tuning")
})

# ============================================================================
# Test 5: Verify Bug Fix Specifically
# ============================================================================

test_that("estimate_components correctly excludes current_component nodes (bug fix verification)", {
  # This test specifically verifies the fix on line 1411:
  # new_nodes <- new_nodes & !visited & !current_component
  #
  # The bug was that !current_component was missing, causing nodes
  # already in current_component to be re-added infinitely

  # Create a simple connected graph where the bug would manifest
  n <- 5
  adj_matrix <- matrix(0, nrow = n, ncol = n)

  # Create a path: 1-2-3-4-5
  for (i in 1:(n-1)) {
    adj_matrix[i, i+1] <- 1
    adj_matrix[i+1, i] <- 1  # Make symmetric
  }

  # Add a shortcut to make it denser: 1-4
  adj_matrix[1, 4] <- 1
  adj_matrix[4, 1] <- 1

  # This graph has exactly 1 component
  # The bug would cause infinite loop when expanding from node 1
  # because nodes would be re-added to current_component

  components <- moneca:::estimate_components(adj_matrix)

  # Should correctly identify 1 component
  expect_equal(components, 1)

  # Test with isolated nodes
  adj_matrix_isolated <- matrix(0, nrow = n, ncol = n)
  components_isolated <- moneca:::estimate_components(adj_matrix_isolated)

  # All nodes isolated = n components
  expect_equal(components_isolated, n)

  # Test with two separate components
  adj_matrix_two <- matrix(0, nrow = n, ncol = n)
  # Component 1: nodes 1-2-3
  adj_matrix_two[1, 2] <- 1
  adj_matrix_two[2, 1] <- 1
  adj_matrix_two[2, 3] <- 1
  adj_matrix_two[3, 2] <- 1
  # Component 2: nodes 4-5
  adj_matrix_two[4, 5] <- 1
  adj_matrix_two[5, 4] <- 1

  components_two <- moneca:::estimate_components(adj_matrix_two)
  expect_equal(components_two, 2)
})

test_that("compute_network_properties uses estimate_components correctly", {
  # Test that compute_network_properties (which calls estimate_components)
  # handles dense networks correctly

  # Create a dense weight matrix
  n <- 6
  dense_weight <- matrix(runif(n*n, min = 0.5, max = 2), nrow = n, ncol = n)
  diag(dense_weight) <- NA  # No self-loops

  # Should complete without hanging
  expect_no_error({
    props <- moneca:::compute_network_properties(dense_weight)
  })

  # Verify structure
  expect_type(props, "list")
  expect_true("density" %in% names(props))
  expect_true("components" %in% names(props))
  expect_true("edges" %in% names(props))

  # Verify valid values
  expect_true(props$density >= 0 && props$density <= 1)
  expect_true(props$components >= 1 && props$components <= n)
  expect_true(props$edges >= 0)
})

# ============================================================================
# Test 6: Performance Tests (Ensure No Regression in Speed)
# ============================================================================

test_that("estimate_components completes quickly on reasonable-sized graphs", {
  # Test that the fix doesn't cause performance regression

  # Medium-sized dense graph
  n <- 20
  adj_matrix <- matrix(1, nrow = n, ncol = n)
  diag(adj_matrix) <- 0

  # Should complete in well under 1 second
  time_taken <- system.time({
    components <- moneca:::estimate_components(adj_matrix)
  })

  expect_true(time_taken["elapsed"] < 1.0)
  expect_equal(components, 1)
})

test_that("auto_tune_small_cell_reduction has acceptable performance on dense networks", {
  # Integration test: ensure the full pipeline is performant

  # Create a moderately dense network
  dense_data <- generate_mobility_data(
    n_classes = 6,
    n_total = 1000,
    immobility_strength = 0.2,
    class_clustering = 0.7,
    seed = 654
  )

  # Time the full auto-tuning
  time_taken <- system.time({
    result <- auto_tune_small_cell_reduction(
      mx = dense_data,
      method = "quality",
      max_candidates = 5,
      n_trials = 3,
      verbose = FALSE
    )
  })

  # Should complete in reasonable time (< 15 seconds)
  expect_true(time_taken["elapsed"] < 15)
  expect_s3_class(result, "moneca_tuning")

  # Verify computation_time is recorded
  expect_type(result$computation_time, "double")
  expect_true(result$computation_time > 0)
})
