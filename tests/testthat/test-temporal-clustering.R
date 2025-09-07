# Tests for temporal clustering functions

# Setup test data
create_test_matrices <- function(n_periods = 5, n_classes = 4, seed = 123) {
  set.seed(seed)
  
  matrices <- list()
  for (i in 1:n_periods) {
    # Create slightly different matrices for each period
    base_matrix <- generate_mobility_data(n_classes = n_classes, seed = seed + i)
    matrices[[i]] <- base_matrix
  }
  
  names(matrices) <- paste0("period_", 1:n_periods)
  return(matrices)
}

test_that("moneca_temporal handles basic input correctly", {
  
  # Create test data
  test_matrices <- create_test_matrices(n_periods = 4, n_classes = 4)
  
  # Test basic functionality
  result <- moneca_temporal(
    matrix_list = test_matrices,
    window_size = 2,
    segment.levels = 2,
    verbose = FALSE
  )
  
  # Check return class and structure
  expect_s3_class(result, "moneca_temporal")
  expect_true(is.list(result))
  
  # Check required components
  required_components <- c("windows", "segment_maps", "stable_labels", 
                          "transition_matrix", "moneca_results", "parameters")
  expect_true(all(required_components %in% names(result)))
  
  # Check dimensions
  expect_equal(length(result$windows), 3)  # 4 periods - 2 window_size + 1
  expect_equal(length(result$segment_maps), 3)
  expect_equal(length(result$stable_labels), 3)
  expect_equal(length(result$moneca_results), 3)
})

test_that("moneca_temporal handles edge cases correctly", {
  
  # Test minimum window size
  test_matrices <- create_test_matrices(n_periods = 3, n_classes = 4)
  
  result <- moneca_temporal(
    matrix_list = test_matrices,
    window_size = 3,
    segment.levels = 2,
    verbose = FALSE
  )
  
  expect_equal(length(result$windows), 1)
  
  # Test single class (should handle gracefully)
  single_class_matrices <- lapply(1:3, function(i) {
    matrix(c(10, 1, 1, 10), nrow = 2, ncol = 2,
           dimnames = list(c("A", "Total"), c("A", "Total")))
  })
  
  expect_no_error({
    result_single <- moneca_temporal(
      matrix_list = single_class_matrices,
      window_size = 2,
      verbose = FALSE
    )
  })
})

test_that("moneca_temporal input validation works", {
  
  # Test invalid input types
  expect_error(
    moneca_temporal(matrix_list = "not_a_list"),
    "matrix_list must be a list"
  )
  
  # Test insufficient periods
  test_matrices <- create_test_matrices(n_periods = 2, n_classes = 4)
  
  expect_error(
    moneca_temporal(matrix_list = test_matrices, window_size = 5),
    "Number of periods must be at least equal to window_size"
  )
  
  # Test invalid stability method
  expect_error(
    moneca_temporal(
      matrix_list = test_matrices,
      stability_method = "invalid_method"
    ),
    "stability_method must be one of"
  )
  
  # Test invalid aggregation method  
  expect_error(
    moneca_temporal(
      matrix_list = test_matrices,
      aggregation_method = "invalid_method"
    ),
    "aggregation_method must be one of"
  )
})

test_that("aggregate_mobility_window works correctly", {
  
  # Create simple test matrices
  m1 <- matrix(c(10, 2, 1, 20), nrow = 2, ncol = 2)
  m2 <- matrix(c(15, 3, 2, 25), nrow = 2, ncol = 2)
  m3 <- matrix(c(12, 1, 3, 22), nrow = 2, ncol = 2)
  
  matrices <- list(m1, m2, m3)
  
  # Test mean aggregation
  result_mean <- aggregate_mobility_window(matrices, method = "mean")
  expected_mean <- (m1 + m2 + m3) / 3
  expect_equal(result_mean, expected_mean)
  
  # Test sum aggregation
  result_sum <- aggregate_mobility_window(matrices, method = "sum")
  expected_sum <- m1 + m2 + m3
  expect_equal(result_sum, expected_sum)
  
  # Test weighted aggregation
  result_weighted <- aggregate_mobility_window(matrices, method = "weighted")
  expect_true(is.matrix(result_weighted))
  expect_equal(dim(result_weighted), c(2, 2))
})

test_that("segment matching works across different methods", {
  
  # Create test data with clear segment patterns
  test_matrices <- create_test_matrices(n_periods = 4, n_classes = 5)
  
  # Test Hungarian method
  result_hungarian <- moneca_temporal(
    matrix_list = test_matrices,
    window_size = 2,
    stability_method = "hungarian",
    verbose = FALSE
  )
  
  expect_s3_class(result_hungarian, "moneca_temporal")
  expect_true("stable_mapping" %in% names(result_hungarian))
  
  # Test Jaccard method
  result_jaccard <- moneca_temporal(
    matrix_list = test_matrices,
    window_size = 2,
    stability_method = "jaccard",
    verbose = FALSE
  )
  
  expect_s3_class(result_jaccard, "moneca_temporal")
  
  # Test overlap method
  result_overlap <- moneca_temporal(
    matrix_list = test_matrices,
    window_size = 2,
    stability_method = "overlap",
    verbose = FALSE
  )
  
  expect_s3_class(result_overlap, "moneca_temporal")
  
  # All methods should produce valid stable labels
  expect_true(all(sapply(result_hungarian$stable_labels, function(x) {
    all(c("name", "membership", "stable_label") %in% names(x))
  })))
  
  expect_true(all(sapply(result_jaccard$stable_labels, function(x) {
    all(c("name", "membership", "stable_label") %in% names(x))
  })))
  
  expect_true(all(sapply(result_overlap$stable_labels, function(x) {
    all(c("name", "membership", "stable_label") %in% names(x))
  })))
})

test_that("transition matrix computation works", {
  
  test_matrices <- create_test_matrices(n_periods = 4, n_classes = 4)
  
  result <- moneca_temporal(
    matrix_list = test_matrices,
    window_size = 2,
    verbose = FALSE
  )
  
  transition_matrix <- result$transition_matrix
  
  # Check basic properties
  expect_true(is.matrix(transition_matrix))
  expect_equal(nrow(transition_matrix), ncol(transition_matrix))
  expect_true(all(rownames(transition_matrix) == colnames(transition_matrix)))
  
  # Check that probabilities are valid
  expect_true(all(transition_matrix >= 0, na.rm = TRUE))
  expect_true(all(transition_matrix <= 1, na.rm = TRUE))
  
  # Check that rows sum to 1 or 0 (no transitions from that segment)
  row_sums <- rowSums(transition_matrix, na.rm = TRUE)
  expect_true(all(row_sums <= 1.01))  # Allow small numerical errors
})

test_that("print method works for moneca_temporal", {
  
  test_matrices <- create_test_matrices(n_periods = 3, n_classes = 4)
  
  result <- moneca_temporal(
    matrix_list = test_matrices,
    window_size = 2,
    verbose = FALSE
  )
  
  # Test that print doesn't error
  expect_output(print(result), "Temporal MONECA Analysis Results")
  expect_output(print(result), "Time periods:")
  expect_output(print(result), "Window size:")
})

test_that("moneca_temporal preserves matrix names and dimensions", {
  
  # Create matrices with specific names
  test_matrices <- create_test_matrices(n_periods = 3, n_classes = 3)
  
  # Ensure consistent naming
  for (i in seq_along(test_matrices)) {
    rownames(test_matrices[[i]]) <- c("Class1", "Class2", "Class3", "Total")
    colnames(test_matrices[[i]]) <- c("Class1", "Class2", "Class3", "Total")
  }
  
  result <- moneca_temporal(
    matrix_list = test_matrices,
    window_size = 2,
    verbose = FALSE
  )
  
  # Check that node names are preserved
  all_node_names <- unique(unlist(lapply(result$stable_labels, function(x) x$name)))
  expected_names <- c("Class1", "Class2", "Class3")
  
  expect_true(all(expected_names %in% all_node_names))
})

test_that("moneca_temporal handles matrices with different structures", {
  
  # Test warning for inconsistent row names
  m1 <- generate_mobility_data(n_classes = 4, seed = 123)
  m2 <- generate_mobility_data(n_classes = 4, seed = 124)
  
  # Change row names of second matrix
  rownames(m2)[1] <- "DifferentName"
  colnames(m2)[1] <- "DifferentName"
  
  test_matrices <- list(m1, m2)
  
  expect_warning(
    result <- moneca_temporal(
      matrix_list = test_matrices,
      window_size = 2,
      verbose = FALSE
    ),
    "Row names differ"
  )
  
  # Should still complete successfully
  expect_s3_class(result, "moneca_temporal")
})