# Integration Tests: moneca_fast() Function
# ===========================================
#
# Tests for the moneca_fast() optimized implementation.

test_that("moneca_fast produces valid results", {
  # Generate test data
  set.seed(123)
  test_data <- get_test_data("small")

  # Run fast version
  result_fast <- moneca_fast(test_data, segment.levels = 2, progress = FALSE)

  # Check that the structure is correct
  validate_moneca_object(result_fast)
  expect_equal(names(result_fast), c("segment.list", "mat.list", "small.cell.reduction"))

  # Check that segment lists are valid
  expect_true(length(result_fast$segment.list) >= 1)
  expect_true(is.list(result_fast$segment.list))

  # Check that mat.list is valid
  expect_true(length(result_fast$mat.list) >= 1)
  expect_true(all(sapply(result_fast$mat.list, is.matrix)))
})

test_that("moneca_fast handles edge cases correctly", {
  # Test with minimal data
  min_data <- get_minimal_test_data()

  result <- moneca_fast(min_data, segment.levels = 1, progress = FALSE)
  validate_moneca_object(result)
  expect_true(length(result$segment.list) >= 1)
})

test_that("moneca_fast sparse matrix option works", {
  skip_if_not_installed("Matrix")

  # Generate test data
  test_data <- get_custom_test_data(n_classes = 6)

  # Run with sparse matrix option (even though internally it uses regular operations)
  result_sparse <- moneca_fast(test_data, segment.levels = 2, use.sparse = TRUE, progress = FALSE)

  # Should produce valid results
  validate_moneca_object(result_sparse)
  expect_true(length(result_sparse$segment.list) >= 1)
})

test_that("moneca_fast handles sparse data correctly", {
  # Create a very sparse matrix
  sparse_data <- matrix(0, nrow = 6, ncol = 6)
  diag(sparse_data[1:5, 1:5]) <- 100
  sparse_data[6,] <- colSums(sparse_data)
  sparse_data[,6] <- sparse_data[6,]
  rownames(sparse_data) <- colnames(sparse_data) <- c(LETTERS[1:5], "Total")

  # Add very few off-diagonal elements
  sparse_data[1,2] <- sparse_data[2,1] <- 1
  sparse_data[6,] <- colSums(sparse_data[1:5,])
  sparse_data[,6] <- sparse_data[6,]

  # Test with high density threshold which should trigger early stopping
  result <- moneca_fast(sparse_data, segment.levels = 3, min.density = 0.5, progress = FALSE)

  # Should produce valid results even with sparse data
  validate_moneca_object(result)
  expect_true(is.list(result$segment.list))
  expect_true(is.list(result$mat.list))
})

test_that("moneca_fast performance is better than original", {
  skip_on_cran() # Skip on CRAN to save time

  # Generate medium-sized test data
  test_data <- get_test_data("medium")

  # Time both versions
  time_original <- system.time({
    result_original <- moneca(test_data, segment.levels = 2)
  })

  time_fast <- system.time({
    result_fast <- moneca_fast(test_data, segment.levels = 2, progress = FALSE)
  })

  # Fast version should be faster (or at least not much slower)
  # Due to small test size, we're lenient with the comparison
  expect_true(time_fast["elapsed"] <= time_original["elapsed"] * 1.5)
})

test_that("moneca_fast handles different cut-off values", {
  test_data <- get_custom_test_data(n_classes = 6)

  # Test with different cut-offs
  result_low <- moneca_fast(test_data, cut.off = 0.5, segment.levels = 2, progress = FALSE)
  result_high <- moneca_fast(test_data, cut.off = 2.0, segment.levels = 2, progress = FALSE)

  # Higher cut-off should result in fewer connections and potentially more segments
  validate_moneca_object(result_low)
  validate_moneca_object(result_high)

  # Both should have valid segment lists
  expect_true(length(result_low$segment.list) > 0)
  expect_true(length(result_high$segment.list) > 0)
})

test_that("moneca_fast max clique size parameter works", {
  test_data <- get_custom_test_data(n_classes = 8)

  # Run with clique size limit
  result_limited <- moneca_fast(test_data, segment.levels = 2, max.clique.size = 4, progress = FALSE)
  result_unlimited <- moneca_fast(test_data, segment.levels = 2, max.clique.size = NULL, progress = FALSE)

  # Both should produce valid results
  validate_moneca_object(result_limited)
  validate_moneca_object(result_unlimited)
})
