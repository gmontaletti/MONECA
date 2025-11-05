test_that("moneca_fast produces valid results", {
  # Generate test data
  set.seed(123)
  test_data <- generate_mobility_data(n_classes = 5, immobility_strength = 0.7)
  
  # Run fast version
  result_fast <- moneca_fast(test_data, segment.levels = 2, progress = FALSE)
  
  # Check that the structure is correct
  expect_s3_class(result_fast, "moneca")
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
  min_data <- matrix(c(10, 2, 12,
                      1, 10, 11,
                      11, 12, 23), 
                    nrow = 3, byrow = TRUE)
  rownames(min_data) <- colnames(min_data) <- c("A", "B", "Total")
  
  result <- moneca_fast(min_data, segment.levels = 1, progress = FALSE)
  expect_s3_class(result, "moneca")
  expect_true(length(result$segment.list) >= 1)
})

test_that("moneca_fast sparse matrix option works", {
  skip_if_not_installed("Matrix")
  
  # Generate test data
  test_data <- generate_mobility_data(n_classes = 6)
  
  # Run with sparse matrix option (even though internally it uses regular operations)
  result_sparse <- moneca_fast(test_data, segment.levels = 2, use.sparse = TRUE, progress = FALSE)
  
  # Should produce valid results
  expect_s3_class(result_sparse, "moneca")
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
  expect_s3_class(result, "moneca")
  expect_true(is.list(result$segment.list))
  expect_true(is.list(result$mat.list))
})

test_that("moneca_fast performance is better than original", {
  skip_on_cran() # Skip on CRAN to save time
  
  # Generate medium-sized test data
  test_data <- generate_mobility_data(n_classes = 15, seed = 42)
  
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
  test_data <- generate_mobility_data(n_classes = 6)
  
  # Test with different cut-offs
  result_low <- moneca_fast(test_data, cut.off = 0.5, segment.levels = 2, progress = FALSE)
  result_high <- moneca_fast(test_data, cut.off = 2.0, segment.levels = 2, progress = FALSE)
  
  # Higher cut-off should result in fewer connections and potentially more segments
  expect_s3_class(result_low, "moneca")
  expect_s3_class(result_high, "moneca")
  
  # Both should have valid segment lists
  expect_true(length(result_low$segment.list) > 0)
  expect_true(length(result_high$segment.list) > 0)
})

test_that("moneca_fast max clique size parameter works", {
  test_data <- generate_mobility_data(n_classes = 8)

  # Run with clique size limit
  result_limited <- moneca_fast(test_data, segment.levels = 2, max.clique.size = 4, progress = FALSE)
  result_unlimited <- moneca_fast(test_data, segment.levels = 2, max.clique.size = NULL, progress = FALSE)

  # Both should produce valid results
  expect_s3_class(result_limited, "moneca")
  expect_s3_class(result_unlimited, "moneca")
})

test_that("moneca_fast produces identical results to original moneca", {
  # Test with multiple scenarios to ensure full equivalence

  # Scenario 1: Original bug case (8 classes, 2 levels)
  set.seed(12345)
  test_data_1 <- generate_mobility_data(n_classes = 8, seed = 12345)

  result_original_1 <- moneca(test_data_1, segment.levels = 2)
  result_fast_1 <- moneca_fast(test_data_1, segment.levels = 2, progress = FALSE)

  # Should have identical segment lists
  expect_identical(result_fast_1$segment.list, result_original_1$segment.list,
                   info = "Scenario 1: segment.list should be identical")

  # Should have same number of levels
  expect_equal(length(result_fast_1$segment.list), length(result_original_1$segment.list),
               info = "Scenario 1: should have same number of segment list levels")

  # Should have same number of segments at each level
  for (i in seq_along(result_original_1$segment.list)) {
    expect_equal(length(result_fast_1$segment.list[[i]]),
                 length(result_original_1$segment.list[[i]]),
                 info = sprintf("Scenario 1: Level %d segment count mismatch", i))
  }

  # Scenario 2: Larger dataset (10 classes, 3 levels)
  set.seed(456)
  test_data_2 <- generate_mobility_data(n_classes = 10, seed = 456)

  result_original_2 <- moneca(test_data_2, segment.levels = 3)
  result_fast_2 <- moneca_fast(test_data_2, segment.levels = 3, progress = FALSE)

  expect_identical(result_fast_2$segment.list, result_original_2$segment.list,
                   info = "Scenario 2: segment.list should be identical")

  # Scenario 3: Smaller dataset (5 classes, 2 levels)
  set.seed(789)
  test_data_3 <- generate_mobility_data(n_classes = 5, seed = 789)

  result_original_3 <- moneca(test_data_3, segment.levels = 2)
  result_fast_3 <- moneca_fast(test_data_3, segment.levels = 2, progress = FALSE)

  expect_identical(result_fast_3$segment.list, result_original_3$segment.list,
                   info = "Scenario 3: segment.list should be identical")

  # Scenario 4: Single level segmentation
  set.seed(111)
  test_data_4 <- generate_mobility_data(n_classes = 6, seed = 111)

  result_original_4 <- moneca(test_data_4, segment.levels = 1)
  result_fast_4 <- moneca_fast(test_data_4, segment.levels = 1, progress = FALSE)

  expect_identical(result_fast_4$segment.list, result_original_4$segment.list,
                   info = "Scenario 4: segment.list should be identical")
})