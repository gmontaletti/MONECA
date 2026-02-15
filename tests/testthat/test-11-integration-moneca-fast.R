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
  expect_true(all(
    c("segment.list", "mat.list", "small.cell.reduction") %in%
      names(result_fast)
  ))

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
  result_sparse <- moneca_fast(
    test_data,
    segment.levels = 2,
    use.sparse = TRUE,
    progress = FALSE
  )

  # Should produce valid results
  validate_moneca_object(result_sparse)
  expect_true(length(result_sparse$segment.list) >= 1)
})

test_that("moneca_fast handles sparse data correctly", {
  # Create a very sparse matrix
  sparse_data <- matrix(0, nrow = 6, ncol = 6)
  diag(sparse_data[1:5, 1:5]) <- 100
  sparse_data[6, ] <- colSums(sparse_data)
  sparse_data[, 6] <- sparse_data[6, ]
  rownames(sparse_data) <- colnames(sparse_data) <- c(LETTERS[1:5], "Total")

  # Add very few off-diagonal elements
  sparse_data[1, 2] <- sparse_data[2, 1] <- 1
  sparse_data[6, ] <- colSums(sparse_data[1:5, ])
  sparse_data[, 6] <- sparse_data[6, ]

  # Test with high density threshold which should trigger early stopping
  result <- moneca_fast(
    sparse_data,
    segment.levels = 3,
    min.density = 0.5,
    progress = FALSE
  )

  # Should produce valid results even with sparse data
  validate_moneca_object(result)
  expect_true(is.list(result$segment.list))
  expect_true(is.list(result$mat.list))
})

test_that("moneca_fast completes on medium data and produces valid output", {
  skip_on_cran()

  test_data <- get_test_data("medium")
  result_fast <- moneca_fast(test_data, segment.levels = 2, progress = FALSE)
  validate_moneca_object(result_fast)
  expect_true(!is.null(result_fast$segment_metadata))
})

test_that("moneca_fast handles different cut-off values", {
  test_data <- get_custom_test_data(n_classes = 6)

  # Test with different cut-offs
  result_low <- moneca_fast(
    test_data,
    cut.off = 0.5,
    segment.levels = 2,
    progress = FALSE
  )
  result_high <- moneca_fast(
    test_data,
    cut.off = 2.0,
    segment.levels = 2,
    progress = FALSE
  )

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
  result_limited <- moneca_fast(
    test_data,
    segment.levels = 2,
    max.clique.size = 4,
    progress = FALSE
  )
  result_unlimited <- moneca_fast(
    test_data,
    segment.levels = 2,
    max.clique.size = NULL,
    progress = FALSE
  )

  # Both should produce valid results
  validate_moneca_object(result_limited)
  validate_moneca_object(result_unlimited)
})

# Margin Handling Tests -----

test_that("moneca_fast output includes margins_added and density_reduction fields", {
  test_data <- get_test_data("small")
  result <- moneca_fast(
    test_data,
    segment.levels = 2,
    progress = FALSE,
    reduce_density = FALSE
  )

  expect_true("margins_added" %in% names(result))
  expect_true("density_reduction" %in% names(result))
  expect_false(result$margins_added)
  expect_null(result$density_reduction)
})

test_that("moneca_fast auto-detects margins present", {
  test_data <- get_test_data("small")
  result <- moneca_fast(
    test_data,
    segment.levels = 2,
    progress = FALSE,
    has_margins = "auto",
    reduce_density = FALSE
  )
  expect_false(result$margins_added)
})

test_that("moneca_fast auto-detects margins absent", {
  no_margins <- get_test_data_no_margins("small")
  result <- moneca_fast(
    no_margins,
    segment.levels = 2,
    progress = FALSE,
    has_margins = "auto",
    reduce_density = FALSE
  )
  expect_true(result$margins_added)
})

test_that("has_margins = FALSE forces margin generation", {
  test_data <- get_test_data("small")
  # Pass a matrix with margins but force has_margins = FALSE
  # This should add margins on top of margins (testing the flag behavior)
  n <- nrow(test_data) - 1
  core <- test_data[1:n, 1:n]
  result <- moneca_fast(
    core,
    segment.levels = 2,
    progress = FALSE,
    has_margins = FALSE,
    reduce_density = FALSE
  )
  expect_true(result$margins_added)
  validate_moneca_object(result)
})

test_that("matrix without margins produces same segments as with margins", {
  set.seed(42)
  with_margins <- get_test_data("small")
  without_margins <- get_test_data_no_margins("small")

  result_with <- moneca_fast(
    with_margins,
    segment.levels = 2,
    progress = FALSE,
    has_margins = TRUE,
    reduce_density = FALSE
  )
  result_without <- moneca_fast(
    without_margins,
    segment.levels = 2,
    progress = FALSE,
    has_margins = FALSE,
    reduce_density = FALSE
  )

  expect_moneca_equivalent(
    result_with,
    result_without,
    info = "With vs without margins should produce identical segments"
  )
})

test_that("moneca_fast handles matrix without names and no margins", {
  set.seed(42)
  no_margins <- get_test_data_no_margins("small")
  rownames(no_margins) <- NULL
  colnames(no_margins) <- NULL

  result <- moneca_fast(
    no_margins,
    segment.levels = 2,
    progress = FALSE,
    has_margins = FALSE,
    reduce_density = FALSE
  )
  validate_moneca_object(result)
  expect_true(result$margins_added)
})

# Density Reduction Integration Tests -----

test_that("reduce_density = FALSE preserves current behavior", {
  test_data <- get_test_data("small")

  result_default <- moneca_fast(
    test_data,
    segment.levels = 2,
    progress = FALSE,
    reduce_density = FALSE
  )
  result_original <- moneca(test_data, segment.levels = 2)

  expect_moneca_equivalent(
    result_default,
    result_original,
    info = "reduce_density = FALSE should not alter results"
  )
})

test_that("reduce_density = TRUE on 30+ class matrix applies reduction", {
  skip_on_cran()

  large_data <- get_medium_large_test_data(n_classes = 30, seed = 456)
  result <- moneca_fast(
    large_data,
    segment.levels = 2,
    progress = FALSE,
    reduce_density = TRUE
  )

  validate_moneca_object(result)
  expect_false(is.null(result$density_reduction))
  expect_true(is.list(result$density_reduction))
  expect_true("method" %in% names(result$density_reduction))
  expect_true("k" %in% names(result$density_reduction))
})

test_that("reduce_density = 'auto' skips small matrices", {
  test_data <- get_test_data("small")
  result <- moneca_fast(
    test_data,
    segment.levels = 2,
    progress = FALSE,
    reduce_density = "auto"
  )
  expect_null(result$density_reduction)
})

test_that("density_params are passed through correctly", {
  skip_on_cran()

  large_data <- get_medium_large_test_data(n_classes = 30, seed = 456)
  result <- moneca_fast(
    large_data,
    segment.levels = 2,
    progress = FALSE,
    reduce_density = TRUE,
    density_params = list(method = "svd", k = 10)
  )

  validate_moneca_object(result)
  expect_equal(result$density_reduction$method, "svd")
  expect_equal(result$density_reduction$k, 10)
})
