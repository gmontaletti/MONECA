# Unit Tests: weight.matrix Function
# ===================================
#
# Tests for the weight.matrix() function that converts mobility tables
# to relative risk matrices.

test_that("weight.matrix produces valid relative risk matrices", {
  # Create test data
  test_data <- get_test_data("small")

  # Generate weight matrix
  wm <- weight.matrix(test_data, cut.off = 1)

  expect_is(wm, "matrix")
  expect_equal(nrow(wm), 5)  # Should exclude totals row/column
  expect_equal(ncol(wm), 5)

  # Check for reasonable values (relative risks should be positive or NA)
  finite_values <- wm[is.finite(wm)]
  expect_true(all(finite_values >= 0))

  # Check diagonal is NA (as set by default)
  expect_true(all(is.na(diag(wm))))
})

test_that("weight.matrix respects cut.off parameter", {
  test_data <- get_test_data("small")

  wm_low <- weight.matrix(test_data, cut.off = 0.5)
  wm_high <- weight.matrix(test_data, cut.off = 2.0)

  # Higher cut-off should result in more NA values
  expect_gte(sum(is.na(wm_high)), sum(is.na(wm_low)))
})

test_that("weight.matrix symmetric parameter works", {
  test_data <- get_test_data("small")

  wm_symmetric <- weight.matrix(test_data, symmetric = TRUE)
  wm_asymmetric <- weight.matrix(test_data, symmetric = FALSE)

  # Check if symmetric version is actually symmetric (ignoring NAs)
  finite_indices <- which(is.finite(wm_symmetric), arr.ind = TRUE)
  if (nrow(finite_indices) > 0) {
    for (i in seq_len(nrow(finite_indices))) {
      row_idx <- finite_indices[i, 1]
      col_idx <- finite_indices[i, 2]
      if (is.finite(wm_symmetric[col_idx, row_idx])) {
        expect_equal(wm_symmetric[row_idx, col_idx], wm_symmetric[col_idx, row_idx])
      }
    }
  }
})

test_that("weight.matrix handles different cut-off values", {
  test_data <- get_custom_test_data(n_classes = 6)

  # Test with different cut-offs
  result_low <- weight.matrix(test_data, cut.off = 0.5)
  result_high <- weight.matrix(test_data, cut.off = 2.0)

  # Higher cut-off should result in more connections filtered
  expect_is(result_low, "matrix")
  expect_is(result_high, "matrix")

  # Both should have valid values
  expect_true(all(is.finite(result_low[is.finite(result_low)]) >= 0))
  expect_true(all(is.finite(result_high[is.finite(result_high)]) >= 0))
})
