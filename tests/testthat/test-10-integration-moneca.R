# Integration Tests: moneca() Function
# ======================================
#
# Tests for the main moneca() function and its full pipeline.

test_that("moneca function runs without errors", {
  test_data <- get_test_data("small")

  expect_no_error({
    seg <- moneca(test_data, segment.levels = 3)
  })

  seg <- moneca(test_data, segment.levels = 3)

  validate_moneca_object(seg)
  expect_gte(length(seg$segment.list), 1)  # May stop early if only one segment remains
  expect_lte(length(seg$segment.list), 4)  # May have up to segment.levels + 1
  expect_gte(length(seg$mat.list), 1)
  expect_lte(length(seg$mat.list), 4)  # May have up to segment.levels + 1
})

test_that("moneca produces hierarchical segmentation", {
  test_data <- get_custom_test_data(n_classes = 6, seed = 123)
  seg <- moneca(test_data, segment.levels = 3)

  # Check that each level has fewer or equal segments than the previous
  n_segments <- sapply(seg$segment.list, length)

  # First level should be individual positions
  expect_equal(n_segments[1], 6)  # n_classes (6 classes in test data)

  # Subsequent levels should generally have fewer segments
  for (i in 2:length(n_segments)) {
    expect_lte(n_segments[i], n_segments[i-1])
  }
})

test_that("moneca handles edge cases", {
  # Test with minimal data
  small_data <- get_minimal_test_data()

  expect_no_error({
    seg_small <- moneca(small_data, segment.levels = 2)
  })

  # Test with single level
  expect_no_error({
    seg_single <- moneca(small_data, segment.levels = 1)
  })
})

test_that("moneca respects parameters", {
  test_data <- get_test_data("small")

  # Test different cut-off values
  seg_low <- moneca(test_data, cut.off = 0.5, segment.levels = 2)
  seg_high <- moneca(test_data, cut.off = 2.0, segment.levels = 2)

  validate_moneca_object(seg_low)
  validate_moneca_object(seg_high)

  # Both should produce valid results
  expect_true(length(seg_low$segment.list) > 0)
  expect_true(length(seg_high$segment.list) > 0)
})
