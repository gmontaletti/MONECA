# Test file for parallel processing in moneca_temporal

test_that("parallel parameter works in moneca_temporal", {
  skip_on_cran()  # Skip on CRAN due to parallel processing
  
  # Generate test data
  set.seed(123)
  test_matrices <- lapply(1:6, function(i) {
    generate_mobility_data(
      n_classes = 5,
      seed = i * 100,
      immobility_strength = 0.6
    )
  })
  
  # Test sequential processing
  result_seq <- moneca_temporal(
    matrix_list = test_matrices,
    window_size = 2,
    segment.levels = 2,
    parallel = FALSE,
    verbose = FALSE
  )
  
  # Test parallel processing
  result_par <- moneca_temporal(
    matrix_list = test_matrices,
    window_size = 2,
    segment.levels = 2,
    parallel = TRUE,
    verbose = FALSE
  )
  
  # Check both produce valid results
  expect_s3_class(result_seq, "moneca_temporal")
  expect_s3_class(result_par, "moneca_temporal")
  
  # Check same number of windows
  expect_equal(length(result_seq$windows), length(result_par$windows))
  
  # Check both have transition matrices
  expect_true(!is.null(result_seq$transition_matrix))
  expect_true(!is.null(result_par$transition_matrix))
  
  # Check dimensions match (results may vary slightly due to algorithm)
  expect_equal(length(result_seq$segment_maps), length(result_par$segment_maps))
})

test_that("parallel falls back to sequential for single window", {
  # Generate minimal data for single window
  set.seed(123)
  test_matrices <- lapply(1:3, function(i) {
    generate_mobility_data(
      n_classes = 4,
      seed = i * 100
    )
  })
  
  # This should only create 1 window (3 periods, window size 3)
  result <- moneca_temporal(
    matrix_list = test_matrices,
    window_size = 3,
    parallel = TRUE,  # Request parallel but should use sequential
    verbose = FALSE
  )
  
  expect_s3_class(result, "moneca_temporal")
  expect_equal(length(result$windows), 1)
})

test_that("parallel processing handles edge cases", {
  # Test with very small dataset
  set.seed(123)
  small_matrices <- lapply(1:2, function(i) {
    generate_mobility_data(n_classes = 3, seed = i * 50)
  })
  
  result_small <- moneca_temporal(
    matrix_list = small_matrices,
    window_size = 2,
    parallel = TRUE,
    verbose = FALSE
  )
  
  expect_s3_class(result_small, "moneca_temporal")
  expect_equal(length(result_small$windows), 1)
})

test_that("parallel processing respects verbose parameter", {
  set.seed(123)
  test_matrices <- lapply(1:4, function(i) {
    generate_mobility_data(n_classes = 4, seed = i * 100)
  })
  
  # Capture output
  output <- capture.output({
    result <- moneca_temporal(
      matrix_list = test_matrices,
      window_size = 2,
      parallel = TRUE,
      verbose = TRUE
    )
  })
  
  # Check that parallel-specific messages appear
  expect_true(any(grepl("parallel|cores", output, ignore.case = TRUE)))
})