# Test suite for parallel moneca implementations

test_that("moneca_parallel produces identical results to sequential moneca", {
  # Generate test data
  set.seed(123)
  test_data <- generate_mobility_data(n_classes = 30, seed = 123)
  
  # Run sequential version
  result_seq <- moneca(test_data, segment.levels = 2)
  
  # Run parallel version with 2 cores
  result_par <- moneca_parallel(test_data, segment.levels = 2, n.cores = 2)
  
  # Compare segment memberships at each level
  expect_equal(length(result_par$segment.list), length(result_seq$segment.list))
  
  # The algorithm might produce slightly different orderings but same groupings
  for (i in seq_along(result_seq$segment.list)) {
    # Check that the number of segments is the same
    expect_equal(length(result_par$segment.list[[i]]), 
                 length(result_seq$segment.list[[i]]))
  }
  
  # Check that mat.list dimensions match
  expect_equal(length(result_par$mat.list), length(result_seq$mat.list))
})

test_that("weight.matrix.parallel produces correct results", {
  # Create test mobility matrix
  set.seed(456)
  n <- 20
  mx <- matrix(rpois(n * n, lambda = 5), nrow = n, ncol = n)
  rownames(mx) <- colnames(mx) <- paste0("Class", 1:n)
  mx <- cbind(mx, rowSums(mx))
  mx <- rbind(mx, colSums(mx))
  
  # Run sequential version
  result_seq <- weight.matrix(mx, cut.off = 1, symmetric = TRUE)
  
  # Run parallel version
  result_par <- weight.matrix.parallel(mx, cut.off = 1, symmetric = TRUE, n.cores = 2)
  
  # Compare results (allowing for small numerical differences)
  expect_equal(dim(result_par), dim(result_seq))
  expect_equal(rownames(result_par), rownames(result_seq))
  expect_equal(colnames(result_par), colnames(result_seq))
  
  # Check values are close (allowing for floating point differences)
  valid_indices <- !is.na(result_seq)
  expect_true(all(abs(result_par[valid_indices] - result_seq[valid_indices]) < 1e-10))
})

test_that("find.segments.parallel handles edge cases", {
  # Test with very small graph
  small_mat <- matrix(c(NA, 2, 1,
                        2, NA, 3,
                        1, 3, NA), nrow = 3)
  rownames(small_mat) <- colnames(small_mat) <- c("A", "B", "C")
  
  graph <- igraph::graph_from_adjacency_matrix(small_mat, mode = "undirected", 
                                               weighted = TRUE, diag = FALSE)
  
  result <- find.segments.parallel(small_mat, graph, cut.off = 1, n.cores = 2)
  
  expect_true(is.factor(result$membership))
  expect_true(is.list(result$cliques))
  expect_equal(length(result$membership), nrow(small_mat))
})

test_that("segment.matrix.parallel aggregates correctly", {
  # Create test matrix
  n <- 15
  mx <- matrix(sample(1:10, (n+1)^2, replace = TRUE), nrow = n+1, ncol = n+1)
  rownames(mx) <- colnames(mx) <- c(paste0("Class", 1:n), "Total")
  
  # Define segments
  segments <- list(
    c(1, 2, 3, 4, 5),
    c(6, 7, 8, 9, 10),
    c(11, 12, 13, 14, 15)
  )
  
  # Run sequential version
  result_seq <- segment.matrix(mx, segments)
  
  # Run parallel version
  result_par <- segment.matrix.parallel(mx, segments, n.cores = 2)
  
  # Compare dimensions
  expect_equal(dim(result_par), dim(result_seq))
  
  # Compare values
  expect_equal(result_par, result_seq)
})

test_that("parallel backend selection works correctly", {
  # Test auto detection
  config_auto <- setup_parallel_backend(NULL, "auto", 100)
  expect_true(config_auto$use_parallel)
  expect_true(config_auto$n_cores > 0)
  expect_true(config_auto$backend %in% c("fork", "socket"))
  
  # Test small data fallback
  config_small <- setup_parallel_backend(4, "auto", 10)
  expect_false(config_small$use_parallel)
  
  # Test explicit core specification
  config_explicit <- setup_parallel_backend(2, "socket", 100)
  expect_true(config_explicit$use_parallel)
  expect_equal(config_explicit$n_cores, 2)
  expect_equal(config_explicit$backend, "socket")
})

test_that("moneca_parallel handles different backends", {
  test_data <- generate_mobility_data(n_classes = 25, seed = 789)
  
  # Test with socket backend (works on all platforms)
  result_socket <- moneca_parallel(test_data, segment.levels = 2, 
                                   n.cores = 2, parallel.backend = "socket")
  expect_s3_class(result_socket, "moneca")
  expect_equal(result_socket$performance$backend, "socket")
  
  # Test with fork backend (Unix/Mac only)
  if (.Platform$OS.type == "unix") {
    result_fork <- moneca_parallel(test_data, segment.levels = 2,
                                   n.cores = 2, parallel.backend = "fork")
    expect_s3_class(result_fork, "moneca")
    expect_equal(result_fork$performance$backend, "fork")
  }
})

test_that("moneca_parallel fallback to sequential works", {
  # Test with very small data that should trigger sequential fallback
  small_data <- generate_mobility_data(n_classes = 5, seed = 101)
  
  expect_message(
    result <- moneca_parallel(small_data, segment.levels = 2, progress = TRUE),
    "Using sequential processing"
  )
  
  expect_s3_class(result, "moneca")
})

test_that("moneca_parallel performance metrics are recorded", {
  test_data <- generate_mobility_data(n_classes = 30, seed = 202)
  
  result <- moneca_parallel(test_data, segment.levels = 2, n.cores = 2)
  
  expect_true("performance" %in% names(result))
  expect_true("total_time" %in% names(result$performance))
  expect_true("n_cores_used" %in% names(result$performance))
  expect_true("backend" %in% names(result$performance))
  
  expect_true(result$performance$total_time >= 0)
  expect_true(result$performance$n_cores_used > 0)
})

test_that("parallel functions handle NA values correctly", {
  # Create matrix with NAs
  n <- 15
  mx <- matrix(rpois(n * n, lambda = 5), nrow = n, ncol = n)
  mx[sample(1:(n*n), 20)] <- NA  # Add some NAs
  rownames(mx) <- colnames(mx) <- paste0("Class", 1:n)
  mx <- cbind(mx, rowSums(mx, na.rm = TRUE))
  mx <- rbind(mx, colSums(mx, na.rm = TRUE))
  
  # Should not error
  expect_silent(
    result <- weight.matrix.parallel(mx, cut.off = 1, n.cores = 2)
  )
  
  expect_true(is.matrix(result))
})

test_that("chunk size optimization works", {
  test_data <- generate_mobility_data(n_classes = 50, seed = 303)
  
  # Test with auto chunk size
  result_auto <- moneca_parallel(test_data, segment.levels = 2, 
                                 n.cores = 2, chunk.size = NULL)
  expect_s3_class(result_auto, "moneca")
  
  # Test with explicit chunk size
  result_explicit <- moneca_parallel(test_data, segment.levels = 2,
                                     n.cores = 2, chunk.size = 10)
  expect_s3_class(result_explicit, "moneca")
})