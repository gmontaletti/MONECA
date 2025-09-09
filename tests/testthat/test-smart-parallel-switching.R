# Test Smart Parallel Processing Decision Engine for MONECA

library(testthat)

test_that("detect_system_resources works correctly", {
  resources <- detect_system_resources(verbose = FALSE)
  
  # Basic structure tests
  expect_type(resources, "list")
  expect_s3_class(resources, "moneca_system_resources")
  
  # Required fields
  expect_true("total_cores" %in% names(resources))
  expect_true("usable_cores" %in% names(resources))
  expect_true("memory_gb" %in% names(resources))
  expect_true("has_parallel_packages" %in% names(resources))
  expect_true("platform" %in% names(resources))
  expect_true("parallel_backend" %in% names(resources))
  expect_true("max_recommended_cores" %in% names(resources))
  
  # Value validation
  expect_gte(resources$total_cores, 1)
  expect_gte(resources$usable_cores, 1)
  expect_lte(resources$usable_cores, resources$total_cores)
  expect_gte(resources$memory_gb, 0.5)  # At least 0.5 GB
  expect_type(resources$has_parallel_packages, "logical")
  expect_true(resources$platform %in% c("unix", "windows"))
  expect_true(resources$parallel_backend %in% c("fork", "socket"))
  
  # Platform-specific backend
  if (.Platform$OS.type == "unix") {
    expect_equal(resources$parallel_backend, "fork")
  } else {
    expect_equal(resources$parallel_backend, "socket")
  }
})

test_that("estimate_memory_requirements works correctly", {
  memory_est <- estimate_memory_requirements(
    n_combinations = 25,
    matrix_size = 50,
    n_bootstrap = 30,
    cache_enabled = TRUE
  )
  
  # Structure tests
  expect_type(memory_est, "list")
  expect_true("per_combination_mb" %in% names(memory_est))
  expect_true("total_estimated_mb" %in% names(memory_est))
  expect_true("bootstrap_overhead_mb" %in% names(memory_est))
  expect_true("cache_overhead_mb" %in% names(memory_est))
  expect_true("recommended_chunk_size" %in% names(memory_est))
  
  # Value validation
  expect_gt(memory_est$per_combination_mb, 0)
  expect_gt(memory_est$total_estimated_mb, 0)
  expect_gte(memory_est$bootstrap_overhead_mb, 0)
  expect_gte(memory_est$cache_overhead_mb, 0)
  expect_gte(memory_est$recommended_chunk_size, 1)
  expect_lte(memory_est$recommended_chunk_size, 25)
  
  # Relationship tests
  expect_lte(memory_est$per_combination_mb, memory_est$total_estimated_mb)
})

test_that("estimate_parallel_benefit provides reasonable estimates", {
  resources <- detect_system_resources(verbose = FALSE)
  
  benefit_est <- estimate_parallel_benefit(
    n_combinations = 50,
    matrix_size = 100,
    n_bootstrap = 25,
    n_cores = 4,
    system_resources = resources
  )
  
  # Structure tests
  expect_type(benefit_est, "list")
  expect_true("sequential_time_est" %in% names(benefit_est))
  expect_true("parallel_time_est" %in% names(benefit_est))
  expect_true("speedup_est" %in% names(benefit_est))
  expect_true("parallel_overhead_est" %in% names(benefit_est))
  expect_true("efficiency_est" %in% names(benefit_est))
  expect_true("recommendation" %in% names(benefit_est))
  
  # Value validation
  expect_gt(benefit_est$sequential_time_est, 0)
  expect_gt(benefit_est$parallel_time_est, 0)
  expect_gt(benefit_est$speedup_est, 0)
  expect_gte(benefit_est$parallel_overhead_est, 0)
  expect_gte(benefit_est$efficiency_est, 0)
  expect_lte(benefit_est$efficiency_est, 1)
  expect_true(benefit_est$recommendation %in% c("parallel", "sequential"))
})

test_that("get_optimal_cores returns reasonable values", {
  resources <- detect_system_resources(verbose = FALSE)
  
  # Small problem
  cores_small <- get_optimal_cores(
    n_combinations = 5,
    matrix_size = 20,
    system_resources = resources
  )
  expect_gte(cores_small, 1)
  expect_lte(cores_small, resources$usable_cores)
  
  # Large problem
  cores_large <- get_optimal_cores(
    n_combinations = 100,
    matrix_size = 200,
    system_resources = resources
  )
  expect_gte(cores_large, 1)
  expect_lte(cores_large, resources$max_recommended_cores)
  
  # Should generally recommend more cores for larger problems
  # (unless constrained by other factors)
  if (resources$usable_cores > 1) {
    # At least one of these should be true for sensible behavior
    expect_true(cores_large >= cores_small || cores_large >= 2)
  }
})

test_that("should_use_parallel makes correct decisions", {
  # Test explicit preferences
  decision_seq <- should_use_parallel(
    n_combinations = 100,
    matrix_size = 50,
    user_preference = "sequential",
    verbose = FALSE
  )
  expect_false(decision_seq$use_parallel)
  expect_true(decision_seq$user_override)
  
  decision_par <- should_use_parallel(
    n_combinations = 100,
    matrix_size = 50,
    user_preference = "parallel",
    verbose = FALSE
  )
  # Should either use parallel or explain why not (e.g., missing packages)
  expect_type(decision_par$use_parallel, "logical")
  expect_type(decision_par$reasoning, "character")
  
  # Test automatic decision for very small problems
  decision_small <- should_use_parallel(
    n_combinations = 3,
    matrix_size = 10,
    user_preference = "auto",
    verbose = FALSE
  )
  expect_false(decision_small$use_parallel)
  expect_false(decision_small$user_override)
  
  # Test automatic decision for larger problems
  decision_large <- should_use_parallel(
    n_combinations = 50,
    matrix_size = 100,
    user_preference = "auto",
    verbose = FALSE
  )
  expect_type(decision_large$use_parallel, "logical")
  expect_false(decision_large$user_override)
  
  # Test logical preferences (backward compatibility)
  decision_true <- should_use_parallel(
    n_combinations = 20,
    matrix_size = 30,
    user_preference = TRUE,
    verbose = FALSE
  )
  expect_type(decision_true$use_parallel, "logical")
  
  decision_false <- should_use_parallel(
    n_combinations = 20,
    matrix_size = 30,
    user_preference = FALSE,
    verbose = FALSE
  )
  expect_false(decision_false$use_parallel)
})

test_that("should_use_parallel handles edge cases", {
  # Test with missing system resources
  decision <- should_use_parallel(
    n_combinations = 25,
    matrix_size = 50,
    system_resources = NULL,  # Should auto-detect
    verbose = FALSE
  )
  expect_type(decision, "list")
  expect_true("system_info" %in% names(decision))
  
  # Test invalid user preference
  expect_error(
    should_use_parallel(
      n_combinations = 10,
      matrix_size = 20,
      user_preference = "invalid",
      verbose = FALSE
    ),
    "user_preference must be one of"
  )
})

test_that("create_parallel_config works correctly", {
  decision <- should_use_parallel(
    n_combinations = 25,
    matrix_size = 50,
    user_preference = "auto",
    verbose = FALSE
  )
  
  config <- create_parallel_config(decision, "auto")
  
  # Structure tests
  expect_type(config, "list")
  expect_s3_class(config, "moneca_parallel_config")
  expect_true("use_parallel" %in% names(config))
  expect_true("n_cores" %in% names(config))
  expect_true("backend" %in% names(config))
  expect_true("cluster" %in% names(config))
  expect_true("decision_info" %in% names(config))
  
  # Value validation
  expect_type(config$use_parallel, "logical")
  expect_gte(config$n_cores, 1)
  expect_type(config$backend, "character")
  expect_null(config$cluster)  # Not initialized yet
  expect_identical(config$decision_info, decision)
  
  # Backend selection
  if (config$use_parallel) {
    expect_true(config$backend %in% c("fork", "socket"))
  } else {
    expect_equal(config$backend, "sequential")
  }
})

test_that("smart parallel switching integrates with existing functions", {
  # Skip if required packages not available
  skip_if_not_installed("foreach")
  skip_if_not_installed("doParallel")
  
  # Generate test data
  test_data <- matrix(rpois(25*25, lambda = 10), nrow = 25, ncol = 25)
  # Add row and column totals
  test_data <- cbind(test_data, rowSums(test_data))
  test_data <- rbind(test_data, colSums(test_data))
  rownames(test_data) <- paste0("C", 1:26)
  colnames(test_data) <- paste0("C", 1:26)
  
  # Test auto_tune_small_cell_reduction with smart switching
  result_auto <- auto_tune_small_cell_reduction(
    mx = test_data,
    method = "stability",
    n_trials = 5,  # Small for quick testing
    candidate_values = c(0, 1, 2),
    parallel = "auto",
    verbose = FALSE
  )
  
  expect_s3_class(result_auto, "moneca_tuning")
  expect_true("parallel_info" %in% names(result_auto))
  expect_type(result_auto$parallel_info, "list")
  expect_true("use_parallel" %in% names(result_auto$parallel_info))
  
  # Test explicit sequential preference
  result_seq <- auto_tune_small_cell_reduction(
    mx = test_data,
    method = "quality",
    candidate_values = c(0, 1),
    parallel = "sequential",
    verbose = FALSE
  )
  
  expect_s3_class(result_seq, "moneca_tuning")
  expect_false(result_seq$parallel_info$use_parallel)
  expect_true(result_seq$parallel_info$user_override)
})

test_that("print methods work correctly", {
  resources <- detect_system_resources(verbose = FALSE)
  
  # Test that print doesn't error
  expect_output(print(resources), "MONECA System Resources")
  expect_output(print(resources), "CPU Cores")
  expect_output(print(resources), "Platform")
  
  # Test missing packages message
  if (!resources$has_parallel_packages) {
    expect_output(print(resources), "Missing packages")
    expect_output(print(resources), "install.packages")
  }
})

# Integration test with joint parameter tuning
test_that("joint parameter tuning uses smart parallel switching", {
  # Skip if required packages not available
  skip_if_not_installed("foreach")
  skip_if_not_installed("doParallel")
  
  # Generate small test data for quick testing
  test_data <- matrix(rpois(16*16, lambda = 5), nrow = 16, ncol = 16)
  # Add row and column totals
  test_data <- cbind(test_data, rowSums(test_data))
  test_data <- rbind(test_data, colSums(test_data))
  rownames(test_data) <- paste0("C", 1:17)
  colnames(test_data) <- paste0("C", 1:17)
  
  # Test with auto parallel decision
  result <- auto_tune_joint_parameters(
    mx = test_data,
    method = "grid",
    n_grid_points = 3,  # Small grid for testing
    n_bootstrap = 3,    # Few bootstrap samples
    objectives = "quality",  # Single objective for speed
    parallel = "auto",
    verbose = FALSE
  )
  
  expect_s3_class(result, "moneca_joint_tuning")
  expect_true("parallel_info" %in% names(result))
  expect_type(result$parallel_info, "list")
  expect_true("use_parallel" %in% names(result$parallel_info))
  expect_true("reasoning" %in% names(result$parallel_info))
  
  # Test explicit sequential
  result_seq <- auto_tune_joint_parameters(
    mx = test_data,
    method = "grid",
    n_grid_points = 2,
    n_bootstrap = 2,
    objectives = "quality",
    parallel = "sequential",
    verbose = FALSE
  )
  
  expect_false(result_seq$parallel_info$use_parallel)
  expect_true(result_seq$parallel_info$user_override)
})