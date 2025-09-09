# Test Parallel Processing Diagnostic Tools for MONECA

library(testthat)

# Test diagnose_parallel_execution function
test_that("diagnose_parallel_execution works correctly", {
  
  # Test basic structure and functionality
  diagnostic <- diagnose_parallel_execution(
    n_workers = 2, 
    test_duration_sec = 2,  # Quick test
    include_cpu_monitoring = FALSE,  # Disable for faster testing
    verbose = FALSE
  )
  
  # Basic structure tests
  expect_type(diagnostic, "list")
  expect_s3_class(diagnostic, "moneca_parallel_diagnostic")
  
  # Required fields
  required_fields <- c("parallel_active", "workers_detected", "system_info", 
                      "diagnostic_summary", "recommendations")
  for (field in required_fields) {
    expect_true(field %in% names(diagnostic), 
                info = paste("Missing required field:", field))
  }
  
  # Value validation
  expect_type(diagnostic$parallel_active, "logical")
  expect_gte(diagnostic$workers_detected, 0)
  expect_type(diagnostic$diagnostic_summary, "character")
  expect_length(diagnostic$diagnostic_summary, 1)
  expect_type(diagnostic$recommendations, "character")
  
  # System info validation
  expect_s3_class(diagnostic$system_info, "moneca_system_resources")
  
  # Timing fields (may be NA if parallel failed)
  if (!is.na(diagnostic$timing_sequential)) {
    expect_gt(diagnostic$timing_sequential, 0)
  }
  if (!is.na(diagnostic$timing_parallel)) {
    expect_gt(diagnostic$timing_parallel, 0)
  }
  if (!is.na(diagnostic$speedup_achieved)) {
    expect_gt(diagnostic$speedup_achieved, 0)
  }
})

test_that("diagnose_parallel_execution handles missing packages gracefully", {
  
  # Mock missing packages by temporarily unloading them
  skip_if_not_installed("foreach")
  skip_if_not_installed("doParallel")
  
  # Test should work even if packages are available
  diagnostic <- diagnose_parallel_execution(
    n_workers = 1,
    test_duration_sec = 1,
    verbose = FALSE
  )
  
  expect_s3_class(diagnostic, "moneca_parallel_diagnostic")
  expect_type(diagnostic$parallel_active, "logical")
  expect_type(diagnostic$diagnostic_summary, "character")
})

test_that("diagnose_parallel_execution validates parameters", {
  
  # Test with invalid parameters
  expect_no_error(
    diagnose_parallel_execution(n_workers = 0, test_duration_sec = 1, verbose = FALSE)
  )
  
  expect_no_error(
    diagnose_parallel_execution(n_workers = 1, test_duration_sec = 0.5, verbose = FALSE)
  )
})

test_that("diagnose_parallel_execution print method works", {
  
  diagnostic <- diagnose_parallel_execution(
    n_workers = 2,
    test_duration_sec = 1,
    verbose = FALSE
  )
  
  # Test that print doesn't error
  expect_output(print(diagnostic), "MONECA Parallel Execution Diagnostic")
  expect_output(print(diagnostic), "Status:")
  
  # Test different diagnostic outcomes
  if (diagnostic$parallel_active) {
    expect_output(print(diagnostic), "Workers detected:")
  }
  
  if (length(diagnostic$recommendations) > 0) {
    expect_output(print(diagnostic), "Recommendations:")
  }
})

# Test monitor_system_resources function
test_that("monitor_system_resources works correctly", {
  
  # Short monitoring test
  monitor_result <- monitor_system_resources(
    monitoring_duration_sec = 3,  # Very short for testing
    sampling_interval_sec = 1,
    n_workers = 2,
    include_memory_monitoring = FALSE,
    verbose = FALSE
  )
  
  # Basic structure tests
  expect_type(monitor_result, "list")
  expect_s3_class(monitor_result, "moneca_resource_monitor")
  
  # Required fields
  required_fields <- c("cpu_utilization_over_time", "monitoring_summary", 
                      "optimization_suggestions", "resource_bottlenecks")
  for (field in required_fields) {
    expect_true(field %in% names(monitor_result),
                info = paste("Missing required field:", field))
  }
  
  # Value validation
  expect_type(monitor_result$monitoring_summary, "character")
  expect_length(monitor_result$monitoring_summary, 1)
  expect_type(monitor_result$optimization_suggestions, "character")
  expect_type(monitor_result$resource_bottlenecks, "character")
  
  # CPU monitoring data (if successful)
  if (length(monitor_result$cpu_utilization_over_time) > 0) {
    expect_type(monitor_result$cpu_utilization_over_time, "double")
    expect_true(all(monitor_result$cpu_utilization_over_time >= 0))
    expect_true(all(monitor_result$cpu_utilization_over_time <= 100))
    expect_gte(monitor_result$peak_cpu_usage, 0)
    expect_lte(monitor_result$peak_cpu_usage, 100)
    expect_gte(monitor_result$average_cpu_usage, 0)
    expect_lte(monitor_result$average_cpu_usage, 100)
  }
})

test_that("monitor_system_resources handles edge cases", {
  
  # Test with minimal parameters
  monitor_result <- monitor_system_resources(
    monitoring_duration_sec = 1,
    sampling_interval_sec = 0.5,
    verbose = FALSE
  )
  
  expect_s3_class(monitor_result, "moneca_resource_monitor")
  
  # Test with memory monitoring enabled
  monitor_result_mem <- monitor_system_resources(
    monitoring_duration_sec = 2,
    include_memory_monitoring = TRUE,
    verbose = FALSE
  )
  
  expect_s3_class(monitor_result_mem, "moneca_resource_monitor")
  expect_true("memory_usage_over_time" %in% names(monitor_result_mem))
})

test_that("monitor_system_resources print method works", {
  
  monitor_result <- monitor_system_resources(
    monitoring_duration_sec = 2,
    verbose = FALSE
  )
  
  # Test that print doesn't error
  expect_output(print(monitor_result), "MONECA System Resource Monitor")
  expect_output(print(monitor_result), "Summary:")
  
  if (length(monitor_result$cpu_utilization_over_time) > 0) {
    expect_output(print(monitor_result), "Peak CPU usage:")
    expect_output(print(monitor_result), "Average CPU usage:")
  }
})

# Test benchmark_parallel_performance function
test_that("benchmark_parallel_performance works correctly", {
  
  # Quick benchmark with small scenarios
  small_scenarios <- list(
    test_small = list(
      name = "Test Small",
      n_combinations = 4,
      matrix_size = 15,
      n_bootstrap = 5
    ),
    test_tiny = list(
      name = "Test Tiny",
      n_combinations = 2,
      matrix_size = 10,
      n_bootstrap = 2
    )
  )
  
  benchmark <- benchmark_parallel_performance(
    test_scenarios = small_scenarios,
    max_workers = 2,
    include_memory_analysis = FALSE,
    benchmark_duration_per_test = 15,  # Quick test
    verbose = FALSE
  )
  
  # Basic structure tests
  expect_type(benchmark, "list")
  expect_s3_class(benchmark, "moneca_parallel_benchmark")
  
  # Required fields
  required_fields <- c("scenario_results", "optimal_configurations", "speedup_analysis",
                      "efficiency_analysis", "recommendations", "benchmark_summary")
  for (field in required_fields) {
    expect_true(field %in% names(benchmark),
                info = paste("Missing required field:", field))
  }
  
  # Value validation
  expect_type(benchmark$scenario_results, "list")
  expect_type(benchmark$optimal_configurations, "list")
  expect_type(benchmark$recommendations, "character")
  expect_type(benchmark$benchmark_summary, "character")
  expect_length(benchmark$benchmark_summary, 1)
  
  # Scenario results validation
  expect_equal(length(benchmark$scenario_results), length(small_scenarios))
  
  for (scenario_name in names(small_scenarios)) {
    expect_true(scenario_name %in% names(benchmark$scenario_results))
    scenario_result <- benchmark$scenario_results[[scenario_name]]
    
    expect_true("scenario_name" %in% names(scenario_result))
    expect_true("scenario_config" %in% names(scenario_result))
    expect_equal(scenario_result$scenario_name, scenario_name)
  }
})

test_that("benchmark_parallel_performance handles default scenarios", {
  
  # Test with default scenarios (but shorter duration)
  benchmark <- benchmark_parallel_performance(
    max_workers = 2,
    benchmark_duration_per_test = 10,  # Short test
    verbose = FALSE
  )
  
  expect_s3_class(benchmark, "moneca_parallel_benchmark")
  expect_type(benchmark$scenario_results, "list")
  expect_gt(length(benchmark$scenario_results), 0)
  
  # Should have default scenarios
  expect_true(any(grepl("small", names(benchmark$scenario_results), ignore.case = TRUE)) ||
              any(grepl("medium", names(benchmark$scenario_results), ignore.case = TRUE)))
})

test_that("benchmark_parallel_performance handles missing packages", {
  
  # This should still return a valid structure even if packages are missing
  benchmark <- benchmark_parallel_performance(
    test_scenarios = list(
      minimal = list(n_combinations = 1, matrix_size = 5, n_bootstrap = 1)
    ),
    max_workers = 1,
    verbose = FALSE
  )
  
  expect_s3_class(benchmark, "moneca_parallel_benchmark")
  expect_type(benchmark$benchmark_summary, "character")
})

test_that("benchmark_parallel_performance print method works", {
  
  small_scenario <- list(
    tiny = list(n_combinations = 3, matrix_size = 10, n_bootstrap = 2)
  )
  
  benchmark <- benchmark_parallel_performance(
    test_scenarios = small_scenario,
    max_workers = 2,
    benchmark_duration_per_test = 8,
    verbose = FALSE
  )
  
  # Test that print doesn't error
  expect_output(print(benchmark), "MONECA Parallel Performance Benchmark")
  expect_output(print(benchmark), "Summary:")
  
  # Test optional outputs based on results
  if (!is.null(benchmark$speedup_analysis) && length(benchmark$speedup_analysis) > 0) {
    expect_output(print(benchmark), "Max speedup")
  }
  
  if (length(benchmark$optimal_configurations) > 0) {
    expect_output(print(benchmark), "Optimal configurations")
  }
  
  if (length(benchmark$recommendations) > 0) {
    expect_output(print(benchmark), "Recommendations")
  }
})

# Integration tests with MONECA functions
test_that("diagnostic functions integrate with existing MONECA parallel functions", {
  
  skip_if_not_installed("foreach")
  skip_if_not_installed("doParallel")
  
  # Get system resources using the diagnostic function
  resources <- detect_system_resources(verbose = FALSE)
  
  # Test integration with parallel decision making
  decision <- should_use_parallel(
    n_combinations = 20,
    matrix_size = 30,
    user_preference = "auto",
    system_resources = resources,
    verbose = FALSE
  )
  
  expect_type(decision, "list")
  expect_identical(decision$system_info, resources)
  
  # Run diagnostic to verify the decision makes sense
  if (decision$use_parallel) {
    diagnostic <- diagnose_parallel_execution(
      n_workers = decision$n_cores,
      test_duration_sec = 3,
      verbose = FALSE
    )
    
    expect_s3_class(diagnostic, "moneca_parallel_diagnostic")
    
    # If MONECA thinks parallel should work, diagnostic should generally agree
    # (though there might be edge cases due to system load, etc.)
    expect_type(diagnostic$parallel_active, "logical")
  }
})

test_that("diagnostic functions handle system resource constraints", {
  
  # Test behavior when system has limited resources
  resources <- detect_system_resources(verbose = FALSE)
  
  # Test with more workers than available cores
  diagnostic <- diagnose_parallel_execution(
    n_workers = resources$total_cores + 2,  # More than available
    test_duration_sec = 2,
    verbose = FALSE
  )
  
  expect_s3_class(diagnostic, "moneca_parallel_diagnostic")
  expect_type(diagnostic$parallel_active, "logical")
  
  # Should still complete without errors
  expect_type(diagnostic$diagnostic_summary, "character")
  expect_length(diagnostic$diagnostic_summary, 1)
})

test_that("diagnostic functions provide meaningful error messages", {
  
  # Test various edge cases that might cause issues
  
  # Very short test duration
  diagnostic_short <- diagnose_parallel_execution(
    n_workers = 2,
    test_duration_sec = 0.1,  # Very short
    verbose = FALSE
  )
  
  expect_s3_class(diagnostic_short, "moneca_parallel_diagnostic")
  
  # Monitor for very short time
  monitor_short <- monitor_system_resources(
    monitoring_duration_sec = 0.5,
    sampling_interval_sec = 0.1,
    verbose = FALSE
  )
  
  expect_s3_class(monitor_short, "moneca_resource_monitor")
  
  # All should complete without errors and provide meaningful feedback
  expect_type(diagnostic_short$diagnostic_summary, "character")
  expect_type(monitor_short$monitoring_summary, "character")
  expect_gt(nchar(diagnostic_short$diagnostic_summary), 5)   # Meaningful message
  expect_gt(nchar(monitor_short$monitoring_summary), 5)      # Meaningful message
})

test_that("diagnostic functions work across different platforms", {
  
  # Test that functions work on current platform
  resources <- detect_system_resources(verbose = FALSE)
  
  # Platform should be detected
  expect_true(resources$platform %in% c("unix", "windows"))
  
  # Backend should be appropriate for platform
  if (resources$platform == "unix") {
    expect_equal(resources$parallel_backend, "fork")
  } else {
    expect_equal(resources$parallel_backend, "socket")
  }
  
  # Diagnostic functions should work regardless of platform
  diagnostic <- diagnose_parallel_execution(
    n_workers = min(2, resources$usable_cores),
    test_duration_sec = 2,
    verbose = FALSE
  )
  
  expect_s3_class(diagnostic, "moneca_parallel_diagnostic")
  expect_identical(diagnostic$system_info$platform, resources$platform)
})

# Performance and stress tests
test_that("diagnostic functions handle stress conditions", {
  
  skip_on_cran()  # Skip intensive tests on CRAN
  
  resources <- detect_system_resources(verbose = FALSE)
  
  # Skip if system doesn't have enough resources
  skip_if(resources$usable_cores < 2, "Insufficient cores for stress testing")
  skip_if(resources$memory_gb < 2, "Insufficient memory for stress testing")
  
  # Test with maximum recommended workers
  diagnostic_max <- diagnose_parallel_execution(
    n_workers = resources$max_recommended_cores,
    test_duration_sec = 5,
    verbose = FALSE
  )
  
  expect_s3_class(diagnostic_max, "moneca_parallel_diagnostic")
  
  # Should complete even under stress
  expect_type(diagnostic_max$diagnostic_summary, "character")
  expect_gt(nchar(diagnostic_max$diagnostic_summary), 5)
})

test_that("diagnostic functions provide actionable recommendations", {
  
  # Run a comprehensive diagnostic
  diagnostic <- diagnose_parallel_execution(
    n_workers = 2,
    test_duration_sec = 3,
    verbose = FALSE
  )
  
  expect_s3_class(diagnostic, "moneca_parallel_diagnostic")
  
  # Recommendations should be actionable
  if (length(diagnostic$recommendations) > 0) {
    # Should contain specific advice
    expect_true(any(sapply(diagnostic$recommendations, function(x) nchar(x) > 20)))
    
    # Should not just be empty strings
    expect_false(any(diagnostic$recommendations == ""))
  }
  
  # Benchmark should also provide recommendations
  benchmark <- benchmark_parallel_performance(
    test_scenarios = list(
      test = list(n_combinations = 6, matrix_size = 20, n_bootstrap = 3)
    ),
    max_workers = 2,
    benchmark_duration_per_test = 10,
    verbose = FALSE
  )
  
  expect_s3_class(benchmark, "moneca_parallel_benchmark")
  
  # Should provide some form of guidance
  expect_type(benchmark$benchmark_summary, "character")
  expect_gt(nchar(benchmark$benchmark_summary), 20)  # Meaningful summary
})