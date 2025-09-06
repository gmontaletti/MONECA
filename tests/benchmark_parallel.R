#!/usr/bin/env Rscript

#' Benchmark Script for Parallel moneca Implementation
#'
#' This script compares the performance of sequential vs parallel moneca
#' across different matrix sizes and number of cores.

library(moneca)

# Benchmarking function
benchmark_moneca <- function(n_classes, segment_levels = 2, n_cores_list = c(1, 2, 4), 
                            n_runs = 3, seed = 123) {
  
  cat("\n", paste(rep("=", 60), collapse = ""), "\n")
  cat("Benchmarking with", n_classes, "x", n_classes, "mobility matrix\n")
  cat(paste(rep("=", 60), collapse = ""), "\n\n")
  
  # Generate test data
  set.seed(seed)
  test_data <- generate_mobility_data(n_classes = n_classes, 
                                      immobility_strength = 5,
                                      class_clustering = 0.7,
                                      noise_level = 0.1,
                                      seed = seed)
  
  results <- list()
  
  # Test sequential version (baseline)
  cat("Testing sequential moneca (baseline)...\n")
  times_seq <- numeric(n_runs)
  
  for (run in 1:n_runs) {
    cat("  Run", run, "of", n_runs, "... ")
    start_time <- Sys.time()
    result_seq <- moneca(test_data, segment.levels = segment_levels)
    end_time <- Sys.time()
    times_seq[run] <- as.numeric(difftime(end_time, start_time, units = "secs"))
    cat(sprintf("%.2f seconds\n", times_seq[run]))
  }
  
  avg_time_seq <- mean(times_seq)
  results$sequential <- list(
    times = times_seq,
    avg_time = avg_time_seq,
    n_segments = lapply(result_seq$segment.list, length)
  )
  
  cat(sprintf("\nSequential average: %.2f seconds (baseline)\n\n", avg_time_seq))
  
  # Test parallel versions with different core counts
  for (n_cores in n_cores_list) {
    cat("Testing parallel moneca with", n_cores, "cores...\n")
    times_par <- numeric(n_runs)
    
    for (run in 1:n_runs) {
      cat("  Run", run, "of", n_runs, "... ")
      start_time <- Sys.time()
      result_par <- moneca_parallel(test_data, 
                                    segment.levels = segment_levels,
                                    n.cores = n_cores,
                                    progress = FALSE)
      end_time <- Sys.time()
      times_par[run] <- as.numeric(difftime(end_time, start_time, units = "secs"))
      cat(sprintf("%.2f seconds\n", times_par[run]))
    }
    
    avg_time_par <- mean(times_par)
    speedup <- avg_time_seq / avg_time_par
    
    results[[paste0("parallel_", n_cores, "cores")]] <- list(
      times = times_par,
      avg_time = avg_time_par,
      speedup = speedup,
      n_segments = lapply(result_par$segment.list, length)
    )
    
    cat(sprintf("\nParallel (%d cores) average: %.2f seconds (%.2fx speedup)\n\n", 
                n_cores, avg_time_par, speedup))
  }
  
  # Test different parallel backends
  cat("Testing parallel backends...\n")
  
  # Socket backend
  cat("  Socket backend... ")
  start_time <- Sys.time()
  result_socket <- moneca_parallel(test_data, 
                                   segment.levels = segment_levels,
                                   n.cores = 2,
                                   parallel.backend = "socket",
                                   progress = FALSE)
  end_time <- Sys.time()
  time_socket <- as.numeric(difftime(end_time, start_time, units = "secs"))
  cat(sprintf("%.2f seconds\n", time_socket))
  
  results$backend_socket <- list(
    time = time_socket,
    speedup = avg_time_seq / time_socket
  )
  
  # Fork backend (Unix/Mac only)
  if (.Platform$OS.type == "unix") {
    cat("  Fork backend... ")
    start_time <- Sys.time()
    result_fork <- moneca_parallel(test_data, 
                                   segment.levels = segment_levels,
                                   n.cores = 2,
                                   parallel.backend = "fork",
                                   progress = FALSE)
    end_time <- Sys.time()
    time_fork <- as.numeric(difftime(end_time, start_time, units = "secs"))
    cat(sprintf("%.2f seconds\n", time_fork))
    
    results$backend_fork <- list(
      time = time_fork,
      speedup = avg_time_seq / time_fork
    )
  }
  
  return(results)
}

# Function to create performance summary table
create_summary_table <- function(all_results) {
  cat("\n", paste(rep("=", 80), collapse = ""), "\n")
  cat("PERFORMANCE SUMMARY TABLE\n")
  cat(paste(rep("=", 80), collapse = ""), "\n\n")
  
  # Header
  cat(sprintf("%-15s | %-10s | %-12s | %-12s | %-12s | %-10s\n", 
              "Matrix Size", "Method", "Avg Time (s)", "Speedup", "Efficiency", "Segments"))
  cat(paste(rep("-", 80), collapse = ""), "\n")
  
  for (size_name in names(all_results)) {
    size <- as.numeric(gsub("size_", "", size_name))
    results <- all_results[[size_name]]
    
    # Sequential baseline
    cat(sprintf("%-15s | %-10s | %12.2f | %12s | %12s | %-10s\n",
                paste0(size, "x", size),
                "Sequential",
                results$sequential$avg_time,
                "1.00x",
                "100%",
                paste(unlist(results$sequential$n_segments), collapse = "-")))
    
    # Parallel versions
    for (method in names(results)) {
      if (startsWith(method, "parallel_")) {
        n_cores <- as.numeric(gsub("parallel_|cores", "", method))
        efficiency <- (results[[method]]$speedup / n_cores) * 100
        
        cat(sprintf("%-15s | %-10s | %12.2f | %12.2fx | %11.1f%% | %-10s\n",
                    "",
                    paste0(n_cores, " cores"),
                    results[[method]]$avg_time,
                    results[[method]]$speedup,
                    efficiency,
                    paste(unlist(results[[method]]$n_segments), collapse = "-")))
      }
    }
    cat("\n")
  }
}

# Function to test scaling behavior
test_scaling <- function() {
  cat("\n", paste(rep("=", 80), collapse = ""), "\n")
  cat("TESTING SCALING BEHAVIOR\n")
  cat(paste(rep("=", 80), collapse = ""), "\n\n")
  
  sizes <- c(20, 50, 100, 150)
  scaling_results <- list()
  
  for (n in sizes) {
    cat("\nTesting with", n, "classes...\n")
    
    # Generate data
    test_data <- generate_mobility_data(n_classes = n, seed = 42)
    
    # Sequential
    start <- Sys.time()
    res_seq <- moneca(test_data, segment.levels = 2)
    time_seq <- as.numeric(difftime(Sys.time(), start, units = "secs"))
    
    # Parallel with 2 cores
    start <- Sys.time()
    res_par2 <- moneca_parallel(test_data, segment.levels = 2, 
                                n.cores = 2, progress = FALSE)
    time_par2 <- as.numeric(difftime(Sys.time(), start, units = "secs"))
    
    # Parallel with 4 cores (if available)
    if (parallel::detectCores() >= 4) {
      start <- Sys.time()
      res_par4 <- moneca_parallel(test_data, segment.levels = 2, 
                                  n.cores = 4, progress = FALSE)
      time_par4 <- as.numeric(difftime(Sys.time(), start, units = "secs"))
    } else {
      time_par4 <- NA
    }
    
    scaling_results[[paste0("n", n)]] <- list(
      n = n,
      time_seq = time_seq,
      time_par2 = time_par2,
      time_par4 = time_par4,
      speedup2 = time_seq / time_par2,
      speedup4 = if (!is.na(time_par4)) time_seq / time_par4 else NA
    )
    
    cat(sprintf("  Sequential: %.2fs | 2 cores: %.2fs (%.2fx) | 4 cores: %s\n",
                time_seq, time_par2, time_seq/time_par2,
                if (!is.na(time_par4)) sprintf("%.2fs (%.2fx)", time_par4, time_seq/time_par4) else "N/A"))
  }
  
  # Plot scaling if possible
  cat("\n", paste(rep("-", 60), collapse = ""), "\n")
  cat("Scaling Summary:\n")
  cat(paste(rep("-", 60), collapse = ""), "\n\n")
  
  cat(sprintf("%-10s | %-12s | %-12s | %-12s | %-12s\n",
              "Size", "Sequential", "2 Cores", "Speedup", "Efficiency"))
  cat(paste(rep("-", 60), collapse = ""), "\n")
  
  for (res in scaling_results) {
    cat(sprintf("%-10d | %11.2fs | %11.2fs | %11.2fx | %11.1f%%\n",
                res$n, res$time_seq, res$time_par2, res$speedup2, 
                (res$speedup2 / 2) * 100))
  }
  
  return(scaling_results)
}

# Main benchmarking execution
main <- function() {
  cat("\n")
  cat("╔══════════════════════════════════════════════════════════════╗\n")
  cat("║         MONECA PARALLEL PERFORMANCE BENCHMARK SUITE         ║\n")
  cat("╚══════════════════════════════════════════════════════════════╝\n")
  cat("\n")
  
  # System information
  cat("System Information:\n")
  cat("  Platform:", R.version$platform, "\n")
  cat("  R Version:", R.version$version.string, "\n")
  cat("  Available cores:", parallel::detectCores(), "\n")
  cat("  Date:", Sys.Date(), "\n")
  cat("\n")
  
  # Determine number of cores to test
  available_cores <- parallel::detectCores()
  if (available_cores >= 4) {
    cores_to_test <- c(1, 2, 4)
  } else if (available_cores >= 2) {
    cores_to_test <- c(1, 2)
  } else {
    cores_to_test <- c(1)
  }
  
  cat("Testing with cores:", paste(cores_to_test, collapse = ", "), "\n\n")
  
  # Run benchmarks for different sizes
  all_results <- list()
  
  # Small matrix
  all_results$size_30 <- benchmark_moneca(30, segment_levels = 2, 
                                          n_cores_list = cores_to_test,
                                          n_runs = 3)
  
  # Medium matrix
  all_results$size_75 <- benchmark_moneca(75, segment_levels = 2,
                                          n_cores_list = cores_to_test,
                                          n_runs = 2)
  
  # Large matrix (if enough memory)
  tryCatch({
    all_results$size_150 <- benchmark_moneca(150, segment_levels = 2,
                                             n_cores_list = cores_to_test,
                                             n_runs = 1)
  }, error = function(e) {
    cat("\nSkipping 150x150 matrix due to:", e$message, "\n")
  })
  
  # Create summary table
  create_summary_table(all_results)
  
  # Test scaling behavior
  cat("\n")
  scaling_results <- test_scaling()
  
  # Final recommendations
  cat("\n", paste(rep("=", 80), collapse = ""), "\n")
  cat("RECOMMENDATIONS\n")
  cat(paste(rep("=", 80), collapse = ""), "\n\n")
  
  # Analyze results and provide recommendations
  avg_speedups <- numeric()
  for (size_results in all_results) {
    for (method in names(size_results)) {
      if (startsWith(method, "parallel_") && endsWith(method, "2cores")) {
        avg_speedups <- c(avg_speedups, size_results[[method]]$speedup)
      }
    }
  }
  
  avg_speedup <- mean(avg_speedups, na.rm = TRUE)
  
  cat("Based on the benchmark results:\n\n")
  cat(sprintf("• Average speedup with 2 cores: %.2fx\n", avg_speedup))
  cat(sprintf("• Average parallel efficiency: %.1f%%\n", (avg_speedup / 2) * 100))
  cat("\n")
  
  if (avg_speedup > 1.5) {
    cat("✓ Parallel processing provides significant performance benefits\n")
    cat("✓ Recommended for matrices larger than 50x50\n")
  } else if (avg_speedup > 1.2) {
    cat("✓ Parallel processing provides moderate performance benefits\n")
    cat("✓ Recommended for matrices larger than 100x100\n")
  } else {
    cat("⚠ Parallel processing provides limited benefits on this system\n")
    cat("⚠ Consider using only for very large matrices (>200x200)\n")
  }
  
  if (.Platform$OS.type == "unix") {
    cat("✓ Fork backend available and recommended for better performance\n")
  } else {
    cat("• Socket backend will be used (Windows system)\n")
  }
  
  cat("\n")
  cat("Optimal number of cores for your system: ", 
      min(ceiling(available_cores / 2), 4), "\n")
  
  cat("\n", paste(rep("=", 80), collapse = ""), "\n\n")
  
  invisible(list(results = all_results, scaling = scaling_results))
}

# Run if executed directly
if (!interactive()) {
  results <- main()
} else {
  cat("Run main() to execute the benchmark suite\n")
}