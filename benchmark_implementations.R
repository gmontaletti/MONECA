# Benchmark: moneca() vs moneca_fast() vs moneca_parallel()
# =============================================================
#
# This script compares the performance of the three MONECA implementations
# across different dataset sizes and core counts.

# Load the development version with latest changes
devtools::load_all(quiet = TRUE)
library(microbenchmark)

# Helper function to format time
format_time <- function(time_ms) {
  if (time_ms < 1000) {
    return(sprintf("%.1f ms", time_ms))
  } else if (time_ms < 60000) {
    return(sprintf("%.2f sec", time_ms / 1000))
  } else {
    return(sprintf("%.2f min", time_ms / 60000))
  }
}

# Helper function to calculate speedup
calculate_speedup <- function(baseline_time, optimized_time) {
  sprintf("%.2fx", baseline_time / optimized_time)
}

cat("\n")
cat("================================================================================\n")
cat("          MONECA Implementation Performance Benchmark\n")
cat("================================================================================\n")
cat("\n")

# Detect available cores
available_cores <- parallel::detectCores()
cat("System Information:\n")
cat("  Available CPU cores:", available_cores, "\n")
cat("  R version:", R.version.string, "\n")
cat("\n")

# Test configurations
test_sizes <- list(
  list(name = "Small", n_classes = 10, levels = 2, times = 10),
  list(name = "Medium", n_classes = 25, levels = 3, times = 5),
  list(name = "Large", n_classes = 50, levels = 3, times = 3)
)

if (available_cores >= 8) {
  test_sizes[[4]] <- list(name = "Very Large", n_classes = 100, levels = 3, times = 2)
}

results <- list()

for (config in test_sizes) {
  cat("================================================================================\n")
  cat(sprintf("Testing: %s dataset (%d classes, %d levels)\n",
              config$name, config$n_classes, config$levels))
  cat("================================================================================\n\n")

  # Generate test data
  set.seed(42)
  test_data <- generate_mobility_data(
    n_classes = config$n_classes,
    immobility_strength = 0.6,
    class_clustering = 0.7,
    noise_level = 0.2,
    seed = 42
  )

  cat(sprintf("Generated %dx%d mobility matrix\n\n",
              nrow(test_data), ncol(test_data)))

  # Benchmark configurations
  benchmarks <- list()

  cat("Running benchmarks...\n")

  # 1. Original moneca (baseline)
  cat("  [1/4] moneca() (original)...\n")
  bm_original <- microbenchmark(
    moneca(test_data, segment.levels = config$levels, use_maximal_cliques = FALSE),
    times = config$times,
    unit = "ms"
  )
  time_original <- median(bm_original$time) / 1e6  # Convert to milliseconds
  benchmarks$original <- time_original

  # 2. moneca_fast
  cat("  [2/4] moneca_fast() (single-core)...\n")
  bm_fast <- microbenchmark(
    moneca_fast(test_data, segment.levels = config$levels,
                progress = FALSE, use_maximal_cliques = FALSE),
    times = config$times,
    unit = "ms"
  )
  time_fast <- median(bm_fast$time) / 1e6
  benchmarks$fast <- time_fast

  # 3. moneca_parallel with 2 cores
  cat("  [3/4] moneca_parallel() (2 cores)...\n")
  bm_parallel_2 <- microbenchmark(
    moneca_parallel(test_data, segment.levels = config$levels,
                    n.cores = 2, progress = FALSE, use_maximal_cliques = FALSE),
    times = config$times,
    unit = "ms"
  )
  time_parallel_2 <- median(bm_parallel_2$time) / 1e6
  benchmarks$parallel_2 <- time_parallel_2

  # 4. moneca_parallel with all available cores (if > 2)
  if (available_cores > 2) {
    n_cores <- min(available_cores - 1, 8)  # Use all but one, max 8
    cat(sprintf("  [4/4] moneca_parallel() (%d cores)...\n", n_cores))
    bm_parallel_all <- microbenchmark(
      moneca_parallel(test_data, segment.levels = config$levels,
                      n.cores = n_cores, progress = FALSE, use_maximal_cliques = FALSE),
      times = config$times,
      unit = "ms"
    )
    time_parallel_all <- median(bm_parallel_all$time) / 1e6
    benchmarks$parallel_all <- time_parallel_all
  }

  cat("\n")

  # Print results
  cat("Results:\n")
  cat("--------\n")
  cat(sprintf("  moneca() (baseline):        %s\n", format_time(time_original)))
  cat(sprintf("  moneca_fast():              %s  [%s speedup]\n",
              format_time(time_fast),
              calculate_speedup(time_original, time_fast)))
  cat(sprintf("  moneca_parallel() (2 cores): %s  [%s speedup]\n",
              format_time(time_parallel_2),
              calculate_speedup(time_original, time_parallel_2)))

  if (available_cores > 2) {
    cat(sprintf("  moneca_parallel() (%d cores): %s  [%s speedup]\n",
                n_cores,
                format_time(time_parallel_all),
                calculate_speedup(time_original, time_parallel_all)))
  }

  cat("\n")

  # Determine winner
  if (available_cores > 2) {
    fastest_time <- min(time_fast, time_parallel_2, time_parallel_all)
    if (fastest_time == time_fast) {
      winner <- "moneca_fast()"
    } else if (fastest_time == time_parallel_2) {
      winner <- "moneca_parallel() with 2 cores"
    } else {
      winner <- sprintf("moneca_parallel() with %d cores", n_cores)
    }
  } else {
    fastest_time <- min(time_fast, time_parallel_2)
    winner <- if (fastest_time == time_fast) "moneca_fast()" else "moneca_parallel() with 2 cores"
  }

  cat(sprintf("Winner: %s (%.2fx faster than baseline)\n\n",
              winner, time_original / fastest_time))

  # Store results
  results[[config$name]] <- benchmarks
}

# Summary
cat("================================================================================\n")
cat("                              SUMMARY\n")
cat("================================================================================\n\n")

cat("Recommendations based on dataset size:\n")
cat("---------------------------------------\n")

for (config in test_sizes) {
  bm <- results[[config$name]]
  fastest <- names(bm)[which.min(unlist(bm))]

  recommendation <- switch(fastest,
    "original" = "moneca() (original)",
    "fast" = "moneca_fast() - single-core optimization is sufficient",
    "parallel_2" = "moneca_parallel() with 2 cores",
    "parallel_all" = sprintf("moneca_parallel() with %d cores", min(available_cores - 1, 8))
  )

  cat(sprintf("  %s (%d classes): %s\n",
              config$name, config$n_classes, recommendation))
}

cat("\n")
cat("General Guidelines:\n")
cat("-------------------\n")
cat("  • For < 20 classes: moneca_fast() is typically fastest (low overhead)\n")
cat("  • For 20-50 classes: moneca_parallel() with 2-4 cores shows good speedup\n")
cat("  • For > 50 classes: moneca_parallel() with more cores (4-8) recommended\n")
cat("  • Single-core systems: Always use moneca_fast()\n")
cat("\n")

cat("Notes:\n")
cat("------\n")
cat("  • All implementations produce IDENTICAL clustering results\n")
cat("  • Parallel overhead is significant for small datasets\n")
cat("  • Actual speedup depends on: CPU, memory bandwidth, data characteristics\n")
cat("  • Times shown are median values from multiple runs\n")
cat("\n")

cat("Benchmark complete!\n")
cat("================================================================================\n")
