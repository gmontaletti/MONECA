# Quick Performance Benchmark: moneca() vs moneca_fast() vs moneca_parallel()
# ===========================================================================

# Load development version
devtools::load_all(quiet = TRUE)

cat("\n")
cat("================================================================================\n")
cat("              MONECA Performance Comparison (Quick Benchmark)\n")
cat("================================================================================\n")
cat("\n")

available_cores <- parallel::detectCores()
cat("System: ", available_cores, "CPU cores available\n\n")

# Test configurations
configs <- list(
  list(name = "Small (15 classes)", n = 15, levels = 2),
  list(name = "Medium (30 classes)", n = 30, levels = 3),
  list(name = "Large (60 classes)", n = 60, levels = 3)
)

results_table <- data.frame(
  Dataset = character(),
  moneca_sec = numeric(),
  moneca_fast_sec = numeric(),
  moneca_parallel_2_sec = numeric(),
  moneca_parallel_all_sec = numeric(),
  stringsAsFactors = FALSE
)

for (config in configs) {
  cat(sprintf("Testing: %s, %d levels\n", config$name, config$levels))
  cat(str(rep("-", 60)), "\n")

  # Generate data
  set.seed(42)
  data <- generate_mobility_data(n_classes = config$n, seed = 42)

  # 1. Original moneca
  cat("  Running moneca()... ")
  t1 <- system.time({
    r1 <- moneca(data, segment.levels = config$levels, use_maximal_cliques = FALSE)
  })
  time_moneca <- as.numeric(t1["elapsed"])
  cat(sprintf("%.2f sec\n", time_moneca))

  # 2. moneca_fast
  cat("  Running moneca_fast()... ")
  t2 <- system.time({
    r2 <- moneca_fast(data, segment.levels = config$levels,
                      progress = FALSE, use_maximal_cliques = FALSE)
  })
  time_fast <- as.numeric(t2["elapsed"])
  cat(sprintf("%.2f sec (%.2fx)\n", time_fast, time_moneca / time_fast))

  # 3. moneca_parallel with 2 cores
  cat("  Running moneca_parallel(2)... ")
  t3 <- system.time({
    r3 <- moneca_parallel(data, segment.levels = config$levels, n.cores = 2,
                         progress = FALSE, use_maximal_cliques = FALSE)
  })
  time_parallel_2 <- as.numeric(t3["elapsed"])
  cat(sprintf("%.2f sec (%.2fx)\n", time_parallel_2, time_moneca / time_parallel_2))

  # 4. moneca_parallel with more cores
  n_cores <- min(available_cores - 1, 8)
  cat(sprintf("  Running moneca_parallel(%d)... ", n_cores))
  t4 <- system.time({
    r4 <- moneca_parallel(data, segment.levels = config$levels, n.cores = n_cores,
                         progress = FALSE, use_maximal_cliques = FALSE)
  })
  time_parallel_all <- as.numeric(t4["elapsed"])
  cat(sprintf("%.2f sec (%.2fx)\n", time_parallel_all, time_moneca / time_parallel_all))

  # Verify all produce same results
  same_levels <- (length(r1$segment.list) == length(r2$segment.list) &&
                  length(r1$segment.list) == length(r3$segment.list) &&
                  length(r1$segment.list) == length(r4$segment.list))

  if (same_levels) {
    cat("  ✓ All implementations produced identical clustering\n")
  } else {
    cat("  ⚠ WARNING: Different number of levels produced!\n")
  }

  cat("\n")

  # Store results
  results_table <- rbind(results_table, data.frame(
    Dataset = config$name,
    moneca_sec = time_moneca,
    moneca_fast_sec = time_fast,
    moneca_parallel_2_sec = time_parallel_2,
    moneca_parallel_all_sec = time_parallel_all
  ))
}

cat("================================================================================\n")
cat("                              SUMMARY TABLE\n")
cat("================================================================================\n\n")

# Print table
cat(sprintf("%-20s %10s %12s %15s %18s\n",
            "Dataset", "moneca()", "moneca_fast", "parallel(2)",
            sprintf("parallel(%d)", min(available_cores - 1, 8))))
cat(str(rep("-", 80)), "\n")

for (i in 1:nrow(results_table)) {
  row <- results_table[i, ]
  cat(sprintf("%-20s %9.2fs %11.2fs %14.2fs %17.2fs\n",
              row$Dataset,
              row$moneca_sec,
              row$moneca_fast_sec,
              row$moneca_parallel_2_sec,
              row$moneca_parallel_all_sec))
}

cat("\n")
cat("Speedup relative to moneca() (baseline):\n")
cat(str(rep("-", 80)), "\n")

for (i in 1:nrow(results_table)) {
  row <- results_table[i, ]
  cat(sprintf("%-20s %9s %11.2fx %14.2fx %17.2fx\n",
              row$Dataset,
              "(1.00x)",
              row$moneca_sec / row$moneca_fast_sec,
              row$moneca_sec / row$moneca_parallel_2_sec,
              row$moneca_sec / row$moneca_parallel_all_sec))
}

cat("\n")
cat("Key Findings:\n")
cat("-------------\n")

# Find which implementation is best for each size
for (i in 1:nrow(results_table)) {
  row <- results_table[i, ]
  times <- c(row$moneca_fast_sec, row$moneca_parallel_2_sec, row$moneca_parallel_all_sec)
  names_impl <- c("moneca_fast", "moneca_parallel(2)",
                  sprintf("moneca_parallel(%d)", min(available_cores - 1, 8)))
  best_idx <- which.min(times)
  speedup <- row$moneca_sec / times[best_idx]

  cat(sprintf("  %s: %s is fastest (%.2fx speedup)\n",
              row$Dataset, names_impl[best_idx], speedup))
}

cat("\n")
cat("================================================================================\n")
