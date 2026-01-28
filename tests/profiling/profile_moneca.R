# 1. Profile moneca and moneca_fast -----
# Benchmarks both implementations at multiple scales and saves profiling data.
# Requires: profvis, bench (optional)

library(moneca)

# 2. Generate test matrices -----
set.seed(42)
scales <- list(
  small = generate_mobility_data(n_classes = 10, seed = 101),
  medium = generate_mobility_data(n_classes = 30, seed = 202),
  large = generate_mobility_data(n_classes = 60, seed = 303)
)

cat("Matrix dimensions:\n")
for (nm in names(scales)) {
  cat(sprintf("  %s: %d x %d\n", nm, nrow(scales[[nm]]), ncol(scales[[nm]])))
}

# 3. Profvis profiling (if available) -----
if (requireNamespace("profvis", quietly = TRUE)) {
  prof_dir <- file.path("tests", "profiling", "results")
  dir.create(prof_dir, recursive = TRUE, showWarnings = FALSE)

  for (nm in names(scales)) {
    mx <- scales[[nm]]
    cat(sprintf("\nProfiling %s (%dx%d)...\n", nm, nrow(mx), ncol(mx)))

    # Profile moneca()
    p_orig <- profvis::profvis(
      {
        moneca(mx, segment.levels = 3)
      },
      simplify = FALSE
    )
    saveRDS(p_orig, file.path(prof_dir, paste0("profvis_moneca_", nm, ".rds")))

    # Profile moneca_fast() (from R/moneca_fast.R)
    p_fast <- profvis::profvis(
      {
        moneca_fast(mx, segment.levels = 3, progress = FALSE)
      },
      simplify = FALSE
    )
    saveRDS(
      p_fast,
      file.path(prof_dir, paste0("profvis_moneca_fast_", nm, ".rds"))
    )

    cat(sprintf("  Saved profvis results for %s\n", nm))
  }
} else {
  cat("profvis not installed -- skipping detailed profiling.\n")
}

# 4. bench::mark comparison -----
if (requireNamespace("bench", quietly = TRUE)) {
  cat("\n--- bench::mark comparison ---\n")
  results <- list()

  for (nm in names(scales)) {
    mx <- scales[[nm]]
    cat(sprintf(
      "\nBenchmarking %s (%dx%d)...\n",
      nm,
      nrow(mx) - 1,
      ncol(mx) - 1
    ))

    bm <- bench::mark(
      moneca = moneca(mx, segment.levels = 3),
      moneca_fast = moneca_fast(mx, segment.levels = 3, progress = FALSE),
      iterations = 3,
      check = FALSE
    )
    results[[nm]] <- bm
    print(bm[, c("expression", "min", "median", "mem_alloc", "n_itr")])
  }
} else {
  cat("\nbench not installed -- using Sys.time() fallback.\n")
  for (nm in names(scales)) {
    mx <- scales[[nm]]
    cat(sprintf("\nTiming %s (%dx%d):\n", nm, nrow(mx) - 1, ncol(mx) - 1))

    t1 <- Sys.time()
    r1 <- moneca(mx, segment.levels = 3)
    t2 <- Sys.time()
    r2 <- moneca_fast(mx, segment.levels = 3, progress = FALSE)
    t3 <- Sys.time()

    cat(sprintf("  moneca:      %.3fs\n", as.numeric(t2 - t1, units = "secs")))
    cat(sprintf("  moneca_fast: %.3fs\n", as.numeric(t3 - t2, units = "secs")))
    cat(sprintf(
      "  speedup:     %.1fx\n",
      as.numeric(t2 - t1) / as.numeric(t3 - t2)
    ))
  }
}

# 5. Correctness check -----
cat("\n--- Correctness verification ---\n")
for (nm in names(scales)) {
  mx <- scales[[nm]]
  r1 <- moneca(mx, segment.levels = 3)
  r2 <- moneca_fast(mx, segment.levels = 3, progress = FALSE)
  match <- identical(r1$segment.list, r2$segment.list)
  cat(sprintf("  %s: %s\n", nm, if (match) "MATCH" else "DIFFER"))
}

cat("\nDone.\n")
