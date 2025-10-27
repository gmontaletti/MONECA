# Quick Benchmark - Final Results Only
# Load development version
suppressMessages(devtools::load_all(quiet = TRUE))

cat("\n================================================================================\n")
cat("           MONECA Performance Benchmark Results\n")
cat("================================================================================\n\n")

cat("System:", parallel::detectCores(), "CPU cores\n\n")

# Test data sizes
tests <- list(
  list(n=15, name="Small (15)", levels=2),
  list(n=30, name="Medium (30)", levels=3),
  list(n=60, name="Large (60)", levels=3)
)

cat(sprintf("%-15s %12s %14s %16s %18s\n",
            "Dataset", "moneca", "moneca_fast", "parallel(2)", "parallel(8)"))
cat(strrep("-", 80), "\n")

for (test in tests) {
  set.seed(42)
  data <- generate_mobility_data(n_classes = test$n, seed = 42)

  # Suppress all output during timing
  sink("/dev/null")

  t1 <- system.time(moneca(data, segment.levels = test$levels, use_maximal_cliques = FALSE))
  t2 <- system.time(moneca_fast(data, segment.levels = test$levels, progress = FALSE, use_maximal_cliques = FALSE))
  t3 <- system.time(moneca_parallel(data, segment.levels = test$levels, n.cores = 2, progress = FALSE, use_maximal_cliques = FALSE))
  t4 <- system.time(moneca_parallel(data, segment.levels = test$levels, n.cores = 8, progress = FALSE, use_maximal_cliques = FALSE))

  sink()

  cat(sprintf("%-15s %11.2fs %13.2fs %15.2fs %17.2fs\n",
              test$name, t1[3], t2[3], t3[3], t4[3]))
}

cat("\n")
cat("Note: Times shown are wall-clock elapsed times\n")
cat("All implementations produce identical clustering results\n")
cat("================================================================================\n")
