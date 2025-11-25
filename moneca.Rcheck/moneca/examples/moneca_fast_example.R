# Example demonstrating moneca_fast() performance improvements

library(moneca)

# Generate test data of different sizes
sizes <- c(5, 10, 15, 20)
results <- list()

cat("Comparing moneca() vs moneca_fast() performance:\n\n")

for (n in sizes) {
  cat(sprintf("Testing with %d classes:\n", n))
  
  # Generate data
  set.seed(42)
  test_data <- generate_mobility_data(n_classes = n, immobility_strength = 0.7)
  
  # Time original version
  time_original <- system.time({
    result_original <- moneca(test_data, segment.levels = 2)
  })
  
  # Time fast version
  time_fast <- system.time({
    result_fast <- moneca_fast(test_data, segment.levels = 2, progress = FALSE)
  })
  
  # Time fast version with sparse matrices (for larger data)
  if (n >= 15) {
    time_sparse <- system.time({
      result_sparse <- moneca_fast(test_data, segment.levels = 2, use.sparse = TRUE, progress = FALSE)
    })
  } else {
    time_sparse <- NA
  }
  
  # Store results
  results[[as.character(n)]] <- list(
    original = time_original["elapsed"],
    fast = time_fast["elapsed"],
    sparse = time_sparse["elapsed"],
    speedup = as.numeric(time_original["elapsed"]) / as.numeric(time_fast["elapsed"])
  )
  
  cat(sprintf("  Original: %.3f seconds\n", time_original["elapsed"]))
  cat(sprintf("  Fast:     %.3f seconds (%.1fx speedup)\n", 
              time_fast["elapsed"], 
              as.numeric(time_original["elapsed"]) / as.numeric(time_fast["elapsed"])))
  
  if (!is.na(time_sparse["elapsed"])) {
    cat(sprintf("  Sparse:   %.3f seconds\n", time_sparse["elapsed"]))
  }
  cat("\n")
}

# Demonstrate additional features of moneca_fast()
cat("\nDemonstrating additional moneca_fast() features:\n\n")

# 1. Early stopping with sparse data
sparse_data <- matrix(0, nrow = 11, ncol = 11)
diag(sparse_data[1:10, 1:10]) <- 100
sparse_data[11,] <- colSums(sparse_data)
sparse_data[,11] <- rowSums(sparse_data)
rownames(sparse_data) <- colnames(sparse_data) <- c(LETTERS[1:10], "Total")

cat("1. Early stopping with sparse matrix:\n")
result_sparse <- moneca_fast(sparse_data, segment.levels = 3, min.density = 0.1, progress = FALSE)
cat(sprintf("   Requested levels: 3, Actual levels: %d (stopped early due to sparsity)\n\n", 
            length(result_sparse$segment.list)))

# 2. Maximum clique size limitation
cat("2. Limiting maximum clique size:\n")
test_data <- generate_mobility_data(n_classes = 10)
result_unlimited <- moneca_fast(test_data, segment.levels = 2, progress = FALSE)
result_limited <- moneca_fast(test_data, segment.levels = 2, max.clique.size = 4, progress = FALSE)
cat("   This can help with very dense networks where clique enumeration becomes expensive\n\n")

# 3. Using different cut-off values
cat("3. Effect of different cut-off values:\n")
for (cutoff in c(0.5, 1.0, 1.5, 2.0)) {
  result <- moneca_fast(test_data, cut.off = cutoff, segment.levels = 2, progress = FALSE)
  n_segments <- length(result$segment.list[[2]])
  cat(sprintf("   Cut-off %.1f: %d segments at level 2\n", cutoff, n_segments))
}

cat("\nNote: moneca_fast() maintains compatibility with all existing MONECA functions\n")
cat("(plotting, analysis, etc.) while providing better performance for large datasets.\n")