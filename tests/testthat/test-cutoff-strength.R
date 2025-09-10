# Test suite for cut-off and strength-based analysis functions

library(testthat)
library(moneca)

test_that("analyze_rr_distribution works correctly", {
  # Generate test data
  mx <- generate_mobility_data(n_classes = 5, seed = 123)
  
  # Analyze RR distribution
  rr_dist <- analyze_rr_distribution(mx)
  
  # Check structure
  expect_s3_class(rr_dist, "rr_distribution")
  expect_true("summary" %in% names(rr_dist))
  expect_true("quantiles" %in% names(rr_dist))
  expect_true("suggested_cutoffs" %in% names(rr_dist))
  
  # Check values are reasonable
  expect_true(all(rr_dist$suggested_cutoffs > 0))
  expect_true(rr_dist$n_values > 0)
  expect_length(rr_dist$quantiles, 11)  # Deciles
})

test_that("evaluate_cutoff_strength returns correct metrics", {
  # Generate test data
  mx <- generate_mobility_data(n_classes = 5, seed = 123)
  
  # Evaluate at different cut-offs
  metrics_low <- evaluate_cutoff_strength(mx, cut.off = 0.5)
  metrics_high <- evaluate_cutoff_strength(mx, cut.off = 2.0)
  
  # Check structure
  expect_type(metrics_low, "list")
  expect_true("mean_strength" %in% names(metrics_low))
  expect_true("strength_ratio" %in% names(metrics_low))
  expect_true("n_edges" %in% names(metrics_low))
  
  # Higher cut-off should result in fewer edges
  expect_true(metrics_high$n_edges <= metrics_low$n_edges)
  
  # Check value ranges
  expect_true(metrics_low$strength_ratio >= 0 && metrics_low$strength_ratio <= 1)
  expect_true(metrics_low$gini_coefficient >= 0 && metrics_low$gini_coefficient <= 1)
})

test_that("calculate_strength_density works with different methods", {
  # Create a simple weighted graph
  adj_mat <- matrix(c(0, 2, 3, 
                     2, 0, 1,
                     3, 1, 0), nrow = 3)
  g <- igraph::graph_from_adjacency_matrix(adj_mat, mode = "undirected", weighted = TRUE)
  
  # Test different methods
  density_ratio <- calculate_strength_density(g, method = "ratio")
  density_norm <- calculate_strength_density(g, method = "normalized")
  density_cv <- calculate_strength_density(g, method = "cv")
  
  # All should be between 0 and 1
  expect_true(density_ratio >= 0 && density_ratio <= 1)
  expect_true(density_norm >= 0 && density_norm <= 1)
  expect_true(density_cv >= 0 && density_cv <= 1)
  
  # Test with empty graph
  g_empty <- igraph::make_empty_graph(5)
  expect_equal(calculate_strength_density(g_empty), 0)
})

test_that("calculate_gini works correctly", {
  # Perfect equality
  equal_strengths <- rep(10, 5)
  expect_equal(calculate_gini(equal_strengths), 0)
  
  # Perfect inequality (one node has everything)
  unequal_strengths <- c(100, 0, 0, 0, 0)
  gini_unequal <- calculate_gini(unequal_strengths)
  expect_true(gini_unequal > 0.5)  # Should be high
  
  # Empty vector
  expect_equal(calculate_gini(numeric(0)), 0)
  
  # All zeros
  expect_equal(calculate_gini(rep(0, 5)), 0)
})

test_that("find_natural_breaks identifies reasonable breakpoints", {
  # Create data with clear clusters
  rr_values <- c(rnorm(30, mean = 0.5, sd = 0.1),
                rnorm(30, mean = 1.5, sd = 0.1),
                rnorm(30, mean = 3, sd = 0.1))
  
  breaks <- find_natural_breaks(rr_values, n_breaks = 2)
  
  # Should find breaks between the clusters
  expect_length(breaks, 2)
  expect_true(breaks[1] > 0.7 && breaks[1] < 1.2)
  expect_true(breaks[2] > 1.8 && breaks[2] < 2.5)
})

test_that("find_elbow_point identifies elbow correctly", {
  # Create L-shaped curve
  x <- 1:10
  y <- c(10, 8, 6, 4, 3, 2.5, 2.2, 2.1, 2.05, 2)
  
  elbow <- find_elbow_point(x, y)
  
  # Elbow should be around 4-6
  expect_true(elbow >= 3 && elbow <= 7)
})

test_that("cutoff_sensitivity_analysis runs without errors", {
  # Small test data for speed
  mx <- generate_mobility_data(n_classes = 4, seed = 123)
  
  # Run sensitivity analysis
  sensitivity <- cutoff_sensitivity_analysis(mx, 
                                            cutoff_range = seq(0.5, 2, by = 0.5),
                                            verbose = FALSE)
  
  # Check structure
  expect_s3_class(sensitivity, "cutoff_sensitivity")
  expect_true(is.data.frame(sensitivity))
  expect_true("cutoff" %in% names(sensitivity))
  expect_true("mean_strength" %in% names(sensitivity))
  expect_true("modularity" %in% names(sensitivity))
  
  # Check computed metrics
  expect_true(all(sensitivity$edge_retention >= 0 & sensitivity$edge_retention <= 1))
  expect_true(all(sensitivity$strength_retention >= 0 & sensitivity$strength_retention <= 1))
})

test_that("find_optimal_cutoff works with different criteria", {
  # Generate test data
  mx <- generate_mobility_data(n_classes = 5, seed = 123)
  
  # Test elbow criterion
  optimal_elbow <- find_optimal_cutoff(mx, criterion = "elbow",
                                       cutoff_range = seq(0.5, 2, by = 0.25),
                                       verbose = FALSE)
  
  expect_s3_class(optimal_elbow, "optimal_cutoff")
  expect_true(optimal_elbow$optimal_cutoff > 0)
  expect_equal(optimal_elbow$criterion, "elbow")
  
  # Test balance criterion
  optimal_balance <- find_optimal_cutoff(mx, criterion = "balance",
                                         cutoff_range = seq(0.5, 2, by = 0.25),
                                         verbose = FALSE)
  
  expect_true(optimal_balance$optimal_cutoff > 0)
  expect_equal(optimal_balance$criterion, "balance")
  
  # Test modularity criterion
  optimal_mod <- find_optimal_cutoff(mx, criterion = "modularity",
                                     cutoff_range = seq(0.5, 2, by = 0.25),
                                     verbose = FALSE)
  
  expect_true(optimal_mod$optimal_cutoff > 0)
  expect_equal(optimal_mod$criterion, "modularity")
})

test_that("moneca_fast uses strength-based density correctly", {
  # Generate test data
  mx <- generate_mobility_data(n_classes = 6, seed = 456)
  
  # Run moneca_fast with very high min.density (should trigger early stopping)
  seg_high <- moneca_fast(mx, segment.levels = 2, min.density = 0.99, progress = FALSE)
  
  # With such high threshold, should get trivial segmentation
  expect_equal(length(seg_high$segment.list[[1]]), nrow(mx) - 1)
  
  # Run with normal min.density
  seg_normal <- moneca_fast(mx, segment.levels = 2, min.density = 0.01, progress = FALSE)
  
  # Should get non-trivial segmentation
  expect_true(length(seg_normal$segment.list) >= 2)
})

test_that("strength density calculation is consistent", {
  # Create a weighted matrix
  mx <- generate_mobility_data(n_classes = 5, seed = 789)
  
  # Get weight matrix
  weight_mat <- weight.matrix(mx, cut.off = 1)
  weight_mat[is.na(weight_mat)] <- 0
  
  # Create graph
  g <- moneca_graph_from_adjacency(weight_mat, mode = "undirected", 
                                   weighted = TRUE, diag = FALSE)
  
  # Calculate strength density
  strengths <- igraph::strength(g, mode = "all")
  if (length(strengths) > 0 && any(strengths > 0)) {
    strength_density <- mean(strengths) / max(strengths)
  } else {
    strength_density <- 0
  }
  
  # Should be between 0 and 1
  expect_true(strength_density >= 0 && strength_density <= 1)
  
  # Compare with edge density - they should be different
  edge_density <- igraph::edge_density(g)
  expect_false(isTRUE(all.equal(strength_density, edge_density)))
})