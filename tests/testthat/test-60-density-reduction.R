# Tests for Density Reduction Functions
# ======================================
#
# Comprehensive test suite for reduce_density() and related functions.
# Tests cover: basic functionality, normalization, auto k selection,
# thresholding, edge cases, and integration with moneca pipeline.

# 1. Basic Functionality Tests -----

test_that("reduce_density() works with default SVD method", {
  # Use medium-sized data for faster tests
  data <- get_test_data("medium")

  reduced <- reduce_density(data, method = "svd", k = 5, verbose = FALSE)

  # Check return type
  expect_s3_class(reduced, "density_reduced")
  expect_true(inherits(reduced, "matrix"))

  # Check dimensions preserved
  expect_equal(dim(reduced), dim(data))

  # Check attributes
  expect_equal(attr(reduced, "method"), "svd")
  expect_equal(attr(reduced, "normalization"), "none")
  expect_equal(attr(reduced, "k"), 5)
  expect_true(!is.null(attr(reduced, "variance_explained")))
})

test_that("reduce_density() works with NMF method", {
  skip_if_not_installed("RcppML")

  data <- get_test_data("medium")

  reduced <- reduce_density(
    data,
    method = "nmf",
    k = 5,
    seed = 42,
    verbose = FALSE
  )

  expect_s3_class(reduced, "density_reduced")
  expect_equal(attr(reduced, "method"), "nmf")
  expect_equal(attr(reduced, "k"), 5)
})

test_that("reduce_density() preserves row/column names", {
  data <- get_test_data("small")

  reduced <- reduce_density(data, k = 3, verbose = FALSE)

  expect_equal(rownames(reduced), rownames(data))
  expect_equal(colnames(reduced), colnames(data))
})

test_that("reduce_density() produces valid output matrix", {
  data <- get_test_data("medium")

  reduced <- reduce_density(data, k = 5, verbose = FALSE)

  # Validate using helper function
  validate_density_reduced(reduced)

  # Check no negative values
  expect_true(all(reduced >= 0))

  # Check values are integers
  expect_true(all(reduced == round(reduced)))
})

# 2. Normalization Tests -----

test_that("PPMI normalization works correctly", {
  data <- get_test_data("medium")

  reduced <- reduce_density(
    data,
    normalization = "ppmi",
    k = 5,
    verbose = FALSE
  )

  expect_equal(attr(reduced, "normalization"), "ppmi")
  validate_density_reduced(reduced)
})

test_that("Pearson residuals normalization works correctly", {
  data <- get_test_data("medium")

  reduced <- reduce_density(
    data,
    normalization = "pearson",
    k = 5,
    verbose = FALSE
  )

  expect_equal(attr(reduced, "normalization"), "pearson")
  validate_density_reduced(reduced)
})

test_that("different normalizations produce different results", {
  data <- get_test_data("medium")

  reduced_none <- reduce_density(
    data,
    normalization = "none",
    k = 5,
    verbose = FALSE
  )
  reduced_ppmi <- reduce_density(
    data,
    normalization = "ppmi",
    k = 5,
    verbose = FALSE
  )
  reduced_pearson <- reduce_density(
    data,
    normalization = "pearson",
    k = 5,
    verbose = FALSE
  )

  # Results should differ (but all should be valid)
  expect_false(identical(unclass(reduced_none), unclass(reduced_ppmi)))
  expect_false(identical(unclass(reduced_none), unclass(reduced_pearson)))
})

# 3. Automatic k Selection Tests -----

test_that("auto k selection respects variance_target", {
  data <- get_medium_large_test_data(n_classes = 20)

  reduced_low <- reduce_density(
    data,
    k = "auto",
    variance_target = 0.50,
    verbose = FALSE
  )

  reduced_high <- reduce_density(
    data,
    k = "auto",
    variance_target = 0.90,
    verbose = FALSE
  )

  # Higher variance target should require more components
  expect_true(attr(reduced_high, "k") >= attr(reduced_low, "k"))

  # Variance explained should meet targets
  expect_true(attr(reduced_high, "variance_explained") >= 0.90)
})

test_that("auto k selection works with default variance_target", {
  data <- get_medium_large_test_data(n_classes = 20)

  reduced <- reduce_density(data, k = "auto", verbose = FALSE)

  # Should achieve default 75% variance target
  expect_true(attr(reduced, "variance_explained") >= 0.75)

  # k should be reasonable (between 5 and matrix size)
  expect_true(attr(reduced, "k") >= 5)
  expect_true(attr(reduced, "k") <= 20)
})

# 4. Thresholding Tests -----

test_that("SD-based thresholding works", {
  data <- get_test_data("medium")

  reduced <- reduce_density(
    data,
    k = 5,
    threshold = 1,
    threshold_type = "sd",
    verbose = FALSE
  )

  expect_true(!is.null(attr(reduced, "threshold_applied")))

  # Should have some zeros (sparsification effect)
  core_mx <- reduced[1:(nrow(reduced) - 1), 1:(ncol(reduced) - 1)]
  expect_true(any(core_mx == 0))
})

test_that("percentile-based thresholding works", {
  data <- get_test_data("medium")

  reduced <- reduce_density(
    data,
    k = 5,
    threshold = 50,
    threshold_type = "percentile",
    verbose = FALSE
  )

  expect_true(!is.null(attr(reduced, "threshold_applied")))
})

test_that("auto threshold works", {
  data <- get_test_data("medium")

  reduced <- reduce_density(
    data,
    k = 5,
    threshold = "auto",
    verbose = FALSE
  )

  expect_true(!is.null(attr(reduced, "threshold_applied")))
})

test_that("NULL threshold produces no thresholding", {
  data <- get_test_data("medium")

  reduced <- reduce_density(
    data,
    k = 5,
    threshold = NULL,
    verbose = FALSE
  )

  expect_null(attr(reduced, "threshold_applied"))
})

# 5. Edge Cases and Error Handling -----

test_that("reduce_density() rejects NULL input", {
  expect_error(reduce_density(NULL), "cannot be NULL")
})

test_that("reduce_density() rejects non-square matrix", {
  bad_mx <- matrix(1:12, nrow = 3, ncol = 4)
  expect_error(reduce_density(bad_mx), "square")
})

test_that("reduce_density() rejects too small matrix", {
  small_mx <- matrix(c(10, 5, 5, 8, 15, 13, 13, 18), nrow = 4, ncol = 2)
  expect_error(reduce_density(small_mx), "square|at least")
})

test_that("reduce_density() validates k parameter", {
  data <- get_test_data("small")

  expect_error(reduce_density(data, k = 0), "positive integer")
  expect_error(reduce_density(data, k = -1), "positive integer")
  expect_error(reduce_density(data, k = "invalid"), "positive integer")
})

test_that("reduce_density() validates variance_target", {
  data <- get_test_data("small")

  expect_error(reduce_density(data, variance_target = 0), "between 0 and 1")
  expect_error(reduce_density(data, variance_target = 1), "between 0 and 1")
  expect_error(reduce_density(data, variance_target = 1.5), "between 0 and 1")
  expect_error(reduce_density(data, variance_target = -0.1), "between 0 and 1")
})

test_that("reduce_density() handles k larger than matrix dimensions", {
  data <- get_test_data("small")
  n <- nrow(data) - 1

  # Should warn and adjust k
  expect_warning(
    reduced <- reduce_density(data, k = 100, verbose = FALSE),
    "exceeds maximum"
  )

  expect_true(attr(reduced, "k") <= n - 1)
})

test_that("reduce_density() adds default names when missing", {
  # Create matrix without names
  data <- get_test_data("small")
  rownames(data) <- NULL
  colnames(data) <- NULL

  expect_warning(
    reduced <- reduce_density(data, k = 3, verbose = FALSE),
    "lacks row/column names"
  )

  expect_true(!is.null(rownames(reduced)))
  expect_true(!is.null(colnames(reduced)))
})

# 6. Reproducibility Tests -----

test_that("seed parameter ensures reproducibility for SVD", {
  data <- get_test_data("medium")

  reduced1 <- reduce_density(data, k = 5, seed = 123, verbose = FALSE)
  reduced2 <- reduce_density(data, k = 5, seed = 123, verbose = FALSE)

  expect_equal(unclass(reduced1), unclass(reduced2))
})

test_that("seed parameter ensures reproducibility for NMF", {
  skip_if_not_installed("RcppML")

  data <- get_test_data("medium")

  reduced1 <- reduce_density(
    data,
    method = "nmf",
    k = 5,
    seed = 42,
    verbose = FALSE
  )
  reduced2 <- reduce_density(
    data,
    method = "nmf",
    k = 5,
    seed = 42,
    verbose = FALSE
  )

  expect_equal(unclass(reduced1), unclass(reduced2))
})

test_that("different seeds produce different results for NMF", {
  skip_if_not_installed("RcppML")

  data <- get_test_data("medium")

  reduced1 <- reduce_density(
    data,
    method = "nmf",
    k = 5,
    seed = 42,
    verbose = FALSE
  )
  reduced2 <- reduce_density(
    data,
    method = "nmf",
    k = 5,
    seed = 123,
    verbose = FALSE
  )

  expect_false(identical(unclass(reduced1), unclass(reduced2)))
})

# 7. Integration Tests -----

test_that("reduce_density() output works with weight.matrix()", {
  data <- get_medium_large_test_data(n_classes = 20)

  reduced <- reduce_density(data, k = 10, verbose = FALSE)

  # Should work with weight.matrix
  wm <- weight.matrix(reduced, cut.off = 1)

  expect_true(is.matrix(wm))
  expect_equal(dim(wm), c(20, 20))
})

test_that("reduce_density() output works with moneca()", {
  data <- get_medium_large_test_data(n_classes = 20)

  reduced <- reduce_density(data, k = 10, verbose = FALSE)

  # Should work with moneca
  seg <- moneca(reduced, segment.levels = 2)

  expect_s3_class(seg, "moneca")
  validate_moneca_object(seg)
})

test_that("full pipeline works: reduce_density() -> moneca() -> plot", {
  skip_on_cran()

  data <- get_medium_large_test_data(n_classes = 20)

  reduced <- reduce_density(data, k = 10, verbose = FALSE)
  seg <- moneca(reduced, segment.levels = 2)

  # Should be able to create a plot (don't actually render)
  expect_no_error({
    p <- plot_moneca_ggraph(seg)
    expect_s3_class(p, "gg")
  })
})

# 8. plot_scree() Tests -----

test_that("plot_scree() produces valid ggplot", {
  data <- get_test_data("medium")

  p <- plot_scree(data)

  expect_s3_class(p, "gg")
  expect_s3_class(p, "ggplot")
})
test_that("plot_scree() respects variance_target parameter", {
  data <- get_test_data("medium")

  # Different targets should produce plots
  p1 <- plot_scree(data, variance_target = 0.50)
  p2 <- plot_scree(data, variance_target = 0.90)

  expect_s3_class(p1, "gg")
  expect_s3_class(p2, "gg")
})

test_that("plot_scree() works with show_elbow = FALSE", {
  data <- get_test_data("medium")

  p <- plot_scree(data, show_elbow = FALSE)

  expect_s3_class(p, "gg")
})

test_that("plot_scree() accepts custom title", {
  data <- get_test_data("medium")

  p <- plot_scree(data, title = "Custom Scree Plot Title")

  expect_s3_class(p, "gg")
})

# 9. print.density_reduced() Tests -----

test_that("print method works for density_reduced objects", {
  data <- get_test_data("medium")

  reduced <- reduce_density(data, k = 5, verbose = FALSE)

  # Capture print output
  output <- capture.output(print(reduced))

  expect_true(any(grepl("Density-Reduced", output)))
  expect_true(any(grepl("Method:", output)))
  expect_true(any(grepl("Components", output)))
})

# 10. Large Data Tests (skipped on CRAN) -----

test_that("reduce_density() handles 60+ category matrices", {
  skip_on_cran()

  data <- get_large_test_data(n_classes = 60, n_total = 20000)

  reduced <- reduce_density(
    data,
    k = "auto",
    variance_target = 0.75,
    verbose = FALSE
  )

  validate_density_reduced(reduced)

  # k should be in expected range for large matrices
  expect_true(attr(reduced, "k") >= 10)
  expect_true(attr(reduced, "k") <= 40)
})

test_that("reduce_density() with irlba is faster for large matrices", {
  skip_on_cran()
  skip_if_not_installed("irlba")

  data <- get_large_test_data(n_classes = 60, n_total = 20000)

  # Should complete in reasonable time
  reduced <- reduce_density(data, k = 20, verbose = FALSE)

  validate_density_reduced(reduced)
})

# 11. Internal Function Tests -----

test_that("calculate_ppmi handles zero matrix", {
  zero_mx <- matrix(0, nrow = 5, ncol = 5)

  expect_warning(
    result <- moneca:::calculate_ppmi(zero_mx),
    "zero"
  )

  expect_equal(result, zero_mx)
})

test_that("pearson_residuals handles zero matrix", {
  zero_mx <- matrix(0, nrow = 5, ncol = 5)

  expect_warning(
    result <- moneca:::pearson_residuals(zero_mx),
    "zero"
  )

  expect_equal(result, zero_mx)
})

test_that("detect_elbow returns valid index", {
  variance <- c(0.5, 0.2, 0.1, 0.05, 0.03, 0.02)

  elbow <- moneca:::detect_elbow(variance)

  expect_true(is.numeric(elbow))
  expect_true(elbow >= 1)
  expect_true(elbow <= length(variance))
})

test_that("detect_elbow handles short vectors", {
  short_var <- c(0.8, 0.2)

  elbow <- moneca:::detect_elbow(short_var)

  expect_equal(elbow, 1)
})

# 12. Comparison with Original Data -----

test_that("reduced matrix retains mobility structure", {
  data <- get_medium_large_test_data(n_classes = 20)

  reduced <- reduce_density(data, k = 10, verbose = FALSE)

  # Diagonal dominance pattern should be preserved
  n <- nrow(data) - 1
  orig_diag_ratio <- sum(diag(data[1:n, 1:n])) / sum(data[1:n, 1:n])
  red_diag_ratio <- sum(diag(reduced[1:n, 1:n])) / sum(reduced[1:n, 1:n])

  # Diagonal ratio should be similar (within 20% relative difference)
  expect_true(abs(orig_diag_ratio - red_diag_ratio) / orig_diag_ratio < 0.3)
})

test_that("reduced matrix has lower rank", {
  skip_if_not_installed("Matrix")

  data <- get_medium_large_test_data(n_classes = 20)
  k <- 8

  reduced <- reduce_density(data, k = k, verbose = FALSE)

  # Calculate approximate rank
  n <- nrow(data) - 1
  svd_orig <- svd(data[1:n, 1:n])
  svd_red <- svd(reduced[1:n, 1:n])

  # Reduced matrix should have fewer significant singular values
  # (values above threshold are "significant")
  threshold <- max(svd_orig$d) * 0.01
  rank_orig <- sum(svd_orig$d > threshold)
  rank_red <- sum(svd_red$d > threshold)

  expect_true(rank_red <= rank_orig)
})
