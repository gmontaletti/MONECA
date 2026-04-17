# Tests for Dual-Matrix System in moneca_fast()
# ==============================================
#
# When reduce_density is active, the reduced matrix should only determine
# cluster topology. The original matrix should be used for all stored
# aggregations and metrics, preserving count totals.

# 1. Sum preservation -----

test_that("mat.list[[1]] preserves original matrix sum with density reduction", {
  skip_if_not_installed("RcppML")

  mob <- get_test_data("medium", seed = 42)
  n <- nrow(mob) - 1
  original_sum <- sum(mob[1:n, 1:n])

  result <- moneca_fast(
    mob,
    segment.levels = 2,
    reduce_density = TRUE,
    density_params = list(method = "svd", seed = 123),
    progress = FALSE
  )

  # mat.list[[1]] core should equal original input sum
  stored_n <- nrow(result$mat.list[[1]]) - 1
  stored_sum <- sum(result$mat.list[[1]][1:stored_n, 1:stored_n])
  expect_equal(
    stored_sum,
    original_sum,
    info = "mat.list[[1]] must contain original counts, not reduced"
  )
})

# 2. mat.list uses original values -----

test_that("mat.list[[1]] is identical to input matrix (not reduced)", {
  skip_if_not_installed("RcppML")

  mob <- get_test_data("medium", seed = 42)

  result <- moneca_fast(
    mob,
    segment.levels = 2,
    reduce_density = TRUE,
    density_params = list(method = "svd", seed = 123),
    progress = FALSE
  )

  stored <- result$mat.list[[1]]
  # If sparse was used, convert back

  if (inherits(stored, "sparseMatrix")) {
    stored <- as.matrix(stored)
  }

  expect_equal(
    unname(stored),
    unname(mob),
    info = "mat.list[[1]] should be the original matrix with margins"
  )
})

# 3. Sum invariance across levels -----

test_that("core cell sums are invariant across mat.list levels", {
  skip_if_not_installed("RcppML")

  mob <- get_test_data("medium", seed = 42)
  n <- nrow(mob) - 1
  original_sum <- sum(mob[1:n, 1:n])

  result <- moneca_fast(
    mob,
    segment.levels = 3,
    reduce_density = TRUE,
    density_params = list(method = "svd", seed = 123),
    progress = FALSE
  )

  for (k in seq_along(result$mat.list)) {
    mat_k <- result$mat.list[[k]]
    if (inherits(mat_k, "sparseMatrix")) {
      mat_k <- as.matrix(mat_k)
    }
    nk <- nrow(mat_k) - 1
    level_sum <- sum(mat_k[1:nk, 1:nk])
    expect_equal(
      level_sum,
      original_sum,
      tolerance = 1e-6,
      info = sprintf("mat.list[[%d]] core sum should equal original", k)
    )
  }
})

# 4. No-reduction path is unchanged (regression guard) -----

test_that("reduce_density = FALSE produces identical output to baseline", {
  mob <- get_test_data("small", seed = 42)

  result_no_reduction <- moneca_fast(
    mob,
    segment.levels = 2,
    reduce_density = FALSE,
    progress = FALSE
  )

  # Run again to verify determinism
  result_again <- moneca_fast(
    mob,
    segment.levels = 2,
    reduce_density = FALSE,
    progress = FALSE
  )

  expect_equal(
    result_no_reduction$segment.list,
    result_again$segment.list,
    info = "reduce_density = FALSE must be deterministic"
  )

  expect_equal(
    result_no_reduction$mat.list,
    result_again$mat.list,
    info = "mat.list must be identical across runs with no reduction"
  )

  expect_null(
    result_no_reduction$density_reduction,
    info = "density_reduction should be NULL when not applied"
  )
})

# 5. Density reduction metadata is stored -----

test_that("density_reduction metadata is present when reduction is applied", {
  skip_if_not_installed("RcppML")

  mob <- get_test_data("medium", seed = 42)

  result <- moneca_fast(
    mob,
    segment.levels = 2,
    reduce_density = TRUE,
    density_params = list(method = "svd", seed = 123),
    progress = FALSE
  )

  expect_false(
    is.null(result$density_reduction),
    info = "density_reduction should contain metadata"
  )
  expect_true("method" %in% names(result$density_reduction))
  expect_true("k" %in% names(result$density_reduction))
})

# 6. Isolates summary uses original counts -----

test_that("isolates_summary uses original matrix counts with density reduction", {
  skip_if_not_installed("RcppML")

  mob <- get_test_data("medium", seed = 42)
  n <- nrow(mob) - 1
  original_sum <- sum(mob[1:n, 1:n])

  result <- moneca_fast(
    mob,
    segment.levels = 2,
    reduce_density = TRUE,
    density_params = list(method = "svd", seed = 123),
    progress = FALSE,
    isolates = TRUE
  )

  # The mobility_matrix in isolates_summary should sum to original total
  iso_sum <- sum(result$isolates_summary$mobility_matrix)
  expect_equal(
    iso_sum,
    original_sum,
    info = "isolates_summary mobility_matrix should use original counts"
  )
})
