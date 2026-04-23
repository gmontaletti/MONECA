# Tests for sparse-matrix support in moneca_fast()
#
# Verifies that the sparse path (use.sparse = TRUE, or sparseMatrix input)
# produces results equivalent to the dense path and preserves sparsity in
# aggregated mat.list entries. Also exercises auto-detection of sparse input
# and the interaction of sparse routing with max.clique.size and
# symmetric_method parameters.

test_that("moneca_fast sparse path matches dense path - sum reciprocity", {
  skip_if_not_installed("Matrix")
  set.seed(1)
  mx <- generate_mobility_data(n_classes = 40, seed = 1)

  a <- moneca_fast(mx, use.sparse = FALSE, segment.levels = 3, progress = FALSE)
  b <- moneca_fast(
    Matrix::Matrix(mx, sparse = TRUE),
    use.sparse = TRUE,
    segment.levels = 3,
    progress = FALSE
  )

  expect_identical(a$segment.list, b$segment.list)
  # Aggregated matrices agree numerically AND structurally (including dimnames)
  for (k in seq_along(a$mat.list)) {
    expect_equal(
      as.matrix(b$mat.list[[k]]),
      as.matrix(a$mat.list[[k]]),
      tolerance = 1e-10
    )
  }
})

test_that("sparse path keeps mat.list entries as sparseMatrix", {
  skip_if_not_installed("Matrix")
  set.seed(2)
  mx <- generate_mobility_data(n_classes = 30, seed = 2)
  b <- moneca_fast(
    Matrix::Matrix(mx, sparse = TRUE),
    use.sparse = TRUE,
    segment.levels = 2,
    progress = FALSE
  )
  # First entry is the original input (sparse); aggregated levels also sparse
  expect_true(inherits(b$mat.list[[1]], "sparseMatrix"))
  expect_true(inherits(b$mat.list[[2]], "sparseMatrix"))
})

test_that("sparse path matches dense path with symmetric_method = 'min'", {
  skip_if_not_installed("Matrix")
  set.seed(3)
  mx <- generate_mobility_data(n_classes = 35, seed = 3)

  a <- moneca_fast(
    mx,
    use.sparse = FALSE,
    segment.levels = 3,
    symmetric_method = "min",
    progress = FALSE
  )
  b <- moneca_fast(
    Matrix::Matrix(mx, sparse = TRUE),
    use.sparse = TRUE,
    segment.levels = 3,
    symmetric_method = "min",
    progress = FALSE
  )

  expect_identical(a$segment.list, b$segment.list)
})

test_that("moneca_fast auto-detects sparse input without use.sparse flag", {
  skip_if_not_installed("Matrix")
  set.seed(4)
  mx <- generate_mobility_data(n_classes = 25, seed = 4)
  mx_sparse <- Matrix::Matrix(mx, sparse = TRUE)

  expect_no_error(
    b <- moneca_fast(mx_sparse, segment.levels = 2, progress = FALSE)
  )
  expect_s3_class(b, "moneca")
  expect_true(inherits(b$mat.list[[2]], "sparseMatrix"))
})

test_that("sparse path honors max.clique.size parameter", {
  skip_if_not_installed("Matrix")
  set.seed(5)
  mx <- generate_mobility_data(n_classes = 30, seed = 5)

  a <- moneca_fast(
    mx,
    use.sparse = FALSE,
    segment.levels = 2,
    max.clique.size = 3,
    progress = FALSE
  )
  b <- moneca_fast(
    Matrix::Matrix(mx, sparse = TRUE),
    use.sparse = TRUE,
    segment.levels = 2,
    max.clique.size = 3,
    progress = FALSE
  )

  expect_identical(a$segment.list, b$segment.list)
})

test_that("sparse path matches dense at n=300 (NaN reciprocal regression)", {
  # Regression test for the sparse-vs-dense parity bug described in
  # reference/moneca/profiling_moneca_fast.md (sec. 6 and 10.4). On synthetic
  # fixtures with n >= ~300, the dense path generates NaN entries at positions
  # where core == 0 AND exp == 0 (0/0), which propagate through rr + t(rr)
  # and drop the symmetric edge. The sparse path used to silently treat the
  # missing reciprocal as a zero, leaving the edge in place and diverging at
  # level 2+. The fix in weight_matrix_sparse() detects these reciprocal-NaN
  # positions and drops them to match dense.
  skip_if_not_installed("Matrix")
  set.seed(303)
  mx <- generate_mobility_data(n_classes = 300, seed = 303)

  a <- moneca_fast(mx, use.sparse = FALSE, segment.levels = 4, progress = FALSE)
  b <- moneca_fast(
    Matrix::Matrix(mx, sparse = TRUE),
    use.sparse = TRUE,
    segment.levels = 4,
    progress = FALSE
  )

  expect_identical(a$segment.list, b$segment.list)
  for (k in seq_along(a$mat.list)) {
    expect_equal(
      as.matrix(b$mat.list[[k]]),
      as.matrix(a$mat.list[[k]]),
      tolerance = 1e-10
    )
  }
})

test_that("sparse path preserves Inf edges at zero-expected positions", {
  # Regression: when a row/col margin is 0 but the core has non-zero entries,
  # dense weight.matrix() produces Inf (x/0 with x > 0) and keeps those as
  # strong edges after symmetrization. The sparse path used to drop them via
  # is.finite(), causing pattern mismatches on real-world fixtures with
  # slightly inconsistent margins. The fix preserves Inf values.
  skip_if_not_installed("Matrix")
  # Construct a tiny case: row 2 has zero outflow margin but core(2,3) > 0.
  core <- matrix(c(10, 0, 0, 0, 2, 3, 0, 1, 4), 3, 3, byrow = TRUE)
  rs <- rowSums(core)
  rs[2] <- 0 # Force inconsistent margin: margin 0 but core row non-zero
  cs <- colSums(core)
  gt <- sum(core)
  mx <- rbind(cbind(core, rs), c(cs, gt))
  rownames(mx) <- colnames(mx) <- c("a", "b", "c", "Total")

  wm_sparse_fn <- get("weight_matrix_sparse", envir = asNamespace("moneca"))
  wm_d <- weight.matrix(mx, cut.off = 1, symmetric = TRUE)
  wm_d[is.na(wm_d)] <- 0
  wm_s <- as.matrix(wm_sparse_fn(
    Matrix::Matrix(mx, sparse = TRUE),
    cut.off = 1,
    symmetric_method = "sum"
  ))
  expect_identical(wm_d > 0, wm_s > 0)
  expect_identical(sum(is.infinite(wm_d)), sum(is.infinite(wm_s)))
})

test_that("sparse path matches dense on real 127x127 fixture when available", {
  skip_if_not_installed("Matrix")
  real_path <- file.path(
    "/Users/giampaolomontaletti/Documents/funzioni",
    "data_pipeline/output/dashboard/profession_matrix_8day.rds"
  )
  skip_if_not(
    file.exists(real_path),
    "real 127x127 fixture not available in this environment"
  )
  mx_raw <- readRDS(real_path)
  n <- nrow(mx_raw)
  # Drop the 3 trailing armed-forces rows to match the profiling config
  mx <- mx_raw[1:(n - 3), 1:(n - 3)]

  a <- moneca_fast(mx, use.sparse = FALSE, segment.levels = 3, progress = FALSE)
  b <- moneca_fast(
    Matrix::Matrix(mx, sparse = TRUE),
    use.sparse = TRUE,
    segment.levels = 3,
    progress = FALSE
  )
  expect_identical(a$segment.list, b$segment.list)
})

test_that("use.sparse = TRUE on dense base input falls back to dense", {
  # Profiling report sec. 10.2: sparse path on dense input (real 127x127,
  # 82% density) gives no wall-time benefit and uses ~20% more memory.
  # Honour the sparseMatrix S4 class when the caller passes one, but on a
  # plain dense base matrix with > 50% density, fall back to the dense path
  # with an informational message.
  skip_if_not_installed("Matrix")
  set.seed(42)
  n <- 40L
  mx <- matrix(rpois(n * n, lambda = 5), n, n)
  rownames(mx) <- colnames(mx) <- c(paste0("c", 1:(n - 1)), "Total")
  mx[n, ] <- colSums(mx[-n, , drop = FALSE])
  mx[, n] <- c(rowSums(mx[-n, -n, drop = FALSE]), sum(mx[-n, -n]))
  stopifnot(mean(mx[-n, -n] != 0) > 0.5)

  msgs <- capture_messages(
    r <- moneca_fast(
      mx,
      use.sparse = TRUE,
      segment.levels = 2,
      progress = FALSE
    )
  )
  expect_true(any(grepl("falling back to dense", msgs)))
  # Result still valid and of moneca class
  expect_s3_class(r, "moneca")
  # Fallback path: mat.list should be dense base matrices, not sparseMatrix
  expect_true(is.matrix(r$mat.list[[1]]))
})

test_that("sparseMatrix input always keeps the sparse path", {
  # Respect the caller's storage choice: if they pass a sparseMatrix, don't
  # densify even when density is high. Only the dense-input-with-use.sparse
  # combination triggers the fallback.
  skip_if_not_installed("Matrix")
  set.seed(43)
  n <- 40L
  mx <- matrix(rpois(n * n, lambda = 5), n, n)
  rownames(mx) <- colnames(mx) <- c(paste0("c", 1:(n - 1)), "Total")
  mx[n, ] <- colSums(mx[-n, , drop = FALSE])
  mx[, n] <- c(rowSums(mx[-n, -n, drop = FALSE]), sum(mx[-n, -n]))
  mx_sp <- Matrix::Matrix(mx, sparse = TRUE)

  msgs <- capture_messages(
    r <- moneca_fast(
      mx_sp,
      use.sparse = TRUE,
      segment.levels = 2,
      progress = FALSE
    )
  )
  expect_false(any(grepl("falling back to dense", msgs)))
  expect_true(inherits(r$mat.list[[1]], "sparseMatrix"))
})

test_that("large sparse matrix stays within memory budget", {
  skip_on_cran()
  skip_if_not_installed("Matrix")
  skip_if_not(
    nzchar(Sys.getenv("MONECA_LARGE_TESTS")),
    "set MONECA_LARGE_TESTS=1 to run"
  )

  set.seed(6)
  # 2000 x 2000 at ~0.5% density - dense would be ~32 MB, this is a smoke
  # test of the sparse path not of the OOM scenario (which needs >24 GB)
  mx <- Matrix::rsparsematrix(2000, 2000, density = 0.005)
  mx <- abs(mx) * 1000 # positive mobility-like counts
  mx <- round(mx)
  # Add margins manually
  n <- nrow(mx)
  rs <- Matrix::rowSums(mx)
  cs <- Matrix::colSums(mx)
  gt <- sum(mx)
  mx <- Matrix::cbind2(mx, rs)
  mx <- Matrix::rbind2(mx, c(cs, gt))
  rownames(mx) <- colnames(mx) <- c(paste0("c", 1:n), "Total")

  expect_no_error(
    b <- moneca_fast(
      mx,
      use.sparse = TRUE,
      segment.levels = 2,
      has_margins = TRUE,
      progress = FALSE
    )
  )
  expect_true(inherits(b$mat.list[[2]], "sparseMatrix"))
})
