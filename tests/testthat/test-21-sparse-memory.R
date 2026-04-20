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
