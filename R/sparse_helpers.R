#' Sparse Weight Matrix Computation
#'
#' Computes the relative risk weight matrix directly from a sparse mobility
#' matrix, without densifying. Mirrors the arithmetic of
#' \code{\link{weight.matrix}} but only materializes values at positions where
#' the core sub-matrix is non-zero.
#'
#' Internally, "no edge" is represented as structural zero (drop0'd), not
#' \code{NA}. This is the key departure from the dense path and is why the
#' sparse result remains sparse.
#'
#' @param mx A square sparse matrix (dgCMatrix) with margins in the last
#'   row/column.
#' @param cut.off Numeric threshold. Values strictly below \code{cut.off} are
#'   dropped to zero.
#' @param symmetric_method One of \code{"sum"} or \code{"min"}. When
#'   \code{"sum"}, the matrix is symmetrized as \code{rr + t(rr)}. When
#'   \code{"min"}, symmetrization is \code{pmin(rr, t(rr)) * 2} where pmin is
#'   taken only at positions non-zero in both directions.
#' @param small.cell.reduction Numeric threshold. Core cells strictly below
#'   this value are zeroed before computing relative risk.
#' @param diagonal Optional. If \code{NULL} (the default) the diagonal is
#'   zeroed after symmetrization (matches \code{weight.matrix()} which sets
#'   \code{NA}; zero is the sparse-safe equivalent).
#'
#' @return A \code{dgCMatrix} of relative risks with structural zeros where no
#'   edge exists. Row and column names are preserved from the input core
#'   sub-matrix.
#'
#' @keywords internal
#' @noRd
weight_matrix_sparse <- function(
  mx,
  cut.off = 1,
  symmetric_method = "sum",
  small.cell.reduction = 0,
  diagonal = NULL
) {
  if (!requireNamespace("Matrix", quietly = TRUE)) {
    stop("weight_matrix_sparse() requires the 'Matrix' package.")
  }
  if (is.null(mx)) {
    stop("Input cannot be NULL")
  }
  l <- nrow(mx)
  if (l < 2L) {
    stop("Matrix must have at least 2 rows and columns")
  }
  if (nrow(mx) != ncol(mx)) {
    stop("Matrix must be square (same number of rows and columns)")
  }

  # Core sub-matrix (everything except margins)
  core <- mx[-l, -l, drop = FALSE]
  if (!inherits(core, "sparseMatrix")) {
    core <- Matrix::Matrix(core, sparse = TRUE)
  }

  # Marginal vectors and totals
  o.r.s <- as.numeric(mx[-l, l])
  o.c.s <- as.numeric(mx[l, -l])
  total.total <- as.numeric(mx[l, l])
  total.mobility <- as.numeric(sum(core))

  # Apply small cell reduction on the core (structural drop)
  if (small.cell.reduction > 0) {
    core <- core * (core >= small.cell.reduction)
    core <- Matrix::drop0(core)
  }

  # Guard against degenerate totals
  if (!is.finite(total.total) || total.total == 0) {
    out <- Matrix::sparseMatrix(
      i = integer(0),
      j = integer(0),
      x = numeric(0),
      dims = c(l - 1L, l - 1L),
      dimnames = list(rownames(mx)[-l], colnames(mx)[-l])
    )
    return(out)
  }

  # Extract triplets (1-based indices via Matrix::summary)
  trip <- Matrix::summary(core)
  if (nrow(trip) == 0L) {
    out <- Matrix::sparseMatrix(
      i = integer(0),
      j = integer(0),
      x = numeric(0),
      dims = c(l - 1L, l - 1L),
      dimnames = list(rownames(mx)[-l], colnames(mx)[-l])
    )
    return(out)
  }

  # Compute expected counts only at nonzero positions
  exp_ij <- (o.r.s[trip$i] / total.total) *
    (o.c.s[trip$j] / total.total) *
    total.mobility

  # Relative risk at triplet positions. By construction trip$x > 0
  # (structural non-zeros), so rr_x is finite when exp_ij > 0 and Inf when
  # exp_ij == 0. Inf is a legitimate value: weight.matrix() produces Inf at
  # these positions and keeps them as strong edges after symmetrization.
  rr_x <- trip$x / exp_ij

  # Parity with dense weight.matrix():
  # In the dense path, rr(p,q) = core(p,q) / exp(p,q); when core(p,q) == 0
  # AND exp(p,q) == 0 the result is NaN. After sym = rr + t(rr) (or pmin),
  # a NaN reciprocal turns sym(i,j) into NaN, which find.segments drops via
  # !is.na(mat). The sparse path only materializes positions where core > 0,
  # so it never "sees" the reciprocal NaN and instead treats the absent
  # reciprocal as a silent zero, leaving the edge in place -- a divergence
  # from dense. Detect reciprocal-NaN cases explicitly and drop them here.
  #
  # Reciprocal (j, i) is NaN in dense iff core(j, i) == 0 AND exp(j, i) == 0.
  # exp(j, i) == 0 iff o.r.s[j] == 0 OR o.c.s[i] == 0.
  recip_core_zero <- as.numeric(core[cbind(trip$j, trip$i)]) == 0
  recip_exp_zero <- (o.r.s[trip$j] == 0) | (o.c.s[trip$i] == 0)
  keep <- !(recip_core_zero & recip_exp_zero) & !is.nan(rr_x)

  rr <- Matrix::sparseMatrix(
    i = trip$i[keep],
    j = trip$j[keep],
    x = rr_x[keep],
    dims = c(l - 1L, l - 1L),
    dimnames = list(rownames(mx)[-l], colnames(mx)[-l])
  )

  # Symmetrize
  if (identical(symmetric_method, "sum")) {
    rr <- rr + Matrix::t(rr)
  } else if (identical(symmetric_method, "min")) {
    rr <- sparse_pmin_symmetric(rr)
    rr <- rr * 2
  } else {
    stop(
      "symmetric_method must be 'sum' or 'min', got: '",
      symmetric_method,
      "'"
    )
  }

  # Ensure we can touch @x slot safely (coerce to dgCMatrix if needed)
  if (!inherits(rr, "dgCMatrix")) {
    rr <- as(rr, "generalMatrix")
    rr <- as(rr, "CsparseMatrix")
  }

  # Apply cut-off: drop entries strictly below the threshold
  if (length(rr@x) > 0L) {
    below <- rr@x < cut.off
    if (any(below)) {
      rr@x[below] <- 0
      rr <- Matrix::drop0(rr)
    }
  }

  # Diagonal handling: mirror weight.matrix() contract
  if (is.null(diagonal)) {
    Matrix::diag(rr) <- 0
    rr <- Matrix::drop0(rr)
  }

  rr
}

#' Element-wise Minimum of a Sparse Matrix and its Transpose
#'
#' Computes \code{pmin(m, t(m))} restricted to the positions that are
#' structurally non-zero in BOTH \code{m} and \code{t(m)}. Positions present in
#' only one side are treated as zero and dropped.
#'
#' This matches the semantics needed by the min-reciprocity symmetrization in
#' \code{\link{weight_matrix_sparse}}: a pair scores only if both directions
#' contribute.
#'
#' @param m A sparse matrix (dgCMatrix).
#'
#' @return A \code{dgCMatrix} of the same dimensions and dimnames as \code{m}.
#'
#' @keywords internal
#' @noRd
sparse_pmin_symmetric <- function(m) {
  if (!requireNamespace("Matrix", quietly = TRUE)) {
    stop("sparse_pmin_symmetric() requires the 'Matrix' package.")
  }
  if (!inherits(m, "sparseMatrix")) {
    stop("Input must be a sparseMatrix")
  }

  ma <- as(m, "TsparseMatrix")
  mb <- as(Matrix::t(m), "TsparseMatrix")

  dims <- dim(m)
  dn <- dimnames(m)

  if (length(ma@x) == 0L || length(mb@x) == 0L) {
    return(Matrix::sparseMatrix(
      i = integer(0),
      j = integer(0),
      x = numeric(0),
      dims = dims,
      dimnames = dn
    ))
  }

  df_a <- data.frame(i = ma@i, j = ma@j, a = ma@x)
  df_b <- data.frame(i = mb@i, j = mb@j, b = mb@x)
  merged <- merge(df_a, df_b, by = c("i", "j"))

  if (nrow(merged) == 0L) {
    return(Matrix::sparseMatrix(
      i = integer(0),
      j = integer(0),
      x = numeric(0),
      dims = dims,
      dimnames = dn
    ))
  }

  Matrix::sparseMatrix(
    i = merged$i + 1L,
    j = merged$j + 1L,
    x = pmin(merged$a, merged$b),
    dims = dims,
    dimnames = dn
  )
}

#' Sparse Segment Matrix Aggregation
#'
#' Aggregates a sparse mobility matrix by segment membership, producing the
#' next-level mobility matrix while keeping the result sparse.
#'
#' @param mx A sparse mobility matrix (dgCMatrix) with margins in the last
#'   row/column.
#' @param segments A list with a \code{membership} element (factor or integer
#'   vector), matching the output of \code{find.segments.fast}.
#'
#' @return A sparse matrix where row \code{k} / column \code{k} is the
#'   aggregated mobility for segment \code{k}, and the last row/column holds
#'   the margins.
#'
#' @keywords internal
#' @noRd
segment_matrix_sparse <- function(mx, segments) {
  if (!requireNamespace("Matrix", quietly = TRUE)) {
    stop("segment_matrix_sparse() requires the 'Matrix' package.")
  }
  if (!inherits(mx, "sparseMatrix")) {
    mx <- Matrix::Matrix(mx, sparse = TRUE)
  }

  # Match segment.matrix.fast (dense): label the margin row with
  # length(membership) + 1, and sort levels numerically to mirror
  # rowsum(reorder = TRUE).
  memb_int <- as.integer(segments$membership)
  groups.1 <- c(memb_int, length(memb_int) + 1L)
  f <- factor(groups.1, levels = sort(unique(groups.1)))

  G <- Matrix::fac2sparse(f) # k x n indicator

  res <- G %*% mx %*% Matrix::t(G)

  # Preserve group labels on both dimensions
  grp_names <- levels(f)
  dimnames(res) <- list(grp_names, grp_names)

  # Ensure dgCMatrix for downstream slot access
  if (!inherits(res, "dgCMatrix")) {
    res <- as(res, "generalMatrix")
    res <- as(res, "CsparseMatrix")
  }
  res
}

#' Extract Edge Indices from a Sparse Matrix
#'
#' Returns a 2-column integer matrix \code{(row, col)} of 1-based indices where
#' \code{mat[i, j] > cut.off}. Mirrors the output shape of
#' \code{which(..., arr.ind = TRUE)} used in the dense edge-enumeration path.
#'
#' @param mat A sparse matrix (dgCMatrix).
#' @param cut.off Numeric threshold; strictly greater-than comparison.
#'
#' @return Integer matrix with columns \code{"row"} and \code{"col"}.
#'
#' @keywords internal
#' @noRd
sparse_edges_to_arr_ind <- function(mat, cut.off) {
  if (!requireNamespace("Matrix", quietly = TRUE)) {
    stop("sparse_edges_to_arr_ind() requires the 'Matrix' package.")
  }
  trip <- Matrix::summary(mat)
  keep <- trip$x > cut.off
  cbind(row = as.integer(trip$i[keep]), col = as.integer(trip$j[keep]))
}
