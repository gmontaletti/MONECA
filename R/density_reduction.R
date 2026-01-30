# Density Reduction for Large Mobility Matrices
# =============================================
#
# This module provides preprocessing functions to reduce noise in large
# mobility matrices (60+ categories) through dimensionality reduction
# techniques (SVD/NMF) before MONECA analysis.
#
# Pipeline: raw_matrix -> reduce_density() -> weight.matrix() -> moneca()

#' @importFrom stats sd quantile
NULL

# 1. Main Function -----

#' Reduce Density of Large Mobility Matrices
#'
#' Preprocesses large mobility matrices to reduce noise through dimensionality
#' reduction techniques. This function is designed for matrices with 60+
#' categories where noise can obscure meaningful mobility patterns.
#'
#' @param mx A mobility matrix with totals row/column in standard moneca format.
#'   Must have at least 4 rows/columns (excluding totals).
#' @param method Character string specifying the dimensionality reduction method:
#'   \itemize{
#'     \item \code{"svd"} (default): Singular Value Decomposition - fast and
#'       memory-efficient, produces continuous values
#'     \item \code{"nmf"}: Non-negative Matrix Factorization - preserves
#'       non-negativity, better interpretability for count data
#'   }
#' @param normalization Character string specifying pre-processing normalization:
#'   \itemize{
#'     \item \code{"none"} (default): No normalization, use raw counts
#'     \item \code{"pearson"}: Pearson residuals - standardizes for marginal effects
#'     \item \code{"ppmi"}: Positive Pointwise Mutual Information - emphasizes
#'       mobility above chance levels
#'   }
#' @param k Number of components to retain. Either \code{"auto"} (default) for
#'   automatic selection based on variance explained, or a positive integer.
#'   Typical range for 60+ category matrices is 15-25.
#' @param variance_target Numeric value (0-1) specifying target cumulative
#'   variance to explain when \code{k = "auto"}. Default is 0.75. Higher values
#'   retain more detail but also more noise.
#' @param threshold Optional sparsification threshold. Can be:
#'   \itemize{
#'     \item \code{NULL} (default): No thresholding
#'     \item \code{"auto"}: Automatic threshold (mean + 1 SD)
#'     \item Numeric: Values below this threshold are set to zero
#'   }
#' @param threshold_type Character string specifying how numeric threshold is
#'   interpreted:
#'   \itemize{
#'     \item \code{"sd"} (default): Number of standard deviations above mean
#'     \item \code{"percentile"}: Percentile threshold (0-100)
#'   }
#' @param verbose Logical indicating whether to print progress information.
#'   Default is \code{FALSE}.
#' @param seed Optional integer for reproducibility. Particularly important
#'   for NMF which uses random initialization.
#' @param filter_quantile Controls the quantile threshold for filtering. Applies
#'   to both SVD and NMF methods. Can be:
#'   \itemize{
#'     \item \code{"auto"} (default): Automatically selects threshold via elbow
#'       detection on reconstruction values
#'     \item Numeric (0-1): Keep cells with reconstruction values above this
#'       quantile. E.g., 0.75 keeps top 75% of cells by reconstruction strength.
#'   }
#'   Both SVD and NMF use reconstruction values as a filter to identify
#'   significant cells, then return the original values for those cells. This
#'   preserves count interpretability while reducing network density.
#'
#' @return An object of class \code{"density_reduced"} (inherits from matrix)
#'   with the following attributes:
#'   \describe{
#'     \item{method}{The reduction method used ("svd" or "nmf")}
#'     \item{normalization}{The normalization method used}
#'     \item{k}{Number of components retained}
#'     \item{variance_explained}{Proportion of variance explained (SVD only)}
#'     \item{filter_quantile}{The filter quantile used to select cells}
#'     \item{threshold_applied}{The threshold value applied (if any)}
#'     \item{original_dims}{Dimensions of the original core matrix}
#'   }
#'
#' @details
#' The function implements a pipeline for reducing noise in large mobility matrices:
#'
#' \enumerate{
#'   \item \strong{Input Validation}: Checks matrix format and extracts core
#'     matrix (excluding totals row/column)
#'   \item \strong{Normalization}: Optionally transforms counts using PPMI or
#'     Pearson residuals
#'   \item \strong{Component Selection}: When \code{k = "auto"}, uses variance
#'     explained with elbow detection
#'   \item \strong{Dimensionality Reduction}: Applies SVD or NMF to extract
#'     dominant patterns
#'   \item \strong{Reconstruction}: Reconstructs matrix from reduced components
#'   \item \strong{Filtering}: Uses reconstruction values as a filter to identify
#'     significant cells. Cells with reconstruction values below the threshold
#'     (based on \code{filter_quantile}) are set to zero. Original cell values
#'     are preserved for cells that pass the filter.
#'   \item \strong{Thresholding}: Optionally applies additional thresholding
#'   \item \strong{Totals}: Recalculates row/column totals
#' }
#'
#' For SVD, the reconstruction is \eqn{M \approx U_k \Sigma_k V_k^T} where k
#' components are retained. For NMF, the factorization is \eqn{M \approx W H}
#' with non-negative factors. Both methods use reconstruction values to filter
#' cells while preserving original counts for retained cells.
#'
#' @section Dependencies:
#' \itemize{
#'   \item \code{irlba}: Optional, for fast truncated SVD. Falls back to base R
#'     \code{svd()} if not available.
#'   \item \code{RcppML}: Required only for \code{method = "nmf"}
#' }
#'
#' @examples
#' # Generate large synthetic data
#' large_data <- generate_mobility_data(n_classes = 60, n_total = 30000, seed = 123)
#'
#' # Basic SVD reduction with auto k selection
#' reduced <- reduce_density(large_data, verbose = TRUE)
#' print(reduced)
#'
#' # SVD with PPMI normalization
#' reduced_ppmi <- reduce_density(large_data,
#'                                normalization = "ppmi",
#'                                variance_target = 0.80,
#'                                verbose = TRUE)
#'
#' # NMF reduction with fixed k
#' \dontrun{
#' reduced_nmf <- reduce_density(large_data,
#'                               method = "nmf",
#'                               k = 20,
#'                               seed = 42)
#' }
#'
#' # With thresholding to increase sparsity
#' reduced_sparse <- reduce_density(large_data,
#'                                  threshold = 1,
#'                                  threshold_type = "sd")
#'
#' # Continue with standard moneca pipeline
#' seg <- moneca(reduced, segment.levels = 3)
#'
#' @seealso
#' \code{\link{plot_scree}} for visualizing component selection,
#' \code{\link{weight.matrix}} for the next step in the pipeline,
#' \code{\link{moneca}} for the main analysis function
#'
#' @export
reduce_density <- function(
  mx,
  method = c("svd", "nmf"),
  normalization = c("none", "pearson", "ppmi"),
  k = "auto",
  variance_target = 0.75,
  threshold = NULL,
  threshold_type = c("sd", "percentile"),
  verbose = FALSE,
  seed = NULL,
  filter_quantile = "auto"
) {
  # Match arguments

  method <- match.arg(method)
  normalization <- match.arg(normalization)
  threshold_type <- match.arg(threshold_type)

  # Set seed for reproducibility
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # 1. Input validation -----
  if (is.null(mx)) {
    stop("Input matrix cannot be NULL")
  }

  if (!is.matrix(mx)) {
    mx <- tryCatch(
      as.matrix(mx),
      error = function(e) {
        stop("Input must be convertible to a matrix: ", e$message)
      }
    )
  }

  if (nrow(mx) != ncol(mx)) {
    stop("Matrix must be square")
  }

  if (nrow(mx) < 5) {
    stop("Matrix must have at least 4 categories (5 rows including totals)")
  }

  # Check for row/column names
  if (is.null(rownames(mx)) || is.null(colnames(mx))) {
    warning("Matrix lacks row/column names; using default names")
    rownames(mx) <- colnames(mx) <- c(
      paste0("Class", 1:(nrow(mx) - 1)),
      "Total"
    )
  }

  # Validate k parameter
  if (!identical(k, "auto") && (!is.numeric(k) || k < 1 || k != round(k))) {
    stop("k must be 'auto' or a positive integer")
  }

  # Validate variance_target
  if (
    !is.numeric(variance_target) || variance_target <= 0 || variance_target >= 1
  ) {
    stop("variance_target must be between 0 and 1 (exclusive)")
  }

  # Validate filter_quantile
  if (!identical(filter_quantile, "auto")) {
    if (
      !is.numeric(filter_quantile) ||
        filter_quantile <= 0 ||
        filter_quantile >= 1
    ) {
      stop(
        "filter_quantile must be 'auto' or a numeric value between 0 and 1 (exclusive)"
      )
    }
  }

  # 2. Extract core matrix (without totals) -----
  n <- nrow(mx) - 1
  core_mx <- mx[1:n, 1:n]
  original_total <- sum(core_mx)
  original_names <- rownames(core_mx)

  if (verbose) {
    message(sprintf(
      "Input matrix: %d x %d (core: %d x %d)",
      nrow(mx),
      ncol(mx),
      n,
      n
    ))
    message(sprintf("Total observations: %d", original_total))
  }

  # 3. Apply normalization -----
  normalized_mx <- switch(
    normalization,
    "none" = core_mx,
    "ppmi" = calculate_ppmi(core_mx),
    "pearson" = pearson_residuals(core_mx)
  )

  if (verbose && normalization != "none") {
    message(sprintf("Applied %s normalization", normalization))
  }

  # 4. Select k automatically if needed -----
  if (identical(k, "auto")) {
    k_result <- select_k_auto(normalized_mx, variance_target, verbose)
    k <- k_result$k
    variance_explained <- k_result$variance_explained
  } else {
    # Validate user-specified k
    max_k <- min(nrow(normalized_mx), ncol(normalized_mx)) - 1
    if (k > max_k) {
      warning(sprintf("k=%d exceeds maximum (%d), using k=%d", k, max_k, max_k))
      k <- max_k
    }
    variance_explained <- NULL
  }

  if (verbose) {
    message(sprintf("Using k = %d components", k))
  }

  # 5. Perform dimensionality reduction -----
  if (method == "svd") {
    reconstructed <- perform_svd_reduction(normalized_mx, k, verbose)
    if (is.null(variance_explained)) {
      # Calculate variance explained for user-specified k
      svd_result <- perform_svd(normalized_mx, k)
      total_var <- sum(svd_result$d^2)
      variance_explained <- sum(svd_result$d[1:k]^2) / total_var
    }

    # 6a. Post-processing for SVD: Use as filter (like NMF) -----
    # SVD reconstruction values are used to identify significant cells,
    # but original values are preserved (no distortion)

    recon <- reconstructed
    # Clip negative values for threshold calculation
    recon[recon < 0] <- 0

    # Determine filter quantile (auto or manual)
    actual_quantile <- filter_quantile
    if (identical(filter_quantile, "auto")) {
      actual_quantile <- select_filter_quantile_auto(recon, verbose)
    }

    # Calculate threshold from reconstruction values
    nonzero_recon <- recon[recon > 0]
    if (length(nonzero_recon) > 0) {
      # threshold is the value below which we discard
      # (1 - actual_quantile) gives the proportion to discard
      filter_threshold <- quantile(nonzero_recon, probs = 1 - actual_quantile)

      # Apply filter: keep original values where reconstruction is significant
      reduced_mx <- core_mx # Use original counts (no distortion)
      reduced_mx[recon < filter_threshold] <- 0

      if (verbose) {
        n_cells_kept <- sum(recon >= filter_threshold)
        n_cells_total <- sum(recon > 0)
        message(sprintf(
          "SVD filter: keeping %d/%d cells (%.1f%%) with reconstruction >= %.4f",
          n_cells_kept,
          n_cells_total,
          100 * actual_quantile,
          filter_threshold
        ))
      }
    } else {
      warning(
        "SVD reconstruction produced all zeros; returning original matrix"
      )
      reduced_mx <- core_mx
      actual_quantile <- NA
    }
  } else if (method == "nmf") {
    # NMF returns list with reconstruction and original
    nmf_result <- perform_nmf_reduction(normalized_mx, k, verbose, seed)
    variance_explained <- NULL # NMF doesn't have a clear variance interpretation

    # 6b. Post-processing for NMF: Use as filter -----
    # NMF reconstruction values are used to identify significant cells,
    # but original values are preserved (no distortion)

    recon <- nmf_result$reconstructed

    # Determine filter quantile
    actual_quantile <- filter_quantile
    if (identical(filter_quantile, "auto")) {
      actual_quantile <- select_filter_quantile_auto(recon, verbose)
    }

    # Calculate threshold from reconstruction values
    nonzero_recon <- recon[recon > 0]
    if (length(nonzero_recon) > 0) {
      # threshold is the value below which we discard
      # (1 - actual_quantile) gives the proportion to discard
      filter_threshold <- quantile(nonzero_recon, probs = 1 - actual_quantile)

      # Apply filter: keep original values where reconstruction is significant
      reduced_mx <- core_mx # Use original counts (no distortion)
      reduced_mx[recon < filter_threshold] <- 0

      if (verbose) {
        n_cells_kept <- sum(recon >= filter_threshold)
        n_cells_total <- sum(recon > 0)
        message(sprintf(
          "NMF filter: keeping %d/%d cells (%.1f%%) with reconstruction >= %.4f",
          n_cells_kept,
          n_cells_total,
          100 * actual_quantile,
          filter_threshold
        ))
      }
    } else {
      warning(
        "NMF reconstruction produced all zeros; returning original matrix"
      )
      reduced_mx <- core_mx
    }
  }

  # 7. Apply threshold if requested -----
  threshold_value <- NULL
  if (!is.null(threshold)) {
    threshold_result <- apply_threshold(
      reduced_mx,
      threshold,
      threshold_type,
      verbose
    )
    reduced_mx <- threshold_result$matrix
    threshold_value <- threshold_result$threshold_value
  }

  # 8. Reconstruct full matrix with totals -----
  row_totals <- rowSums(reduced_mx)
  col_totals <- colSums(reduced_mx)
  grand_total <- sum(reduced_mx)

  final_mx <- matrix(0, nrow = n + 1, ncol = n + 1)
  final_mx[1:n, 1:n] <- reduced_mx
  final_mx[1:n, n + 1] <- row_totals
  final_mx[n + 1, 1:n] <- col_totals
  final_mx[n + 1, n + 1] <- grand_total

  # Restore names
  final_names <- c(original_names, "Total")
  rownames(final_mx) <- final_names
  colnames(final_mx) <- final_names

  # 9. Create output object -----
  class(final_mx) <- c("density_reduced", "matrix", "array")
  attr(final_mx, "method") <- method
  attr(final_mx, "normalization") <- normalization
  attr(final_mx, "k") <- k
  attr(final_mx, "variance_explained") <- variance_explained
  attr(final_mx, "threshold_applied") <- threshold_value
  attr(final_mx, "original_dims") <- c(n, n)
  attr(final_mx, "original_total") <- original_total
  attr(final_mx, "reduced_total") <- grand_total
  # Store filter_quantile for both SVD and NMF methods (both use filter approach)
  attr(final_mx, "filter_quantile") <- actual_quantile

  if (verbose) {
    message(sprintf(
      "Reduction complete: %d -> %d observations (%.1f%% retained)",
      original_total,
      grand_total,
      100 * grand_total / original_total
    ))
  }

  return(final_mx)
}

# 2. Internal Helper Functions -----

#' Calculate Positive Pointwise Mutual Information (PPMI)
#'
#' Transforms a mobility matrix using PPMI to emphasize mobility above chance levels.
#'
#' @param mx A mobility matrix (core, without totals)
#' @return PPMI-transformed matrix
#' @keywords internal
calculate_ppmi <- function(mx) {
  # Convert to probabilities
  total <- sum(mx)
  if (total == 0) {
    warning("Matrix sum is zero; returning zero matrix")
    return(mx)
  }

  p_xy <- mx / total
  p_x <- rowSums(mx) / total # P(origin)
  p_y <- colSums(mx) / total # P(destination)

  # Calculate PMI: log2(P(x,y) / (P(x) * P(y)))
  # Avoid division by zero
  expected <- outer(p_x, p_y)
  pmi <- matrix(0, nrow = nrow(mx), ncol = ncol(mx))

  nonzero <- expected > 0 & p_xy > 0
  pmi[nonzero] <- log2(p_xy[nonzero] / expected[nonzero])

  # PPMI: max(0, PMI)
  ppmi <- pmax(pmi, 0)

  return(ppmi)
}

#' Calculate Pearson Residuals
#'
#' Computes Pearson residuals to standardize for marginal distribution effects.
#'
#' @param mx A mobility matrix (core, without totals)
#' @return Matrix of Pearson residuals
#' @keywords internal
pearson_residuals <- function(mx) {
  total <- sum(mx)
  if (total == 0) {
    warning("Matrix sum is zero; returning zero matrix")
    return(mx)
  }

  row_totals <- rowSums(mx)
  col_totals <- colSums(mx)

  # Expected values under independence
  expected <- outer(row_totals, col_totals) / total

  # Pearson residuals: (observed - expected) / sqrt(expected)
  residuals <- matrix(0, nrow = nrow(mx), ncol = ncol(mx))
  nonzero_expected <- expected > 0
  residuals[nonzero_expected] <- (mx[nonzero_expected] -
    expected[nonzero_expected]) /
    sqrt(expected[nonzero_expected])

  return(residuals)
}

#' Automatic Component Selection
#'
#' Selects the number of components k based on variance explained and elbow detection.
#'
#' @param mx Normalized matrix
#' @param variance_target Target cumulative variance (0-1)
#' @param verbose Print progress messages
#' @return List with k and variance_explained
#' @keywords internal
select_k_auto <- function(mx, variance_target, verbose = FALSE) {
  # Compute SVD (or partial SVD if matrix is large)
  n <- min(nrow(mx), ncol(mx))
  max_k <- n - 1

  if (verbose) {
    message("Computing singular values for automatic k selection...")
  }

  # Use irlba for large matrices if available
  if (n > 50 && requireNamespace("irlba", quietly = TRUE)) {
    # Compute enough singular values to reach variance target
    # Start with estimate and expand if needed
    k_estimate <- min(ceiling(n * 0.5), max_k)
    svd_result <- irlba::irlba(mx, nv = k_estimate)
    d <- svd_result$d
  } else {
    svd_result <- svd(mx)
    d <- svd_result$d
  }

  # Calculate cumulative variance explained
  total_var <- sum(d^2)
  cum_var <- cumsum(d^2) / total_var

  # Find k achieving variance_target
  k_variance <- which(cum_var >= variance_target)[1]
  if (is.na(k_variance)) {
    k_variance <- length(d)
  }

  # Also compute elbow point
  k_elbow <- detect_elbow(d^2 / total_var)

  # Use the larger of the two (more conservative)
  k <- max(k_variance, k_elbow)

  # Apply reasonable bounds
  k <- max(k, 5) # At least 5 components
  k <- min(k, max_k, 50) # Cap at 50 or matrix limit

  variance_explained <- cum_var[min(k, length(cum_var))]

  if (verbose) {
    message(sprintf(
      "  Variance target (%.0f%%): k = %d",
      variance_target * 100,
      k_variance
    ))
    message(sprintf("  Elbow detection: k = %d", k_elbow))
    message(sprintf(
      "  Selected k = %d (%.1f%% variance explained)",
      k,
      variance_explained * 100
    ))
  }

  return(list(k = k, variance_explained = variance_explained))
}

#' Detect Elbow Point in Scree Data
#'
#' Uses the maximum distance method to find the elbow point.
#'
#' @param variance_per_component Variance explained per component
#' @return Integer index of elbow point
#' @keywords internal
detect_elbow <- function(variance_per_component) {
  n <- length(variance_per_component)
  if (n < 3) {
    return(1)
  }

  # Create cumulative variance
  cum_var <- cumsum(variance_per_component)

  # Line from first to last point
  x <- 1:n
  y <- cum_var

  # Normalize to [0,1] range
  x_norm <- (x - 1) / (n - 1)
  y_norm <- (y - y[1]) / (y[n] - y[1])

  # Distance from each point to the line connecting first and last
  # Line: y = x (in normalized coordinates)
  distances <- abs(y_norm - x_norm) / sqrt(2)

  # Elbow is point with maximum distance
  elbow <- which.max(distances)

  return(elbow)
}

#' Automatic Filter Quantile Selection for NMF
#'
#' Selects the filter quantile for NMF based on elbow detection on sorted
#' reconstruction values. The elbow point marks where "signal" transitions
#' to "noise" - cells beyond this point contribute little to the overall
#' structure captured by NMF.
#'
#' @param reconstruction NMF reconstruction matrix (W * H)
#' @param verbose Print progress messages
#' @return Numeric quantile value (proportion to keep, e.g., 0.75 = keep top 75%)
#' @keywords internal
select_filter_quantile_auto <- function(reconstruction, verbose = FALSE) {
  # Get non-zero values sorted descending
  nonzero <- reconstruction[reconstruction > 0]

  if (length(nonzero) < 10) {
    if (verbose) {
      message("Too few non-zero values for elbow detection, using default 0.5")
    }
    return(0.5)
  }

  sorted_vals <- sort(nonzero, decreasing = TRUE)
  n <- length(sorted_vals)

  # Calculate contribution of each cell (sorted from highest to lowest)
  total_sum <- sum(sorted_vals)
  contributions <- sorted_vals / total_sum

  # Use elbow detection on contributions
  # This finds where adding more cells gives diminishing returns
  elbow_idx <- detect_elbow(contributions)

  # Convert to quantile: elbow_idx/n gives proportion to KEEP
  quantile_keep <- elbow_idx / n

  # Apply reasonable bounds (keep at least 25%, at most 90%)
  quantile_keep <- max(quantile_keep, 0.25)
  quantile_keep <- min(quantile_keep, 0.90)

  if (verbose) {
    message(sprintf(
      "Auto quantile selection: keep top %.1f%% of cells (elbow at %d/%d)",
      quantile_keep * 100,
      elbow_idx,
      n
    ))
  }

  return(quantile_keep)
}

#' Perform SVD Reduction
#'
#' Applies truncated SVD and reconstructs the matrix.
#'
#' @param mx Normalized matrix
#' @param k Number of components
#' @param verbose Print progress
#' @return Reconstructed matrix
#' @keywords internal
perform_svd_reduction <- function(mx, k, verbose = FALSE) {
  if (verbose) {
    message(sprintf("Performing SVD with k = %d components...", k))
  }

  svd_result <- perform_svd(mx, k)

  # Reconstruct: U_k * diag(d_k) * V_k^T
  reconstructed <- svd_result$u[, 1:k, drop = FALSE] %*%
    diag(svd_result$d[1:k], nrow = k, ncol = k) %*%
    t(svd_result$v[, 1:k, drop = FALSE])

  return(reconstructed)
}

#' Perform SVD (with optional irlba)
#'
#' @param mx Matrix to decompose
#' @param k Number of components
#' @return SVD result list
#' @keywords internal
perform_svd <- function(mx, k) {
  # Use irlba for efficiency on large matrices
  if (
    min(nrow(mx), ncol(mx)) > 50 && requireNamespace("irlba", quietly = TRUE)
  ) {
    return(irlba::irlba(mx, nv = k))
  } else {
    svd_full <- svd(mx)
    return(list(
      u = svd_full$u[, 1:k, drop = FALSE],
      d = svd_full$d[1:k],
      v = svd_full$v[, 1:k, drop = FALSE]
    ))
  }
}

#' Perform NMF Reduction
#'
#' Applies Non-negative Matrix Factorization and returns both reconstruction
#' and original matrix for use as a filter.
#'
#' @param mx Input matrix (should be non-negative)
#' @param k Number of components
#' @param verbose Print progress
#' @param seed Random seed
#' @return List with reconstructed matrix and original (non-negative) matrix
#' @keywords internal
perform_nmf_reduction <- function(mx, k, verbose = FALSE, seed = NULL) {
  if (!requireNamespace("RcppML", quietly = TRUE)) {
    stop(
      "Package 'RcppML' is required for NMF. Install with: install.packages('RcppML')"
    )
  }

  if (verbose) {
    message(sprintf("Performing NMF with k = %d components...", k))
  }

  # NMF requires non-negative input
  mx_nn <- pmax(mx, 0)

  # Set seed if provided
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Perform NMF using RcppML
  nmf_result <- RcppML::nmf(mx_nn, k = k, verbose = FALSE)

  # Reconstruct: W * H
  reconstructed <- nmf_result$w %*% nmf_result$h

  # Return both reconstruction (for filtering) and original (for values)
  return(list(
    reconstructed = reconstructed,
    original = mx_nn
  ))
}

#' Apply Threshold to Matrix
#'
#' Sets values below threshold to zero.
#'
#' @param mx Matrix to threshold
#' @param threshold Threshold specification
#' @param threshold_type Type of threshold
#' @param verbose Print progress
#' @return List with matrix and threshold_value
#' @keywords internal
apply_threshold <- function(mx, threshold, threshold_type, verbose = FALSE) {
  # Calculate threshold value
  if (identical(threshold, "auto")) {
    # Auto: mean + 1 SD
    threshold_value <- mean(mx) + sd(mx)
  } else if (is.numeric(threshold)) {
    if (threshold_type == "sd") {
      threshold_value <- mean(mx) + threshold * sd(mx)
    } else if (threshold_type == "percentile") {
      threshold_value <- quantile(mx, probs = threshold / 100)
    }
  } else {
    stop("Invalid threshold specification")
  }

  # Apply threshold
  mx_thresholded <- mx
  mx_thresholded[mx < threshold_value] <- 0

  if (verbose) {
    n_zeroed <- sum(mx > 0 & mx < threshold_value)
    message(sprintf(
      "Threshold %.2f applied: %d cells set to zero",
      threshold_value,
      n_zeroed
    ))
  }

  return(list(matrix = mx_thresholded, threshold_value = threshold_value))
}

# 3. S3 Methods -----

#' Print Method for density_reduced Objects
#'
#' @param x A density_reduced object
#' @param ... Additional arguments (ignored)
#' @return Invisibly returns the object
#' @export
print.density_reduced <- function(x, ...) {
  cat("Density-Reduced Mobility Matrix\n")
  cat("================================\n")

  dims <- attr(x, "original_dims")
  cat(sprintf("Dimensions: %d x %d categories\n", dims[1], dims[2]))

  cat(sprintf("Method: %s\n", attr(x, "method")))
  cat(sprintf("Normalization: %s\n", attr(x, "normalization")))
  cat(sprintf("Components (k): %d\n", attr(x, "k")))

  var_exp <- attr(x, "variance_explained")
  if (!is.null(var_exp)) {
    cat(sprintf("Variance explained: %.1f%%\n", var_exp * 100))
  }

  filt_q <- attr(x, "filter_quantile")
  if (!is.null(filt_q) && !is.na(filt_q)) {
    cat(sprintf(
      "Filter quantile: %.1f%% (keeps top cells by reconstruction)\n",
      filt_q * 100
    ))
  }

  threshold <- attr(x, "threshold_applied")
  if (!is.null(threshold)) {
    cat(sprintf("Threshold applied: %.2f\n", threshold))
  }

  orig_total <- attr(x, "original_total")
  red_total <- attr(x, "reduced_total")
  cat(sprintf(
    "Observations: %d -> %d (%.1f%% retained)\n",
    orig_total,
    red_total,
    100 * red_total / orig_total
  ))

  cat("\nCore matrix preview (first 5x5):\n")
  n <- min(5, nrow(x) - 1)
  preview <- unclass(x)[1:n, 1:n]
  print(preview)

  invisible(x)
}

# 4. Visualization Functions -----

#' Scree Plot for Component Selection
#'
#' Creates a ggplot2 visualization of variance explained per component,
#' useful for selecting the number of components (k) for dimensionality reduction.
#'
#' @param mx A mobility matrix with totals row/column
#' @param max_components Maximum number of components to display. Default is 50.
#' @param variance_target Optional numeric (0-1) to show target variance line.
#'   Default is 0.75.
#' @param show_elbow Logical indicating whether to highlight the elbow point.
#'   Default is TRUE.
#' @param title Optional plot title
#'
#' @return A ggplot2 object
#'
#' @examples
#' # Generate test data
#' data <- generate_mobility_data(n_classes = 30, seed = 123)
#'
#' # Basic scree plot
#' plot_scree(data)
#'
#' # With custom variance target
#' plot_scree(data, variance_target = 0.80)
#'
#' # Without elbow detection
#' plot_scree(data, show_elbow = FALSE)
#'
#' @seealso \code{\link{reduce_density}} for using the selected k
#'
#' @export
plot_scree <- function(
  mx,
  max_components = 50,
  variance_target = 0.75,
  show_elbow = TRUE,
  title = NULL
) {
  # Extract core matrix
  n <- nrow(mx) - 1
  core_mx <- mx[1:n, 1:n]

  # Compute SVD
  if (n > max_components && requireNamespace("irlba", quietly = TRUE)) {
    svd_result <- irlba::irlba(core_mx, nv = min(max_components, n - 1))
    d <- svd_result$d
  } else {
    svd_result <- svd(core_mx)
    d <- svd_result$d[1:min(max_components, length(svd_result$d))]
  }

  # Calculate variance metrics
  total_var <- sum(svd(core_mx)$d^2) # Need full SVD for total variance
  var_per_comp <- d^2 / total_var
  cum_var <- cumsum(var_per_comp)

  # Create data frame for plotting
  n_comp <- length(d)
  plot_data <- data.frame(
    component = 1:n_comp,
    variance = var_per_comp,
    cumulative = cum_var
  )

  # Find k for variance target
  k_target <- which(cum_var >= variance_target)[1]
  if (is.na(k_target)) {
    k_target <- n_comp
  }

  # Detect elbow
  k_elbow <- detect_elbow(var_per_comp)

  # Create plot
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$component)) +
    ggplot2::geom_bar(
      ggplot2::aes(y = .data$variance),
      stat = "identity",
      fill = "steelblue",
      alpha = 0.7
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = .data$cumulative),
      color = "darkred",
      linewidth = 1.2
    ) +
    ggplot2::geom_point(
      ggplot2::aes(y = .data$cumulative),
      color = "darkred",
      size = 2
    ) +
    ggplot2::scale_y_continuous(
      name = "Variance Explained",
      limits = c(0, 1),
      labels = scales::percent
    ) +
    ggplot2::labs(
      x = "Component",
      title = if (is.null(title)) {
        "Scree Plot: Variance Explained by Component"
      } else {
        title
      },
      subtitle = sprintf(
        "Matrix: %d x %d | Showing first %d components",
        n,
        n,
        n_comp
      )
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      panel.grid.minor = ggplot2::element_blank()
    )

  # Add variance target line
  if (!is.null(variance_target)) {
    p <- p +
      ggplot2::geom_hline(
        yintercept = variance_target,
        linetype = "dashed",
        color = "gray40"
      ) +
      ggplot2::geom_vline(
        xintercept = k_target,
        linetype = "dashed",
        color = "darkred",
        alpha = 0.7
      ) +
      ggplot2::annotate(
        "text",
        x = k_target + 1,
        y = variance_target + 0.05,
        label = sprintf("k=%d (%.0f%%)", k_target, variance_target * 100),
        hjust = 0,
        color = "darkred",
        size = 3.5
      )
  }

  # Add elbow point
  if (show_elbow && k_elbow != k_target) {
    p <- p +
      ggplot2::geom_vline(
        xintercept = k_elbow,
        linetype = "dotted",
        color = "darkgreen",
        alpha = 0.7
      ) +
      ggplot2::annotate(
        "text",
        x = k_elbow + 1,
        y = cum_var[k_elbow] - 0.05,
        label = sprintf("elbow=%d", k_elbow),
        hjust = 0,
        color = "darkgreen",
        size = 3.5
      )
  }

  return(p)
}
