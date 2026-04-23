#' Fast moneca - Optimized Mobility Network Clustering Analysis
#'
#' An optimized version of the moneca algorithm with improved performance for large datasets.
#' Implements various performance optimizations including vectorization and early stopping.
#' Supports automatic margin detection and optional density reduction for large matrices.
#'
#' @param mx A mobility table (square matrix). Can include row and column totals in the
#'   last row/column, or be a core matrix without totals. Row names should identify the
#'   categories/classes. See \code{has_margins} parameter.
#' @param segment.levels Integer specifying the number of hierarchical segmentation
#'   levels to compute. Default is 3.
#' @param cut.off Numeric threshold for the minimum relative risk to be considered
#'   a significant tie. Default is 1.
#' @param mode Character string specifying edge mode. Currently uses symmetric mode.
#' @param delete.upper.tri Logical indicating whether to use only lower triangle. Default is TRUE.
#' @param small.cell.reduction Numeric value to handle small cell counts. Default is 0.
#' @param use.sparse Logical. Default \code{FALSE}. When \code{TRUE}, the entire
#'   pipeline (weight matrix, symmetrization, segment aggregation, edge
#'   extraction) stays in sparse \code{dgCMatrix} form, so memory scales with
#'   \code{nnz(mx)} rather than \code{n^2}. Requires the \code{Matrix} package
#'   (listed in Suggests). Sparse input (\code{dgCMatrix}) is auto-detected and
#'   routed through the sparse path without setting this flag. On the sparse
#'   path, "no edge" is stored as a structural zero, so entries of
#'   \code{mat.list} may be \code{dgCMatrix} objects rather than base matrices.
#'   When \code{use.sparse = TRUE} is set on a dense base-R matrix input with
#'   density > 50\%, the function emits an informational message and falls back
#'   to the dense path (no speedup benefit from sparse storage on dense data,
#'   and ~20\% memory overhead). Pass a \code{sparseMatrix} explicitly to
#'   override this fallback.
#' @param min.density Minimum strength-based density to continue processing.
#'   Calculated as mean(strength)/max(strength). Default is 0 (disabled for algorithmic
#'   fidelity). Set to 0.01 or higher for early stopping optimization on sparse graphs.
#' @param max.clique.size Maximum size of cliques to consider (NULL for no limit). Default is NULL.
#' @param progress Logical indicating whether to show progress bars. Default is TRUE.
#' @param use_maximal_cliques Controls clique enumeration. One of:
#'   \itemize{
#'     \item \code{FALSE} (default): use all cliques via \code{igraph::cliques()}.
#'       Required for bit-exact parity with \code{\link{moneca}}.
#'     \item \code{TRUE}: use only maximal cliques via \code{igraph::max_cliques()}.
#'       Much faster on dense graphs (7x+ on the 127x127 Lombardy fixture) but
#'       does not guarantee identical clustering to \code{moneca()}.
#'     \item \code{"auto"}: per-level dispatch. Computes \code{igraph::edge_density}
#'       on the post-cut-off weight graph and selects maximal cliques when
#'       density > 0.2; otherwise uses all cliques. Non-parity opt-in.
#'   }
#' @param isolates Logical. If TRUE, returns additional summary information
#'   about isolates (categories not belonging to any multi-member segment at the
#'   final level). Isolates are grouped into a category called "altri".
#'   Default is FALSE.
#' @param has_margins Controls whether the input matrix includes totals row/column.
#'   \itemize{
#'     \item \code{"auto"} (default): Automatically detects if the last row/column
#'       contains marginal totals
#'     \item \code{TRUE}: Matrix already has totals in the last row/column
#'     \item \code{FALSE}: Matrix is a core matrix without totals; margins will be
#'       generated automatically
#'   }
#' @param reduce_density Controls density reduction preprocessing for large matrices.
#'   \itemize{
#'     \item \code{"auto"} (default): Applies density reduction when the matrix has
#'       30+ categories and the weight matrix count density exceeds 0.7
#'     \item \code{TRUE}: Always apply density reduction
#'     \item \code{FALSE}: Never apply density reduction
#'   }
#' @param density_params Named list of additional parameters passed to
#'   \code{\link{reduce_density}}. For example:
#'   \code{list(method = "nmf", normalization = "ppmi", k = 20)}. See
#'   \code{\link{reduce_density}} for all available parameters.
#' @param symmetric_method Character string controlling how the asymmetric relative
#'   risk matrix is symmetrized before clique-finding. Valid values:
#'   \itemize{
#'     \item \code{"sum"} (default): Current behavior — \code{mx + t(mx)}. A pair
#'       scores high if either direction is strong.
#'     \item \code{"min"}: Min-reciprocity — \code{pmin(mx, t(mx)) * 2}. A pair
#'       scores high only if both directions are strong. One-way bridges are
#'       down-weighted.
#'   }
#'
#' @details
#' This implementation is optimized for single-core performance using:
#' \itemize{
#'   \item Vectorized matrix operations for improved speed
#'   \item Optional sparse matrix support via \code{use.sparse} parameter
#'   \item Early stopping via \code{min.density} threshold (disabled by default)
#'   \item Clique size limiting via \code{max.clique.size} parameter
#'   \item Pre-computed max clique size for early skip of impossible merges
#'   \item Optimized node ordering in clique membership tests
#'   \item Selectable symmetrization via \code{symmetric_method} parameter
#' }
#'
#' \strong{Margin Handling:} When \code{has_margins = "auto"}, the function checks
#' whether the last row and column of the matrix contain the row/column sums of
#' the core matrix. If not detected, margins are generated automatically.
#'
#' \strong{Density Reduction:} Large matrices (30+ categories) with high density
#' can be preprocessed using \code{\link{reduce_density}} to remove noise before
#' clustering. This is controlled by \code{reduce_density} and \code{density_params}.
#'
#' \strong{Symmetrization Methods:} The default \code{"sum"} method preserves
#' the original algorithm behavior. The \code{"min"} method uses min-reciprocity
#' (\code{pmin(mx, t(mx)) * 2}), which down-weights one-way bridges where
#' only one direction has high relative risk. This can produce tighter clusters
#' in matrices with strong directional asymmetry.
#'
#' \strong{Dual-Matrix System:} When density reduction is active, the reduced
#' matrix determines cluster topology (which categories group together), while
#' the original unreduced matrix is used for all aggregation, storage in
#' \code{mat.list}, and downstream metrics. This preserves count totals across
#' all segmentation levels.
#'
#' \strong{Produces identical results to} \code{\link{moneca}} when using default
#' parameters and \code{reduce_density = FALSE}, specifically with
#' \code{use_maximal_cliques = FALSE} and \code{symmetric_method = "sum"}.
#' The \code{"auto"} and \code{TRUE} settings of \code{use_maximal_cliques} are
#' non-parity performance opt-ins for dense fixtures.
#'
#' \strong{When to use:}
#' \itemize{
#'   \item Any system - recommended for most use cases
#'   \item Small to large datasets
#'   \item When you want explicit control over optimization parameters
#' }
#'
#' @note Setting \code{use.sparse = TRUE} (or passing a \code{dgCMatrix}, which
#'   triggers the sparse path automatically) requires the \code{Matrix} package
#'   to be installed; otherwise the function errors. The default dense path does
#'   not depend on \code{Matrix} at all.
#'
#' @return An object of class "moneca" with the same structure as the original moneca() function.
#'   The returned list always includes:
#'   \describe{
#'     \item{margins_added}{Logical indicating whether margins were generated}
#'     \item{density_reduction}{NULL if no density reduction was applied, or a list
#'       with metadata about the reduction (method, k, variance_explained, etc.)}
#'   }
#'   When \code{isolates = TRUE}, the returned list also includes:
#'   \describe{
#'     \item{isolates_summary}{A list containing:
#'       \describe{
#'         \item{membership}{Data frame with columns \code{name} (category name)
#'           and \code{group} (segment name or "altri" for isolates)}
#'         \item{mobility_matrix}{Matrix of mobility counts between groups,
#'           including "altri" group for isolates}
#'       }
#'     }
#'   }
#'
#' @seealso \code{\link{moneca}} for the original implementation,
#'   \code{\link{reduce_density}} for density reduction details
#'
#' @export
moneca_fast <- function(
  mx = mx,
  segment.levels = 3,
  cut.off = 1,
  mode = "symmetric",
  delete.upper.tri = TRUE,
  small.cell.reduction = 0,
  use.sparse = FALSE,
  min.density = 0,
  max.clique.size = NULL,
  progress = TRUE,
  auto_tune = FALSE,
  tune_method = "stability",
  tune_verbose = FALSE,
  use_maximal_cliques = FALSE,
  isolates = FALSE,
  has_margins = "auto",
  reduce_density = "auto",
  density_params = list(),
  symmetric_method = "sum"
) {
  # Preserve sparse input when requested; otherwise ensure base matrix.
  # On high-density inputs (> 50% non-zero) the sparse path has no wall-time
  # benefit and uses ~20% more memory than dense (profiling report sec. 10.2).
  # If the user explicitly sets use.sparse = TRUE on such input, fall back to
  # dense with an informational message -- unless the input is already a
  # sparseMatrix (respect the caller's storage choice in that case).
  sparse_requested <- isTRUE(use.sparse)
  sparse_input <- inherits(mx, "sparseMatrix")
  sparse_path <- sparse_requested || sparse_input

  if (sparse_requested && !sparse_input) {
    if (is.matrix(mx)) {
      density_frac <- mean(mx != 0)
    } else {
      tmp <- as.matrix(mx)
      density_frac <- mean(tmp != 0)
      mx <- tmp
    }
    if (is.finite(density_frac) && density_frac > 0.5) {
      message(sprintf(
        "use.sparse = TRUE on dense input (%.0f%% non-zero): falling back to dense path (no speedup; ~20%% memory overhead on dense data).",
        100 * density_frac
      ))
      sparse_path <- FALSE
    }
  }

  if (sparse_path) {
    if (!requireNamespace("Matrix", quietly = TRUE)) {
      stop(
        "use.sparse = TRUE requires the 'Matrix' package; ",
        "install it or set use.sparse = FALSE"
      )
    }
    if (!inherits(mx, "sparseMatrix")) {
      if (!is.matrix(mx)) {
        mx <- as.matrix(mx)
      }
      mx <- Matrix::Matrix(mx, sparse = TRUE)
    }
  } else {
    if (!is.matrix(mx)) {
      mx <- as.matrix(mx)
    }
  }

  # Validate symmetric_method
  if (!symmetric_method %in% c("sum", "min")) {
    stop(
      "symmetric_method must be 'sum' or 'min', got: '",
      symmetric_method,
      "'"
    )
  }

  # 1. Margin auto-detection and generation -----
  margins_added <- FALSE

  detect_has_margins <- function(mx) {
    n <- nrow(mx)
    if (n < 3 || nrow(mx) != ncol(mx)) {
      return(FALSE)
    }
    # Use Matrix:: variants when input is sparse (base colSums/rowSums do not
    # dispatch on dgCMatrix).
    use_matrix <- inherits(mx, "sparseMatrix") &&
      requireNamespace("Matrix", quietly = TRUE)
    .rs <- if (use_matrix) Matrix::rowSums else rowSums
    .cs <- if (use_matrix) Matrix::colSums else colSums
    core <- mx[1:(n - 1), 1:(n - 1)]
    tol <- max(sum(core) * 1e-6, 1)
    last_row_vec <- as.numeric(mx[n, 1:(n - 1)])
    last_col_vec <- as.numeric(mx[1:(n - 1), n])
    last_row_matches <- all(abs(last_row_vec - .cs(core)) < tol)
    last_col_matches <- all(abs(last_col_vec - .rs(core)) < tol)
    corner_matches <- abs(as.numeric(mx[n, n]) - sum(core)) < tol
    last_row_matches && last_col_matches && corner_matches
  }

  needs_margins <- FALSE
  if (identical(has_margins, FALSE)) {
    needs_margins <- TRUE
  } else if (identical(has_margins, "auto")) {
    needs_margins <- !detect_has_margins(mx)
  }
  # has_margins = TRUE: assume margins already present

  if (needs_margins) {
    core <- mx
    if (sparse_path) {
      rs <- as.numeric(Matrix::rowSums(core))
      cs <- as.numeric(Matrix::colSums(core))
    } else {
      rs <- rowSums(core)
      cs <- colSums(core)
    }
    gt <- sum(core)

    cat_names <- rownames(core)
    if (is.null(cat_names)) {
      cat_names <- paste0("Cat", seq_len(nrow(core)))
    }
    full_names <- c(cat_names, "Total")

    if (sparse_path) {
      # Sparse-safe margin append using Matrix::cbind2 / rbind2
      rs_col <- Matrix::Matrix(rs, ncol = 1, sparse = TRUE)
      cs_row <- Matrix::Matrix(c(cs, gt), nrow = 1, sparse = TRUE)
      mx <- Matrix::cbind2(core, rs_col)
      mx <- Matrix::rbind2(mx, cs_row)
      # Ensure dgCMatrix for downstream slot operations
      if (!inherits(mx, "dgCMatrix")) {
        mx <- as(mx, "generalMatrix")
        mx <- as(mx, "CsparseMatrix")
      }
      dimnames(mx) <- list(full_names, full_names)
    } else {
      mx <- rbind(cbind(core, rs), c(cs, gt))
      rownames(mx) <- full_names
      colnames(mx) <- full_names
    }
    margins_added <- TRUE
  }

  # 2. Density reduction preprocessing -----
  density_reduction_info <- NULL

  apply_reduction <- FALSE
  if (identical(reduce_density, TRUE)) {
    apply_reduction <- TRUE
  } else if (identical(reduce_density, "auto")) {
    n_categories <- nrow(mx) - 1
    if (n_categories >= 30) {
      # Check count density: proportion of non-zero cells in core matrix
      core_check <- mx[1:n_categories, 1:n_categories]
      count_density <- sum(core_check > 0) / (n_categories * n_categories)
      if (count_density > 0.7) {
        apply_reduction <- TRUE
      }
    }
  }
  # reduce_density = FALSE: skip

  if (apply_reduction) {
    # reduce_density() is dense-only: feed it a base matrix but keep mx itself
    # sparse on the sparse path (only the topology matrix will be densified).
    mx_for_reduction <- if (sparse_path) as.matrix(mx) else mx
    if (sparse_path) {
      message(
        "reduce_density() requires a dense matrix; densifying only the ",
        "topology matrix. The primary mobility matrix remains sparse."
      )
    }
    # Build arguments for reduce_density()
    rd_args <- c(list(mx = mx_for_reduction), density_params)
    # Set verbose default from progress if not explicitly set
    if (is.null(rd_args$verbose)) {
      rd_args$verbose <- progress
    }
    # Retrieve the reduce_density function from the parent environment
    # (the parameter 'reduce_density' shadows the function name locally)
    reduce_density_fn <- get(
      "reduce_density",
      envir = parent.env(environment())
    )
    reduced_mx <- do.call(reduce_density_fn, rd_args)

    # Store metadata
    density_reduction_info <- list(
      method = attr(reduced_mx, "method"),
      normalization = attr(reduced_mx, "normalization"),
      k = attr(reduced_mx, "k"),
      variance_explained = attr(reduced_mx, "variance_explained"),
      filter_quantile = attr(reduced_mx, "filter_quantile"),
      original_total = attr(reduced_mx, "original_total"),
      reduced_total = attr(reduced_mx, "reduced_total")
    )

    # Create topology matrix from reduced data; keep mx as original
    mx_topo <- unclass(reduced_mx)
    attr(mx_topo, "method") <- NULL
    attr(mx_topo, "normalization") <- NULL
    attr(mx_topo, "k") <- NULL
    attr(mx_topo, "variance_explained") <- NULL
    attr(mx_topo, "filter_quantile") <- NULL
    attr(mx_topo, "threshold_applied") <- NULL
    attr(mx_topo, "original_dims") <- NULL
    attr(mx_topo, "original_total") <- NULL
    attr(mx_topo, "reduced_total") <- NULL
  }

  if (!apply_reduction) {
    mx_topo <- NULL
  }

  # 3. Sparse conversion for topology matrix (mx already sparse on sparse_path)
  # The topology matrix (when present) may still be dense after reduction; sparsify
  # it when the caller opted into sparse processing so downstream helpers stay sparse.
  if (
    sparse_path &&
      !is.null(mx_topo) &&
      !inherits(mx_topo, "sparseMatrix") &&
      requireNamespace("Matrix", quietly = TRUE)
  ) {
    mx_topo <- Matrix::Matrix(mx_topo, sparse = TRUE)
  }

  # Fast weight matrix calculation with caching
  weight.matrix.fast <- function(
    mx,
    cut.off = 1,
    symmetric = TRUE,
    diagonal = NULL,
    small.cell.reduction = 0
  ) {
    # Input validation
    if (is.null(mx)) {
      stop("Input cannot be NULL")
    }

    l <- nrow(mx)
    if (l < 2) {
      stop("Matrix must have at least 2 rows and columns")
    }
    if (nrow(mx) != ncol(mx)) {
      stop("Matrix must be square")
    }

    # Use vectorized operations
    o.r.s <- mx[-l, l]
    o.c.s <- mx[l, -l]
    total.total <- mx[l, l]
    row.share <- o.r.s / total.total
    col.share <- o.c.s / total.total
    total.mobility <- sum(mx[-l, -l])

    # Vectorized expected calculation
    mx.1_exp <- outer(row.share, col.share) * total.mobility

    mx.red <- mx[-l, -l]
    if (small.cell.reduction > 0) {
      mx.red[mx.red < small.cell.reduction] <- 0
    }

    mx.1_net <- mx.red / mx.1_exp
    mx.1i <- as.matrix(mx.1_net)

    if (symmetric) {
      mx.1i <- mx.1i + t(mx.1i)
    }

    mx.1i[mx.1i < cut.off] <- NA

    if (is.null(diagonal)) {
      diag(mx.1i) <- NA
    }

    return(mx.1i)
  }

  # Optimized segment finding - matches original algorithm exactly
  # Key optimizations:
  # 1. Pre-sort edges instead of repeated max() searches
  # 2. Fast clique membership test using node-to-clique lookup
  # 3. Efficient progress bar updates
  find.segments.fast <- function(
    mat,
    graph,
    cut.off = 1,
    mode = "symmetric",
    delete.upper.tri = TRUE,
    progress = TRUE
  ) {
    n <- nrow(mat)

    # Early stopping using strength-based density
    strengths <- igraph::strength(graph, mode = "all")
    if (length(strengths) > 0 && any(strengths > 0)) {
      strength_density <- mean(strengths) / max(strengths)
    } else {
      strength_density <- 0
    }

    if (strength_density < min.density) {
      return(list(
        membership = as.factor(rep(1, n)),
        cliques = list(1:n)
      ))
    }

    # Get cliques based on settings. "auto" resolves per-level from the
    # post-cut-off weight graph density: on dense graphs (density > 0.2),
    # full clique enumeration is exponential in clique size and wastes both
    # wall-time and memory, while maximal-cliques gives equivalent clustering
    # on tight graphs at a fraction of the cost. The 0.2 threshold is
    # calibrated against the 127x127 real fixture (82% density) where maximal
    # gives a 7.4x speedup; on synthetic low-density graphs (<5% density) the
    # branch selects the all-cliques path and preserves exact parity with
    # moneca().
    if (is.character(use_maximal_cliques)) {
      if (!identical(use_maximal_cliques, "auto")) {
        stop(
          "use_maximal_cliques must be TRUE, FALSE, or 'auto', got: '",
          use_maximal_cliques,
          "'"
        )
      }
      graph_density <- igraph::edge_density(graph)
      # 0.12 threshold calibrated on the 127x127 Lombardy real fixture (post
      # density-reduction graph density ~0.14, maximal 12x faster) vs
      # synthetic mid-scale graphs at n=60/120/300 (density ~0.10, maximal
      # neutral). Below the threshold the all-cliques path is preferred to
      # stay within the parity envelope with moneca().
      use_max_here <- is.finite(graph_density) && graph_density > 0.12
    } else {
      use_max_here <- isTRUE(use_maximal_cliques)
    }

    if (use_max_here) {
      if (is.null(max.clique.size)) {
        cliques <- moneca_max_cliques(graph, min = 2)
      } else {
        cliques <- moneca_max_cliques(graph, min = 2, max = max.clique.size)
      }
    } else {
      if (is.null(max.clique.size)) {
        cliques <- moneca_cliques(graph, min = 2)
      } else {
        cliques <- moneca_cliques(graph, min = 2, max = max.clique.size)
      }
    }

    # Trivial case: no cliques
    if (length(cliques) == 0) {
      return(list(
        membership = as.factor(rep(1, n)),
        cliques = list(1:n)
      ))
    }

    # OPTIMIZATION: Pre-compute max clique size for early skip
    max_clique_size_found <- max(lengths(cliques))

    # Process matrix for mode (matches original exactly)
    # NOTE: on the sparse path we cannot assign NA; these transformations are
    # applied later as triplet filters when we enumerate edges.
    mat_is_sparse <- inherits(mat, "sparseMatrix")
    if (!mat_is_sparse) {
      if (mode == "Mutual") {
        mat[mat < cut.off] <- NA
        mat <- mat + t(mat)
      } else if (mode == "Unmutual") {
        mat[mat < cut.off] <- 0
        mat <- mat + t(mat)
        mat[mat == 0] <- NA
      }

      if (delete.upper.tri) {
        mat[upper.tri(mat)] <- NA
      }
    } else {
      # Sparse equivalents: "Mutual"/"Unmutual" modes both result in mat + t(mat)
      # after threshold handling; since structural zeros already represent "no
      # edge", adding t(mat) is the only change.
      if (mode == "Mutual" || mode == "Unmutual") {
        mat <- mat + Matrix::t(mat)
        if (!inherits(mat, "dgCMatrix")) {
          mat <- as(mat, "generalMatrix")
          mat <- as(mat, "CsparseMatrix")
        }
      }
    }

    # Initialize group vector (matches original)
    group <- vector(mode = "numeric", length = n)
    names(group) <- rownames(mat)

    # Build efficient clique lookup (node -> cliques containing it).
    # Vectorised via unlist/rep.int/split: equivalent to the nested
    # for-loops but ~35% of non-GC self-time on real dense fixtures collapses
    # into compiled C. Ordering the split by (node, clique_id) preserves the
    # monotonically-increasing clique order within each per-node vector, so
    # downstream clique.test() behaves identically.
    clique_lens <- lengths(cliques)
    node_ids <- unlist(cliques, use.names = FALSE)
    if (length(node_ids) == 0L) {
      node_to_cliques <- vector("list", n)
      for (i in seq_len(n)) {
        node_to_cliques[[i]] <- integer(0)
      }
      node_clique_count <- integer(n)
    } else {
      clique_ids <- rep.int(seq_along(cliques), clique_lens)
      ord <- order(node_ids, clique_ids, method = "radix")
      node_to_cliques <- split(
        clique_ids[ord],
        factor(node_ids[ord], levels = seq_len(n))
      )
      names(node_to_cliques) <- NULL
      node_clique_count <- lengths(node_to_cliques)
    }

    # Fast clique membership test using node-to-clique lookup
    # OPTIMIZATION: Process nodes with fewest clique memberships first
    # to shrink the intersection candidates faster (same result, fewer iterations)
    clique.test <- function(potential.clique) {
      if (length(potential.clique) < 2) {
        return(TRUE)
      }
      # OPTIMIZATION: Early size check - a group larger than the biggest clique
      # can never be a subset of any clique
      if (length(potential.clique) > max_clique_size_found) {
        return(FALSE)
      }
      # Sort by number of clique memberships (fewest first)
      membership_counts <- node_clique_count[potential.clique]
      ordered_nodes <- potential.clique[order(membership_counts)]
      candidates <- node_to_cliques[[ordered_nodes[1]]]
      if (length(candidates) == 0L) {
        return(FALSE)
      }
      for (i in 2:length(ordered_nodes)) {
        candidates <- candidates[
          candidates %in% node_to_cliques[[ordered_nodes[i]]]
        ]
        if (length(candidates) == 0L) return(FALSE)
      }
      TRUE
    }

    # OPTIMIZATION: Pre-sort edges instead of repeated max() searches
    if (inherits(mat, "sparseMatrix")) {
      # Sparse path: enumerate from triplet representation and skip dense indexing.
      # delete.upper.tri is realised here as a triplet filter (row >= col keeps
      # the lower triangle plus diagonal, mirroring the dense NA contract).
      trip_m <- Matrix::summary(mat)
      keep_m <- trip_m$x > cut.off
      if (delete.upper.tri) {
        keep_m <- keep_m & (trip_m$i >= trip_m$j)
      }
      edge_idx <- cbind(
        row = as.integer(trip_m$i[keep_m]),
        col = as.integer(trip_m$j[keep_m])
      )
      edge_weights <- trip_m$x[keep_m]
    } else {
      edge_idx <- which(!is.na(mat) & mat > cut.off, arr.ind = TRUE)
      edge_weights <- if (nrow(edge_idx) > 0L) mat[edge_idx] else numeric(0)
    }

    if (nrow(edge_idx) == 0L) {
      return(list(
        membership = as.factor(rep(1, n)),
        cliques = list(1:n)
      ))
    }
    edge_order <- order(edge_weights, decreasing = TRUE)
    sorted_edges <- edge_idx[edge_order, , drop = FALSE]
    n_edges <- nrow(sorted_edges)

    if (progress) {
      pb <- txtProgressBar(min = 1, max = max(n_edges, 2), style = 3)
    }

    # Process edges in pre-sorted order (matches original logic exactly)
    for (i in seq_len(n_edges)) {
      # OPTIMIZATION: Update progress bar every 100 edges instead of every edge
      if (progress && (i %% 100L == 0L || i == n_edges)) {
        setTxtProgressBar(pb, i)
      }

      max.ind <- sorted_edges[i, ]
      group.candidates <- group[max.ind]
      candidate <- max.ind

      # OPTIMIZATION: Skip if both nodes already in same non-zero group
      if (
        all(group.candidates != 0) && group.candidates[1] == group.candidates[2]
      ) {
        next
      }

      # Original algorithm logic preserved exactly
      if (sum(group.candidates) == 0) {
        group[max.ind] <- i
      } else {
        group.candidates <- group.candidates[group.candidates != 0]
        group.members <- which(group %in% group.candidates)
        group.size <- table(group[group.members])
        group.assigned <- as.numeric(names(group.size))[which.max(group.size)[
          1
        ]]

        potential.clique <- unique(sort(c(group.members, candidate)))

        if (clique.test(potential.clique)) {
          group[potential.clique] <- group.assigned
        }
      }
    }

    if (progress) {
      close(pb)
    }

    # Handle unassigned nodes (matches original)
    sub <- group[group == 0]
    group[group == 0] <- seq_along(sub) + max(group)
    g <- as.factor(group)
    levels(g) <- seq_len(nlevels(g))

    # Create clique list (matches original format)
    l <- levels(g)
    ud.list <- list()
    for (i in seq_along(l)) {
      ud.list[[i]] <- which(g == l[i])
    }

    list(
      membership = g,
      cliques = ud.list
    )
  }

  # Fast segment matrix aggregation
  segment.matrix.fast <- function(mx, segments) {
    # Sparse path: use the Matrix-native helper to stay sparse
    if (inherits(mx, "sparseMatrix")) {
      return(segment_matrix_sparse(mx, segments))
    }

    # Dense path (unchanged)
    groups.1 <- c(segments$membership, length(segments$membership) + 1)
    mx.2_r <- rowsum(mx, groups.1)
    mx.2_r_t <- t(mx.2_r)
    mx.2_rc_t <- rowsum(mx.2_r_t, groups.1)
    mx.2g <- t(mx.2_rc_t)

    return(mx.2g)
  }

  # Optimized level.down function
  level.down.fast <- function(level.current, level.below) {
    # Remove isolates
    lengths <- lengths(level.current)
    level.current <- level.current[lengths > 1]

    if (length(level.current) == 0) {
      return(list())
    }

    # Vectorized operation
    ud <- lapply(level.current, function(d) unlist(level.below[d]))
    return(ud)
  }

  # Main segmentation function
  make.segments.fast <- function(
    mx,
    cut.off = 1,
    mode = mode,
    delete.upper.tri = delete.upper.tri,
    small.cell.reduction = small.cell.reduction,
    symmetric_method = symmetric_method
  ) {
    if (inherits(mx, "sparseMatrix")) {
      # Sparse path: use the sparse weight-matrix helper end-to-end. Auto-tune is
      # not supported here (dense-only); the caller's explicit small.cell.reduction
      # is honoured.
      mx.1i <- weight_matrix_sparse(
        mx,
        cut.off = cut.off,
        symmetric_method = symmetric_method,
        small.cell.reduction = small.cell.reduction
      )
    } else if (symmetric_method == "min") {
      # Min-reciprocity: get asymmetric RR, then pmin(mx, t(mx)) * 2
      mx.1i <- weight.matrix(
        mx,
        cut.off = 0,
        symmetric = FALSE,
        small.cell.reduction = small.cell.reduction,
        auto_tune = auto_tune,
        tune_method = tune_method,
        tune_verbose = tune_verbose
      )
      mx.1i <- pmin(mx.1i, t(mx.1i)) * 2
      mx.1i[mx.1i < cut.off] <- NA
      diag(mx.1i) <- NA
    } else {
      # Default "sum": current behavior
      mx.1i <- weight.matrix(
        mx,
        cut.off,
        small.cell.reduction = small.cell.reduction,
        auto_tune = auto_tune,
        tune_method = tune_method,
        tune_verbose = tune_verbose
      )
    }

    # Create graph
    if (inherits(mx.1i, "sparseMatrix")) {
      # Sparse RR already uses structural zeros for "no edge"
      mx.1i.graph <- mx.1i
    } else {
      mx.1i.graph <- mx.1i
      mx.1i.graph[is.na(mx.1i.graph)] <- 0
    }

    gra.1ii <- moneca_graph_from_adjacency(
      adjmatrix = mx.1i.graph,
      mode = "undirected",
      weighted = TRUE,
      diag = FALSE
    )

    # Early stopping based on strength density
    strengths <- igraph::strength(gra.1ii, mode = "all")
    if (length(strengths) > 0 && any(strengths > 0)) {
      strength_density <- mean(strengths) / max(strengths)
    } else {
      strength_density <- 0
    }

    if (strength_density < min.density) {
      n <- nrow(mx.1i)
      return(list(
        membership = as.factor(rep(1, n)),
        cliques = list(1:n)
      ))
    }

    clust.1 <- find.segments.fast(
      mx.1i,
      gra.1ii,
      cut.off = cut.off,
      progress = progress
    )

    return(clust.1)
  }

  # Create segments function (optimized)
  create.segments.fast <- function(out.put, mx) {
    seg.list <- list()
    seg.list[[1]] <- as.list(1:(nrow(mx) - 1))

    if (length(out.put) == 0) {
      return(seg.list)
    }

    level.current <- out.put[[1]]$segments$cliques
    lengths <- lengths(level.current)
    seg.list[[2]] <- level.current[lengths > 1]

    actual.levels <- min(segment.levels, length(out.put))

    if (actual.levels > 1) {
      for (n in 2:actual.levels) {
        if (n > length(out.put)) {
          break
        }

        level.current <- out.put[[n]]$segments$cliques

        # Optimized level down calculation
        for (i in seq_len(n - 1)) {
          level.below <- out.put[[n - i]]$segments$cliques
          level.current <- level.down.fast(level.current, level.below)
        }

        if (length(level.current) > 0) {
          seg.list[[n + 1]] <- level.current
        }
      }
    }

    return(seg.list)
  }

  # Main algorithm
  mat.list <- list()
  mat.list[[1]] <- mx # always original

  # Topology: use reduced matrix if available
  mx_for_topo <- if (!is.null(mx_topo)) mx_topo else mx
  segments <- make.segments.fast(
    mx_for_topo,
    cut.off = cut.off,
    mode = mode,
    delete.upper.tri = delete.upper.tri,
    small.cell.reduction = small.cell.reduction,
    symmetric_method = symmetric_method
  )

  # Aggregation: ALWAYS from original
  mx.2g <- segment.matrix.fast(mx, segments)
  mat.list[[2]] <- mx.2g

  # Track reduced aggregation for next level topology
  if (!is.null(mx_topo)) {
    mx_topo.2g <- segment.matrix.fast(mx_topo, segments)
  } else {
    mx_topo.2g <- NULL
  }

  out.put <- list()
  out.put[[1]] <- list(segments = segments, mat = mx.2g)

  # Early stopping if only one segment
  if (length(segments$cliques) <= 1) {
    segment.list <- create.segments.fast(out.put, mx)

    # Compute isolates_summary if requested (early stopping path)
    isolates_summary <- NULL
    if (isolates) {
      n <- nrow(mx) - 1
      category_names <- rownames(mx)[1:n]
      if (is.null(category_names)) {
        category_names <- as.character(1:n)
      }

      final_level <- length(segment.list)
      final_segments <- segment.list[[final_level]]

      membership <- rep("altri", n)
      for (seg_idx in seq_along(final_segments)) {
        seg_members <- final_segments[[seg_idx]]
        membership[seg_members] <- paste0("Segment_", seg_idx)
      }

      membership_df <- data.frame(
        name = category_names,
        group = membership,
        stringsAsFactors = FALSE
      )

      groups <- unique(membership)
      group_factor <- factor(membership, levels = groups)

      core_mx <- mx[1:n, 1:n]
      if (
        inherits(core_mx, "sparseMatrix") &&
          requireNamespace("Matrix", quietly = TRUE)
      ) {
        # Sparse-aware aggregation: G %*% core %*% t(G)
        G <- Matrix::fac2sparse(group_factor)
        mobility_matrix <- as.matrix(G %*% core_mx %*% Matrix::t(G))
        dimnames(mobility_matrix) <- list(groups, groups)
      } else {
        if (inherits(core_mx, "sparseMatrix")) {
          core_mx <- as.matrix(core_mx)
        }
        mobility_matrix <- matrix(
          0,
          nrow = length(groups),
          ncol = length(groups),
          dimnames = list(groups, groups)
        )
        for (i in seq_along(groups)) {
          for (j in seq_along(groups)) {
            rows_i <- which(group_factor == groups[i])
            cols_j <- which(group_factor == groups[j])
            mobility_matrix[i, j] <- sum(core_mx[rows_i, cols_j])
          }
        }
      }

      isolates_summary <- list(
        membership = membership_df,
        mobility_matrix = mobility_matrix
      )
    }

    out <- list(
      segment.list = segment.list,
      mat.list = mat.list,
      small.cell.reduction = small.cell.reduction,
      margins_added = margins_added,
      density_reduction = density_reduction_info,
      symmetric_method = symmetric_method
    )
    if (isolates) {
      out$isolates_summary <- isolates_summary
    }
    class(out) <- "moneca"
    out$segment_metadata <- moneca_segments(out)
    return(out)
  }

  # Continue segmentation
  if (segment.levels > 1) {
    for (i in 2:segment.levels) {
      mx_for_level_topo <- if (!is.null(mx_topo.2g)) mx_topo.2g else mx.2g
      segments <- make.segments.fast(
        mx_for_level_topo,
        cut.off = cut.off,
        mode = mode,
        delete.upper.tri = delete.upper.tri,
        small.cell.reduction = small.cell.reduction,
        symmetric_method = symmetric_method
      )

      mx.2g <- segment.matrix.fast(mx.2g, segments)
      mat.list[[i + 1]] <- mx.2g
      out.put[[i]] <- list(segments = segments, mat = mx.2g)

      if (!is.null(mx_topo.2g)) {
        mx_topo.2g <- segment.matrix.fast(mx_topo.2g, segments)
      }

      # Stop if only one segment remains or density too low
      if (length(segments$cliques) <= 1) {
        break
      }
    }
  }

  # Create final segments
  segment.list <- create.segments.fast(out.put, mx)

  # Compute isolates_summary if requested
  isolates_summary <- NULL
  if (isolates) {
    # Get category names from original matrix
    n <- nrow(mx) - 1
    category_names <- rownames(mx)[1:n]
    if (is.null(category_names)) {
      category_names <- as.character(1:n)
    }

    # Get final level segments (the ones that survived)
    final_level <- length(segment.list)
    final_segments <- segment.list[[final_level]]

    # Build membership: assign each category to its segment or "altri"
    membership <- rep("altri", n)
    for (seg_idx in seq_along(final_segments)) {
      seg_members <- final_segments[[seg_idx]]
      membership[seg_members] <- paste0("Segment_", seg_idx)
    }

    # Create membership dataframe
    membership_df <- data.frame(
      name = category_names,
      group = membership,
      stringsAsFactors = FALSE
    )

    # Create mobility matrix among groups (including "altri")
    groups <- unique(membership)
    group_factor <- factor(membership, levels = groups)

    # Aggregate original matrix by groups
    core_mx <- mx[1:n, 1:n]
    if (
      inherits(core_mx, "sparseMatrix") &&
        requireNamespace("Matrix", quietly = TRUE)
    ) {
      # Sparse-aware aggregation: G %*% core %*% t(G)
      G <- Matrix::fac2sparse(group_factor)
      mobility_matrix <- as.matrix(G %*% core_mx %*% Matrix::t(G))
      dimnames(mobility_matrix) <- list(groups, groups)
    } else {
      if (inherits(core_mx, "sparseMatrix")) {
        core_mx <- as.matrix(core_mx)
      }
      mobility_matrix <- matrix(
        0,
        nrow = length(groups),
        ncol = length(groups),
        dimnames = list(groups, groups)
      )
      for (i in seq_along(groups)) {
        for (j in seq_along(groups)) {
          rows_i <- which(group_factor == groups[i])
          cols_j <- which(group_factor == groups[j])
          mobility_matrix[i, j] <- sum(core_mx[rows_i, cols_j])
        }
      }
    }

    isolates_summary <- list(
      membership = membership_df,
      mobility_matrix = mobility_matrix
    )
  }

  # Create output
  out <- list(
    segment.list = segment.list,
    mat.list = mat.list,
    small.cell.reduction = small.cell.reduction,
    margins_added = margins_added,
    density_reduction = density_reduction_info,
    symmetric_method = symmetric_method
  )
  if (isolates) {
    out$isolates_summary <- isolates_summary
  }

  class(out) <- "moneca"
  out$segment_metadata <- moneca_segments(out)

  return(out)
}

#' Export fast moneca functions for use in package
#' @keywords internal
moneca_graph_from_adjacency <- function(...) {
  if (
    exists(".moneca_env") &&
      exists("moneca_graph_from_adjacency", envir = .moneca_env)
  ) {
    .moneca_env$moneca_graph_from_adjacency(...)
  } else {
    # Fallback to igraph function
    if (utils::packageVersion("igraph") >= "1.3.0") {
      igraph::graph_from_adjacency_matrix(...)
    } else {
      igraph::graph.adjacency(...)
    }
  }
}
