#' Fast moneca - Optimized Mobility Network Clustering Analysis
#'
#' An optimized version of the moneca algorithm with improved performance for large datasets.
#' Implements various performance optimizations including vectorization and early stopping.
#'
#' @param mx A mobility table (square matrix) with row and column totals in the last
#'   row/column. Row names should identify the categories/classes.
#' @param segment.levels Integer specifying the number of hierarchical segmentation
#'   levels to compute. Default is 3.
#' @param cut.off Numeric threshold for the minimum relative risk to be considered
#'   a significant tie. Default is 1.
#' @param mode Character string specifying edge mode. Currently uses symmetric mode.
#' @param delete.upper.tri Logical indicating whether to use only lower triangle. Default is TRUE.
#' @param small.cell.reduction Numeric value to handle small cell counts. Default is 0.
#' @param use.sparse Logical indicating whether to use sparse matrices for large data. Default is FALSE.
#' @param min.density Minimum strength-based density to continue processing.
#'   Calculated as mean(strength)/max(strength). Default is 0 (disabled for algorithmic
#'   fidelity). Set to 0.01 or higher for early stopping optimization on sparse graphs.
#' @param max.clique.size Maximum size of cliques to consider (NULL for no limit). Default is NULL.
#' @param progress Logical indicating whether to show progress bars. Default is TRUE.
#' @param use_maximal_cliques Logical indicating whether to use maximal cliques
#'   (faster, fewer cliques) instead of all cliques (slower, more complete enumeration).
#'   Default is FALSE (use all cliques for algorithmic correctness). Set to TRUE for
#'   performance optimization on very dense graphs.
#' @param isolates Logical. If TRUE, returns additional summary information
#'   about isolates (categories not belonging to any multi-member segment at the
#'   final level). Isolates are grouped into a category called "altri".
#'   Default is FALSE.
#'
#' @details
#' This implementation is optimized for single-core performance using:
#' \itemize{
#'   \item Vectorized matrix operations for improved speed
#'   \item Optional sparse matrix support via \code{use.sparse} parameter
#'   \item Early stopping via \code{min.density} threshold (disabled by default)
#'   \item Clique size limiting via \code{max.clique.size} parameter
#' }
#'
#' \strong{Produces identical results to} \code{\link{moneca}} when using default parameters.
#'
#' \strong{When to use:}
#' \itemize{
#'   \item Any system - recommended for most use cases
#'   \item Small to large datasets
#'   \item When you want explicit control over optimization parameters
#' }
#'
#' @return An object of class "moneca" with the same structure as the original moneca() function.
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
#' @seealso \code{\link{moneca}} for the original implementation
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
  isolates = FALSE
) {
  # Auto-enable sparse for large matrices (n > 50) unless explicitly set
  if (!is.matrix(mx)) {
    mx <- as.matrix(mx)
  }
  if (
    use.sparse || (nrow(mx) > 50 && requireNamespace("Matrix", quietly = TRUE))
  ) {
    if (
      !inherits(mx, "sparseMatrix") &&
        requireNamespace("Matrix", quietly = TRUE)
    ) {
      mx <- Matrix::Matrix(mx, sparse = TRUE)
    }
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

    # Get cliques based on settings
    if (use_maximal_cliques) {
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

    # Process matrix for mode (matches original exactly)
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

    # Initialize group vector (matches original)
    group <- vector(mode = "numeric", length = n)
    names(group) <- rownames(mat)

    # Build efficient clique lookup (node -> cliques containing it)
    # Pre-allocate with known sizes to reduce GC pressure
    node_clique_count <- integer(n)
    for (ci in seq_along(cliques)) {
      for (node in cliques[[ci]]) {
        node_clique_count[node] <- node_clique_count[node] + 1L
      }
    }
    node_to_cliques <- vector("list", n)
    for (i in seq_len(n)) {
      node_to_cliques[[i]] <- integer(node_clique_count[i])
    }
    node_clique_pos <- integer(n)
    for (ci in seq_along(cliques)) {
      for (node in cliques[[ci]]) {
        node_clique_pos[node] <- node_clique_pos[node] + 1L
        node_to_cliques[[node]][node_clique_pos[node]] <- ci
      }
    }

    # Fast clique membership test using node-to-clique lookup
    clique.test <- function(potential.clique) {
      if (length(potential.clique) < 2) {
        return(TRUE)
      }
      candidates <- node_to_cliques[[potential.clique[1]]]
      if (length(candidates) == 0L) {
        return(FALSE)
      }
      for (i in 2:length(potential.clique)) {
        candidates <- candidates[
          candidates %in% node_to_cliques[[potential.clique[i]]]
        ]
        if (length(candidates) == 0L) return(FALSE)
      }
      TRUE
    }

    # OPTIMIZATION: Pre-sort edges instead of repeated max() searches
    edge_idx <- which(!is.na(mat) & mat > cut.off, arr.ind = TRUE)

    if (nrow(edge_idx) == 0L) {
      return(list(
        membership = as.factor(rep(1, n)),
        cliques = list(1:n)
      ))
    }

    edge_weights <- mat[edge_idx]
    edge_order <- order(edge_weights, decreasing = TRUE)
    sorted_edges <- edge_idx[edge_order, , drop = FALSE]
    n_edges <- nrow(sorted_edges)

    if (progress) {
      pb <- txtProgressBar(min = 1, max = max(n_edges, 2), style = 3)
    }

    # Process edges in pre-sorted order (matches original logic exactly)
    for (i in seq_len(n_edges)) {
      if (progress) {
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
    # Use standard aggregation
    groups.1 <- c(segments$membership, length(segments$membership) + 1)

    # Convert to regular matrix if sparse (rowsum doesn't work with sparse matrices)
    if (inherits(mx, "sparseMatrix")) {
      mx <- as.matrix(mx)
    }

    # Standard aggregation
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
    small.cell.reduction = small.cell.reduction
  ) {
    mx.1i <- weight.matrix(
      mx,
      cut.off,
      small.cell.reduction = small.cell.reduction,
      auto_tune = auto_tune,
      tune_method = tune_method,
      tune_verbose = tune_verbose
    )

    # Create graph
    mx.1i.graph <- mx.1i
    mx.1i.graph[is.na(mx.1i.graph)] <- 0

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
  mat.list[[1]] <- mx

  segments <- make.segments.fast(
    mx,
    cut.off = cut.off,
    mode = mode,
    delete.upper.tri = delete.upper.tri,
    small.cell.reduction = small.cell.reduction
  )

  mx.2g <- segment.matrix.fast(mx, segments)
  mat.list[[2]] <- mx.2g

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

      isolates_summary <- list(
        membership = membership_df,
        mobility_matrix = mobility_matrix
      )
    }

    if (isolates) {
      out <- list(
        segment.list = segment.list,
        mat.list = mat.list,
        small.cell.reduction = small.cell.reduction,
        isolates_summary = isolates_summary
      )
    } else {
      out <- list(
        segment.list = segment.list,
        mat.list = mat.list,
        small.cell.reduction = small.cell.reduction
      )
    }
    class(out) <- "moneca"
    return(out)
  }

  # Continue segmentation
  if (segment.levels > 1) {
    for (i in 2:segment.levels) {
      segments <- make.segments.fast(
        mx.2g,
        cut.off = cut.off,
        mode = mode,
        delete.upper.tri = delete.upper.tri,
        small.cell.reduction = small.cell.reduction
      )

      mx.2g <- segment.matrix.fast(mx.2g, segments)
      mat.list[[i + 1]] <- mx.2g
      out.put[[i]] <- list(segments = segments, mat = mx.2g)

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

    isolates_summary <- list(
      membership = membership_df,
      mobility_matrix = mobility_matrix
    )
  }

  # Create output
  if (isolates) {
    out <- list(
      segment.list = segment.list,
      mat.list = mat.list,
      small.cell.reduction = small.cell.reduction,
      isolates_summary = isolates_summary
    )
  } else {
    out <- list(
      segment.list = segment.list,
      mat.list = mat.list,
      small.cell.reduction = small.cell.reduction
    )
  }

  class(out) <- "moneca"

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
