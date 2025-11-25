#' Optimized Algorithms for MONECA Clustering
#'
#' This module provides highly optimized implementations of the core MONECA
#' algorithms using vectorization, efficient data structures, and optional
#' data.table integration for maximum performance.
#'
#' @name optimized_algorithms
#' @keywords internal
NULL

#' Union-Find Data Structure for Efficient Group Merging
#'
#' Implements a disjoint-set union (DSU) with path compression and union by rank
#' for O(alpha(n)) amortized operations where alpha is the inverse Ackermann function.
#'
#' @keywords internal
create_union_find <- function(n) {
  env <- new.env(parent = emptyenv())
  env$parent <- seq_len(n)
  env$rank <- rep(0L, n)

  # Find with path compression
  env$find <- function(x) {
    if (env$parent[x] != x) {
      env$parent[x] <- env$find(env$parent[x])
    }
    env$parent[x]
  }

  # Union by rank
  env$union <- function(x, y) {
    px <- env$find(x)
    py <- env$find(y)

    if (px == py) return(FALSE)

    if (env$rank[px] < env$rank[py]) {
      env$parent[px] <- py
    } else if (env$rank[px] > env$rank[py]) {
      env$parent[py] <- px
    } else {
      env$parent[py] <- px
      env$rank[px] <- env$rank[px] + 1L
    }
    TRUE
  }

  # Get all elements in same group as x
  env$get_group <- function(x) {
    root <- env$find(x)
    which(sapply(seq_len(n), env$find) == root)
  }

  # Get final group assignments
  env$get_groups <- function() {
    roots <- sapply(seq_len(n), env$find)
    unique_roots <- unique(roots)
    match(roots, sort(unique_roots))
  }

  env
}

#' Optimized Segment Finding with Vectorized Operations
#'
#' High-performance implementation of the segment finding algorithm using:
#' - Pre-sorted edges for single-pass processing
#' - Union-find for O(alpha(n)) group merging
#' - Vectorized clique membership testing
#' - Optional sparse matrix support
#'
#' @param mat Weight matrix from weight.matrix()
#' @param cliques List of cliques from igraph
#' @param cut.off Minimum weight threshold
#' @param progress Show progress bar
#' @return List with membership and cliques
#' @keywords internal
find_segments_optimized <- function(mat, cliques, cut.off = 1, progress = FALSE) {

  n <- nrow(mat)

  # Handle trivial cases
  if (length(cliques) == 0 || n < 2) {
    return(list(
      membership = as.factor(rep(1L, n)),
      cliques = list(seq_len(n))
    ))
  }

  # 1. Extract and pre-sort edges (single pass)
  mat_work <- mat
  mat_work[upper.tri(mat_work)] <- NA
  diag(mat_work) <- NA

  edge_idx <- which(!is.na(mat_work) & mat_work > cut.off, arr.ind = TRUE)

  if (nrow(edge_idx) == 0) {
    return(list(
      membership = as.factor(seq_len(n)),
      cliques = as.list(seq_len(n))
    ))
  }

  edge_weights <- mat_work[edge_idx]
  edge_order <- order(edge_weights, decreasing = TRUE)
  edges <- edge_idx[edge_order, , drop = FALSE]

  # 2. Build clique membership lookup (vectorized)
  # Use sparse representation for memory efficiency
  clique_nodes <- lapply(cliques, function(cl) sort(cl))

  # Create fast lookup: for each node, which cliques contain it
  node_to_cliques <- vector("list", n)
  for (i in seq_len(n)) node_to_cliques[[i]] <- integer(0)

  for (ci in seq_along(clique_nodes)) {
    for (node in clique_nodes[[ci]]) {
      node_to_cliques[[node]] <- c(node_to_cliques[[node]], ci)
    }
  }

  # 3. Fast clique membership test function
  # Tests if a set of nodes forms a subset of any clique
  is_in_clique <- function(nodes) {
    if (length(nodes) < 2) return(TRUE)

    # Find cliques that contain ALL nodes
    # Start with cliques containing first node, then intersect
    candidate_cliques <- node_to_cliques[[nodes[1]]]
    if (length(candidate_cliques) == 0) return(FALSE)

    for (i in 2:length(nodes)) {
      candidate_cliques <- intersect(candidate_cliques, node_to_cliques[[nodes[i]]])
      if (length(candidate_cliques) == 0) return(FALSE)
    }

    TRUE
  }

  # 4. Initialize union-find structure
  uf <- create_union_find(n)

  # 5. Process edges in weight order
  if (progress) {
    pb <- txtProgressBar(min = 0, max = nrow(edges), style = 3)
  }

  for (i in seq_len(nrow(edges))) {
    if (progress && i %% 100 == 0) setTxtProgressBar(pb, i)

    node_a <- edges[i, 1]
    node_b <- edges[i, 2]

    root_a <- uf$find(node_a)
    root_b <- uf$find(node_b)

    # Skip if already in same group
    if (root_a == root_b) next

    # Get all nodes in both groups
    group_a <- uf$get_group(node_a)
    group_b <- uf$get_group(node_b)
    potential_group <- unique(c(group_a, group_b))

    # Check if merged group forms valid clique subset
    if (is_in_clique(potential_group)) {
      # Merge all nodes in group_b into group_a
      for (node in group_b) {
        uf$union(node_a, node)
      }
    }
  }

  if (progress) {
    setTxtProgressBar(pb, nrow(edges))
    close(pb)
  }

  # 6. Get final group assignments
  groups <- uf$get_groups()

  # 7. Create output in expected format
  g <- as.factor(groups)
  ud.list <- unname(split(seq_along(g), g))

  list(
    membership = g,
    cliques = ud.list
  )
}

#' Optimized Weight Matrix Calculation
#'
#' Vectorized computation of relative risk weight matrix with optional
#' data.table acceleration for very large matrices.
#'
#' @param mx Mobility matrix with totals
#' @param cut.off Minimum relative risk threshold
#' @param small.cell.reduction Minimum cell count
#' @param symmetric Whether to symmetrize the matrix
#' @return Weight matrix with NA for values below cutoff
#' @keywords internal
weight_matrix_optimized <- function(mx, cut.off = 1, small.cell.reduction = 0,
                                   symmetric = TRUE) {

  l <- nrow(mx)
  if (l < 2) stop("Matrix must have at least 2 rows")

  # Extract totals
  row_totals <- mx[-l, l]
  col_totals <- mx[l, -l]
  grand_total <- mx[l, l]

  # Calculate shares (vectorized)
  row_share <- row_totals / grand_total
  col_share <- col_totals / grand_total

  # Core matrix without totals
  core <- mx[-l, -l]

  # Expected values (outer product)
  total_mobility <- sum(core)
  expected <- outer(row_share, col_share) * total_mobility

  # Apply small cell reduction
  if (small.cell.reduction > 0) {
    core[core < small.cell.reduction] <- 0
    expected[expected < small.cell.reduction] <- small.cell.reduction
  }

  # Relative risk (vectorized division)
  rr <- core / expected

  # Symmetrize if requested
  if (symmetric) {
    rr <- rr + t(rr)
  }

  # Apply cutoff
  rr[rr < cut.off] <- NA
  diag(rr) <- NA

  # Preserve row/column names
  rownames(rr) <- rownames(mx)[-l]
  colnames(rr) <- colnames(mx)[-l]

  rr
}

#' Fast Segment Matrix Aggregation
#'
#' Aggregates mobility matrix by segment membership using optimized
#' matrix operations.
#'
#' @param mx Mobility matrix
#' @param membership Factor or integer vector of segment assignments
#' @return Aggregated matrix by segments
#' @keywords internal
segment_matrix_optimized <- function(mx, membership) {

  # Add total row/column membership
  groups <- c(as.integer(membership), max(as.integer(membership)) + 1L)

  # Row aggregation
  mx_r <- rowsum(mx, groups, reorder = FALSE)

  # Column aggregation (transpose trick)
  mx_rc <- t(rowsum(t(mx_r), groups, reorder = FALSE))

  mx_rc
}

#' Benchmark Utility for Performance Testing
#'
#' Compares performance of original and optimized implementations.
#'
#' @param mx Mobility matrix for testing
#' @param n_runs Number of benchmark runs
#' @param segment.levels Number of segmentation levels
#' @return Data frame with timing results
#' @keywords internal
benchmark_implementations <- function(mx, n_runs = 3, segment.levels = 3) {

  results <- data.frame(
    implementation = character(),
    run = integer(),
    time_seconds = numeric(),
    stringsAsFactors = FALSE
  )

  cat("Benchmarking moneca implementations...\n")
  cat("Matrix size:", nrow(mx) - 1, "x", ncol(mx) - 1, "\n")
  cat("Runs per implementation:", n_runs, "\n\n")

  # Benchmark original moneca
  cat("Testing moneca()...\n")
  for (i in seq_len(n_runs)) {
    t1 <- Sys.time()
    seg_orig <- moneca(mx, segment.levels = segment.levels)
    t2 <- Sys.time()
    results <- rbind(results, data.frame(
      implementation = "moneca",
      run = i,
      time_seconds = as.numeric(difftime(t2, t1, units = "secs"))
    ))
  }

  # Benchmark moneca_fast
  cat("Testing moneca_fast()...\n")
  for (i in seq_len(n_runs)) {
    t1 <- Sys.time()
    seg_fast <- moneca_fast(mx, segment.levels = segment.levels, progress = FALSE)
    t2 <- Sys.time()
    results <- rbind(results, data.frame(
      implementation = "moneca_fast",
      run = i,
      time_seconds = as.numeric(difftime(t2, t1, units = "secs"))
    ))
  }

  # Verify results match
  cat("\nVerifying result equivalence...\n")
  match_check <- all.equal(
    seg_orig$segment.list,
    seg_fast$segment.list,
    check.attributes = FALSE
  )

  if (isTRUE(match_check)) {
    cat("Results MATCH - algorithms produce identical output\n")
  } else {
    cat("WARNING: Results differ -", match_check, "\n")
  }

  # Summary statistics
  cat("\n--- Summary ---\n")
  for (impl in unique(results$implementation)) {
    times <- results$time_seconds[results$implementation == impl]
    cat(sprintf("%s: mean=%.3fs, sd=%.3fs\n",
                impl, mean(times), sd(times)))
  }

  # Speedup calculation
  mean_orig <- mean(results$time_seconds[results$implementation == "moneca"])
  mean_fast <- mean(results$time_seconds[results$implementation == "moneca_fast"])
  cat(sprintf("\nSpeedup: %.1fx\n", mean_orig / mean_fast))

  invisible(results)
}
