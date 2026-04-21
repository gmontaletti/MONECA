# moneca_simple.R
# -----------------------------------------------------------------------------
# Didactic, stripped-down version of moneca_fast().
#
# Goal: expose the core algorithm in the clearest possible form. This file is
# NOT part of the package API and is not loaded by the package. It is intended
# to be read top-to-bottom as a teaching aid.
#
# What is removed compared to moneca_fast():
#   - sparse-matrix path
#   - density-reduction preprocessing
#   - auto-tuning of small.cell.reduction
#   - min-reciprocity symmetrization (only the default "sum" is kept)
#   - max.clique.size, use_maximal_cliques
#   - min.density early-stop
#   - isolates summary
#   - progress bar
#
# The five main steps are marked with section headers. To experiment:
#   devtools::load_all()  # to get moneca_cliques()/moneca_graph_from_adjacency()
#   source(system.file("examples", "moneca_simple.R", package = "moneca"))
#   seg <- moneca_simple(generate_mobility_data(n_classes = 6, seed = 1))
# -----------------------------------------------------------------------------

moneca_simple <- function(
  mx,
  segment.levels = 3,
  cut.off = 1,
  small.cell.reduction = 0
) {
  # 1. Add margins if missing -----
  # moneca expects the last row/col to hold row-sums, col-sums and grand total.
  mx <- ms_add_margins(mx)

  # 2. Iterative segmentation -----
  # At each level we build a relative-risk weight matrix from the current
  # mobility table, turn it into a graph, find cliques, assign categories to
  # segments, and aggregate the mobility table by segment. The aggregated
  # table becomes the input for the next level.
  mat.list <- list(mx)
  out.put <- list()
  current <- mx

  for (level in seq_len(segment.levels)) {
    segments <- ms_make_segments(current, cut.off, small.cell.reduction)
    current <- ms_aggregate(current, segments)

    mat.list[[level + 1L]] <- current
    out.put[[level]] <- list(segments = segments, mat = current)

    # Stop once everything has collapsed into a single segment.
    if (length(segments$cliques) <= 1L) break
  }

  # 3. Build the nested segment.list -----
  # For each level L, which ORIGINAL categories belong to each surviving segment.
  segment.list <- ms_build_segment_list(out.put, mx, segment.levels)

  # 4. Assemble moneca object -----
  out <- list(
    segment.list = segment.list,
    mat.list = mat.list,
    small.cell.reduction = small.cell.reduction
  )
  class(out) <- "moneca"
  out
}


# 1. Margin handling ----------------------------------------------------------

# Append row-sums, col-sums and grand total as the last row/column when they
# are not already present.
ms_add_margins <- function(mx) {
  if (!is.matrix(mx)) {
    mx <- as.matrix(mx)
  }
  n <- nrow(mx)
  if (n >= 3L && n == ncol(mx)) {
    core <- mx[-n, -n]
    tol <- max(sum(core) * 1e-6, 1)
    row_ok <- all(abs(mx[n, -n] - colSums(core)) < tol)
    col_ok <- all(abs(mx[-n, n] - rowSums(core)) < tol)
    corner_ok <- abs(mx[n, n] - sum(core)) < tol
    if (row_ok && col_ok && corner_ok) return(mx)
  }

  nm <- rownames(mx)
  if (is.null(nm)) {
    nm <- paste0("Cat", seq_len(nrow(mx)))
  }
  out <- rbind(cbind(mx, rowSums(mx)), c(colSums(mx), sum(mx)))
  dimnames(out) <- list(c(nm, "Total"), c(nm, "Total"))
  out
}


# 2. Weight matrix ------------------------------------------------------------

# Relative-risk weights: observed / expected under margin independence,
# symmetrized via mx + t(mx). Entries below cut.off and the diagonal become NA.
ms_weight_matrix <- function(mx, cut.off, small.cell.reduction) {
  n <- nrow(mx)
  core <- mx[-n, -n]
  if (small.cell.reduction > 0) {
    core[core < small.cell.reduction] <- 0
  }

  total <- mx[n, n]
  r_shr <- mx[-n, n] / total
  c_shr <- mx[n, -n] / total
  expected <- outer(r_shr, c_shr) * sum(core)

  rr <- core / expected
  W <- rr + t(rr)
  W[W < cut.off] <- NA
  diag(W) <- NA
  W
}


# 3. Graph + clique-based segment assignment --------------------------------

# One segmentation step: weights -> graph -> cliques -> greedy group growth.
ms_make_segments <- function(mx, cut.off, small.cell.reduction) {
  W <- ms_weight_matrix(mx, cut.off, small.cell.reduction)
  W0 <- W
  W0[is.na(W0)] <- 0

  g <- moneca_graph_from_adjacency(
    adjmatrix = W0,
    mode = "undirected",
    weighted = TRUE,
    diag = FALSE
  )
  cliques <- moneca_cliques(g, min = 2)

  ms_assign_segments(W, cliques, cut.off)
}

# Greedy clique-based assignment.
# Walk edges from strongest to weakest. A pair joins an existing group only
# if the resulting group is still a subset of some clique; otherwise the edge
# is skipped. Nodes never touched end up as their own singleton group.
ms_assign_segments <- function(W, cliques, cut.off) {
  n <- nrow(W)
  group <- setNames(integer(n), rownames(W))

  # Node -> list of clique indices it belongs to. Used for fast subset tests.
  node_to_cliques <- vector("list", n)
  for (ci in seq_along(cliques)) {
    for (node in cliques[[ci]]) {
      node_to_cliques[[node]] <- c(node_to_cliques[[node]], ci)
    }
  }

  # A set of nodes is "inside some clique" iff the intersection of their
  # clique-membership lists is non-empty.
  fits_in_clique <- function(nodes) {
    if (length(nodes) < 2L) {
      return(TRUE)
    }
    length(Reduce(intersect, node_to_cliques[nodes])) > 0L
  }

  # Edges above cut-off in the lower triangle, sorted by weight descending.
  idx <- which(lower.tri(W) & !is.na(W) & W > cut.off, arr.ind = TRUE)
  if (nrow(idx) == 0L) {
    return(list(
      membership = as.factor(rep(1L, n)),
      cliques = list(seq_len(n))
    ))
  }
  idx <- idx[order(W[idx], decreasing = TRUE), , drop = FALSE]

  for (i in seq_len(nrow(idx))) {
    pair <- idx[i, ]
    gc <- group[pair]

    # Already in the same non-zero group: nothing to do.
    if (all(gc != 0L) && gc[1L] == gc[2L]) {
      next
    }

    if (sum(gc) == 0L) {
      # Neither node is grouped yet: seed a new group labelled by edge rank.
      group[pair] <- i
    } else {
      # At least one node is grouped: try to merge into the majority group.
      active <- gc[gc != 0L]
      members <- which(group %in% active)
      tbl <- table(group[members])
      target <- as.numeric(names(tbl))[which.max(tbl)[1L]]
      candidate <- unique(sort(c(members, pair)))
      if (fits_in_clique(candidate)) group[candidate] <- target
    }
  }

  # Promote unassigned nodes to singleton groups.
  unassigned <- which(group == 0L)
  if (length(unassigned) > 0L) {
    group[unassigned] <- seq_along(unassigned) + max(group)
  }

  g <- as.factor(group)
  levels(g) <- seq_len(nlevels(g))
  list(
    membership = g,
    cliques = lapply(levels(g), function(lv) which(g == lv))
  )
}


# 4. Aggregate the mobility table by segment ---------------------------------

# Collapse rows/columns according to segment membership. The margin row/col
# (last one) stays in its own group so that totals are preserved.
ms_aggregate <- function(mx, segments) {
  groups <- c(segments$membership, length(segments$membership) + 1L)
  t(rowsum(t(rowsum(mx, groups)), groups))
}


# 5. Flatten per-level cliques into the nested segment.list ------------------

# seg.list[[L]] returns, for each segment surviving at level L, the ids of the
# ORIGINAL categories it contains. Level 1 is the trivial one-per-category
# partition.
ms_build_segment_list <- function(out.put, mx, segment.levels) {
  seg.list <- list()
  seg.list[[1L]] <- as.list(seq_len(nrow(mx) - 1L))
  if (length(out.put) == 0L) {
    return(seg.list)
  }

  level.current <- out.put[[1L]]$segments$cliques
  seg.list[[2L]] <- level.current[lengths(level.current) > 1L]

  depth <- min(segment.levels, length(out.put))
  if (depth > 1L) {
    for (n in 2:depth) {
      level.current <- out.put[[n]]$segments$cliques
      # Translate clique ids through each previous level, back to originals.
      for (i in seq_len(n - 1L)) {
        below <- out.put[[n - i]]$segments$cliques
        level.current <- lapply(
          level.current[lengths(level.current) > 1L],
          function(d) unlist(below[d])
        )
      }
      if (length(level.current) > 0L) seg.list[[n + 1L]] <- level.current
    }
  }
  seg.list
}
