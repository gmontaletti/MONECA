# Directed Clustering Analysis and Comparison Framework
#
# Functions for analyzing asymmetry in MONECA segments and comparing
# alternative clustering results.

# 1. Helper: extract sub-matrix with margins -----

#' Extract Sub-matrix with Margins for a Segment
#'
#' Given the original mobility matrix (with margins) and a vector of member
#' indices, builds the corresponding sub-matrix including recalculated row,
#' column, and grand totals.
#'
#' @param mat Full mobility matrix with margins (last row/col are totals).
#' @param members Integer vector of row/column indices (1-based, excluding
#'   the margin row/col).
#'
#' @return A square matrix of dimension `(length(members) + 1)` with
#'   recalculated margins.
#'
#' @keywords internal
.extract_submatrix_with_margins <- function(mat, members) {
  n <- nrow(mat)
  inner <- mat[-n, -n]
  sub <- inner[members, members, drop = FALSE]

  row_totals <- rowSums(sub)
  col_totals <- colSums(sub)
  grand_total <- sum(sub)

  out <- rbind(
    cbind(sub, row_totals),
    c(col_totals, grand_total)
  )
  rn <- rownames(inner)[members]
  rownames(out) <- c(rn, "Total")
  colnames(out) <- c(rn, "Total")
  out
}

# 2. Pairwise asymmetry computation -----

#' Compute Pairwise Asymmetry from an Asymmetric RR Matrix
#'
#' For every pair (i, j) where i < j, computes
#' `abs(RR_ij - RR_ji) / (RR_ij + RR_ji)`.
#'
#' @param rr_asym Asymmetric relative risk matrix (no NAs expected for
#'   within-segment pairs, but handled gracefully).
#'
#' @return A data.frame with columns `from`, `to`, `rr_ij`, `rr_ji`, `asym`.
#'
#' @keywords internal
.pairwise_asymmetry <- function(rr_asym) {
  nms <- rownames(rr_asym)
  n <- nrow(rr_asym)
  if (n < 2) {
    return(data.frame(
      from = character(0),
      to = character(0),
      rr_ij = numeric(0),
      rr_ji = numeric(0),
      asym = numeric(0),
      stringsAsFactors = FALSE
    ))
  }

  pairs <- which(upper.tri(rr_asym), arr.ind = TRUE)
  from_idx <- pairs[, 1]
  to_idx <- pairs[, 2]

  rr_ij <- rr_asym[cbind(from_idx, to_idx)]
  rr_ji <- rr_asym[cbind(to_idx, from_idx)]

  # Replace NA with 0 for computation
  rr_ij[is.na(rr_ij)] <- 0
  rr_ji[is.na(rr_ji)] <- 0

  denom <- rr_ij + rr_ji
  asym <- ifelse(denom == 0, 0, abs(rr_ij - rr_ji) / denom)

  data.frame(
    from = nms[from_idx],
    to = nms[to_idx],
    rr_ij = rr_ij,
    rr_ji = rr_ji,
    asym = asym,
    stringsAsFactors = FALSE
  )
}

# 3. compute_asymmetry_scores -----

#' Compute Asymmetry Scores for MONECA Segments
#'
#' Quantifies the degree of directional asymmetry within each segment at
#' a given hierarchical level. For every within-segment pair (i, j), the
#' asymmetry is `abs(RR_ij - RR_ji) / (RR_ij + RR_ji)`, ranging from 0
#' (perfectly symmetric) to 1 (completely one-directional).
#'
#' @param segments A moneca object returned by [moneca()] or [moneca_fast()].
#' @param level Integer. Hierarchical level to evaluate (default 2).
#' @param method Character. Aggregation method per segment:
#'   * `"max_pair"` (default): maximum pairwise asymmetry.
#'   * `"mean_pair"`: mean pairwise asymmetry weighted by flow volume.
#'
#' @return A `data.frame` with one row per segment and columns:
#'   \describe{
#'     \item{segment_id}{Integer segment index at the requested level.}
#'     \item{n_members}{Number of categories in the segment.}
#'     \item{asymmetry_score}{Aggregate asymmetry (max or mean).}
#'     \item{max_pair_from}{Origin category of the most asymmetric pair.}
#'     \item{max_pair_to}{Destination category of the most asymmetric pair.}
#'     \item{max_pair_asym}{Asymmetry value of the most asymmetric pair.}
#'   }
#'
#' @details
#' The function retrieves the original mobility matrix from
#' `segments$mat.list[[1]]` and computes an asymmetric relative risk matrix
#' via [weight.matrix()] with `symmetric = FALSE` and `cut.off = 0`.
#' Pairs with zero flow in both directions receive an asymmetry of 0.
#' Pairs with flow in only one direction receive the maximum asymmetry of 1.
#'
#' @examples
#' mob <- generate_mobility_data(n_classes = 6, seed = 42)
#' seg <- moneca(mob, segment.levels = 3)
#' compute_asymmetry_scores(seg, level = 2)
#'
#' @export
compute_asymmetry_scores <- function(segments, level = 2, method = "max_pair") {
  # Input validation
  if (!inherits(segments, "moneca")) {
    stop("'segments' must be a moneca object.", call. = FALSE)
  }
  if (!is.numeric(level) || length(level) != 1 || level < 1) {
    stop("'level' must be a single positive integer.", call. = FALSE)
  }
  if (level > length(segments$segment.list)) {
    stop(
      "'level' exceeds available levels (max ",
      length(segments$segment.list),
      ").",
      call. = FALSE
    )
  }
  method <- match.arg(method, c("max_pair", "mean_pair"))

  orig_mat <- segments$mat.list[[1]]
  cat_names <- rownames(orig_mat)
  cat_names <- cat_names[-length(cat_names)]

  # Asymmetric RR (cut.off = 0 retains all values)
  rr_asym <- weight.matrix(orig_mat, cut.off = 0, symmetric = FALSE)

  seg_list <- segments$segment.list[[level]]
  n_seg <- length(seg_list)

  out <- data.frame(
    segment_id = integer(n_seg),
    n_members = integer(n_seg),
    asymmetry_score = numeric(n_seg),
    max_pair_from = character(n_seg),
    max_pair_to = character(n_seg),
    max_pair_asym = numeric(n_seg),
    stringsAsFactors = FALSE
  )

  for (s in seq_len(n_seg)) {
    members <- seg_list[[s]]
    out$segment_id[s] <- s
    out$n_members[s] <- length(members)

    if (length(members) < 2) {
      out$asymmetry_score[s] <- 0
      out$max_pair_from[s] <- NA_character_
      out$max_pair_to[s] <- NA_character_
      out$max_pair_asym[s] <- 0
      next
    }

    sub_rr <- rr_asym[members, members, drop = FALSE]
    pw <- .pairwise_asymmetry(sub_rr)

    if (nrow(pw) == 0) {
      out$asymmetry_score[s] <- 0
      out$max_pair_from[s] <- NA_character_
      out$max_pair_to[s] <- NA_character_
      out$max_pair_asym[s] <- 0
      next
    }

    # Max pair
    max_idx <- which.max(pw$asym)
    out$max_pair_from[s] <- pw$from[max_idx]
    out$max_pair_to[s] <- pw$to[max_idx]
    out$max_pair_asym[s] <- pw$asym[max_idx]

    if (method == "max_pair") {
      out$asymmetry_score[s] <- pw$asym[max_idx]
    } else {
      # Weighted mean: weight by total flow volume
      weights <- pw$rr_ij + pw$rr_ji
      total_w <- sum(weights)
      if (total_w == 0) {
        out$asymmetry_score[s] <- 0
      } else {
        out$asymmetry_score[s] <- sum(pw$asym * weights) / total_w
      }
    }
  }

  out
}

# 4. flag_asymmetric_segments -----

#' Flag Segments Exceeding an Asymmetry Threshold
#'
#' Convenience wrapper around [compute_asymmetry_scores()] that identifies
#' segments whose asymmetry score exceeds a user-defined threshold.
#'
#' @inheritParams compute_asymmetry_scores
#' @param threshold Numeric in \[0, 1\]. Segments with an asymmetry score
#'   strictly above this value are flagged (default 0.5).
#'
#' @return A list with components:
#'   \describe{
#'     \item{scores}{The full `data.frame` returned by
#'       [compute_asymmetry_scores()].}
#'     \item{flagged}{Logical vector of length equal to the number of
#'       segments; `TRUE` for segments above the threshold.}
#'     \item{threshold}{The threshold used.}
#'   }
#'
#' @examples
#' mob <- generate_mobility_data(n_classes = 6, seed = 42)
#' seg <- moneca(mob, segment.levels = 3)
#' flags <- flag_asymmetric_segments(seg, threshold = 0.3)
#' flags$scores[flags$flagged, ]
#'
#' @export
flag_asymmetric_segments <- function(
  segments,
  threshold = 0.5,
  level = 2,
  method = "max_pair"
) {
  if (
    !is.numeric(threshold) ||
      length(threshold) != 1 ||
      threshold < 0 ||
      threshold > 1
  ) {
    stop("'threshold' must be a single numeric value in [0, 1].", call. = FALSE)
  }

  scores <- compute_asymmetry_scores(segments, level = level, method = method)
  flagged <- scores$asymmetry_score > threshold

  list(
    scores = scores,
    flagged = flagged,
    threshold = threshold
  )
}

# 5. split_asymmetric_segment (internal) -----

#' Split an Asymmetric Segment by Removing High-Asymmetry Edges
#'
#' Attempts to split a single segment into sub-groups by iteratively removing
#' the edges with the highest pairwise asymmetry until the underlying graph
#' disconnects into valid components (each with at least `min_segment_size`
#' members).
#'
#' @param segments A moneca object.
#' @param segment_idx Integer index of the segment within
#'   `segments$segment.list[[level]]`.
#' @param level Integer hierarchical level.
#' @param max_splits Maximum number of resulting components (default 2).
#' @param min_segment_size Minimum number of members per resulting component
#'   (default 2).
#'
#' @return A list of integer vectors, each containing the member indices of a
#'   resulting sub-segment. If no valid split is found, returns a list
#'   containing the original member vector unchanged.
#'
#' @keywords internal
split_asymmetric_segment <- function(
  segments,
  segment_idx,
  level,
  max_splits = 2,
  min_segment_size = 2
) {
  members <- segments$segment.list[[level]][[segment_idx]]

  if (length(members) < 2 * min_segment_size) {
    return(list(members))
  }

  orig_mat <- segments$mat.list[[1]]
  sub_mat <- .extract_submatrix_with_margins(orig_mat, members)

  # Asymmetric RR for scoring

  rr_asym <- weight.matrix(sub_mat, cut.off = 0, symmetric = FALSE)
  pw <- .pairwise_asymmetry(rr_asym)

  if (nrow(pw) == 0) {
    return(list(members))
  }

  # Symmetric weight matrix for graph connectivity
  rr_sym <- weight.matrix(sub_mat, cut.off = 0, symmetric = TRUE)
  rr_sym[is.na(rr_sym)] <- 0
  diag(rr_sym) <- 0

  # Rank pairs by asymmetry (descending)
  pw <- pw[order(pw$asym, decreasing = TRUE), , drop = FALSE]

  local_names <- rownames(rr_sym)

  for (k in seq_len(nrow(pw))) {
    i_idx <- match(pw$from[k], local_names)
    j_idx <- match(pw$to[k], local_names)
    if (is.na(i_idx) || is.na(j_idx)) {
      next
    }

    # Zero out the edge in both directions
    trial_adj <- rr_sym
    trial_adj[i_idx, j_idx] <- 0
    trial_adj[j_idx, i_idx] <- 0

    g <- moneca_graph_from_adjacency(
      trial_adj,
      mode = "undirected",
      weighted = TRUE,
      diag = FALSE
    )
    comp <- moneca_components(g)

    n_comp <- comp$no
    if (n_comp < 2 || n_comp > max_splits) {
      # Update adjacency for cumulative removal if graph stays connected
      if (n_comp == 1) {
        rr_sym <- trial_adj
      }
      next
    }

    # Validate component sizes
    comp_sizes <- tabulate(comp$membership, nbins = n_comp)
    if (all(comp_sizes >= min_segment_size)) {
      result <- vector("list", n_comp)
      for (c_idx in seq_len(n_comp)) {
        local_members <- which(comp$membership == c_idx)
        result[[c_idx]] <- members[local_members]
      }
      return(result)
    }
  }

  # No valid split found
  list(members)
}

# 6. refine_segments -----

#' Refine MONECA Segments by Splitting Asymmetric Clusters
#'
#' Identifies segments with high directional asymmetry and attempts to split
#' them into more homogeneous sub-groups. The refined segmentation is returned
#' as a modified moneca object with updated `segment.list` and metadata.
#'
#' @inheritParams flag_asymmetric_segments
#' @param max_splits_per_segment Integer. Maximum number of sub-segments
#'   allowed per split (default 2).
#' @param min_segment_size Integer. Minimum number of members per resulting
#'   sub-segment (default 2). Prevents creation of trivially small clusters.
#'
#' @return A moneca object (class preserved) with:
#'   * `segment.list[[level]]` updated to replace flagged segments with their
#'     splits (inserted in order).
#'   * `asymmetry_refinement` list attached, containing `scores`, `threshold`,
#'     and `splits_performed` (a named list mapping original segment indices to
#'     the number of sub-segments produced).
#'
#'   If no segments exceed the threshold, the object is returned unchanged
#'   with an informational message.
#'
#' @examples
#' \dontrun{
#' mob <- generate_mobility_data(n_classes = 8, seed = 99)
#' seg <- moneca(mob, segment.levels = 3)
#' refined <- refine_segments(seg, threshold = 0.3, level = 2)
#' }
#'
#' @keywords internal
refine_segments <- function(
  segments,
  threshold = 0.5,
  level = 2,
  method = "max_pair",
  max_splits_per_segment = 2,
  min_segment_size = 2
) {
  if (!inherits(segments, "moneca")) {
    stop("'segments' must be a moneca object.", call. = FALSE)
  }

  flags <- flag_asymmetric_segments(
    segments,
    threshold = threshold,
    level = level,
    method = method
  )

  if (!any(flags$flagged)) {
    message(
      "No segments exceed the asymmetry threshold of ",
      threshold,
      ". Returning unchanged."
    )
    segments$asymmetry_refinement <- list(
      scores = flags$scores,
      threshold = threshold,
      splits_performed = list()
    )
    return(segments)
  }

  flagged_indices <- which(flags$flagged)
  orig_seg_list <- segments$segment.list[[level]]
  new_seg_list <- list()
  splits_performed <- list()

  for (s in seq_along(orig_seg_list)) {
    if (s %in% flagged_indices) {
      sub_segs <- split_asymmetric_segment(
        segments,
        segment_idx = s,
        level = level,
        max_splits = max_splits_per_segment,
        min_segment_size = min_segment_size
      )
      for (ss in sub_segs) {
        new_seg_list[[length(new_seg_list) + 1]] <- ss
      }
      splits_performed[[as.character(s)]] <- length(sub_segs)
    } else {
      new_seg_list[[length(new_seg_list) + 1]] <- orig_seg_list[[s]]
    }
  }

  segments$segment.list[[level]] <- new_seg_list
  segments$asymmetry_refinement <- list(
    scores = flags$scores,
    threshold = threshold,
    splits_performed = splits_performed
  )

  segments
}

# 7. Adjusted Rand Index (base R) -----

#' Compute Adjusted Rand Index Between Two Clusterings
#'
#' Implements the ARI using the contingency table approach. Handles edge
#' cases where all items belong to a single cluster.
#'
#' @param cl1 Integer or factor vector of cluster assignments (length n).
#' @param cl2 Integer or factor vector of cluster assignments (length n).
#'
#' @return Numeric scalar. ARI ranges from -1 to 1; 1 indicates perfect
#'   agreement, 0 indicates chance-level agreement.
#'
#' @keywords internal
.adjusted_rand_index <- function(cl1, cl2) {
  n <- length(cl1)
  if (n != length(cl2)) {
    stop("Clustering vectors must have equal length.", call. = FALSE)
  }
  if (n == 0) {
    return(NA_real_)
  }

  # Contingency table
  tab <- table(cl1, cl2)
  n_ij <- as.numeric(tab)

  a_i <- as.numeric(rowSums(tab))
  b_j <- as.numeric(colSums(tab))

  sum_comb_nij <- sum(choose(n_ij, 2))
  sum_comb_ai <- sum(choose(a_i, 2))
  sum_comb_bj <- sum(choose(b_j, 2))
  comb_n <- choose(n, 2)

  if (comb_n == 0) {
    return(NA_real_)
  }

  expected <- sum_comb_ai * sum_comb_bj / comb_n
  max_index <- 0.5 * (sum_comb_ai + sum_comb_bj)
  denom <- max_index - expected

  if (abs(denom) < .Machine$double.eps) {
    # Both clusterings are identical singletons or identical single-cluster
    if (sum_comb_nij == expected) {
      return(1)
    }
    return(0)
  }

  (sum_comb_nij - expected) / denom
}

# 8. compare_moneca_results -----

#' Compare Two MONECA Clustering Results
#'
#' Provides a comprehensive comparison of two moneca segmentation results at
#' a given hierarchical level. Metrics include segment sizes, pairwise Jaccard
#' overlap, Adjusted Rand Index, quality deltas, and asymmetry deltas.
#'
#' @param seg1 A moneca object (first result).
#' @param seg2 A moneca object (second result).
#' @param level Integer. Hierarchical level to compare (default 2).
#' @param labels Character vector of length 2. Human-readable labels for the
#'   two results (default `c("Result 1", "Result 2")`).
#'
#' @return An S3 object of class `"moneca_comparison"` (a list) with:
#'   \describe{
#'     \item{labels}{Character vector of the two labels.}
#'     \item{level}{Integer level compared.}
#'     \item{segment_sizes}{A `data.frame` with columns `label`,
#'       `segment_idx`, and `n_members`.}
#'     \item{jaccard}{A numeric matrix of Jaccard indices, rows correspond
#'       to seg1 segments and columns to seg2 segments.}
#'     \item{ari}{Numeric scalar, the Adjusted Rand Index.}
#'     \item{quality_delta}{A `data.frame` of quality metric differences
#'       (seg2 minus seg1), or `NULL` if quality computation fails.}
#'     \item{asymmetry_delta}{A `data.frame` with columns
#'       `segment_id_1`, `asymmetry_1`, `segment_id_2`, `asymmetry_2`,
#'       or `NULL` if computation fails.}
#'   }
#'
#' @examples
#' \dontrun{
#' mob <- generate_mobility_data(n_classes = 6, seed = 42)
#' seg1 <- moneca(mob, segment.levels = 3)
#' seg2 <- refine_segments(seg1, threshold = 0.3, level = 2)
#' cmp <- compare_moneca_results(seg1, seg2, level = 2,
#'   labels = c("Original", "Refined"))
#' print(cmp)
#' }
#'
#' @keywords internal
compare_moneca_results <- function(
  seg1,
  seg2,
  level = 2,
  labels = c("Result 1", "Result 2")
) {
  if (!inherits(seg1, "moneca") || !inherits(seg2, "moneca")) {
    stop("Both 'seg1' and 'seg2' must be moneca objects.", call. = FALSE)
  }
  if (!is.numeric(level) || length(level) != 1 || level < 1) {
    stop("'level' must be a single positive integer.", call. = FALSE)
  }
  if (length(labels) != 2) {
    stop("'labels' must be a character vector of length 2.", call. = FALSE)
  }

  # Category names (excluding margin row)
  names1 <- rownames(seg1$mat.list[[1]])
  names1 <- names1[-length(names1)]
  names2 <- rownames(seg2$mat.list[[1]])
  names2 <- names2[-length(names2)]
  n <- length(names1)

  if (n != length(names2)) {
    stop(
      "The two moneca objects have different numbers of categories (",
      n,
      " vs ",
      length(names2),
      ").",
      call. = FALSE
    )
  }

  seg_list1 <- seg1$segment.list[[level]]
  seg_list2 <- seg2$segment.list[[level]]

  # 8a. Segment sizes -----
  sizes1 <- data.frame(
    label = labels[1],
    segment_idx = seq_along(seg_list1),
    n_members = vapply(seg_list1, length, integer(1)),
    stringsAsFactors = FALSE
  )
  sizes2 <- data.frame(
    label = labels[2],
    segment_idx = seq_along(seg_list2),
    n_members = vapply(seg_list2, length, integer(1)),
    stringsAsFactors = FALSE
  )
  segment_sizes <- rbind(sizes1, sizes2)

  # 8b. Membership vectors -----
  mem1 <- integer(n)
  for (s in seq_along(seg_list1)) {
    mem1[seg_list1[[s]]] <- s
  }
  mem2 <- integer(n)
  for (s in seq_along(seg_list2)) {
    mem2[seg_list2[[s]]] <- s
  }

  # 8c. Jaccard overlap matrix -----
  n1 <- length(seg_list1)
  n2 <- length(seg_list2)
  jaccard_mat <- matrix(0, nrow = n1, ncol = n2)
  rownames(jaccard_mat) <- paste0(labels[1], "_S", seq_len(n1))
  colnames(jaccard_mat) <- paste0(labels[2], "_S", seq_len(n2))

  for (i in seq_len(n1)) {
    set_a <- seg_list1[[i]]
    for (j in seq_len(n2)) {
      set_b <- seg_list2[[j]]
      inter <- length(intersect(set_a, set_b))
      union_size <- length(union(set_a, set_b))
      jaccard_mat[i, j] <- if (union_size == 0) 0 else inter / union_size
    }
  }

  # 8d. Adjusted Rand Index -----
  ari <- .adjusted_rand_index(mem1, mem2)

  # 8e. Quality comparison -----
  quality_delta <- tryCatch(
    {
      q1 <- segment.quality(seg1)
      q2 <- segment.quality(seg2)
      # Extract numeric columns shared between both
      num_cols1 <- vapply(q1, is.numeric, logical(1))
      num_cols2 <- vapply(q2, is.numeric, logical(1))
      data.frame(
        metric = "segment_quality",
        label_1 = labels[1],
        mean_within_mobility_1 = if (any(num_cols1)) {
          mean(q1[[which(num_cols1)[1]]], na.rm = TRUE)
        } else {
          NA_real_
        },
        label_2 = labels[2],
        mean_within_mobility_2 = if (any(num_cols2)) {
          mean(q2[[which(num_cols2)[1]]], na.rm = TRUE)
        } else {
          NA_real_
        },
        stringsAsFactors = FALSE
      )
    },
    error = function(e) NULL
  )

  # 8f. Asymmetry comparison -----
  asymmetry_delta <- tryCatch(
    {
      a1 <- compute_asymmetry_scores(seg1, level = level)
      a2 <- compute_asymmetry_scores(seg2, level = level)
      list(
        asymmetry_1 = a1,
        asymmetry_2 = a2,
        mean_asym_1 = mean(a1$asymmetry_score, na.rm = TRUE),
        mean_asym_2 = mean(a2$asymmetry_score, na.rm = TRUE)
      )
    },
    error = function(e) NULL
  )

  result <- list(
    labels = labels,
    level = level,
    segment_sizes = segment_sizes,
    jaccard = jaccard_mat,
    ari = ari,
    quality_delta = quality_delta,
    asymmetry_delta = asymmetry_delta
  )
  class(result) <- "moneca_comparison"
  result
}

# 9. print.moneca_comparison -----

#' Print Method for moneca_comparison Objects
#'
#' Displays a structured summary of the comparison between two MONECA
#' clustering results.
#'
#' @param x A `moneca_comparison` object returned by
#'   [compare_moneca_results()].
#' @param ... Additional arguments (currently unused).
#'
#' @return Invisibly returns `x`.
#'
#' @keywords internal
print.moneca_comparison <- function(x, ...) {
  cat("=== MONECA Comparison ===\n")
  cat("Level:", x$level, "\n")
  cat("Labels:", x$labels[1], "vs", x$labels[2], "\n\n")

  # ARI
  cat("Adjusted Rand Index:", round(x$ari, 4), "\n\n")

  # Segment sizes
  cat("-- Segment Sizes --\n")
  for (lab in x$labels) {
    sub <- x$segment_sizes[x$segment_sizes$label == lab, ]
    cat(
      lab,
      ": ",
      nrow(sub),
      " segments, sizes = [",
      paste(sub$n_members, collapse = ", "),
      "]\n",
      sep = ""
    )
  }
  cat("\n")

  # Top Jaccard overlaps
  cat("-- Top Jaccard Overlaps --\n")
  jac_vals <- as.vector(x$jaccard)
  jac_order <- order(jac_vals, decreasing = TRUE)
  n_show <- min(5, length(jac_vals))
  for (k in seq_len(n_show)) {
    idx <- jac_order[k]
    row_idx <- ((idx - 1) %% nrow(x$jaccard)) + 1
    col_idx <- ((idx - 1) %/% nrow(x$jaccard)) + 1
    if (jac_vals[idx] > 0) {
      cat(
        "  ",
        rownames(x$jaccard)[row_idx],
        " <-> ",
        colnames(x$jaccard)[col_idx],
        ": ",
        round(jac_vals[idx], 3),
        "\n",
        sep = ""
      )
    }
  }
  cat("\n")

  # Quality delta
  if (!is.null(x$quality_delta)) {
    cat("-- Quality --\n")
    cat(
      "  ",
      x$labels[1],
      " mean within-mobility: ",
      round(x$quality_delta$mean_within_mobility_1, 4),
      "\n",
      sep = ""
    )
    cat(
      "  ",
      x$labels[2],
      " mean within-mobility: ",
      round(x$quality_delta$mean_within_mobility_2, 4),
      "\n",
      sep = ""
    )
    cat("\n")
  }

  # Asymmetry delta
  if (!is.null(x$asymmetry_delta)) {
    cat("-- Asymmetry --\n")
    cat(
      "  ",
      x$labels[1],
      " mean asymmetry: ",
      round(x$asymmetry_delta$mean_asym_1, 4),
      "\n",
      sep = ""
    )
    cat(
      "  ",
      x$labels[2],
      " mean asymmetry: ",
      round(x$asymmetry_delta$mean_asym_2, 4),
      "\n",
      sep = ""
    )
  }

  invisible(x)
}
