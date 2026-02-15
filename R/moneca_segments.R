# Canonical Segment Metadata — moneca_segments S3 Class
# =====================================================
#
# Single source of truth for segment naming, group membership, and
# representative-node selection across all MONECA functions.

#' Canonical Segment Metadata for MONECA Results
#'
#' Constructs a \code{moneca_segments} object that provides a single,
#' authoritative description of every segment at every hierarchical level.
#' Group membership is built using the same progressive-merge algorithm as
#' \code{\link{level.matrix}}, and the representative node for each group is
#' chosen by maximising total directed strength in the asymmetric weight
#' matrix.
#'
#' @param segments A moneca object returned by \code{\link{moneca}} or
#'   \code{\link{moneca_fast}}.
#'
#' @return A \code{moneca_segments} object (S3 class) containing:
#' \describe{
#'   \item{\code{levels}}{A list with one entry per hierarchical level.
#'     Each entry has \code{$groups} (list of integer vectors of member
#'     indices) and \code{$map} (data.frame with columns \code{segment},
#'     \code{main_node}, \code{members}, \code{n_members}, \code{label}).}
#'   \item{\code{weight_matrix}}{The asymmetric relative-risk matrix used
#'     for centrality computation (no \code{NA} values).}
#'   \item{\code{original_names}}{Character vector of node names (excluding
#'     "Total").}
#'   \item{\code{n_levels}}{Integer, number of hierarchical levels.}
#'   \item{\code{small.cell.reduction}}{Numeric, the \code{small.cell.reduction}
#'     parameter stored in the moneca object.}
#' }
#'
#' @export
#'
#' @examples
#' mob <- generate_mobility_data(n_classes = 6, n_total = 500, seed = 42)
#' seg <- moneca_fast(mob, segment.levels = 2)
#' meta <- moneca_segments(seg)
#' print(meta)
#' get_segment_map(meta, level = 2)
moneca_segments <- function(segments) {
  if (!inherits(segments, "moneca")) {
    stop(
      "'segments' must be a moneca object returned by moneca() or moneca_fast().",
      call. = FALSE
    )
  }

  # 1. Original node names (excluding Total) -----
  all_names <- rownames(segments$mat.list[[1]])
  total_idx <- which(all_names == "Total")
  if (length(total_idx) > 0) {
    original_names <- all_names[-total_idx]
  } else {
    original_names <- all_names
  }
  n_nodes <- length(original_names)

  # 2. Asymmetric weight matrix for centrality -----
  scr <- if (!is.null(segments$small.cell.reduction)) {
    segments$small.cell.reduction
  } else {
    5
  }
  wm <- weight.matrix(
    segments$mat.list[[1]],
    cut.off = 1,
    symmetric = FALSE,
    small.cell.reduction = scr
  )
  wm[is.na(wm)] <- 0

  # 3. Number of levels -----
  n_levels <- length(segments$segment.list)

  # 4. Build levels list -----
  levels_out <- vector("list", n_levels)

  # Level 1 is trivial: each node alone
  level1_groups <- as.list(seq_len(n_nodes))
  level1_map <- data.frame(
    segment = seq_len(n_nodes),
    main_node = original_names,
    members = original_names,
    n_members = rep(1L, n_nodes),
    label = original_names,
    stringsAsFactors = FALSE
  )
  levels_out[[1]] <- list(groups = level1_groups, map = level1_map)

  # Higher levels: progressive merge
  if (n_levels >= 2) {
    groups <- level1_groups

    for (lvl in 2:n_levels) {
      cliques_lvl <- segments$segment.list[[lvl]]
      new_groups <- list()
      merged <- logical(length(groups))

      for (cl in cliques_lvl) {
        to_merge <- which(vapply(
          groups,
          function(g) all(g %in% cl),
          logical(1)
        ))
        new_groups <- c(new_groups, list(unlist(groups[to_merge])))
        merged[to_merge] <- TRUE
      }

      # Carry forward groups not absorbed by any clique
      remaining <- which(!merged)
      for (idx in remaining) {
        new_groups <- c(new_groups, list(groups[[idx]]))
      }

      groups <- new_groups

      # Build map for this level
      n_groups <- length(groups)
      map_df <- data.frame(
        segment = integer(n_groups),
        main_node = character(n_groups),
        members = character(n_groups),
        n_members = integer(n_groups),
        label = character(n_groups),
        stringsAsFactors = FALSE
      )

      for (i in seq_len(n_groups)) {
        members_idx <- groups[[i]]
        member_names <- original_names[members_idx]

        if (length(members_idx) == 1L) {
          main_node <- member_names
        } else {
          sub_wm <- wm[members_idx, members_idx, drop = FALSE]
          str_val <- rowSums(sub_wm) + colSums(sub_wm)
          main_node <- member_names[which.max(str_val)]
        }

        n_extra <- length(members_idx) - 1L
        label <- if (n_extra > 0L) {
          paste0(main_node, "+", n_extra)
        } else {
          main_node
        }

        map_df$segment[i] <- i
        map_df$main_node[i] <- main_node
        map_df$members[i] <- paste(member_names, collapse = ", ")
        map_df$n_members[i] <- length(members_idx)
        map_df$label[i] <- label
      }

      levels_out[[lvl]] <- list(groups = groups, map = map_df)
    }
  }

  structure(
    list(
      levels = levels_out,
      weight_matrix = wm,
      original_names = original_names,
      n_levels = n_levels,
      small.cell.reduction = scr
    ),
    class = "moneca_segments"
  )
}


#' Print Method for moneca_segments Objects
#'
#' @param x A \code{moneca_segments} object.
#' @param ... Additional arguments (ignored).
#'
#' @return Invisibly returns \code{x}.
#'
#' @export
print.moneca_segments <- function(x, ...) {
  cat("moneca_segments object\n")
  cat("  Nodes:", length(x$original_names), "\n")
  cat("  Levels:", x$n_levels, "\n")
  for (lvl in seq_len(x$n_levels)) {
    n_groups <- length(x$levels[[lvl]]$groups)
    cat(sprintf(
      "    Level %d: %d group%s\n",
      lvl,
      n_groups,
      if (n_groups != 1) "s" else ""
    ))
  }
  invisible(x)
}


# 5. Accessor functions -----

#' Get Segment Map at a Given Level
#'
#' Returns the segment map data frame for a specific hierarchical level.
#'
#' @param meta A \code{moneca_segments} object.
#' @param level Integer. The hierarchical level (default 2).
#'
#' @return A data.frame with columns \code{segment}, \code{main_node},
#'   \code{members}, \code{n_members}, and \code{label}.
#'
#' @export
#'
#' @examples
#' mob <- generate_mobility_data(n_classes = 6, n_total = 500, seed = 42)
#' seg <- moneca_fast(mob, segment.levels = 2)
#' meta <- moneca_segments(seg)
#' get_segment_map(meta, level = 2)
get_segment_map <- function(meta, level = 2) {
  if (!inherits(meta, "moneca_segments")) {
    stop("'meta' must be a moneca_segments object.", call. = FALSE)
  }
  if (level < 1 || level > meta$n_levels) {
    stop(
      "'level' must be between 1 and ",
      meta$n_levels,
      ".",
      call. = FALSE
    )
  }
  meta$levels[[level]]$map
}


#' Get Segment Groups at a Given Level
#'
#' Returns the list of integer vectors representing group membership.
#'
#' @param meta A \code{moneca_segments} object.
#' @param level Integer. The hierarchical level (default 2).
#'
#' @return A list of integer vectors, one per group, containing member indices.
#'
#' @export
#'
#' @examples
#' mob <- generate_mobility_data(n_classes = 6, n_total = 500, seed = 42)
#' seg <- moneca_fast(mob, segment.levels = 2)
#' meta <- moneca_segments(seg)
#' get_segment_groups(meta, level = 2)
get_segment_groups <- function(meta, level = 2) {
  if (!inherits(meta, "moneca_segments")) {
    stop("'meta' must be a moneca_segments object.", call. = FALSE)
  }
  if (level < 1 || level > meta$n_levels) {
    stop(
      "'level' must be between 1 and ",
      meta$n_levels,
      ".",
      call. = FALSE
    )
  }
  meta$levels[[level]]$groups
}


#' Get Representative Node for a Segment
#'
#' Returns the name of the most central node in a specific group.
#'
#' @param meta A \code{moneca_segments} object.
#' @param level Integer. The hierarchical level.
#' @param group_id Integer. The group (segment) index.
#'
#' @return A character string: the representative node name.
#'
#' @export
#'
#' @examples
#' mob <- generate_mobility_data(n_classes = 6, n_total = 500, seed = 42)
#' seg <- moneca_fast(mob, segment.levels = 2)
#' meta <- moneca_segments(seg)
#' get_representative(meta, level = 2, group_id = 1)
get_representative <- function(meta, level, group_id) {
  if (!inherits(meta, "moneca_segments")) {
    stop("'meta' must be a moneca_segments object.", call. = FALSE)
  }
  if (level < 1 || level > meta$n_levels) {
    stop(
      "'level' must be between 1 and ",
      meta$n_levels,
      ".",
      call. = FALSE
    )
  }
  map_df <- meta$levels[[level]]$map
  if (group_id < 1 || group_id > nrow(map_df)) {
    stop(
      "'group_id' must be between 1 and ",
      nrow(map_df),
      ".",
      call. = FALSE
    )
  }
  map_df$main_node[group_id]
}


#' Get Segment Label
#'
#' Returns the formatted label for a specific group (e.g., \code{"A+2"} for a
#' group of 3 with representative \code{"A"}).
#'
#' @param meta A \code{moneca_segments} object.
#' @param level Integer. The hierarchical level.
#' @param group_id Integer. The group (segment) index.
#'
#' @return A character string: the formatted label.
#'
#' @export
#'
#' @examples
#' mob <- generate_mobility_data(n_classes = 6, n_total = 500, seed = 42)
#' seg <- moneca_fast(mob, segment.levels = 2)
#' meta <- moneca_segments(seg)
#' get_segment_label(meta, level = 2, group_id = 1)
get_segment_label <- function(meta, level, group_id) {
  if (!inherits(meta, "moneca_segments")) {
    stop("'meta' must be a moneca_segments object.", call. = FALSE)
  }
  if (level < 1 || level > meta$n_levels) {
    stop(
      "'level' must be between 1 and ",
      meta$n_levels,
      ".",
      call. = FALSE
    )
  }
  map_df <- meta$levels[[level]]$map
  if (group_id < 1 || group_id > nrow(map_df)) {
    stop(
      "'group_id' must be between 1 and ",
      nrow(map_df),
      ".",
      call. = FALSE
    )
  }
  map_df$label[group_id]
}
