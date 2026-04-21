# Hierarchical multi-level MONECA visualization
# =================================================
#
# Top-down recursive circle-partitioning layout: the most-aggregated level
# occupies non-overlapping circular regions; each region is recursively
# partitioned into its children down to individual base nodes.

# 0. Suppress R CMD check NOTEs for variables used inside aes() -----
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    "cx",
    "cy",
    "r",
    "x",
    "y",
    "x0",
    "y0",
    "x1",
    "y1",
    "x2",
    "y2",
    "xend",
    "lw",
    "w",
    "segment_id",
    "level_assigned",
    "parent_segment_id",
    "label",
    "name",
    "segment_label"
  ))
}


# 1. Public exported function -----

#' Hierarchical Circular Layout for MONECA Segments
#'
#' @title Hierarchical Circular Layout for MONECA Segments
#'
#' @description
#' Produces a single `ggplot` object in which each top-level MONECA segment
#' occupies a non-overlapping circular region, and each region is recursively
#' subdivided into its children all the way down to individual base nodes.
#' The result eliminates the hull-overlap artefact of `plot_moneca_ggraph()`
#' because the layout is constructed top-down: parent regions are partitioned
#' first, then each child is placed strictly inside its parent.
#'
#' Three label tiers are drawn:
#' * **Top tier** – top-level segment labels (bold, large).
#' * **Sub tier** – labels from `top_level - 1` at sub-region centroids (plain,
#'   smaller). When `top_level == 2` the sub tier coincides with leaf (level-1)
#'   node names, which is documented as graceful degradation.
#' * **Leaf tier** – base-node names, drawn only when
#'   `show_node_labels = TRUE`.
#'
#' @param segments A `moneca` object returned by [moneca()] or
#'   [moneca_fast()].
#' @param top_level Integer. The highest segmentation level to use as the
#'   outer partition. Defaults to `meta$n_levels`. Must be `>= 2`.
#' @param levels_to_show Integer vector. Which levels to render as filled
#'   regions. Defaults to `seq_len(top_level)`.
#' @param layout Character. Meta-graph layout method for placing child circles
#'   inside their parent. One of `"circle_pack"` (uses `packcircles` if
#'   installed, otherwise falls back to `"fr_pack"`), `"fr_pack"` (FR layout
#'   on the meta-adjacency graph), or `"stress_pack"` (stress-based layout
#'   via `ggraph::create_layout`). Default `"circle_pack"`.
#' @param sub_layout Character. Layout algorithm used for positioning base
#'   nodes inside each leaf-level region. One of `"fr"` (Fruchterman–Reingold),
#'   `"stress"`, or `"kk"` (Kamada–Kawai). Default `"fr"`.
#' @param region_padding Numeric in [0, 1). Fraction of a region's radius
#'   reserved as padding between the region boundary and its children.
#'   Default `0.15`.
#' @param region_shape Character. How regions are drawn:
#'   `"circle"` draws exact circles (strict non-overlap guarantee);
#'   `"hull"` draws smoothed convex hulls over leaf nodes (matches the
#'   aesthetic of `plot_moneca_ggraph()`);
#'   `"auto"` uses circles for the top level and hulls for sub-levels.
#'   Default `"hull"`.
#' @param region_radius Character. How child-circle radii are sized:
#'   `"sqrt"` (area-proportional to member count),
#'   `"linear"` (radius proportional to member count),
#'   `"equal"` (all children get the same radius).
#'   Default `"sqrt"`.
#' @param show_edges Character. Which edges to draw:
#'   `"leaf"` renders base-level edges from `base_weights`;
#'   `"top"` renders edges between top-level region centroids;
#'   `"none"` suppresses edges entirely.
#'   Default `"leaf"`.
#' @param show_node_labels Logical. Whether to draw base-node name labels
#'   (leaf tier). Default `FALSE`.
#' @param label_size_top Numeric. Font size for top-tier labels. Default `6`.
#' @param label_size_sub Numeric. Font size for sub-tier labels. Default `3.5`.
#' @param label_size_leaf Numeric. Font size for leaf-tier labels. Default `2`.
#' @param node_size Character or numeric. `"total"` sizes nodes by marginal
#'   total from the mobility matrix; `"mobility"` by off-diagonal mobility
#'   rate; a single number uses a fixed size. Default `"total"`.
#' @param node_color Character. `"segment"` colours nodes by their top-level
#'   segment; any valid colour string applies a fixed colour. Default
#'   `"segment"`.
#' @param node_alpha Numeric in [0, 1]. Node opacity. Default `0.85`.
#' @param edge_alpha Numeric in [0, 1]. Edge opacity. Default `0.4`.
#' @param edge_color Character. Edge colour. Default `"grey55"`.
#' @param stroke_top Numeric. Stroke width of the outermost (top-level)
#'   region boundary. Inner levels scale proportionally. Default `1.2`.
#' @param alpha_top Numeric. Fill alpha of the outermost region. Inner
#'   levels scale proportionally. Default `0.25`.
#' @param color_palette Character. Colour palette name. Supports
#'   `"okabe-ito"` (CVD-safe default), viridis families, and any
#'   RColorBrewer qualitative palette. Default `"okabe-ito"`.
#' @param theme_style Character. `ggplot2` theme base: `"void"`, `"minimal"`,
#'   or `"classic"`. Default `"void"`.
#' @param title Character or `NULL`. Plot title. Default `NULL`.
#' @param seed Integer or `NULL`. Random seed for reproducible non-deterministic
#'   layouts (FR, stress, KK). When non-`NULL` the current `.Random.seed` is
#'   saved before and restored after each layout call. Default `NULL`.
#' @param ... Additional arguments (currently unused, reserved for future
#'   extensions).
#'
#' @return A `ggplot` object. Print it or pass it to `ggsave()`.
#'
#' @details
#' **Algorithm overview** — The canvas is treated as a unit circle centred at
#' the origin. The top-level segments partition this space via
#' `.hp_meta_layout()`, which assigns each segment a non-overlapping sub-circle
#' with radius proportional to its member count (controlled by
#' `region_radius`). Each sub-circle is then recursively partitioned in the
#' same way, down to individual base nodes.
#'
#' **`segment.edges()` is not used** for meta-adjacency. That function returns
#' a base-node × base-node matrix, not a segment × segment matrix. The
#' inter-segment meta-adjacency is built internally by aggregating
#' `base_weights` (the level-1 relative-risk matrix) across group member
#' pairs.
#'
#' **`top_level == 2` degradation** — When `top_level == 2` the sub tier
#' (`top_level - 1 == 1`) refers to individual base nodes, so sub-tier labels
#' repeat the leaf-node names. This is expected behaviour.
#'
#' **Hull vs. circle** — Convex hulls (`region_shape = "hull"`) may expand
#' slightly outside the bounding circle due to `ggforce::geom_shape(expand)`.
#' Use `region_shape = "circle"` for a strict non-overlap guarantee.
#'
#' **`packcircles`** — When `layout = "circle_pack"` and `packcircles` is not
#' installed, a `message()` is emitted and the function falls back to
#' `layout = "fr_pack"` automatically.
#'
#' @examples
#' \dontrun{
#' mob <- generate_mobility_data(n_classes = 10, seed = 42)
#' seg <- moneca(mob, segment.levels = 3)
#' p <- plot_moneca_hierarchical(seg, seed = 1)
#' print(p)
#'
#' # Explicit top level with circle regions
#' p2 <- plot_moneca_hierarchical(
#'   seg,
#'   top_level    = 3,
#'   region_shape = "circle",
#'   layout       = "fr_pack",
#'   title        = "Hierarchical segments",
#'   seed         = 42
#' )
#' print(p2)
#' }
#'
#' @seealso [plot_moneca_ggraph()] for the standard network layout,
#'   [moneca()] for the clustering algorithm.
#'
#' @export
plot_moneca_hierarchical <- function(
  segments,
  top_level = NULL,
  levels_to_show = NULL,
  layout = c("circle_pack", "fr_pack", "stress_pack"),
  sub_layout = c("fr", "stress", "kk"),
  region_padding = 0.15,
  region_shape = c("hull", "circle", "auto"),
  region_radius = c("sqrt", "linear", "equal"),
  show_edges = c("leaf", "top", "none"),
  show_node_labels = FALSE,
  label_size_top = 6,
  label_size_sub = 3.5,
  label_size_leaf = 2,
  node_size = "total",
  node_color = "segment",
  node_alpha = 0.85,
  edge_alpha = 0.4,
  edge_color = "grey55",
  stroke_top = 1.2,
  alpha_top = 0.25,
  color_palette = "okabe-ito",
  theme_style = c("void", "minimal", "classic"),
  title = NULL,
  seed = NULL,
  ...
) {
  # 1. Match multi-choice arguments -----
  layout <- match.arg(layout)
  sub_layout <- match.arg(sub_layout)
  region_shape <- match.arg(region_shape)
  region_radius <- match.arg(region_radius)
  show_edges <- match.arg(show_edges)
  theme_style <- match.arg(theme_style)

  # 2. Validate inputs -----
  .hp_validate_inputs(segments, top_level, levels_to_show)

  # 3. Metadata -----
  meta <- .get_metadata(segments)

  # 4. Resolve top_level -----
  if (is.null(top_level)) {
    top_level <- meta$n_levels
  }
  if (top_level < 2) {
    stop(
      "Hierarchical plotting requires at least 2 levels; ",
      "this segments object has only 1.",
      call. = FALSE
    )
  }
  top_level <- as.integer(top_level)

  # 5. Resolve levels_to_show -----
  if (is.null(levels_to_show)) {
    levels_to_show <- seq_len(top_level)
  }

  # 6. Base weight matrix -----
  base_weights <- weight.matrix(
    segments$mat.list[[1]],
    cut.off = 1,
    symmetric = FALSE,
    small.cell.reduction = meta$small.cell.reduction
  )
  base_weights[is.na(base_weights)] <- 0

  # 7. Meta-edges: segment-to-segment adjacency at each level -----
  #    Built by aggregating base_weights over group member pairs.
  #    segment.edges() is NOT used here because it operates in base-node
  #    space, not segment space.
  meta_edges <- .hp_collect_meta_edges(base_weights, meta, top_level)

  # 7b. Resolve circle_pack -> fr_pack fallback ONCE if packcircles missing,
  #     so the recursive layout doesn't emit the message at every level.
  if (
    identical(layout, "circle_pack") &&
      !requireNamespace("packcircles", quietly = TRUE)
  ) {
    message(
      "packcircles not installed; falling back to layout = 'fr_pack'. ",
      "Install packcircles for the circle-pack layout."
    )
    layout <- "fr_pack"
  }

  # 8. Segment colours -----
  segment_colors <- .compute_segment_colors(meta, levels_to_show, color_palette)

  # 9. Node sizes -----
  mobility_matrix <- segments$mat.list[[1]]
  n_nodes <- nrow(base_weights)

  if (identical(node_size, "total")) {
    node_totals <- (mobility_matrix[nrow(mobility_matrix), seq_len(n_nodes)] +
      mobility_matrix[seq_len(n_nodes), ncol(mobility_matrix)]) /
      2
    node_sz <- as.numeric(node_totals)
    # Normalise to [2, 10] for plotting
    rng <- range(node_sz, na.rm = TRUE)
    if (diff(rng) > 0) {
      node_sz <- 2 + 8 * (node_sz - rng[1]) / diff(rng)
    } else {
      node_sz <- rep(5, n_nodes)
    }
  } else if (identical(node_size, "mobility")) {
    mob_mat_sq <- mobility_matrix[seq_len(n_nodes), seq_len(n_nodes)]
    mob_rates <- 1 - diag(mob_mat_sq) / pmax(rowSums(mob_mat_sq), 1e-9)
    node_sz <- 2 + 8 * pmin(pmax(mob_rates, 0), 1)
  } else if (is.numeric(node_size) && length(node_size) == n_nodes) {
    node_sz <- node_size
  } else {
    node_sz <- rep(5, n_nodes)
  }

  # 10. Recursive layout -----
  #     Seed recursion at a virtual root one level above top_level so the
  #     first .hp_layout_region call partitions ALL nodes into the
  #     meta$levels[[top_level]]$groups children. The virtual root region
  #     (level = top_level + 1, segment_id = 0) is filtered out before
  #     rendering and before the public attribute is attached.
  rng_seed <- if (!is.null(seed)) {
    if (exists(".Random.seed", envir = globalenv())) {
      get(".Random.seed", envir = globalenv())
    } else {
      NULL
    }
  }
  if (!is.null(seed)) {
    set.seed(seed)
  }

  layout_result <- .hp_layout_region(
    meta = meta,
    level = top_level + 1L,
    member_indices = seq_len(n_nodes),
    center = c(0, 0),
    radius = 1,
    base_weights = base_weights,
    meta_edges = meta_edges,
    sub_layout = sub_layout,
    layout = layout,
    padding = region_padding,
    radius_mode = region_radius,
    segment_id = 0L,
    parent_id = NA_integer_
  )

  if (!is.null(seed) && !is.null(rng_seed)) {
    assign(".Random.seed", rng_seed, envir = globalenv())
  }

  # Drop the virtual root region (level = top_level + 1, segment_id = 0):
  # it exists only to seed the recursion. Rendering and the public
  # attribute see only real regions at levels 1..top_level.
  layout_result$regions <- layout_result$regions[
    layout_result$regions$level <= top_level,
    ,
    drop = FALSE
  ]

  # 11. Render -----
  params <- list(
    region_shape = region_shape,
    show_edges = show_edges,
    show_node_labels = show_node_labels,
    label_size_top = label_size_top,
    label_size_sub = label_size_sub,
    label_size_leaf = label_size_leaf,
    node_alpha = node_alpha,
    edge_alpha = edge_alpha,
    edge_color = edge_color,
    stroke_top = stroke_top,
    alpha_top = alpha_top,
    theme_style = theme_style,
    title = title,
    node_color = node_color,
    levels_to_show = levels_to_show,
    top_level = top_level
  )

  p <- .hp_render(
    layout_result = layout_result,
    segments = segments,
    meta = meta,
    base_weights = base_weights,
    node_sz = node_sz,
    segment_colors = segment_colors,
    params = params
  )

  # Attach layout data as attributes for programmatic inspection / testing.
  # Regions: data.frame(level, segment_id, cx, cy, r, parent_id) per region.
  # Leaves:  data.frame(node_idx, x, y) per base node.
  attr(p, "moneca_regions") <- layout_result$regions
  attr(p, "moneca_leaves") <- layout_result$leaves
  p
}


# 2. Private helpers (.hp_ prefix) -----

# 2a. Input validation -----
.hp_validate_inputs <- function(segments, top_level, levels_to_show) {
  if (is.null(segments)) {
    stop("segments is NULL.", call. = FALSE)
  }
  if (!inherits(segments, "moneca")) {
    stop(
      "segments must be a moneca object created by moneca() or moneca_fast().",
      call. = FALSE
    )
  }
  if (is.null(segments$mat.list) || length(segments$mat.list) == 0) {
    stop(
      "segments$mat.list is empty; the moneca object appears incomplete. ",
      "Please re-run moneca().",
      call. = FALSE
    )
  }
  if (is.null(segments$mat.list[[1]])) {
    stop(
      "segments$mat.list[[1]] is NULL; the moneca object appears incomplete. ",
      "Please re-run moneca().",
      call. = FALSE
    )
  }
  if (!is.null(top_level)) {
    if (!is.numeric(top_level) || length(top_level) != 1 || is.na(top_level)) {
      stop("top_level must be a single integer.", call. = FALSE)
    }
    if (top_level < 1) {
      stop("top_level must be >= 1.", call. = FALSE)
    }
  }
  if (!is.null(levels_to_show)) {
    if (!is.numeric(levels_to_show) || any(is.na(levels_to_show))) {
      stop(
        "levels_to_show must be a numeric vector with no NAs.",
        call. = FALSE
      )
    }
  }
  invisible(NULL)
}


# 2b. Collect meta-edge adjacency matrices per level -----
#
# Returns a list indexed from 2 to top_level. Entry k contains an
# n_groups_k x n_groups_k matrix of aggregated weights between the
# segment groups at level k. Weights are the sum of base_weights[i, j]
# over all cross-group pairs (i, j). The diagonal (within-group) is
# set to 0.
.hp_collect_meta_edges <- function(base_weights, meta, top_level) {
  result <- vector("list", top_level)

  for (k in seq(2, top_level)) {
    groups <- meta$levels[[k]]$groups
    ng <- length(groups)
    adj <- matrix(0, nrow = ng, ncol = ng)

    if (ng > 1) {
      for (i in seq_len(ng)) {
        for (j in seq_len(ng)) {
          if (i != j) {
            ii <- groups[[i]]
            jj <- groups[[j]]
            # Sum of all directed edges from group i to group j
            adj[i, j] <- sum(base_weights[ii, jj, drop = FALSE])
          }
        }
      }
    }
    result[[k]] <- adj
  }
  result
}


# 2c. Compute child-circle radius given member count -----
.hp_region_radius <- function(n_members, total, parent_r, padding, mode) {
  effective_r <- parent_r * (1 - padding)
  raw <- switch(
    mode,
    sqrt = sqrt(n_members / total),
    linear = n_members / total,
    equal = rep(1 / length(n_members), length(n_members))
  )
  # Normalise so that radii sum to a reasonable fraction of effective_r,
  # then scale so max(dist_from_parent + r) <= effective_r.
  # Here we return unnormalised proportional radii; the meta_layout
  # function applies the final scaling after placing centers.
  raw / sum(raw) * effective_r * 0.85
}


# 2d. Fit point coordinates into a bounding circle -----
#
# Recenters coords at origin, scales so all points lie within radius,
# then translates to center. Adds tiny jitter when all points coincide.
.hp_fit_into_circle <- function(coords, center, radius) {
  n <- nrow(coords)
  if (n == 0) {
    return(coords)
  }

  # Jitter degenerate case
  if (
    n == 1 ||
      (length(unique(coords[, 1])) == 1 &&
        length(unique(coords[, 2])) == 1)
  ) {
    coords <- coords +
      matrix(
        stats::runif(2 * n, -1e-3, 1e-3),
        nrow = n,
        ncol = 2
      )
  }

  # Recenter at origin
  centroid <- colMeans(coords)
  coords[, 1] <- coords[, 1] - centroid[1]
  coords[, 2] <- coords[, 2] - centroid[2]

  # Scale to unit circle
  max_norm <- max(sqrt(rowSums(coords^2)))
  if (max_norm > 0) {
    coords <- coords / max_norm
  }

  # Scale to radius and translate
  coords <- coords * radius
  coords[, 1] <- coords[, 1] + center[1]
  coords[, 2] <- coords[, 2] + center[2]
  coords
}


# 2e. Iterative circle-separation (pairwise repulsion) -----
#
# Pushes overlapping circle pairs apart. Pure base R, no dependencies.
# Returns updated centers matrix (n x 2).
.hp_separate_circles <- function(centers, radii, max_iter = 50) {
  n <- nrow(centers)
  if (n <= 1) {
    return(centers)
  }

  for (iter in seq_len(max_iter)) {
    moved <- FALSE
    for (i in seq_len(n - 1)) {
      for (j in seq(i + 1, n)) {
        dx <- centers[j, 1] - centers[i, 1]
        dy <- centers[j, 2] - centers[i, 2]
        dist_ij <- sqrt(dx^2 + dy^2)
        min_dist <- radii[i] + radii[j]
        if (dist_ij < min_dist - 1e-9) {
          moved <- TRUE
          overlap <- min_dist - dist_ij
          # Unit direction vector
          if (dist_ij < 1e-12) {
            angle <- stats::runif(1, 0, 2 * pi)
            ux <- cos(angle)
            uy <- sin(angle)
          } else {
            ux <- dx / dist_ij
            uy <- dy / dist_ij
          }
          push <- overlap / 2
          centers[i, 1] <- centers[i, 1] - ux * push
          centers[i, 2] <- centers[i, 2] - uy * push
          centers[j, 1] <- centers[j, 1] + ux * push
          centers[j, 2] <- centers[j, 2] + uy * push
        }
      }
    }
    if (!moved) break
  }
  centers
}


# 2f. Meta-layout: place child circles inside a parent circle -----
#
# Returns a data.frame(seg_id, cx, cy, r) with one row per child.
# seg_id is the 1-based index of the child within meta$levels[[level]]$groups.
.hp_meta_layout <- function(
  adj,
  n_members,
  method,
  parent_center,
  parent_radius,
  padding,
  radius_mode
) {
  n <- length(n_members)
  effective_r <- parent_radius * (1 - padding)

  # Compute proportional radii
  total_n <- sum(n_members)
  raw_radii <- switch(
    radius_mode,
    sqrt = sqrt(n_members / total_n),
    linear = n_members / total_n,
    equal = rep(1 / n, n)
  )
  # Normalise: scale so radii are usable but not too large
  raw_radii <- raw_radii / max(raw_radii)

  # 1-circle degenerate case: only one child
  if (n == 1) {
    return(data.frame(
      seg_id = 1L,
      cx = parent_center[1],
      cy = parent_center[2],
      r = effective_r,
      stringsAsFactors = FALSE
    ))
  }

  # --- Attempt circle_pack via packcircles ---
  if (identical(method, "circle_pack")) {
    if (requireNamespace("packcircles", quietly = TRUE)) {
      # circleProgressiveLayout returns data.frame(x, y, radius)
      pack <- packcircles::circleProgressiveLayout(
        x = raw_radii,
        sizetype = "radius"
      )
      raw_cx <- pack$x
      raw_cy <- pack$y
      raw_r <- pack$radius

      # Re-scale so that the bounding circle of the packing fits in effective_r
      dists <- sqrt(raw_cx^2 + raw_cy^2) + raw_r
      scale_factor <- effective_r / max(dists)
      final_cx <- raw_cx * scale_factor + parent_center[1]
      final_cy <- raw_cy * scale_factor + parent_center[2]
      final_r <- raw_r * scale_factor

      return(data.frame(
        seg_id = seq_len(n),
        cx = final_cx,
        cy = final_cy,
        r = final_r,
        stringsAsFactors = FALSE
      ))
    } else {
      message(
        "packcircles not installed; falling back to layout = 'fr_pack'. ",
        "Install packcircles for the circle-pack layout."
      )
      method <- "fr_pack"
    }
  }

  # --- FR-pack or stress-pack: lay out meta-nodes then assign radii ---
  # Build igraph meta-graph
  g_meta <- moneca_graph_from_adjacency(
    adj,
    mode = "directed",
    weighted = TRUE
  )

  any_edges <- igraph::ecount(g_meta) > 0

  if (identical(method, "stress_pack") && any_edges) {
    if (!requireNamespace("ggraph", quietly = TRUE)) {
      warning("ggraph not installed; falling back to fr_pack.")
      method <- "fr_pack"
    }
  }

  if (identical(method, "stress_pack") && any_edges) {
    tg <- tidygraph::as_tbl_graph(g_meta)
    layout_df <- ggraph::create_layout(tg, layout = "stress")
    raw_cx <- layout_df$x
    raw_cy <- layout_df$y
  } else {
    # fr_pack (also fallback for stress when no edges)
    if (any_edges) {
      coords_mat <- igraph::layout_with_fr(g_meta)
    } else {
      # Random layout for disconnected meta-graph
      coords_mat <- matrix(
        stats::runif(2 * n, -1, 1),
        nrow = n,
        ncol = 2
      )
    }
    raw_cx <- coords_mat[, 1]
    raw_cy <- coords_mat[, 2]
  }

  # Assign radii proportional to membership
  final_r <- raw_radii * effective_r * 0.5

  # Recenter at origin, then scale positions so max(dist + r) <= effective_r
  raw_cx <- raw_cx - mean(raw_cx)
  raw_cy <- raw_cy - mean(raw_cy)

  dists <- sqrt(raw_cx^2 + raw_cy^2)
  max_reach <- max(dists + final_r)
  if (max_reach > 1e-9) {
    shrink <- effective_r / max_reach
    raw_cx <- raw_cx * shrink
    raw_cy <- raw_cy * shrink
    final_r <- final_r * shrink
  }

  # Separate overlapping circles
  centers_mat <- matrix(c(raw_cx, raw_cy), ncol = 2)
  centers_mat <- .hp_separate_circles(centers_mat, final_r)

  # Re-check after separation and rescale if needed
  dists2 <- sqrt(centers_mat[, 1]^2 + centers_mat[, 2]^2)
  max_reach2 <- max(dists2 + final_r)
  if (max_reach2 > effective_r) {
    shrink2 <- effective_r / max_reach2
    centers_mat <- centers_mat * shrink2
    final_r <- final_r * shrink2
  }

  data.frame(
    seg_id = seq_len(n),
    cx = centers_mat[, 1] + parent_center[1],
    cy = centers_mat[, 2] + parent_center[2],
    r = final_r,
    stringsAsFactors = FALSE
  )
}


# 2g. Recursive region layout workhorse -----
#
# Returns list(
#   leaves  = data.frame(node_idx, x, y),
#   regions = data.frame(level, segment_id, cx, cy, r, parent_id)
# )
#
# segment_id is the 1-based group index within meta$levels[[level]]$groups.
# parent_id is the segment_id of the caller (NA for the root call).
# The root call should pass level = top_level and member_indices = all nodes.
.hp_layout_region <- function(
  meta,
  level,
  member_indices,
  center,
  radius,
  base_weights,
  meta_edges,
  sub_layout,
  layout,
  padding,
  radius_mode,
  segment_id,
  parent_id
) {
  n <- length(member_indices)

  # --- Singleton ---
  if (n == 1) {
    leaf_df <- data.frame(
      node_idx = member_indices,
      x = center[1],
      y = center[2],
      stringsAsFactors = FALSE
    )
    region_df <- data.frame(
      level = level,
      segment_id = segment_id,
      cx = center[1],
      cy = center[2],
      r = radius,
      parent_id = parent_id,
      stringsAsFactors = FALSE
    )
    return(list(leaves = leaf_df, regions = region_df))
  }

  # --- Leaf level (level == 1): place base nodes using sub_layout ---
  if (level == 1) {
    sub_adj <- base_weights[member_indices, member_indices, drop = FALSE]
    g_sub <- moneca_graph_from_adjacency(
      sub_adj,
      mode = "directed",
      weighted = TRUE
    )

    any_edges <- igraph::ecount(g_sub) > 0

    if (any_edges) {
      coords_mat <- switch(
        sub_layout,
        fr = igraph::layout_with_fr(g_sub),
        kk = igraph::layout_with_kk(g_sub),
        stress = {
          if (requireNamespace("ggraph", quietly = TRUE)) {
            tg_sub <- tidygraph::as_tbl_graph(g_sub)
            ld <- ggraph::create_layout(tg_sub, layout = "stress")
            as.matrix(ld[, c("x", "y")])
          } else {
            igraph::layout_with_fr(g_sub)
          }
        }
      )
    } else {
      coords_mat <- matrix(
        stats::runif(2 * n, -1, 1),
        nrow = n,
        ncol = 2
      )
    }

    coords_mat <- .hp_fit_into_circle(
      coords_mat,
      center,
      radius * (1 - padding)
    )

    leaf_df <- data.frame(
      node_idx = member_indices,
      x = coords_mat[, 1],
      y = coords_mat[, 2],
      stringsAsFactors = FALSE
    )
    region_df <- data.frame(
      level = level,
      segment_id = segment_id,
      cx = center[1],
      cy = center[2],
      r = radius,
      parent_id = parent_id,
      stringsAsFactors = FALSE
    )
    return(list(leaves = leaf_df, regions = region_df))
  }

  # --- Recursive case: level >= 2 ---
  # Identify children at level - 1 whose group is a subset of member_indices
  child_groups_all <- meta$levels[[level - 1]]$groups
  is_child <- vapply(
    child_groups_all,
    function(g) length(g) > 0 && all(g %in% member_indices),
    logical(1)
  )
  child_idx <- which(is_child)
  child_groups <- child_groups_all[child_idx]

  # If no children found (degenerate), treat as leaf
  if (length(child_idx) == 0) {
    return(.hp_layout_region(
      meta = meta,
      level = 1L,
      member_indices = member_indices,
      center = center,
      radius = radius,
      base_weights = base_weights,
      meta_edges = meta_edges,
      sub_layout = sub_layout,
      layout = layout,
      padding = padding,
      radius_mode = radius_mode,
      segment_id = segment_id,
      parent_id = parent_id
    ))
  }

  # Only one child: skip meta-layout, recurse directly
  if (length(child_idx) == 1) {
    child_result <- .hp_layout_region(
      meta = meta,
      level = level - 1L,
      member_indices = child_groups[[1]],
      center = center,
      radius = radius * (1 - padding),
      base_weights = base_weights,
      meta_edges = meta_edges,
      sub_layout = sub_layout,
      layout = layout,
      padding = padding,
      radius_mode = radius_mode,
      segment_id = child_idx[1],
      parent_id = segment_id
    )
    # Add current-level region record
    current_region <- data.frame(
      level = level,
      segment_id = segment_id,
      cx = center[1],
      cy = center[2],
      r = radius,
      parent_id = parent_id,
      stringsAsFactors = FALSE
    )
    return(list(
      leaves = child_result$leaves,
      regions = rbind(current_region, child_result$regions)
    ))
  }

  # Multiple children: build meta-adjacency for children
  meta_adj_full <- meta_edges[[level - 1]]

  if (
    !is.null(meta_adj_full) &&
      nrow(meta_adj_full) >= max(child_idx) &&
      ncol(meta_adj_full) >= max(child_idx)
  ) {
    meta_adj <- meta_adj_full[child_idx, child_idx, drop = FALSE]
  } else {
    # Fallback: build meta-adjacency directly from base_weights
    nc <- length(child_idx)
    meta_adj <- matrix(0, nrow = nc, ncol = nc)
    for (i in seq_len(nc)) {
      for (j in seq_len(nc)) {
        if (i != j) {
          ii <- child_groups[[i]]
          jj <- child_groups[[j]]
          meta_adj[i, j] <- sum(base_weights[ii, jj, drop = FALSE])
        }
      }
    }
  }

  child_n <- vapply(child_groups, length, integer(1))

  meta_layout_df <- .hp_meta_layout(
    adj = meta_adj,
    n_members = child_n,
    method = layout,
    parent_center = center,
    parent_radius = radius,
    padding = padding,
    radius_mode = radius_mode
  )

  # Accumulate current-level region record
  current_region <- data.frame(
    level = level,
    segment_id = segment_id,
    cx = center[1],
    cy = center[2],
    r = radius,
    parent_id = parent_id,
    stringsAsFactors = FALSE
  )

  all_leaves <- vector("list", length(child_idx))
  all_regions <- list(current_region)

  for (i in seq_along(child_idx)) {
    child_result <- .hp_layout_region(
      meta = meta,
      level = level - 1L,
      member_indices = child_groups[[i]],
      center = c(meta_layout_df$cx[i], meta_layout_df$cy[i]),
      radius = meta_layout_df$r[i],
      base_weights = base_weights,
      meta_edges = meta_edges,
      sub_layout = sub_layout,
      layout = layout,
      padding = padding,
      radius_mode = radius_mode,
      segment_id = child_idx[i],
      parent_id = segment_id
    )
    all_leaves[[i]] <- child_result$leaves
    all_regions <- c(all_regions, list(child_result$regions))
  }

  list(
    leaves = do.call(rbind, all_leaves),
    regions = do.call(rbind, all_regions)
  )
}


# 2h. Build convex hull polygons for each region at each level -----
#
# Returns a data.frame(x, y, level, segment_id) with hull vertices.
.hp_build_hull_polygons <- function(leaves, regions) {
  result_list <- vector("list", nrow(regions))

  for (i in seq_len(nrow(regions))) {
    lvl <- regions$level[i]
    seg_id <- regions$segment_id[i]

    # Find all leaf nodes belonging to this region by cross-referencing:
    # a leaf belongs to this region if its node_idx is a member of
    # meta$levels[[lvl]]$groups[[seg_id]].
    # Since we don't have meta here, use the region spatial bounding:
    # leaves whose coordinates are within radius of region center.
    cx_r <- regions$cx[i]
    cy_r <- regions$cy[i]
    r_r <- regions$r[i]
    dists_r <- sqrt((leaves$x - cx_r)^2 + (leaves$y - cy_r)^2)
    idx_r <- which(dists_r <= r_r * 1.05) # small tolerance

    if (length(idx_r) < 3) {
      next
    }

    hull_idx <- grDevices::chull(leaves$x[idx_r], leaves$y[idx_r])
    hull_pts <- leaves[idx_r[hull_idx], , drop = FALSE]

    result_list[[i]] <- data.frame(
      x = hull_pts$x,
      y = hull_pts$y,
      level = lvl,
      segment_id = seg_id,
      stringsAsFactors = FALSE
    )
  }

  result <- do.call(rbind, Filter(Negate(is.null), result_list))
  if (is.null(result)) {
    data.frame(
      x = numeric(0),
      y = numeric(0),
      level = integer(0),
      segment_id = integer(0),
      stringsAsFactors = FALSE
    )
  } else {
    result
  }
}


# 2i. Render: assemble ggplot layers -----
.hp_render <- function(
  layout_result,
  segments,
  meta,
  base_weights,
  node_sz,
  segment_colors,
  params
) {
  leaves <- layout_result$leaves
  regions <- layout_result$regions

  # Remove the root region (segment_id == 0, added by the top-level call)
  regions <- regions[regions$segment_id != 0L, , drop = FALSE]

  top_level <- params$top_level
  levels_to_show <- params$levels_to_show
  region_shape <- params$region_shape
  show_edges <- params$show_edges
  stroke_top <- params$stroke_top
  alpha_top <- params$alpha_top
  theme_style <- params$theme_style

  node_names <- meta$original_names

  # Node-level segment assignment (top level for colour)
  top_groups <- meta$levels[[top_level]]$groups
  top_map <- meta$levels[[top_level]]$map
  node_seg_label <- character(length(node_names))
  for (g in seq_along(top_groups)) {
    node_seg_label[top_groups[[g]]] <- top_map$label[g]
  }

  # Merge coordinates with node attributes
  node_df <- data.frame(
    node_idx = leaves$node_idx,
    x = leaves$x,
    y = leaves$y,
    name = node_names[leaves$node_idx],
    seg_label = node_seg_label[leaves$node_idx],
    node_size = node_sz[leaves$node_idx],
    stringsAsFactors = FALSE
  )

  # Assign segment colour per node
  node_color_vec <- if (identical(params$node_color, "segment")) {
    seg_cols <- segment_colors[node_df$seg_label]
    ifelse(is.na(seg_cols), "grey60", seg_cols)
  } else {
    rep(params$node_color, nrow(node_df))
  }
  node_df$color <- node_color_vec

  # Start ggplot with empty canvas (coord_equal handled at end)
  p <- ggplot2::ggplot() +
    ggplot2::coord_equal(
      xlim = c(-1.15, 1.15),
      ylim = c(-1.15, 1.15),
      expand = TRUE
    )

  # ---- Layer 1: Region fills (back to front, largest level first) ----
  # Draw levels in decreasing order so larger regions are drawn first
  levels_for_regions <- intersect(
    levels_to_show[order(levels_to_show, decreasing = TRUE)],
    unique(regions$level)
  )

  for (L in levels_for_regions) {
    regions_L <- regions[regions$level == L, , drop = FALSE]
    if (nrow(regions_L) == 0) {
      next
    }

    # Alpha and stroke decay with depth
    fill_alpha <- alpha_top * (L / top_level) * 0.6
    stroke_w <- stroke_top * (L / top_level)

    # Determine shape mode for this level
    draw_circle <- switch(
      region_shape,
      circle = TRUE,
      hull = FALSE,
      auto = (L == top_level)
    )

    for (i in seq_len(nrow(regions_L))) {
      seg_id_L <- regions_L$segment_id[i]
      cx_L <- regions_L$cx[i]
      cy_L <- regions_L$cy[i]
      r_L <- regions_L$r[i]

      # Segment label for colour lookup
      if (
        L <= meta$n_levels &&
          seg_id_L >= 1 &&
          seg_id_L <= length(meta$levels[[L]]$groups)
      ) {
        grp_L <- meta$levels[[L]]$groups[[seg_id_L]]
        # Representative node label from top-level assignment
        rep_label <- if (length(grp_L) > 0) node_seg_label[grp_L[1]] else ""
      } else {
        rep_label <- ""
      }

      fill_col <- if (
        !is.null(segment_colors) && rep_label %in% names(segment_colors)
      ) {
        segment_colors[rep_label]
      } else {
        "grey80"
      }

      if (draw_circle) {
        # Strict circle (ggforce)
        circ_df <- data.frame(x0 = cx_L, y0 = cy_L, r = r_L)
        if (requireNamespace("ggforce", quietly = TRUE)) {
          p <- p +
            ggforce::geom_circle(
              data = circ_df,
              ggplot2::aes(x0 = x0, y0 = y0, r = r),
              fill = fill_col,
              alpha = fill_alpha,
              color = "grey25",
              linewidth = stroke_w,
              inherit.aes = FALSE
            )
        } else {
          # Fallback polygon circle approximation
          theta <- seq(0, 2 * pi, length.out = 61)
          circ_poly <- data.frame(
            x = cx_L + r_L * cos(theta),
            y = cy_L + r_L * sin(theta)
          )
          p <- p +
            ggplot2::geom_polygon(
              data = circ_poly,
              ggplot2::aes(x = x, y = y),
              fill = fill_col,
              alpha = fill_alpha,
              color = "grey25",
              linewidth = stroke_w,
              inherit.aes = FALSE
            )
        }
      } else {
        # Hull rendering
        # Collect leaves for this region
        dists_L <- sqrt((leaves$x - cx_L)^2 + (leaves$y - cy_L)^2)
        in_region <- which(dists_L <= r_L * 1.05)

        if (length(in_region) >= 3) {
          hull_idx_L <- grDevices::chull(
            leaves$x[in_region],
            leaves$y[in_region]
          )
          hull_df <- data.frame(
            x = leaves$x[in_region[hull_idx_L]],
            y = leaves$y[in_region[hull_idx_L]]
          )
          if (requireNamespace("ggforce", quietly = TRUE)) {
            p <- p +
              ggforce::geom_shape(
                data = hull_df,
                ggplot2::aes(x = x, y = y),
                fill = fill_col,
                alpha = fill_alpha,
                expand = ggplot2::unit(0.15, "cm"),
                radius = ggplot2::unit(0.25, "cm"),
                color = "grey25",
                linewidth = stroke_w,
                show.legend = FALSE,
                inherit.aes = FALSE
              )
          } else {
            p <- p +
              ggplot2::geom_polygon(
                data = hull_df,
                ggplot2::aes(x = x, y = y),
                fill = fill_col,
                alpha = fill_alpha,
                color = "grey25",
                linewidth = stroke_w,
                show.legend = FALSE,
                inherit.aes = FALSE
              )
          }
        } else if (length(in_region) >= 1) {
          # Too few for hull: draw a small circle
          cxs <- mean(leaves$x[in_region])
          cys <- mean(leaves$y[in_region])
          small_r <- r_L * 0.4
          if (requireNamespace("ggforce", quietly = TRUE)) {
            sdf <- data.frame(x0 = cxs, y0 = cys, r = small_r)
            p <- p +
              ggforce::geom_circle(
                data = sdf,
                ggplot2::aes(x0 = x0, y0 = y0, r = r),
                fill = fill_col,
                alpha = fill_alpha,
                color = "grey25",
                linewidth = stroke_w,
                inherit.aes = FALSE
              )
          }
        }
      }
    }
  }

  # ---- Layer 2: Edges ----
  if (!identical(show_edges, "none")) {
    if (identical(show_edges, "leaf")) {
      # Build edge data from base_weights between all leaf pairs
      edge_rows <- list()
      nw <- nrow(base_weights)
      for (i in seq_len(nw)) {
        for (j in seq_len(nw)) {
          if (i != j && base_weights[i, j] > 0) {
            xi <- node_df$x[node_df$node_idx == i]
            yi <- node_df$y[node_df$node_idx == i]
            xj <- node_df$x[node_df$node_idx == j]
            yj <- node_df$y[node_df$node_idx == j]
            if (length(xi) == 1 && length(xj) == 1) {
              edge_rows <- c(
                edge_rows,
                list(data.frame(
                  x1 = xi,
                  y1 = yi,
                  x2 = xj,
                  y2 = yj,
                  w = base_weights[i, j],
                  stringsAsFactors = FALSE
                ))
              )
            }
          }
        }
      }
      if (length(edge_rows) > 0) {
        edge_df <- do.call(rbind, edge_rows)
        w_range <- range(edge_df$w, na.rm = TRUE)
        edge_df$lw <- if (diff(w_range) > 0) {
          0.1 + 1.5 * (edge_df$w - w_range[1]) / diff(w_range)
        } else {
          rep(0.5, nrow(edge_df))
        }
        p <- p +
          ggplot2::geom_segment(
            data = edge_df,
            ggplot2::aes(x = x1, y = y1, xend = x2, yend = y2, linewidth = lw),
            alpha = params$edge_alpha,
            color = params$edge_color,
            inherit.aes = FALSE,
            show.legend = FALSE
          ) +
          ggplot2::scale_linewidth_continuous(
            range = c(0.1, 1.8),
            guide = "none"
          )
      }
    } else if (identical(show_edges, "top")) {
      # Edges between top-level region centroids
      top_regions <- regions[regions$level == top_level, , drop = FALSE]
      meta_adj_top <- meta_edges_at_level(base_weights, meta, top_level)

      if (!is.null(meta_adj_top) && nrow(top_regions) > 1) {
        top_edge_rows <- list()
        for (i in seq_len(nrow(top_regions))) {
          for (j in seq_len(nrow(top_regions))) {
            if (i < j) {
              si <- top_regions$segment_id[i]
              sj <- top_regions$segment_id[j]
              if (
                si >= 1 &&
                  sj >= 1 &&
                  si <= nrow(meta_adj_top) &&
                  sj <= ncol(meta_adj_top)
              ) {
                wij <- meta_adj_top[si, sj] + meta_adj_top[sj, si]
                if (wij > 0) {
                  top_edge_rows <- c(
                    top_edge_rows,
                    list(data.frame(
                      x1 = top_regions$cx[i],
                      y1 = top_regions$cy[i],
                      x2 = top_regions$cx[j],
                      y2 = top_regions$cy[j],
                      w = wij,
                      stringsAsFactors = FALSE
                    ))
                  )
                }
              }
            }
          }
        }
        if (length(top_edge_rows) > 0) {
          top_edge_df <- do.call(rbind, top_edge_rows)
          p <- p +
            ggplot2::geom_segment(
              data = top_edge_df,
              ggplot2::aes(x = x1, y = y1, xend = x2, yend = y2),
              alpha = params$edge_alpha,
              color = params$edge_color,
              linewidth = 0.8,
              inherit.aes = FALSE
            )
        }
      }
    }
  }

  # ---- Layer 3: Nodes ----
  p <- p +
    ggplot2::geom_point(
      data = node_df,
      ggplot2::aes(x = x, y = y),
      size = node_df$node_size / 4,
      color = node_color_vec,
      alpha = params$node_alpha,
      inherit.aes = FALSE
    )

  # ---- Layer 4: Labels ----
  # Top-tier labels at top-level region centroids
  top_regions_L <- regions[regions$level == top_level, , drop = FALSE]
  if (nrow(top_regions_L) > 0) {
    top_label_df <- data.frame(
      x = top_regions_L$cx,
      y = top_regions_L$cy,
      label = vapply(
        seq_len(nrow(top_regions_L)),
        function(i) {
          sid <- top_regions_L$segment_id[i]
          if (sid >= 1 && sid <= nrow(top_map)) top_map$label[sid] else ""
        },
        character(1)
      ),
      stringsAsFactors = FALSE
    )
    top_label_df <- top_label_df[nchar(top_label_df$label) > 0, , drop = FALSE]

    if (nrow(top_label_df) > 0) {
      if (requireNamespace("ggrepel", quietly = TRUE)) {
        p <- p +
          ggrepel::geom_text_repel(
            data = top_label_df,
            ggplot2::aes(x = x, y = y, label = label),
            size = params$label_size_top,
            fontface = "bold",
            color = "grey15",
            box.padding = 0.6,
            inherit.aes = FALSE,
            max.overlaps = Inf
          )
      } else {
        p <- p +
          ggplot2::geom_text(
            data = top_label_df,
            ggplot2::aes(x = x, y = y, label = label),
            size = params$label_size_top,
            fontface = "bold",
            color = "grey15",
            vjust = -0.5,
            inherit.aes = FALSE
          )
      }
    }
  }

  # Sub-tier labels (top_level - 1) at sub-region centroids
  sub_level <- top_level - 1L
  if (sub_level >= 1) {
    sub_regions <- regions[regions$level == sub_level, , drop = FALSE]
    sub_map <- meta$levels[[sub_level]]$map

    if (nrow(sub_regions) > 0) {
      sub_label_df <- data.frame(
        x = sub_regions$cx,
        y = sub_regions$cy,
        label = vapply(
          seq_len(nrow(sub_regions)),
          function(i) {
            sid <- sub_regions$segment_id[i]
            if (sid >= 1 && sid <= nrow(sub_map)) sub_map$label[sid] else ""
          },
          character(1)
        ),
        stringsAsFactors = FALSE
      )
      sub_label_df <- sub_label_df[
        nchar(sub_label_df$label) > 0,
        ,
        drop = FALSE
      ]

      if (nrow(sub_label_df) > 0) {
        if (requireNamespace("ggrepel", quietly = TRUE)) {
          p <- p +
            ggrepel::geom_text_repel(
              data = sub_label_df,
              ggplot2::aes(x = x, y = y, label = label),
              size = params$label_size_sub,
              fontface = "plain",
              color = "grey25",
              box.padding = 0.4,
              inherit.aes = FALSE,
              max.overlaps = Inf
            )
        } else {
          p <- p +
            ggplot2::geom_text(
              data = sub_label_df,
              ggplot2::aes(x = x, y = y, label = label),
              size = params$label_size_sub,
              fontface = "plain",
              color = "grey25",
              vjust = 1.2,
              inherit.aes = FALSE
            )
        }
      }
    }
  }

  # Leaf-tier labels (opt-in)
  if (params$show_node_labels && nrow(node_df) > 0) {
    if (requireNamespace("ggrepel", quietly = TRUE)) {
      p <- p +
        ggrepel::geom_text_repel(
          data = node_df,
          ggplot2::aes(x = x, y = y, label = name),
          size = params$label_size_leaf,
          color = "grey35",
          box.padding = 0.2,
          inherit.aes = FALSE,
          max.overlaps = Inf
        )
    } else {
      p <- p +
        ggplot2::geom_text(
          data = node_df,
          ggplot2::aes(x = x, y = y, label = name),
          size = params$label_size_leaf,
          color = "grey35",
          vjust = -0.3,
          inherit.aes = FALSE
        )
    }
  }

  # ---- Theme ----
  p <- p +
    switch(
      theme_style,
      void = ggplot2::theme_void(),
      minimal = ggplot2::theme_minimal(),
      classic = ggplot2::theme_classic()
    )

  p <- p +
    ggplot2::theme(
      legend.position = "none",
      plot.title = ggplot2::element_text(size = 11, hjust = 0.5, face = "bold")
    )

  if (!is.null(params$title)) {
    p <- p + ggplot2::labs(title = params$title)
  }

  p
}


# 3. Internal utility: meta-edge matrix at a specific level -----
#
# Extracts the segment-to-segment adjacency at level L from base_weights.
# Used only in show_edges = "top" rendering.
meta_edges_at_level <- function(base_weights, meta, level) {
  if (level < 2 || level > meta$n_levels) {
    return(NULL)
  }
  groups <- meta$levels[[level]]$groups
  ng <- length(groups)
  adj <- matrix(0, nrow = ng, ncol = ng)
  for (i in seq_len(ng)) {
    for (j in seq_len(ng)) {
      if (i != j) {
        adj[i, j] <- sum(base_weights[groups[[i]], groups[[j]], drop = FALSE])
      }
    }
  }
  adj
}
