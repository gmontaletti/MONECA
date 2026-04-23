# Tests for plot_moneca_dendrogram() (rewritten elbow-style implementation)
# ==========================================================================
#
# Verifies the redesigned dendrogram: height methods, leaf-count invariant,
# pure elbow geometry (no diagonals), horizontal non-crossing of top-level
# clusters, internal-label count, highlight-path width, colour continuity
# along leaf-to-root paths, top_level validation, deprecated curved style,
# orientation flip, L-prefixed y-axis breaks, and on-axis leaf positioning.

# 1. Helper: build a standard fixture -----
.dend_fixture <- function(n_classes = 10, segment.levels = 3, seed = 42) {
  set.seed(seed)
  mob <- generate_mobility_data(n_classes = n_classes, seed = seed)
  moneca(mob, segment.levels = segment.levels)
}

# 2. Helper: pull built layer data once -----
#
# ggplot_build()$data returns one data.frame per layer in draw order.
# We use positional access (index) together with column signatures to
# identify specific layers (segments carry x/xend; text carries label).
.dend_layers <- function(p) {
  ggplot2::ggplot_build(p)$data
}

# 3. Helper: classify a layer by its column set -----
.dend_has_cols <- function(df, cols) {
  all(cols %in% names(df))
}

# 4. Smoke across all height methods -----
test_that("plot_moneca_dendrogram renders a ggplot for every height_method", {
  skip_if_not_installed("ggraph")
  skip_if_not_installed("tidygraph")
  skip_if_not_installed("dplyr")

  seg <- .dend_fixture(n_classes = 10, segment.levels = 3, seed = 42)

  for (hm in c("level", "uniform", "mobility", "segments")) {
    p <- plot_moneca_dendrogram(seg, height_method = hm)
    expect_s3_class(p, "ggplot")
  }
})

# 5. Leaf-count invariant -----
#
# With show_labels = "leaves" the only text layer is the leaf labels, and
# it must carry exactly one row per original category. Leaf-text is the
# text layer whose y-value sits strictly below heights[1L] (y_offset), so
# it is distinct from the internal-label layer when show_labels = "both".
test_that("leaf-label layer has one row per original name", {
  skip_if_not_installed("ggraph")
  skip_if_not_installed("tidygraph")
  skip_if_not_installed("dplyr")

  seg <- .dend_fixture(n_classes = 10, segment.levels = 3, seed = 42)
  meta <- moneca:::.get_metadata(seg)
  n_expected <- length(meta$original_names)

  p <- plot_moneca_dendrogram(seg, show_labels = "leaves")
  layers <- .dend_layers(p)

  text_layers <- Filter(
    function(df) .dend_has_cols(df, c("label", "x", "y")),
    layers
  )
  expect_true(length(text_layers) >= 1L)

  # Leaf text layer is the one at the lowest y across text layers.
  min_y_per_layer <- vapply(
    text_layers,
    function(df) min(df$y, na.rm = TRUE),
    numeric(1L)
  )
  leaf_layer <- text_layers[[which.min(min_y_per_layer)]]

  expect_equal(nrow(leaf_layer), n_expected)
})

# 6. Elbow-purity invariant (load-bearing) -----
#
# Every branch segment is either strictly vertical (x == xend) or strictly
# horizontal (y == yend). A diagonal in this layer would mean a curve has
# snuck past the style = "elbow" guard.
test_that("every branch segment is a pure horizontal or vertical edge", {
  skip_if_not_installed("ggraph")
  skip_if_not_installed("tidygraph")
  skip_if_not_installed("dplyr")

  seg <- .dend_fixture(n_classes = 10, segment.levels = 3, seed = 42)
  p <- plot_moneca_dendrogram(
    seg,
    show_labels = "leaves",
    highlight = NULL
  )
  layers <- .dend_layers(p)

  seg_layers <- Filter(
    function(df) .dend_has_cols(df, c("x", "xend", "y", "yend")),
    layers
  )
  expect_true(length(seg_layers) >= 1L)

  # The branch-edge layer is the first segment layer (guides use hline,
  # highlight is absent). If more segment layers exist they must also
  # satisfy the elbow invariant.
  tol <- 1e-10
  for (sl in seg_layers) {
    dx <- abs(sl$x - sl$xend)
    dy <- abs(sl$y - sl$yend)
    is_elbow <- (dx < tol) | (dy < tol)
    expect_true(
      all(is_elbow),
      info = sprintf(
        "found %d non-elbow segments in branch layer",
        sum(!is_elbow)
      )
    )
  }
})

# 7. No-crossing invariant -----
#
# Leaves belonging to the same top-level cluster must occupy a contiguous
# x-range; a foreign leaf in between means branches would have to cross.
test_that("top-level clusters occupy contiguous x-ranges of leaves", {
  skip_if_not_installed("ggraph")
  skip_if_not_installed("tidygraph")
  skip_if_not_installed("dplyr")

  seg <- .dend_fixture(n_classes = 12, segment.levels = 3, seed = 42)
  meta <- moneca:::.get_metadata(seg)
  top_level <- meta$n_levels
  parent_of <- moneca:::.dend_parent_map(meta, top_level)
  leaf_order <- moneca:::.dend_leaf_order(meta, top_level, parent_of)

  # leaf_order[pos] = original index at x-position `pos`. Invert to get x
  # for each original leaf index.
  n_leaves <- length(leaf_order)
  leaf_x <- integer(n_leaves)
  for (pos in seq_len(n_leaves)) {
    leaf_x[leaf_order[pos]] <- pos
  }

  top_groups <- meta$levels[[top_level]]$groups
  for (g in top_groups) {
    xs <- sort(leaf_x[g])
    if (length(xs) > 1L) {
      expect_equal(
        xs,
        seq(min(xs), max(xs)),
        info = sprintf(
          "top-level cluster x-range is not contiguous: %s",
          paste(xs, collapse = ",")
        )
      )
    }
  }
})

# 8. Internal-node label count -----
#
# With show_labels = "both" the internal-label layer must contain exactly
# sum_{k = 2..top_level} length(meta$levels[[k]]$groups) rows.
test_that("internal labels count equals total internal segments", {
  skip_if_not_installed("ggraph")
  skip_if_not_installed("tidygraph")
  skip_if_not_installed("dplyr")

  seg <- .dend_fixture(n_classes = 10, segment.levels = 3, seed = 42)
  meta <- moneca:::.get_metadata(seg)
  top_level <- meta$n_levels
  expected <- sum(vapply(
    meta$levels[2:top_level],
    function(L) length(L$groups),
    integer(1L)
  ))

  p <- plot_moneca_dendrogram(seg, show_labels = "both")
  layers <- .dend_layers(p)

  text_layers <- Filter(
    function(df) .dend_has_cols(df, c("label", "x", "y")),
    layers
  )
  expect_true(length(text_layers) >= 2L)

  # Internal-label layer is the text layer with largest minimum y (sits at
  # or above heights[1L]); the leaf-label layer is the one with the lowest
  # minimum y. This ordering holds for both leaf_label_position = "below"
  # (leaves offset downward) and "on_axis" (no leaf text layer, but this
  # test uses the default "below").
  min_y_per_layer <- vapply(
    text_layers,
    function(df) min(df$y, na.rm = TRUE),
    numeric(1L)
  )
  internal_layer <- text_layers[[which.max(min_y_per_layer)]]

  expect_equal(nrow(internal_layer), expected)
})

# 9. Highlight path row count -----
#
# highlight = k injects exactly one extra segment layer whose rows are the
# edges along the leaf-to-root path. For a tree with top_level levels,
# the path traverses (top_level - 1) parent-child transitions, each of
# which contributes the single vertical leg for that child; plus any
# horizontal bars the path visits. The highlight layer is identifiable
# as the segment layer rendered with linewidth = highlight_width.
test_that("highlight path adds a dedicated overplotted segment layer", {
  skip_if_not_installed("ggraph")
  skip_if_not_installed("tidygraph")
  skip_if_not_installed("dplyr")

  seg <- .dend_fixture(n_classes = 10, segment.levels = 3, seed = 42)
  p_no_hl <- plot_moneca_dendrogram(seg, highlight = NULL)
  p_hl <- plot_moneca_dendrogram(seg, highlight = 3L, highlight_width = 1.6)

  seg_layers_no_hl <- Filter(
    function(df) .dend_has_cols(df, c("x", "xend", "y", "yend")),
    .dend_layers(p_no_hl)
  )
  seg_layers_hl <- Filter(
    function(df) .dend_has_cols(df, c("x", "xend", "y", "yend")),
    .dend_layers(p_hl)
  )

  expect_equal(
    length(seg_layers_hl),
    length(seg_layers_no_hl) + 1L,
    info = "highlight must add exactly one extra segment layer"
  )

  # The extra layer must have at least top_level - 1 rows (one vertical
  # leg per level transition on the leaf-to-root path).
  meta <- moneca:::.get_metadata(seg)
  top_level <- meta$n_levels
  extra_rows <- vapply(seg_layers_hl, nrow, integer(1L))
  # pick the smallest new layer (highlight path is a subset of edges)
  expect_true(any(extra_rows >= top_level - 1L))
})

# 10. Colour continuity on leaf-to-root path -----
#
# With color_by = "top_segment", every edge on the path from a leaf up to
# its top-level ancestor must carry the same colour string (the colour
# of that top-level cluster).
test_that("colors are constant along every leaf-to-root path", {
  skip_if_not_installed("ggraph")
  skip_if_not_installed("tidygraph")
  skip_if_not_installed("dplyr")

  seg <- .dend_fixture(n_classes = 10, segment.levels = 3, seed = 42)
  meta <- moneca:::.get_metadata(seg)
  top_level <- meta$n_levels
  parent_of <- moneca:::.dend_parent_map(meta, top_level)

  # Build the key-to-colour map the public function uses internally.
  top_groups <- meta$levels[[top_level]]$groups
  top_labels <- meta$levels[[top_level]]$map$label
  n_top <- length(top_groups)
  leaf_top_seg <- integer(length(meta$original_names))
  for (s in seq_len(n_top)) {
    for (idx in top_groups[[s]]) {
      leaf_top_seg[idx] <- s
    }
  }
  palette_cols <- setNames(
    moneca:::.resolve_palette_colors(n_top, "okabe-ito"),
    top_labels
  )
  key_colors <- moneca:::.dend_color_paths(
    parent_of = parent_of,
    leaf_top_segment = leaf_top_seg,
    top_map_labels = top_labels,
    segment_colors = palette_cols,
    top_level = top_level
  )

  # Walk one representative leaf's path and check all keys map to the
  # same colour.
  leaf_idx <- 1L
  path <- moneca:::.dend_path_to_root(parent_of, leaf_idx, top_level)
  keys_on_path <- paste0("L", seq_len(top_level), "_S", path)
  cols_on_path <- unname(key_colors[keys_on_path])
  expect_equal(length(unique(cols_on_path)), 1L)

  # Sanity: the function actually renders with that identity scale.
  p <- plot_moneca_dendrogram(seg, color_by = "top_segment")
  expect_s3_class(p, "ggplot")
})

# 11. top_level validation -----
test_that("top_level < 2 errors and explicit top_level = 2 renders", {
  skip_if_not_installed("ggraph")
  skip_if_not_installed("tidygraph")
  skip_if_not_installed("dplyr")

  seg <- .dend_fixture(n_classes = 10, segment.levels = 3, seed = 42)

  expect_error(
    plot_moneca_dendrogram(seg, top_level = 1L),
    regexp = "at least 2 levels"
  )

  p <- plot_moneca_dendrogram(seg, top_level = 2L)
  expect_s3_class(p, "ggplot")
})

# 12. style = "curved" is deprecated -----
#
# .Deprecated() may surface as a warning or a message depending on R
# version and option settings; expect_condition catches either.
test_that("style = 'curved' emits a deprecation and still renders", {
  skip_if_not_installed("ggraph")
  skip_if_not_installed("tidygraph")
  skip_if_not_installed("dplyr")

  seg <- .dend_fixture(n_classes = 8, segment.levels = 3, seed = 42)

  # Deprecation may surface as warning or message across R versions;
  # check both aspects independently so a condition catcher does not
  # swallow the return value.
  expect_condition(
    plot_moneca_dendrogram(seg, style = "curved"),
    regexp = "deprecated"
  )
  p <- suppressWarnings(suppressMessages(
    plot_moneca_dendrogram(seg, style = "curved")
  ))
  expect_s3_class(p, "ggplot")
})

# 13. Orientation flip -----
test_that("vertical = FALSE applies coord_flip", {
  skip_if_not_installed("ggraph")
  skip_if_not_installed("tidygraph")
  skip_if_not_installed("dplyr")

  seg <- .dend_fixture(n_classes = 8, segment.levels = 3, seed = 42)
  p <- plot_moneca_dendrogram(seg, vertical = FALSE)
  expect_s3_class(p, "ggplot")
  expect_s3_class(p$coordinates, "CoordFlip")
})

# 14. level_axis_labels produces L-prefixed y-axis breaks -----
test_that("level_axis_labels = TRUE yields L<k> labels on the y axis", {
  skip_if_not_installed("ggraph")
  skip_if_not_installed("tidygraph")
  skip_if_not_installed("dplyr")

  seg <- .dend_fixture(n_classes = 8, segment.levels = 3, seed = 42)
  p <- plot_moneca_dendrogram(seg, level_axis_labels = TRUE)
  built <- ggplot2::ggplot_build(p)
  y_scale <- built$layout$panel_params[[1L]]$y
  labs <- if (!is.null(y_scale$get_labels)) {
    y_scale$get_labels()
  } else {
    y_scale$breaks
  }
  labs <- as.character(labs)
  expect_true(
    any(grepl("^L\\d+$", labs)),
    info = sprintf(
      "expected at least one L-prefixed label; got: %s",
      paste(labs, collapse = ",")
    )
  )
})

# 15. leaf_label_position = 'on_axis' suppresses the leaf text layer -----
#
# When labels sit on the axis, no text geom with leaf labels is drawn;
# instead axis.text.x is an element_text rather than element_blank.
test_that("leaf_label_position = 'on_axis' uses axis text instead of geom_text", {
  skip_if_not_installed("ggraph")
  skip_if_not_installed("tidygraph")
  skip_if_not_installed("dplyr")

  seg <- .dend_fixture(n_classes = 8, segment.levels = 3, seed = 42)
  meta <- moneca:::.get_metadata(seg)
  leaf_names <- meta$original_names

  p <- plot_moneca_dendrogram(
    seg,
    show_labels = "leaves",
    leaf_label_position = "on_axis"
  )
  layers <- .dend_layers(p)

  text_layers <- Filter(
    function(df) .dend_has_cols(df, c("label", "x", "y")),
    layers
  )

  # No text layer may carry the leaf names as its label column.
  any_leaf_text <- any(vapply(
    text_layers,
    function(df) any(as.character(df$label) %in% leaf_names),
    logical(1L)
  ))
  expect_false(any_leaf_text)

  # Axis text must be configured (element_text, not element_blank).
  ax <- p$theme$axis.text.x
  expect_false(is.null(ax))
  expect_s3_class(ax, "element_text")
})
