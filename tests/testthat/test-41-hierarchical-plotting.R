# Tests for plot_moneca_hierarchical()
# =====================================
#
# Verifies the hierarchical circular-partitioning visualization:
# smoke test, geometric non-overlap of top-level circles, singleton
# handling, top_level/levels_to_show parameters, reproducibility,
# label tier hierarchy, region_shape variants, packcircles fallback,
# and top_level == 2 graceful degradation.

# 1. Helper: build a standard fixture -----
.hp_fixture <- function(n_classes = 12, segment.levels = 4, seed = 42) {
  set.seed(seed)
  mob <- generate_mobility_data(n_classes = n_classes, seed = seed)
  moneca(mob, segment.levels = segment.levels)
}

# 2. Helper: extract top-level region circles from a plot's attribute -----
#
# plot_moneca_hierarchical() attaches the regions data frame as
# attr(p, "moneca_regions"). It carries (level, segment_id, cx, cy, r,
# parent_id) per region across all levels. For non-overlap checks we
# select the top-most non-root level (root has segment_id 0).
.hp_top_circles <- function(p) {
  regs <- attr(p, "moneca_regions")
  if (is.null(regs) || nrow(regs) == 0) {
    return(NULL)
  }
  top <- max(regs$level[regs$segment_id != 0L], na.rm = TRUE)
  regs[regs$level == top & regs$segment_id != 0L, , drop = FALSE]
}

# 3. Smoke test -----
test_that("plot_moneca_hierarchical returns a ggplot for a 12-class fixture", {
  skip_if_not_installed("ggraph")
  skip_if_not_installed("tidygraph")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("ggforce")
  skip_if_not_installed("ggrepel")

  seg <- .hp_fixture(n_classes = 12, segment.levels = 4, seed = 42)
  p <- plot_moneca_hierarchical(seg, seed = 1)
  expect_s3_class(p, "ggplot")
})

# 4. Numeric non-overlap of top-level regions (load-bearing) -----
test_that("top-level circular regions do not overlap pairwise", {
  skip_if_not_installed("ggraph")
  skip_if_not_installed("tidygraph")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("ggforce")
  skip_if_not_installed("ggrepel")

  seg <- .hp_fixture(n_classes = 12, segment.levels = 4, seed = 42)
  # Pin top_level so the outermost level has >= 2 segments (default
  # top_level = n_levels can collapse to a single root region on small
  # fixtures, making pairwise non-overlap vacuous).
  p <- plot_moneca_hierarchical(
    seg,
    top_level = 3,
    region_shape = "circle",
    seed = 1
  )

  circ <- .hp_top_circles(p)
  expect_false(is.null(circ), info = "regions attribute must be present")
  expect_true(nrow(circ) >= 2)

  # Pairwise check: distance between centers >= sum of radii (with tiny eps)
  n <- nrow(circ)
  tol <- 1e-6
  ok <- TRUE
  worst <- 0
  for (i in seq_len(n - 1)) {
    for (j in (i + 1):n) {
      d <- sqrt(
        (circ$cx[i] - circ$cx[j])^2 + (circ$cy[i] - circ$cy[j])^2
      )
      slack <- d - (circ$r[i] + circ$r[j])
      if (slack < -tol) {
        ok <- FALSE
        worst <- min(worst, slack)
      }
    }
  }
  expect_true(
    ok,
    info = sprintf("worst overlap slack = %g (tolerance = -%g)", worst, tol)
  )
})

# 4b. Outer-region count matches the requested top_level (regression) -----
#
# Locks in the semantic that top_level = k makes the outermost regions the
# segments at meta$levels[[k]]. A previous off-by-one shifted them down by
# one level (see NEWS.md v1.5.1).
test_that("top_level = k produces exactly length(meta$levels[[k]]$groups) outer regions", {
  skip_if_not_installed("ggraph")
  skip_if_not_installed("tidygraph")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("ggforce")
  skip_if_not_installed("ggrepel")

  seg <- .hp_fixture(n_classes = 30, segment.levels = 4, seed = 42)
  meta <- moneca:::.get_metadata(seg)

  for (k in 2:meta$n_levels) {
    p <- plot_moneca_hierarchical(seg, top_level = k, seed = 1)
    regs <- attr(p, "moneca_regions")
    expect_equal(
      max(regs$level),
      k,
      info = sprintf("top_level=%d: outermost level should be %d", k, k)
    )
    expect_equal(
      sum(regs$level == k),
      length(meta$levels[[k]]$groups),
      info = sprintf(
        "top_level=%d: outer-region count should equal segment count at level %d",
        k,
        k
      )
    )
  }
})

# 5. Singleton handling -----
test_that("singletons at the top level produce finite node coordinates", {
  skip_if_not_installed("ggraph")
  skip_if_not_installed("tidygraph")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("ggforce")
  skip_if_not_installed("ggrepel")

  seg <- .hp_fixture(n_classes = 4, segment.levels = 3, seed = 7)
  p <- plot_moneca_hierarchical(seg, seed = 1)
  expect_s3_class(p, "ggplot")

  built <- ggplot2::ggplot_build(p)
  has_xy <- vapply(
    built$data,
    function(d) all(c("x", "y") %in% names(d)) && nrow(d) > 0,
    logical(1)
  )
  expect_true(any(has_xy))
  for (d in built$data[has_xy]) {
    expect_true(all(is.finite(d$x)))
    expect_true(all(is.finite(d$y)))
  }
})

# 6. top_level parameter validation -----
test_that("top_level parameter is honored and validated", {
  skip_if_not_installed("ggraph")
  skip_if_not_installed("tidygraph")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("ggforce")
  skip_if_not_installed("ggrepel")

  seg <- .hp_fixture(n_classes = 12, segment.levels = 4, seed = 42)

  p2 <- plot_moneca_hierarchical(seg, top_level = 2, seed = 1)
  expect_s3_class(p2, "ggplot")

  expect_error(
    plot_moneca_hierarchical(seg, top_level = 1, seed = 1),
    "Hierarchical plotting requires at least 2 levels"
  )
})

# 7. levels_to_show subset -----
test_that("levels_to_show with a non-contiguous subset renders without error", {
  skip_if_not_installed("ggraph")
  skip_if_not_installed("tidygraph")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("ggforce")
  skip_if_not_installed("ggrepel")

  seg <- .hp_fixture(n_classes = 12, segment.levels = 4, seed = 42)
  meta <- moneca:::.get_metadata(seg)
  top_level <- meta$n_levels

  p <- plot_moneca_hierarchical(
    seg,
    top_level = top_level,
    levels_to_show = c(1L, top_level),
    seed = 1
  )
  expect_s3_class(p, "ggplot")
})

# 8. Reproducibility under fixed seed -----
test_that("identical seed yields identical layer data", {
  skip_if_not_installed("ggraph")
  skip_if_not_installed("tidygraph")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("ggforce")
  skip_if_not_installed("ggrepel")

  seg <- .hp_fixture(n_classes = 12, segment.levels = 4, seed = 42)
  p1 <- plot_moneca_hierarchical(seg, seed = 1)
  p2 <- plot_moneca_hierarchical(seg, seed = 1)

  b1 <- ggplot2::ggplot_build(p1)$data
  b2 <- ggplot2::ggplot_build(p2)$data
  expect_equal(length(b1), length(b2))

  # Compare x/y coordinates layer-by-layer where present
  for (k in seq_along(b1)) {
    d1 <- b1[[k]]
    d2 <- b2[[k]]
    if (all(c("x", "y") %in% names(d1)) && all(c("x", "y") %in% names(d2))) {
      expect_equal(d1$x, d2$x)
      expect_equal(d1$y, d2$y)
    }
  }
})

# 9. Label tier hierarchy -----
test_that("text layers expose at least 2 distinct font sizes (label tiers)", {
  skip_if_not_installed("ggraph")
  skip_if_not_installed("tidygraph")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("ggforce")
  skip_if_not_installed("ggrepel")

  seg <- .hp_fixture(n_classes = 12, segment.levels = 4, seed = 42)
  p <- plot_moneca_hierarchical(seg, show_node_labels = TRUE, seed = 1)

  built <- ggplot2::ggplot_build(p)
  has_size <- vapply(
    built$data,
    function(d) "size" %in% names(d) && nrow(d) > 0,
    logical(1)
  )
  text_layers <- built$data[has_size]
  expect_true(length(text_layers) >= 2)

  all_sizes <- unique(unlist(lapply(text_layers, function(d) unique(d$size))))
  all_sizes <- all_sizes[is.finite(all_sizes)]
  expect_true(length(unique(all_sizes)) >= 2)
})

# 9b. label_levels controls which levels get labels -----
test_that("label_levels selects which levels receive labels", {
  skip_if_not_installed("ggraph")
  skip_if_not_installed("tidygraph")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("ggforce")
  skip_if_not_installed("ggrepel")

  seg <- .hp_fixture(n_classes = 30, segment.levels = 4, seed = 42)

  count_label_layers <- function(p) {
    sum(vapply(
      ggplot2::ggplot_build(p)$data,
      function(d) "label" %in% names(d) && nrow(d) > 0,
      logical(1)
    ))
  }

  # Default (NULL) -> 2 tiers (top + sub)
  p_default <- plot_moneca_hierarchical(seg, top_level = 4, seed = 1)
  expect_equal(count_label_layers(p_default), 2L)

  # "all" -> one layer per level (4)
  p_all <- plot_moneca_hierarchical(
    seg,
    top_level = 4,
    label_levels = "all",
    seed = 1
  )
  expect_equal(count_label_layers(p_all), 4L)

  # "none" -> zero layers
  p_none <- plot_moneca_hierarchical(
    seg,
    top_level = 4,
    label_levels = "none",
    seed = 1
  )
  expect_equal(count_label_layers(p_none), 0L)

  # Explicit subset c(top, top - 2) -> 2 layers, sub-tier skipped
  p_skip <- plot_moneca_hierarchical(
    seg,
    top_level = 4,
    label_levels = c(4L, 2L),
    seed = 1
  )
  expect_equal(count_label_layers(p_skip), 2L)

  # Invalid string errors
  expect_error(
    plot_moneca_hierarchical(seg, label_levels = "bogus"),
    "label_levels"
  )
})

# 10. region_shape variants -----
test_that("region_shape variants 'hull', 'circle', 'auto' all return a ggplot", {
  skip_if_not_installed("ggraph")
  skip_if_not_installed("tidygraph")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("ggforce")
  skip_if_not_installed("ggrepel")

  seg <- .hp_fixture(n_classes = 12, segment.levels = 4, seed = 42)

  for (shape in c("hull", "circle", "auto")) {
    p <- plot_moneca_hierarchical(seg, region_shape = shape, seed = 1)
    expect_s3_class(p, "ggplot")
  }
})

# 11. packcircles fallback -----
test_that("circle_pack falls back with a message when packcircles is missing", {
  skip_if_not_installed("ggraph")
  skip_if_not_installed("tidygraph")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("ggforce")
  skip_if_not_installed("ggrepel")
  skip_if(
    requireNamespace("packcircles", quietly = TRUE),
    "packcircles installed -- fallback path not exercised"
  )

  seg <- .hp_fixture(n_classes = 12, segment.levels = 4, seed = 42)
  expect_message(
    p <- plot_moneca_hierarchical(seg, layout = "circle_pack", seed = 1)
  )
  expect_s3_class(p, "ggplot")
})

# 12. top_level == 2 graceful degradation -----
test_that("top_level == 2 on a 2-level fixture renders with sub tier as leaves", {
  skip_if_not_installed("ggraph")
  skip_if_not_installed("tidygraph")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("ggforce")
  skip_if_not_installed("ggrepel")

  set.seed(11)
  mob <- generate_mobility_data(n_classes = 4, seed = 11)
  seg <- moneca(mob, segment.levels = 2)

  p <- plot_moneca_hierarchical(seg, top_level = 2, seed = 1)
  expect_s3_class(p, "ggplot")

  # Sub tier text should include base-node names (leaf names)
  built <- ggplot2::ggplot_build(p)
  has_label <- vapply(
    built$data,
    function(d) "label" %in% names(d) && nrow(d) > 0,
    logical(1)
  )
  text_layers <- built$data[has_label]
  expect_true(length(text_layers) >= 1)

  all_labels <- unique(unlist(lapply(text_layers, function(d) {
    as.character(d$label)
  })))
  base_names <- rownames(seg$mat.list[[1]])
  base_names <- base_names[base_names != "Total"]
  expect_true(any(base_names %in% all_labels))
})
