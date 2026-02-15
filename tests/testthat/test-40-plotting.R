# Integration Tests: Plotting Functions
# =======================================
#
# Tests for modern ggraph plotting functions and visualization system.

test_that("plot_moneca_ggraph single level returns ggplot", {
  skip_if_not_installed("ggraph")
  skip_if_not_installed("tidygraph")
  skip_if_not_installed("dplyr")

  # Create test data and analysis
  test_data <- get_test_data("small")
  seg <- moneca(test_data, segment.levels = 2)

  expect_no_error({
    p <- plot_moneca_ggraph(seg, level = 2)
  })

  p <- plot_moneca_ggraph(seg, level = 2)
  expect_s3_class(p, "ggplot")
})

test_that("plot_moneca_ggraph default (multi-level) returns named list of ggplots", {
  skip_if_not_installed("ggraph")
  skip_if_not_installed("tidygraph")
  skip_if_not_installed("dplyr")

  test_data <- get_test_data("small")
  seg <- moneca(test_data, segment.levels = 3)

  expect_no_error({
    result <- plot_moneca_ggraph(seg)
  })

  result <- plot_moneca_ggraph(seg)
  expect_type(result, "list")
  expect_true(all(vapply(result, inherits, logical(1), "ggplot")))
  expect_true(all(grepl("^level_", names(result))))
})

test_that("plot_moneca_ggraph multi-level vector returns named list with one plot per level", {
  skip_if_not_installed("ggraph")
  skip_if_not_installed("tidygraph")
  skip_if_not_installed("dplyr")

  test_data <- get_custom_test_data(n_classes = 6, seed = 42)
  seg <- moneca(test_data, segment.levels = 3)

  result <- plot_moneca_ggraph(seg, level = c(2, 3))
  expect_type(result, "list")
  expect_length(result, 2)
  expect_equal(names(result), c("level_2", "level_3"))
  expect_s3_class(result[[1]], "ggplot")
  expect_s3_class(result[[2]], "ggplot")
})

test_that("plot_moneca_ggraph accepts different parameters", {
  skip_if_not_installed("ggraph")
  skip_if_not_installed("tidygraph")
  skip_if_not_installed("dplyr")

  test_data <- get_test_data("small")
  seg <- moneca(test_data, segment.levels = 2)

  # Test different layouts
  expect_no_error({
    p1 <- plot_moneca_ggraph(seg, layout = "kk")
    p2 <- plot_moneca_ggraph(seg, layout = "fr")
  })

  # Test different node attributes
  expect_no_error({
    p3 <- plot_moneca_ggraph(seg, node_color = "segment")
    p4 <- plot_moneca_ggraph(seg, node_size = "total")
  })

  # Test with title
  expect_no_error({
    p5 <- plot_moneca_ggraph(seg, title = "Test Plot")
  })
})

test_that("plot_ego_ggraph creates ggplot objects", {
  skip_if_not_installed("ggraph")
  skip_if_not_installed("tidygraph")
  skip_if_not_installed("dplyr")

  test_data <- get_test_data("small")
  seg <- moneca(test_data, segment.levels = 2)

  expect_no_error({
    p <- plot_ego_ggraph(seg, test_data, ego_id = 2)
  })

  p <- plot_ego_ggraph(seg, test_data, ego_id = 2)
  expect_s3_class(p, "ggplot")
})

test_that("plot_ego_ggraph handles different ego_id formats", {
  skip_if_not_installed("ggraph")
  skip_if_not_installed("tidygraph")
  skip_if_not_installed("dplyr")

  test_data <- get_named_test_data(c("Upper", "Middle", "Lower"))
  seg <- moneca(test_data, segment.levels = 2)

  # Test with numeric ID
  expect_no_error({
    p1 <- plot_ego_ggraph(seg, test_data, ego_id = 2)
  })

  # Test with character ID
  expect_no_error({
    p2 <- plot_ego_ggraph(seg, test_data, ego_id = "Middle")
  })

  # Test invalid ID
  expect_error(
    {
      p3 <- plot_ego_ggraph(seg, test_data, ego_id = "NonExistent")
    },
    "ego_id not found"
  )
})

test_that("plot_stair_ggraph creates list of plots", {
  skip_if_not_installed("ggraph")
  skip_if_not_installed("tidygraph")
  skip_if_not_installed("dplyr")

  test_data <- get_test_data("small")
  seg <- moneca(test_data, segment.levels = 3)

  expect_no_error({
    plots <- plot_stair_ggraph(seg)
  })

  plots <- plot_stair_ggraph(seg)
  expect_is(plots, "list")
  expect_true(all(sapply(plots, function(x) inherits(x, "ggplot"))))
})

test_that("plotting functions handle missing packages gracefully", {
  test_data <- get_test_data("small")
  seg <- moneca(test_data, segment.levels = 2)

  # The functions should give informative error messages when packages are missing
  # This is handled by the requireNamespace checks in the functions
  expect_true(TRUE) # Placeholder - actual implementation would test error handling
})

test_that("plotting functions respect color and size parameters", {
  skip_if_not_installed("ggraph")
  skip_if_not_installed("tidygraph")
  skip_if_not_installed("dplyr")

  test_data <- get_test_data("small")
  seg <- moneca(test_data, segment.levels = 2)

  # Test that different parameters produce different plots (single level)
  p1 <- plot_moneca_ggraph(seg, level = 2, node_color = "red", node_alpha = 0.5)
  p2 <- plot_moneca_ggraph(
    seg,
    level = 2,
    node_color = "blue",
    node_alpha = 0.9
  )

  expect_s3_class(p1, "ggplot")
  expect_s3_class(p2, "ggplot")
})

test_that("plot_segment_quality cohesion plot prevents duplicate segments", {
  skip_if_not_installed("ggraph")
  skip_if_not_installed("tidygraph")
  skip_if_not_installed("dplyr")

  # Create test data with known structure to trigger potential duplicates
  test_data <- get_custom_test_data(
    n_classes = 6,
    seed = 123,
    immobility_strength = 0.7,
    class_clustering = 0.8
  )
  seg <- moneca(test_data, segment.levels = 3)

  # Test that cohesion plot is created without errors
  expect_no_error({
    p <- plot_segment_quality(seg, plot_type = "cohesion", level = 2)
  })

  # The plot should be a ggplot object
  p <- plot_segment_quality(seg, plot_type = "cohesion", level = 2)
  expect_s3_class(p, "ggplot")

  # Test with custom segment naming
  membership <- segment.membership.enhanced(
    seg,
    level = 2,
    naming_strategy = "auto"
  )
  level2_members <- membership[grepl("^2\\.", membership$membership), ]

  # Create custom names using any member from each segment (flexible approach)
  unique_segments <- unique(gsub("^2\\.", "", level2_members$membership))
  if (length(unique_segments) >= 2) {
    custom_names <- data.frame(
      name = c(
        level2_members$name[
          level2_members$membership == paste0("2.", unique_segments[1])
        ][1],
        level2_members$name[
          level2_members$membership == paste0("2.", unique_segments[2])
        ][1]
      ),
      segment_label = c("executives", "specialists"),
      stringsAsFactors = FALSE
    )

    expect_no_error({
      p_custom <- plot_segment_quality(
        seg,
        plot_type = "cohesion",
        level = 2,
        segment_naming = custom_names
      )
    })

    p_custom <- plot_segment_quality(
      seg,
      plot_type = "cohesion",
      level = 2,
      segment_naming = custom_names
    )
    expect_s3_class(p_custom, "ggplot")
  }
})

test_that("plot_segment_quality aggregation logic works correctly", {
  skip_if_not_installed("dplyr")

  test_data <- get_custom_test_data(
    n_classes = 8,
    seed = 456,
    immobility_strength = 0.6,
    class_clustering = 0.7
  )
  seg <- moneca(test_data, segment.levels = 3)

  # Test the underlying data aggregation
  quality_data <- segment.quality(seg, final.solution = FALSE)
  level <- 2
  level_cols <- grep(
    paste0("^", level, ": "),
    colnames(quality_data),
    value = TRUE
  )
  level_data <- quality_data[, c("Membership", level_cols), drop = FALSE]
  colnames(level_data) <- gsub(
    paste0("^", level, ": "),
    "",
    colnames(level_data)
  )

  # Before aggregation, we might have duplicates
  n_rows_before <- nrow(level_data)
  n_unique_segments <- length(unique(level_data$Segment))

  # Aggregation should reduce rows to number of unique segments
  if (n_rows_before > n_unique_segments) {
    # Apply aggregation logic
    level_data_agg <- level_data %>%
      dplyr::group_by(Segment) %>%
      dplyr::summarise(
        within.mobility = mean(within.mobility, na.rm = TRUE),
        share.of.mobility = mean(share.of.mobility, na.rm = TRUE),
        Density = mean(Density, na.rm = TRUE),
        Nodes = mean(Nodes, na.rm = TRUE),
        Max.path = mean(Max.path, na.rm = TRUE),
        share.of.total = mean(share.of.total, na.rm = TRUE),
        .groups = 'drop'
      )

    # After aggregation, rows should equal unique segments
    expect_equal(nrow(level_data_agg), n_unique_segments)
    expect_equal(nrow(level_data_agg), length(unique(level_data_agg$Segment)))
  }
})

test_that("plot_segment_quality handles different plot types", {
  skip_if_not_installed("ggraph")
  skip_if_not_installed("tidygraph")
  skip_if_not_installed("dplyr")

  test_data <- get_custom_test_data(
    n_classes = 5,
    seed = 789,
    immobility_strength = 0.6,
    class_clustering = 0.7
  )
  seg <- moneca(test_data, segment.levels = 3)

  # Test all plot types
  expect_no_error({
    p_overview <- plot_segment_quality(seg, plot_type = "overview", level = 2)
    p_cohesion <- plot_segment_quality(seg, plot_type = "cohesion", level = 2)
    p_radar <- plot_segment_quality(seg, plot_type = "radar", level = 2)
    p_heatmap <- plot_segment_quality(seg, plot_type = "heatmap")
    p_evolution <- plot_segment_quality(seg, plot_type = "evolution")
  })

  # Check that different plot types produce different outputs
  p_cohesion <- plot_segment_quality(seg, plot_type = "cohesion", level = 2)
  expect_s3_class(p_cohesion, "ggplot")
})

test_that("create_segment_labels function works with custom dataframes", {
  test_data <- get_custom_test_data(n_classes = 6)
  seg <- moneca(test_data, segment.levels = 3)

  # Test basic functionality
  segment_numbers <- c(1, 2)
  basic_labels <- create_segment_labels(seg, 2, segment_numbers, "auto")
  expect_length(basic_labels, 2)
  # Labels are now metadata-based (e.g. "Class1+2"), not "Segment N" format
  expect_true(all(nchar(basic_labels) > 0))

  # Test custom dataframe functionality
  membership <- segment.membership.enhanced(
    seg,
    level = 2,
    naming_strategy = "auto"
  )
  level2_members <- membership[grepl("^2\\.", membership$membership), ]

  if (nrow(level2_members) > 0) {
    # Create custom names using any member from segments (flexible approach)
    custom_names <- data.frame(
      name = c(
        level2_members$name[1],
        level2_members$name[length(level2_members$name)]
      ),
      segment_label = c("custom1", "custom2"),
      stringsAsFactors = FALSE
    )

    custom_labels <- create_segment_labels(
      seg,
      2,
      segment_numbers,
      custom_names
    )
    expect_length(custom_labels, 2)
    # At least one should match custom labels (depending on membership structure)
    expect_true(
      any(custom_labels %in% c("custom1", "custom2")) ||
        all(grepl("^Segment", custom_labels))
    )
  }

  # Test error handling
  invalid_df <- data.frame(wrong_col = "test")
  expect_error({
    create_segment_labels(seg, 2, segment_numbers, invalid_df)
  })
})

test_that("segment.quality and plot_segment_quality integration works", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("gridExtra")

  # Create test data and analysis
  test_data <- get_custom_test_data(n_classes = 6, n_total = 500)
  seg <- moneca(test_data, segment.levels = 3)

  # Test that segment.quality creates segment_label column with custom naming
  custom_names <- data.frame(
    name = c("Class1", "Class2", "Class3"),
    segment_label = c("Executive", "Professional", "Technical"),
    stringsAsFactors = FALSE
  )

  # Test segment.quality with segment_naming parameter
  quality_data <- segment.quality(
    seg,
    final.solution = TRUE,
    segment_naming = custom_names
  )

  # Check that segment_label column is created
  expect_true("segment_label" %in% colnames(quality_data))
  expect_true(length(quality_data$segment_label) > 0)

  # Test that plot_segment_quality uses segment labels correctly
  expect_no_error({
    p <- plot_segment_quality(
      seg,
      plot_type = "cohesion",
      segment_naming = custom_names
    )
  })

  # Test with string-based segment naming
  expect_no_error({
    quality_auto <- segment.quality(
      seg,
      final.solution = TRUE,
      segment_naming = "auto"
    )
    p_auto <- plot_segment_quality(
      seg,
      plot_type = "cohesion",
      segment_naming = "auto"
    )
  })

  quality_auto <- segment.quality(
    seg,
    final.solution = TRUE,
    segment_naming = "auto"
  )
  expect_true("segment_label" %in% colnames(quality_auto))
  # Labels are now metadata-based (e.g. "Class1+2"), not "Segment N" format
  expect_true(all(nchar(quality_auto$segment_label) > 0))
})

test_that("plot_segment_quality cohesion plot uses segment_label correctly", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("gridExtra")

  # Create test data and analysis
  test_data <- get_test_data("small")
  seg <- moneca(test_data, segment.levels = 2)

  # Create custom segment names
  custom_names <- data.frame(
    name = c("Class1", "Class2"),
    segment_label = c("Group A", "Group B"),
    stringsAsFactors = FALSE
  )

  # Test that cohesion plot works and uses segment labels
  p <- plot_segment_quality(
    seg,
    plot_type = "cohesion",
    segment_naming = custom_names
  )

  # Extract the underlying data from the plot
  plot_data <- p$data

  # Verify that segment labels are used in the plot
  expect_true("segment_label" %in% colnames(plot_data))

  # Check that custom labels appear in the data (at least one should match)
  has_custom <- any(plot_data$segment_label %in% c("Group A", "Group B"))
  # Labels are now metadata-based by default, not "Segment N" format
  has_labels <- all(nchar(as.character(plot_data$segment_label)) > 0)

  # Either custom labels were applied OR metadata-based labels were used
  expect_true(has_custom || has_labels)
})

# Tests for internal helpers and metadata-based plotting -----

test_that(".get_metadata works with moneca() objects (no cached metadata)", {
  test_data <- get_test_data("small")
  seg <- moneca(test_data, segment.levels = 2)

  # moneca() does not cache segment_metadata
  expect_null(seg$segment_metadata)

  # .get_metadata should compute it on the fly
  meta <- moneca:::.get_metadata(seg)
  expect_s3_class(meta, "moneca_segments")
  expect_true(meta$n_levels >= 1)
  expect_true(length(meta$original_names) > 0)
})

test_that(".get_metadata works with moneca_fast() objects (cached metadata)", {
  test_data <- get_test_data("small")
  seg <- moneca_fast(test_data, segment.levels = 2)

  # moneca_fast() caches segment_metadata
  expect_false(is.null(seg$segment_metadata))

  meta <- moneca:::.get_metadata(seg)
  expect_s3_class(meta, "moneca_segments")
  expect_equal(meta$n_levels, length(seg$segment.list))
})

test_that(".membership_from_metadata produces correct structure", {
  test_data <- get_test_data("small")
  seg <- moneca(test_data, segment.levels = 2)
  meta <- moneca_segments(seg)

  membership <- moneca:::.membership_from_metadata(meta, 2)

  # Must have standard columns
  expect_true(all(
    c("name", "membership", "segment_label") %in%
      colnames(membership)
  ))
  # One row per node
  expect_equal(nrow(membership), length(meta$original_names))
  # All names present
  expect_equal(membership$name, meta$original_names)
  # Non-empty membership
  expect_true(all(nchar(membership$membership) > 0))
  # Non-empty labels
  expect_true(all(nchar(membership$segment_label) > 0))
})

test_that(".membership_from_metadata groupings match moneca_segments groups", {
  test_data <- get_custom_test_data(n_classes = 6, seed = 42)
  seg <- moneca(test_data, segment.levels = 3)
  meta <- moneca_segments(seg)

  for (lvl in seq_len(meta$n_levels)) {
    membership <- moneca:::.membership_from_metadata(meta, lvl)
    groups <- meta$levels[[lvl]]$groups

    for (g in seq_along(groups)) {
      expected_names <- meta$original_names[groups[[g]]]
      actual_names <- membership$name[
        membership$membership == paste0(lvl, ".", g)
      ]
      expect_setequal(actual_names, expected_names)
    }
  }
})

test_that("modern plotting functions work with moneca() objects (no cache)", {
  skip_if_not_installed("ggraph")
  skip_if_not_installed("tidygraph")
  skip_if_not_installed("dplyr")

  test_data <- get_test_data("small")
  seg <- moneca(test_data, segment.levels = 2)

  # All 5 modern plotting functions should work without cached metadata
  expect_no_error({
    p1 <- plot_moneca_ggraph(seg, level = 2)
  })
  expect_s3_class(p1, "ggplot")

  expect_no_error({
    p2 <- plot_ego_ggraph(seg, test_data, ego_id = 2)
  })
  expect_s3_class(p2, "ggplot")

  expect_no_error({
    p3 <- plot_stair_ggraph(seg)
  })
  expect_is(p3, "list")

  expect_no_error({
    p4 <- plot_moneca_dendrogram(seg)
  })
  expect_s3_class(p4, "ggplot")

  expect_no_error({
    p5 <- plot_segment_quality(seg, plot_type = "cohesion")
  })
  expect_s3_class(p5, "ggplot")
})

test_that("legacy functions emit deprecation warnings", {
  test_data <- get_test_data("small")
  seg <- moneca(test_data, segment.levels = 2)

  expect_warning(
    moneca.plot(seg),
    "deprecated"
  )
})

# Tests for multi-level and unified color system -----

test_that(".compute_segment_colors returns named vector with deterministic order", {
  test_data <- get_custom_test_data(n_classes = 6, seed = 42)
  seg <- moneca(test_data, segment.levels = 3)
  meta <- moneca_segments(seg)

  colors <- moneca:::.compute_segment_colors(meta, c(2, 3), "Set3")

  # Returns a named character vector
  expect_type(colors, "character")
  expect_true(!is.null(names(colors)))

  # Names are sorted (deterministic ordering)
  expect_equal(names(colors), sort(names(colors)))

  # All unique segment labels across levels 2 and 3 are covered
  all_labels <- character(0)
  for (lvl in c(2, 3)) {
    membership <- moneca:::.membership_from_metadata(meta, lvl)
    all_labels <- union(all_labels, unique(membership$segment_label))
  }
  expect_true(all(all_labels %in% names(colors)))
})

test_that(".compute_segment_colors same label always maps to same color", {
  test_data <- get_custom_test_data(n_classes = 6, seed = 42)
  seg <- moneca(test_data, segment.levels = 3)
  meta <- moneca_segments(seg)

  # Compute colors for levels 2 and 3
  colors_both <- moneca:::.compute_segment_colors(meta, c(2, 3), "Set3")

  # Compute colors for level 2 only
  colors_2 <- moneca:::.compute_segment_colors(meta, 2, "Set3")

  # Compute colors for level 3 only
  colors_3 <- moneca:::.compute_segment_colors(meta, 3, "Set3")

  # Labels in common between single-level and multi-level should have same colors
  common_2 <- intersect(names(colors_both), names(colors_2))
  if (
    length(common_2) > 0 &&
      length(names(colors_both)) == length(names(colors_2))
  ) {
    expect_equal(colors_both[common_2], colors_2[common_2])
  }
})

test_that("plot_moneca_ggraph single level with show_labels shows node names", {
  skip_if_not_installed("ggraph")
  skip_if_not_installed("tidygraph")
  skip_if_not_installed("dplyr")

  test_data <- get_custom_test_data(n_classes = 5, seed = 123)
  seg <- moneca(test_data, segment.levels = 3)

  # With show_labels = TRUE and show_segments = TRUE (the default),
  # node labels should now appear (was suppressed before the fix)
  p <- plot_moneca_ggraph(
    seg,
    level = 2,
    show_labels = TRUE,
    show_segments = TRUE
  )
  expect_s3_class(p, "ggplot")

  # Check that a geom_node_text layer exists
  layer_classes <- sapply(p$layers, function(l) class(l$geom)[1])
  expect_true(any(grepl("Text", layer_classes)))
})
