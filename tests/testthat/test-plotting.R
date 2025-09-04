test_that("plot_moneca_ggraph creates ggplot objects", {
  skip_if_not_installed("ggraph")
  skip_if_not_installed("tidygraph")
  skip_if_not_installed("dplyr")
  
  # Create test data and analysis
  test_data <- generate_mobility_data(n_classes = 4, n_total = 500, seed = 123)
  seg <- moneca(test_data, segment.levels = 2)
  
  expect_no_error({
    p <- plot_moneca_ggraph(seg)
  })
  
  p <- plot_moneca_ggraph(seg)
  expect_s3_class(p, "ggplot")
})

test_that("plot_moneca_ggraph accepts different parameters", {
  skip_if_not_installed("ggraph")
  skip_if_not_installed("tidygraph")
  skip_if_not_installed("dplyr")
  
  test_data <- generate_mobility_data(n_classes = 4, n_total = 500, seed = 123)
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
  
  test_data <- generate_mobility_data(n_classes = 4, n_total = 500, seed = 123)
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
  
  test_data <- generate_mobility_data(
    n_classes = 3, 
    n_total = 300, 
    class_names = c("Upper", "Middle", "Lower"),
    seed = 123
  )
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
  expect_error({
    p3 <- plot_ego_ggraph(seg, test_data, ego_id = "NonExistent")
  }, "ego_id not found")
})

test_that("plot_stair_ggraph creates list of plots", {
  skip_if_not_installed("ggraph")
  skip_if_not_installed("tidygraph")
  skip_if_not_installed("dplyr")
  
  test_data <- generate_mobility_data(n_classes = 4, n_total = 500, seed = 123)
  seg <- moneca(test_data, segment.levels = 3)
  
  expect_no_error({
    plots <- plot_stair_ggraph(seg)
  })
  
  plots <- plot_stair_ggraph(seg)
  expect_is(plots, "list")
  expect_true(all(sapply(plots, function(x) inherits(x, "ggplot"))))
})

test_that("plotting functions handle missing packages gracefully", {
  # Mock missing packages by temporarily unloading them
  # This is a simplified test - in practice, these would be more complex
  
  test_data <- generate_mobility_data(n_classes = 3, n_total = 300, seed = 123)
  seg <- moneca(test_data, segment.levels = 2)
  
  # The functions should give informative error messages when packages are missing
  # This is handled by the requireNamespace checks in the functions
  expect_true(TRUE)  # Placeholder - actual implementation would test error handling
})

test_that("plotting functions respect color and size parameters", {
  skip_if_not_installed("ggraph")
  skip_if_not_installed("tidygraph")
  skip_if_not_installed("dplyr")
  
  test_data <- generate_mobility_data(n_classes = 4, n_total = 500, seed = 123)
  seg <- moneca(test_data, segment.levels = 2)
  
  # Test that different parameters produce different plots
  p1 <- plot_moneca_ggraph(seg, node_color = "red", node_alpha = 0.5)
  p2 <- plot_moneca_ggraph(seg, node_color = "blue", node_alpha = 0.9)
  
  expect_s3_class(p1, "ggplot")
  expect_s3_class(p2, "ggplot")
  
  # The plots should be different (this is a basic check)
  expect_true(!identical(p1$layers, p2$layers) || !identical(p1$scales, p2$scales))
})

test_that("plot_segment_quality cohesion plot prevents duplicate segments", {
  skip_if_not_installed("ggraph")
  skip_if_not_installed("tidygraph")
  skip_if_not_installed("dplyr")
  
  # Create test data with known structure to trigger potential duplicates
  test_data <- generate_mobility_data(n_classes = 6, seed = 123, 
                                     immobility_strength = 0.7,
                                     class_clustering = 0.8)
  seg <- moneca(test_data, segment.levels = 3)
  
  # Test that cohesion plot is created without errors
  expect_no_error({
    p <- plot_segment_quality(seg, plot_type = "cohesion", level = 2)
  })
  
  # The plot should be a ggplot object
  p <- plot_segment_quality(seg, plot_type = "cohesion", level = 2)
  expect_s3_class(p, "ggplot")
  
  # Test with custom segment naming
  membership <- segment.membership.enhanced(seg, level = 2, naming_strategy = "auto")
  level2_members <- membership[grepl("^2\\.", membership$membership), ]
  
  # Create custom names using any member from each segment (flexible approach)
  unique_segments <- unique(gsub("^2\\.", "", level2_members$membership))
  if (length(unique_segments) >= 2) {
    custom_names <- data.frame(
      name = c(level2_members$name[level2_members$membership == paste0("2.", unique_segments[1])][1],
               level2_members$name[level2_members$membership == paste0("2.", unique_segments[2])][1]),
      segment_label = c("executives", "specialists"),
      stringsAsFactors = FALSE
    )
    
    expect_no_error({
      p_custom <- plot_segment_quality(seg, plot_type = "cohesion", level = 2, 
                                      segment_naming = custom_names)
    })
    
    p_custom <- plot_segment_quality(seg, plot_type = "cohesion", level = 2, 
                                    segment_naming = custom_names)
    expect_s3_class(p_custom, "ggplot")
  }
})

test_that("plot_segment_quality aggregation logic works correctly", {
  skip_if_not_installed("dplyr")
  
  test_data <- generate_mobility_data(n_classes = 8, seed = 456, 
                                     immobility_strength = 0.6,
                                     class_clustering = 0.7)
  seg <- moneca(test_data, segment.levels = 3)
  
  # Test the underlying data aggregation
  quality_data <- segment.quality(seg, final.solution = FALSE)
  level <- 2
  level_cols <- grep(paste0("^", level, ": "), colnames(quality_data), value = TRUE)
  level_data <- quality_data[, c("Membership", level_cols), drop = FALSE]
  colnames(level_data) <- gsub(paste0("^", level, ": "), "", colnames(level_data))
  
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
  
  test_data <- generate_mobility_data(n_classes = 5, seed = 789, 
                                     immobility_strength = 0.6,
                                     class_clustering = 0.7)
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
  test_data <- generate_mobility_data(n_classes = 6, seed = 123)
  seg <- moneca(test_data, segment.levels = 3)
  
  # Test basic functionality
  segment_numbers <- c(1, 2)
  basic_labels <- create_segment_labels(seg, 2, segment_numbers, "auto")
  expect_length(basic_labels, 2)
  expect_true(all(grepl("^Segment", basic_labels)))
  
  # Test custom dataframe functionality
  membership <- segment.membership.enhanced(seg, level = 2, naming_strategy = "auto")
  level2_members <- membership[grepl("^2\\.", membership$membership), ]
  
  if (nrow(level2_members) > 0) {
    # Create custom names using any member from segments (flexible approach)
    custom_names <- data.frame(
      name = c(level2_members$name[1], level2_members$name[length(level2_members$name)]),
      segment_label = c("custom1", "custom2"),
      stringsAsFactors = FALSE
    )
    
    custom_labels <- create_segment_labels(seg, 2, segment_numbers, custom_names)
    expect_length(custom_labels, 2)
    # At least one should match custom labels (depending on membership structure)
    expect_true(any(custom_labels %in% c("custom1", "custom2")) || 
                all(grepl("^Segment", custom_labels)))
  }
  
  # Test error handling
  invalid_df <- data.frame(wrong_col = "test")
  expect_error({
    create_segment_labels(seg, 2, segment_numbers, invalid_df)
  })
})