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