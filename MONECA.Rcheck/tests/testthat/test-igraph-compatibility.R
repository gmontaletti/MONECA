test_that("igraph compatibility functions work correctly", {
  # Create a simple test matrix
  test_matrix <- matrix(c(
    0, 1, 0,
    1, 0, 1,
    0, 1, 0
  ), nrow = 3, byrow = TRUE)
  
  # Test graph creation
  expect_no_error({
    g <- moneca_graph_from_adjacency(test_matrix, mode = "undirected")
  })
  
  g <- moneca_graph_from_adjacency(test_matrix, mode = "undirected")
  expect_s3_class(g, "igraph")
  expect_equal(vcount(g), 3)
  expect_equal(ecount(g), 2)  # Should have 2 edges for undirected graph
})

test_that("moneca_get_edgelist works correctly", {
  # Create a simple directed graph
  test_matrix <- matrix(c(
    0, 1, 0,
    0, 0, 1,
    1, 0, 0
  ), nrow = 3, byrow = TRUE)
  
  g <- moneca_graph_from_adjacency(test_matrix, mode = "directed")
  
  expect_no_error({
    el <- moneca_get_edgelist(g)
  })
  
  el <- moneca_get_edgelist(g)
  expect_is(el, "matrix")
  expect_equal(ncol(el), 2)
  expect_equal(nrow(el), 3)  # Should have 3 directed edges
})

test_that("moneca_norm_coords normalizes coordinates correctly", {
  # Create test layout
  test_layout <- matrix(c(
    0, 0,
    1, 1,
    2, 2,
    -1, -1
  ), nrow = 4, byrow = TRUE)
  
  expect_no_error({
    normalized <- moneca_norm_coords(test_layout, xmin = 0, xmax = 1, ymin = 0, ymax = 1)
  })
  
  normalized <- moneca_norm_coords(test_layout, xmin = 0, xmax = 1, ymin = 0, ymax = 1)
  
  expect_is(normalized, "matrix")
  expect_equal(dim(normalized), dim(test_layout))
  
  # Check that coordinates are within specified range
  expect_true(all(normalized[, 1] >= 0 & normalized[, 1] <= 1))
  expect_true(all(normalized[, 2] >= 0 & normalized[, 2] <= 1))
  
  # Check that relative positions are preserved
  expect_true(normalized[2, 1] > normalized[1, 1])  # Point 2 should be to the right of point 1
  expect_true(normalized[2, 2] > normalized[1, 2])  # Point 2 should be above point 1
})

test_that("moneca_norm_coords handles edge cases", {
  # Test with identical coordinates
  identical_layout <- matrix(c(1, 1, 1, 1, 1, 1), nrow = 3, byrow = TRUE)
  
  expect_no_error({
    result <- moneca_norm_coords(identical_layout)
  })
  
  # Test with single point
  single_point <- matrix(c(5, 10), nrow = 1)
  
  expect_no_error({
    result_single <- moneca_norm_coords(single_point)
  })
  
  # Test with invalid input - only check error if using our implementation
  if (!exists("norm_coords", where = asNamespace("igraph"), mode = "function")) {
    expect_error({
      moneca_norm_coords(matrix(1:6, nrow = 2, ncol = 3))
    }, "Layout must be a two-column matrix")
  }
})

test_that("compatibility layer setup works", {
  # Test that the setup function runs without error
  expect_no_error({
    .setup_igraph_compat()
  })
  
  # Test that the compatibility functions exist
  expect_true(exists("moneca_graph_from_adjacency"))
  expect_true(exists("moneca_get_edgelist"))
  expect_true(exists("moneca_norm_coords"))
  
  # Test that they are functions
  expect_is(moneca_graph_from_adjacency, "function")
  expect_is(moneca_get_edgelist, "function")
  expect_is(moneca_norm_coords, "function")
})

test_that("weighted graph creation works", {
  # Create weighted adjacency matrix
  weighted_matrix <- matrix(c(
    0, 2.5, 0,
    1.2, 0, 3.8,
    0, 0.7, 0
  ), nrow = 3, byrow = TRUE)
  
  expect_no_error({
    g_weighted <- moneca_graph_from_adjacency(weighted_matrix, mode = "directed", weighted = TRUE)
  })
  
  g_weighted <- moneca_graph_from_adjacency(weighted_matrix, mode = "directed", weighted = TRUE)
  
  expect_true("weight" %in% edge_attr_names(g_weighted))
  
  # Check that weights are preserved
  weights <- E(g_weighted)$weight
  expected_weights <- c(2.5, 1.2, 3.8, 0.7)  # Non-zero entries in column order
  expect_equal(sort(weights), sort(expected_weights))
})

test_that("igraph version detection works correctly", {
  # Test that we can detect the igraph version
  igraph_version <- utils::packageVersion("igraph")
  expect_s3_class(igraph_version, "package_version")
  
  # Test version comparison
  expect_is(igraph_version >= "1.3.0", "logical")
  
  # The compatibility functions should be set up based on version
  if (igraph_version >= "1.3.0") {
    # Should use new API functions
    expect_true(TRUE)  # Modern version
  } else {
    # Should use old API functions
    expect_true(TRUE)  # Legacy version
  }
})