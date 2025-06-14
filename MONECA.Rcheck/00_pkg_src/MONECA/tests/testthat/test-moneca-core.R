test_that("weight.matrix produces valid relative risk matrices", {
  # Create test data
  test_data <- generate_mobility_data(n_classes = 4, n_total = 1000, seed = 123)
  
  # Generate weight matrix
  wm <- weight.matrix(test_data, cut.off = 1)
  
  expect_is(wm, "matrix")
  expect_equal(nrow(wm), 4)  # Should exclude totals row/column
  expect_equal(ncol(wm), 4)
  
  # Check for reasonable values (relative risks should be positive or NA)
  finite_values <- wm[is.finite(wm)]
  expect_true(all(finite_values >= 0))
  
  # Check diagonal is NA (as set by default)
  expect_true(all(is.na(diag(wm))))
})

test_that("weight.matrix respects cut.off parameter", {
  test_data <- generate_mobility_data(n_classes = 4, n_total = 1000, seed = 123)
  
  wm_low <- weight.matrix(test_data, cut.off = 0.5)
  wm_high <- weight.matrix(test_data, cut.off = 2.0)
  
  # Higher cut-off should result in more NA values
  expect_gte(sum(is.na(wm_high)), sum(is.na(wm_low)))
})

test_that("weight.matrix symmetric parameter works", {
  test_data <- generate_mobility_data(n_classes = 4, n_total = 1000, seed = 123)
  
  wm_symmetric <- weight.matrix(test_data, symmetric = TRUE)
  wm_asymmetric <- weight.matrix(test_data, symmetric = FALSE)
  
  # Check if symmetric version is actually symmetric (ignoring NAs)
  finite_indices <- which(is.finite(wm_symmetric), arr.ind = TRUE)
  if (nrow(finite_indices) > 0) {
    for (i in seq_len(nrow(finite_indices))) {
      row_idx <- finite_indices[i, 1]
      col_idx <- finite_indices[i, 2]
      if (is.finite(wm_symmetric[col_idx, row_idx])) {
        expect_equal(wm_symmetric[row_idx, col_idx], wm_symmetric[col_idx, row_idx])
      }
    }
  }
})

test_that("moneca function runs without errors", {
  test_data <- generate_mobility_data(n_classes = 5, n_total = 1000, seed = 123)
  
  expect_no_error({
    seg <- moneca(test_data, segment.levels = 3)
  })
  
  seg <- moneca(test_data, segment.levels = 3)
  
  expect_is(seg, "moneca")
  expect_true("segment.list" %in% names(seg))
  expect_true("mat.list" %in% names(seg))
  expect_equal(length(seg$segment.list), 3)  # May stop early if only one segment remains
  expect_equal(length(seg$mat.list), 3)
})

test_that("moneca produces hierarchical segmentation", {
  test_data <- generate_mobility_data(n_classes = 6, n_total = 1000, seed = 123)
  seg <- moneca(test_data, segment.levels = 3)
  
  # Check that each level has fewer or equal segments than the previous
  n_segments <- sapply(seg$segment.list, length)
  
  # First level should be individual positions
  expect_equal(n_segments[1], 6)  # n_classes (6 classes in test data)
  
  # Subsequent levels should generally have fewer segments
  for (i in 2:length(n_segments)) {
    expect_lte(n_segments[i], n_segments[i-1])
  }
})

test_that("segment.membership produces valid membership data", {
  test_data <- generate_mobility_data(n_classes = 4, n_total = 1000, seed = 123)
  seg <- moneca(test_data, segment.levels = 2)
  
  membership <- segment.membership(seg)
  
  expect_is(membership, "data.frame")
  expect_true("name" %in% names(membership))
  expect_true("membership" %in% names(membership))
  expect_equal(nrow(membership), 4)  # n_classes (without totals)
})

test_that("segment.edges produces valid edge matrices", {
  test_data <- generate_mobility_data(n_classes = 4, n_total = 1000, seed = 123)
  seg <- moneca(test_data, segment.levels = 2)
  
  edges <- segment.edges(seg)
  
  expect_is(edges, "matrix")
  expect_equal(nrow(edges), 4)  # n_classes (without totals row/column)
  expect_equal(ncol(edges), 4)
  
  # Check for reasonable values
  finite_values <- edges[is.finite(edges)]
  expect_true(all(finite_values >= 0))
})

test_that("moneca handles edge cases", {
  # Test with minimal data
  small_data <- generate_mobility_data(n_classes = 3, n_total = 100, seed = 123)
  
  expect_no_error({
    seg_small <- moneca(small_data, segment.levels = 2)
  })
  
  # Test with single level
  expect_no_error({
    seg_single <- moneca(small_data, segment.levels = 1)
  })
})

test_that("find.segments produces valid clustering", {
  # Create a simple test matrix
  test_matrix <- matrix(c(
    50, 10, 5,
    15, 60, 10,
    8, 12, 40
  ), nrow = 3, byrow = TRUE)
  
  # Add totals
  row_totals <- rowSums(test_matrix)
  col_totals <- colSums(test_matrix)
  full_matrix <- rbind(
    cbind(test_matrix, row_totals),
    c(col_totals, sum(test_matrix))
  )
  
  wm <- weight.matrix(full_matrix, cut.off = 1)
  
  # Create a simple clique list for testing
  cliques <- list(c(1, 2), c(2, 3), c(1))
  
  expect_no_error({
    result <- find.segments(wm, cliques, cut.off = 1)
  })
  
  result <- find.segments(wm, cliques, cut.off = 1)
  
  expect_true("membership" %in% names(result))
  expect_true("cliques" %in% names(result))
  expect_is(result$membership, "factor")
  expect_is(result$cliques, "list")
})