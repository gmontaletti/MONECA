# Unit Tests: Segment Helper Functions
# ======================================
#
# Tests for segment.membership() and segment.edges() helper functions.

test_that("segment.membership produces valid membership data", {
  test_data <- get_test_data("small")
  seg <- moneca(test_data, segment.levels = 2)

  membership <- segment.membership(seg)

  expect_is(membership, "data.frame")
  expect_true("name" %in% names(membership))
  expect_true("membership" %in% names(membership))
  expect_equal(nrow(membership), 5)  # n_classes (without totals)
})

test_that("segment.edges produces valid edge matrices", {
  test_data <- get_test_data("small")
  seg <- moneca(test_data, segment.levels = 2)

  edges <- segment.edges(seg)

  expect_is(edges, "matrix")
  expect_equal(nrow(edges), 5)  # n_classes (without totals row/column)
  expect_equal(ncol(edges), 5)

  # Check for reasonable values
  finite_values <- edges[is.finite(edges)]
  expect_true(all(finite_values >= 0))
})
