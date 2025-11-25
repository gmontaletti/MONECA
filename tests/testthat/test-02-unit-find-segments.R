# Unit Tests: find.segments Function
# ====================================
#
# Tests for the find.segments() function that identifies cliques
# in weighted networks.

test_that("find.segments produces valid clustering", {
  # Create a simple test matrix
  test_matrix <- create_simple_test_matrix(n_classes = 3, with_totals = TRUE)

  wm <- weight.matrix(test_matrix, cut.off = 1)

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
