# 1. Test isolates parameter for moneca_fast -----

test_that("isolates = FALSE maintains backward compatibility", {
  test_data <- generate_mobility_data(n_classes = 10, seed = 123)

  seg_default <- moneca_fast(test_data, segment.levels = 3, progress = FALSE)
  seg_false <- moneca_fast(
    test_data,
    segment.levels = 3,
    isolates = FALSE,
    progress = FALSE
  )

  expect_identical(seg_default$segment.list, seg_false$segment.list)
  expect_identical(seg_default$mat.list, seg_false$mat.list)
  expect_null(seg_default$isolates_summary)
  expect_null(seg_false$isolates_summary)
})

test_that("isolates = TRUE returns isolates_summary", {
  test_data <- generate_mobility_data(n_classes = 10, seed = 123)
  seg <- moneca_fast(
    test_data,
    segment.levels = 3,
    isolates = TRUE,
    progress = FALSE
  )

  expect_true("isolates_summary" %in% names(seg))
  expect_true("membership" %in% names(seg$isolates_summary))
  expect_true("mobility_matrix" %in% names(seg$isolates_summary))
})

test_that("membership dataframe has correct structure", {
  test_data <- generate_mobility_data(n_classes = 10, seed = 123)
  seg <- moneca_fast(
    test_data,
    segment.levels = 3,
    isolates = TRUE,
    progress = FALSE
  )

  membership <- seg$isolates_summary$membership

  expect_s3_class(membership, "data.frame")
  expect_true("name" %in% names(membership))
  expect_true("group" %in% names(membership))
  expect_equal(nrow(membership), nrow(test_data) - 1)
})

test_that("mobility_matrix sums match original total", {
  test_data <- generate_mobility_data(n_classes = 10, seed = 123)
  seg <- moneca_fast(
    test_data,
    segment.levels = 3,
    isolates = TRUE,
    progress = FALSE
  )

  n <- nrow(test_data) - 1
  original_total <- sum(test_data[1:n, 1:n])
  mobility_total <- sum(seg$isolates_summary$mobility_matrix)

  expect_equal(mobility_total, original_total)
})

test_that("all categories assigned to a group or 'altri'", {
  test_data <- generate_mobility_data(n_classes = 10, seed = 123)
  seg <- moneca_fast(
    test_data,
    segment.levels = 3,
    isolates = TRUE,
    progress = FALSE
  )

  membership <- seg$isolates_summary$membership

  # All groups should be either "Segment_X" or "altri"
  expect_true(all(grepl("^Segment_|^altri$", membership$group)))
})

test_that("isolates_summary groups match segment.list structure", {
  test_data <- generate_mobility_data(n_classes = 10, seed = 123)
  seg <- moneca_fast(
    test_data,
    segment.levels = 3,
    isolates = TRUE,
    progress = FALSE
  )

  membership <- seg$isolates_summary$membership
  final_segments <- seg$segment.list[[length(seg$segment.list)]]

  # Number of Segment_X groups should match final_segments length
  segment_groups <- unique(membership$group[membership$group != "altri"])
  expect_equal(length(segment_groups), length(final_segments))
})

test_that("isolates parameter works with early stopping", {
  # Create data that might trigger early stopping
  test_data <- generate_mobility_data(
    n_classes = 5,
    seed = 456,
    immobility_strength = 0.9
  )
  seg <- moneca_fast(
    test_data,
    segment.levels = 2,
    isolates = TRUE,
    progress = FALSE
  )

  # Should still have valid isolates_summary
  expect_true("isolates_summary" %in% names(seg))
  expect_s3_class(seg$isolates_summary$membership, "data.frame")
  expect_true(is.matrix(seg$isolates_summary$mobility_matrix))
})

test_that("category names are preserved in membership", {
  test_data <- generate_mobility_data(n_classes = 8, seed = 789)
  original_names <- rownames(test_data)[1:(nrow(test_data) - 1)]

  seg <- moneca_fast(
    test_data,
    segment.levels = 3,
    isolates = TRUE,
    progress = FALSE
  )

  membership <- seg$isolates_summary$membership
  expect_equal(membership$name, original_names)
})

test_that("mobility_matrix is square with proper dimensions", {
  test_data <- generate_mobility_data(n_classes = 12, seed = 321)
  seg <- moneca_fast(
    test_data,
    segment.levels = 3,
    isolates = TRUE,
    progress = FALSE
  )

  mobility_matrix <- seg$isolates_summary$mobility_matrix
  membership <- seg$isolates_summary$membership

  # Matrix should be square

  expect_equal(nrow(mobility_matrix), ncol(mobility_matrix))

  # Dimensions should match number of unique groups
  n_groups <- length(unique(membership$group))
  expect_equal(nrow(mobility_matrix), n_groups)
})

test_that("mobility_matrix row/column names match groups", {
  test_data <- generate_mobility_data(n_classes = 10, seed = 111)
  seg <- moneca_fast(
    test_data,
    segment.levels = 3,
    isolates = TRUE,
    progress = FALSE
  )

  mobility_matrix <- seg$isolates_summary$mobility_matrix
  membership <- seg$isolates_summary$membership

  # Row and column names should be the groups
  groups <- unique(membership$group)
  expect_true(all(rownames(mobility_matrix) %in% groups))
  expect_true(all(colnames(mobility_matrix) %in% groups))
})
