# Helper Functions for Test Data Generation
# ==========================================
#
# This file contains shared test data generation functions used across
# the moneca test suite. All functions use standardized parameters to
# ensure consistency across tests.

# 1. Standard Test Data Generators -----

#' Generate test data of standard sizes
#'
#' @param size Character string: "small", "medium", or "large"
#' @param seed Optional seed for reproducibility
#' @return A mobility matrix with totals row/column
get_test_data <- function(size = "small", seed = 42) {
  sizes <- list(
    small = 5,
    medium = 10,
    large = 20
  )

  if (!size %in% names(sizes)) {
    stop("size must be 'small', 'medium', or 'large'")
  }

  n <- sizes[[size]]
  generate_mobility_data(n_classes = n, seed = seed)
}

#' Generate test data with custom parameters
#'
#' @param n_classes Number of classes (default 5)
#' @param n_total Total number of observations (default 1000)
#' @param immobility_strength Diagonal dominance (default 0.7)
#' @param class_clustering Clustering strength (default 0.7)
#' @param noise_level Amount of noise (default 0.1)
#' @param seed Random seed (default 123)
#' @return A mobility matrix with totals row/column
get_custom_test_data <- function(n_classes = 5,
                                 n_total = 1000,
                                 immobility_strength = 0.7,
                                 class_clustering = 0.7,
                                 noise_level = 0.1,
                                 seed = 123) {
  generate_mobility_data(
    n_classes = n_classes,
    n_total = n_total,
    immobility_strength = immobility_strength,
    class_clustering = class_clustering,
    noise_level = noise_level,
    seed = seed
  )
}

#' Generate named test data with custom class names
#'
#' @param class_names Character vector of class names
#' @param seed Random seed (default 123)
#' @return A mobility matrix with custom row/column names
get_named_test_data <- function(class_names = c("Upper", "Middle", "Lower"),
                                seed = 123) {
  generate_mobility_data(
    n_classes = length(class_names),
    class_names = class_names,
    seed = seed
  )
}

# 2. Specialized Test Data Generators -----

#' Generate sparse mobility data (high immobility)
#'
#' @param n_classes Number of classes (default 5)
#' @param seed Random seed (default 456)
#' @return A sparse mobility matrix
get_sparse_test_data <- function(n_classes = 5, seed = 456) {
  generate_mobility_data(
    n_classes = n_classes,
    immobility_strength = 0.9,
    class_clustering = 0.9,
    noise_level = 0.05,
    seed = seed
  )
}

#' Generate dense mobility data (low immobility, high mobility)
#'
#' @param n_classes Number of classes (default 5)
#' @param seed Random seed (default 789)
#' @return A dense mobility matrix
get_dense_test_data <- function(n_classes = 5, seed = 789) {
  generate_mobility_data(
    n_classes = n_classes,
    immobility_strength = 0.1,
    class_clustering = 0.9,
    noise_level = 0.2,
    seed = seed
  )
}

#' Generate fully connected test data (pathological case)
#'
#' @param n_classes Number of classes (default 5)
#' @return A fully connected mobility matrix
get_fully_connected_data <- function(n_classes = 5) {
  dense_matrix <- matrix(100, nrow = n_classes, ncol = n_classes)
  diag(dense_matrix) <- 500
  rownames(dense_matrix) <- paste0("Class", 1:n_classes)
  colnames(dense_matrix) <- paste0("Class", 1:n_classes)
  dense_matrix
}

#' Generate minimal test data (2x2 matrix)
#'
#' @return A minimal 2x2 mobility matrix
get_minimal_test_data <- function() {
  min_data <- matrix(c(10, 2, 12,
                      1, 10, 11,
                      11, 12, 23),
                    nrow = 3, byrow = TRUE)
  rownames(min_data) <- colnames(min_data) <- c("A", "B", "Total")
  min_data
}

# 3. Test Matrix Creation Helpers -----

#' Create a simple weight matrix for testing
#'
#' @param n_classes Number of classes (default 3)
#' @param with_totals Add totals row/column (default TRUE)
#' @return A test matrix
create_simple_test_matrix <- function(n_classes = 3, with_totals = TRUE) {
  if (n_classes == 3) {
    test_matrix <- matrix(c(
      50, 10, 5,
      15, 60, 10,
      8, 12, 40
    ), nrow = 3, byrow = TRUE)
  } else {
    test_matrix <- matrix(
      runif(n_classes^2, min = 5, max = 50),
      nrow = n_classes
    )
    diag(test_matrix) <- diag(test_matrix) * 3
  }

  if (with_totals) {
    row_totals <- rowSums(test_matrix)
    col_totals <- colSums(test_matrix)
    test_matrix <- rbind(
      cbind(test_matrix, row_totals),
      c(col_totals, sum(test_matrix))
    )
  }

  rownames(test_matrix) <- colnames(test_matrix) <-
    c(paste0("Class", 1:n_classes), if (with_totals) "Total" else NULL)

  test_matrix
}

#' Create an adjacency matrix for testing
#'
#' @param n Number of nodes (default 3)
#' @param type Type of graph: "simple", "directed", "weighted" (default "simple")
#' @return An adjacency matrix
create_test_adjacency <- function(n = 3, type = "simple") {
  if (type == "simple") {
    adj_matrix <- matrix(c(
      0, 1, 0,
      1, 0, 1,
      0, 1, 0
    ), nrow = 3, byrow = TRUE)
  } else if (type == "directed") {
    adj_matrix <- matrix(c(
      0, 1, 0,
      0, 0, 1,
      1, 0, 0
    ), nrow = 3, byrow = TRUE)
  } else if (type == "weighted") {
    adj_matrix <- matrix(c(
      0, 2.5, 0,
      1.2, 0, 3.8,
      0, 0.7, 0
    ), nrow = 3, byrow = TRUE)
  } else {
    stop("type must be 'simple', 'directed', or 'weighted'")
  }

  if (nrow(adj_matrix) != n || ncol(adj_matrix) != n) {
    adj_matrix <- matrix(runif(n^2), nrow = n)
    diag(adj_matrix) <- 0
  }

  adj_matrix
}

# 4. Test Result Validation Helpers -----

#' Validate MONECA object structure
#'
#' @param seg A moneca object
#' @return TRUE if valid, otherwise throws error
validate_moneca_object <- function(seg) {
  expect_s3_class(seg, "moneca")
  expect_true("segment.list" %in% names(seg))
  expect_true("mat.list" %in% names(seg))
  expect_true(is.list(seg$segment.list))
  expect_true(is.list(seg$mat.list))
  expect_true(length(seg$segment.list) >= 1)
  expect_true(length(seg$mat.list) >= 1)
  invisible(TRUE)
}

#' Check that mobility matrix has valid totals
#'
#' @param mx A mobility matrix with totals
#' @return TRUE if valid, otherwise throws error
validate_mobility_matrix <- function(mx) {
  n_classes <- nrow(mx) - 1

  # Check row totals
  calculated_row_totals <- rowSums(mx[1:n_classes, 1:n_classes])
  actual_row_totals <- mx[1:n_classes, n_classes + 1]
  expect_equal(calculated_row_totals, actual_row_totals)

  # Check column totals
  calculated_col_totals <- colSums(mx[1:n_classes, 1:n_classes])
  actual_col_totals <- mx[n_classes + 1, 1:n_classes]
  expect_equal(calculated_col_totals, actual_col_totals)

  # Check grand total
  expect_equal(sum(mx[1:n_classes, 1:n_classes]), mx[n_classes + 1, n_classes + 1])

  invisible(TRUE)
}

# 5. Comparative Testing Helpers -----

#' Compare two MONECA results for equivalence
#'
#' @param result1 First moneca object
#' @param result2 Second moneca object
#' @param info Optional info message for expect calls
#' @return TRUE if equivalent, otherwise throws error
expect_moneca_equivalent <- function(result1, result2, info = NULL) {
  # Check structure
  expect_equal(length(result1$segment.list), length(result2$segment.list),
               info = paste(info, "- same number of levels"))

  # Check segment counts at each level
  for (level in seq_along(result1$segment.list)) {
    expect_equal(length(result1$segment.list[[level]]),
                 length(result2$segment.list[[level]]),
                 info = paste(info, sprintf("- Level %d segment count", level)))
  }

  # Check exact segment membership
  expect_equal(result1$segment.list, result2$segment.list,
               info = paste(info, "- segment lists should be identical"))

  invisible(TRUE)
}
