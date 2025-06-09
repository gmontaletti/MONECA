test_that("generate_mobility_data creates valid matrices", {
  # Test basic functionality
  mob_data <- generate_mobility_data(n_classes = 5, n_total = 1000, seed = 123)
  
  expect_is(mob_data, "matrix")
  expect_equal(nrow(mob_data), 6)  # 5 classes + totals
  expect_equal(ncol(mob_data), 6)  # 5 classes + totals
  
  # Check row and column totals
  n_classes <- 5
  calculated_row_totals <- rowSums(mob_data[1:n_classes, 1:n_classes])
  actual_row_totals <- mob_data[1:n_classes, n_classes + 1]
  expect_equal(calculated_row_totals, actual_row_totals)
  
  calculated_col_totals <- colSums(mob_data[1:n_classes, 1:n_classes])
  actual_col_totals <- mob_data[n_classes + 1, 1:n_classes]
  expect_equal(calculated_col_totals, actual_col_totals)
  
  # Check grand total
  expect_equal(sum(mob_data[1:n_classes, 1:n_classes]), mob_data[n_classes + 1, n_classes + 1])
  expect_equal(mob_data[n_classes + 1, n_classes + 1], 1000)
})

test_that("generate_mobility_data parameter validation works", {
  expect_error(generate_mobility_data(n_classes = 1), "n_classes must be at least 2")
  expect_error(generate_mobility_data(n_classes = 5, n_total = 3), "n_total must be at least as large as n_classes")
  expect_error(generate_mobility_data(immobility_strength = 1.5), "immobility_strength must be between 0 and 1")
  expect_error(generate_mobility_data(class_clustering = -0.1), "class_clustering must be between 0 and 1")
  expect_error(generate_mobility_data(noise_level = 1.1), "noise_level must be between 0 and 1")
})

test_that("generate_mobility_data respects custom class names", {
  custom_names <- c("Upper", "Middle", "Lower")
  mob_data <- generate_mobility_data(n_classes = 3, class_names = custom_names, seed = 123)
  
  expect_equal(rownames(mob_data)[1:3], custom_names)
  expect_equal(colnames(mob_data)[1:3], custom_names)
  
  # Test wrong length class names
  expect_error(
    generate_mobility_data(n_classes = 3, class_names = c("A", "B")),
    "Length of class_names must equal n_classes"
  )
})

test_that("generate_mobility_data respects seed parameter", {
  mob_data1 <- generate_mobility_data(n_classes = 4, seed = 456)
  mob_data2 <- generate_mobility_data(n_classes = 4, seed = 456)
  mob_data3 <- generate_mobility_data(n_classes = 4, seed = 789)
  
  expect_equal(mob_data1, mob_data2)
  expect_false(identical(mob_data1, mob_data3))
})

test_that("generate_example_datasets creates valid datasets", {
  examples <- generate_example_datasets()
  
  expect_is(examples, "list")
  expect_true(all(c("simple", "complex", "fluid", "rigid") %in% names(examples)))
  
  # Test each dataset
  for (dataset_name in names(examples)) {
    dataset <- examples[[dataset_name]]
    expect_is(dataset, "matrix")
    
    # Check dimensions are consistent
    expect_equal(nrow(dataset), ncol(dataset))
    
    # Check totals are correct
    n_classes <- nrow(dataset) - 1
    calculated_row_totals <- rowSums(dataset[1:n_classes, 1:n_classes])
    actual_row_totals <- dataset[1:n_classes, n_classes + 1]
    expect_equal(calculated_row_totals, actual_row_totals)
  }
})

test_that("immobility_strength parameter affects diagonal dominance", {
  high_immobility <- generate_mobility_data(n_classes = 4, immobility_strength = 0.9, seed = 123)
  low_immobility <- generate_mobility_data(n_classes = 4, immobility_strength = 0.1, seed = 123)
  
  # Calculate diagonal proportions
  n_classes <- 3
  high_diag_prop <- sum(diag(high_immobility[1:n_classes, 1:n_classes])) / sum(high_immobility[1:n_classes, 1:n_classes])
  low_diag_prop <- sum(diag(low_immobility[1:n_classes, 1:n_classes])) / sum(low_immobility[1:n_classes, 1:n_classes])
  
  expect_gt(high_diag_prop, low_diag_prop)
})