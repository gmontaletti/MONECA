# Test suite for joint parameter tuning functions

test_that("auto_tune_joint_parameters works with grid search", {
  # Generate test data
  set.seed(123)
  test_mx <- generate_mobility_data(n_classes = 4, seed = 123)
  
  # Test grid search
  result <- auto_tune_joint_parameters(
    mx = test_mx,
    method = "grid",
    n_grid_points = 3,  # Small grid for testing
    n_bootstrap = 5,    # Few bootstraps for speed
    verbose = FALSE
  )
  
  # Check result structure
  expect_s3_class(result, "moneca_joint_tuning")
  expect_true(is.numeric(result$optimal_scr))
  expect_true(is.numeric(result$optimal_cutoff))
  expect_true(result$optimal_scr >= 0)
  expect_true(result$optimal_cutoff >= 0.5)
  
  # Check that SCR is integer (new constraint)
  expect_true(result$optimal_scr == floor(result$optimal_scr))
  
  # Check optimization surface (dimensions may vary due to integer constraints)
  expect_true(is.matrix(result$optimization_surface))
  expect_true(nrow(result$optimization_surface) <= 3)  # May be fewer due to integer grid
  expect_true(ncol(result$optimization_surface) == 3)
  
  # Check scores
  expect_true(is.list(result$scores))
  expect_true(length(result$scores) <= 9)  # May be fewer combinations due to integer constraints
})

test_that("auto_tune_joint_parameters works with adaptive refinement", {
  set.seed(123)
  test_mx <- generate_mobility_data(n_classes = 4, seed = 123)
  
  # Test adaptive refinement
  result <- auto_tune_joint_parameters(
    mx = test_mx,
    method = "adaptive",
    verbose = FALSE
  )
  
  expect_s3_class(result, "moneca_joint_tuning")
  expect_true(!is.null(result$refinement_history))
  expect_true("coarse" %in% names(result$refinement_history))
  expect_true("fine" %in% names(result$refinement_history))
})

test_that("analyze_parameter_interaction computes correct metrics", {
  set.seed(123)
  test_mx <- generate_mobility_data(n_classes = 5, seed = 123)
  
  # Analyze interaction
  interaction <- analyze_parameter_interaction(
    mx = test_mx,
    scr = 2,
    cutoff = 1.5,
    n_samples = 5  # Small for testing
  )
  
  expect_s3_class(interaction, "parameter_interaction")
  
  # Check interaction strength
  expect_true(is.numeric(interaction$interaction_strength))
  expect_true(interaction$interaction_strength >= 0)
  expect_true(interaction$interaction_strength <= 1)
  
  # Check marginal effects
  expect_true(is.list(interaction$marginal_effects))
  expect_true("scr" %in% names(interaction$marginal_effects))
  expect_true("cutoff" %in% names(interaction$marginal_effects))
  
  # Check sensitivity
  expect_true(is.list(interaction$sensitivity))
  expect_true(all(c("scr", "cutoff", "relative") %in% names(interaction$sensitivity)))
  
  # Check density surface
  expect_true(is.matrix(interaction$density_surface))
  expect_equal(dim(interaction$density_surface), c(5, 5))
})

test_that("suggest_parameter_ranges provides reasonable suggestions", {
  set.seed(123)
  test_mx <- generate_mobility_data(n_classes = 5, seed = 123)
  
  # Test different methods
  for (method in c("conservative", "moderate", "aggressive")) {
    ranges <- suggest_parameter_ranges(test_mx, method = method)
    
    expect_true(is.list(ranges))
    expect_true(all(c("scr_range", "cutoff_range") %in% names(ranges)))
    
    # Check ranges are valid
    expect_length(ranges$scr_range, 2)
    expect_length(ranges$cutoff_range, 2)
    expect_true(ranges$scr_range[1] <= ranges$scr_range[2])
    expect_true(ranges$cutoff_range[1] <= ranges$cutoff_range[2])
    
    # Check that SCR ranges are integers (new constraint)
    expect_true(all(ranges$scr_range == floor(ranges$scr_range)))
    
    # Check data characteristics
    expect_true(!is.null(ranges$data_characteristics))
    expect_equal(ranges$data_characteristics$suggested_method, method)
  }
  
  # Test that ranges differ by method
  conservative <- suggest_parameter_ranges(test_mx, "conservative")
  aggressive <- suggest_parameter_ranges(test_mx, "aggressive")
  
  expect_true(conservative$scr_range[2] <= aggressive$scr_range[2])
  expect_true(conservative$cutoff_range[2] <= aggressive$cutoff_range[2])
})

test_that("evaluate_parameter_combination handles edge cases", {
  set.seed(123)
  test_mx <- generate_mobility_data(n_classes = 4, seed = 123)
  
  # Test with valid parameters
  scores_valid <- evaluate_parameter_combination(
    mx = test_mx,
    scr = 1,
    cutoff = 1,
    objectives = c("stability", "quality"),
    n_bootstrap = 5
  )
  
  expect_true(is.numeric(scores_valid))
  expect_equal(names(scores_valid), c("stability", "quality"))
  expect_true(all(scores_valid >= 0))
  
  # Test with extreme parameters (should return low/zero scores)
  scores_extreme <- evaluate_parameter_combination(
    mx = test_mx,
    scr = 1000,  # Very high, will filter everything
    cutoff = 10,
    objectives = c("stability", "quality"),
    n_bootstrap = 5
  )
  
  expect_true(all(scores_extreme <= 0.1))  # Should be very low or zero
})

test_that("plot_optimization_surface creates valid plots", {
  # Create mock result object
  mock_result <- list(
    optimal_scr = 2,
    optimal_cutoff = 1.5,
    optimization_surface = matrix(runif(25), 5, 5),
    scr_values = seq(0, 10, length.out = 5),
    cutoff_values = seq(0.5, 2.5, length.out = 5)
  )
  class(mock_result) <- "moneca_joint_tuning"
  
  # Test heatmap
  p_heat <- plot_optimization_surface(mock_result, type = "heatmap")
  expect_s3_class(p_heat, "ggplot")
  
  # Test contour
  p_contour <- plot_optimization_surface(mock_result, type = "contour")
  expect_s3_class(p_contour, "ggplot")
  
  # Test 3D (if plotly available)
  if (requireNamespace("plotly", quietly = TRUE)) {
    p_3d <- plot_optimization_surface(mock_result, type = "3d")
    expect_s3_class(p_3d, "plotly")
  }
})

test_that("joint tuning integrates with moneca function", {
  set.seed(123)
  test_mx <- generate_mobility_data(n_classes = 4, seed = 123)
  
  # Get optimal parameters
  tuning_result <- auto_tune_joint_parameters(
    mx = test_mx,
    method = "grid",
    n_grid_points = 3,
    n_bootstrap = 5,
    verbose = FALSE
  )
  
  # Use in moneca
  segments <- moneca(
    test_mx,
    small.cell.reduction = tuning_result$optimal_scr,
    cut.off = tuning_result$optimal_cutoff,
    segment.levels = 2
  )
  
  expect_s3_class(segments, "MONECA")
  expect_true(length(segments$segment.list) > 0)
})

test_that("parameter interaction strength is computed correctly", {
  # Create a matrix where parameters should interact strongly
  set.seed(123)
  n <- 10
  # Create a sparse matrix where both parameters matter
  test_mx <- matrix(rpois(n*n, lambda = 2), n, n)
  diag(test_mx) <- 0
  
  interaction <- analyze_parameter_interaction(
    mx = test_mx,
    scr = 1,
    cutoff = 1,
    n_samples = 10
  )
  
  # Check that we get a valid interaction measure
  expect_true(interaction$interaction_strength >= 0)
  expect_true(interaction$interaction_strength <= 1)
  
  # Check sensitivity measures
  expect_true(interaction$sensitivity$scr > 0)
  expect_true(interaction$sensitivity$cutoff > 0)
})

test_that("Pareto frontier optimization works", {
  set.seed(123)
  test_mx <- generate_mobility_data(n_classes = 4, seed = 123)
  
  result <- auto_tune_joint_parameters(
    mx = test_mx,
    method = "pareto",
    n_grid_points = 4,
    objectives = c("stability", "quality"),
    verbose = FALSE
  )
  
  expect_s3_class(result, "moneca_joint_tuning")
  expect_true(!is.null(result$pareto_indices))
  expect_true(length(result$pareto_indices) >= 1)
  expect_true(grepl("Pareto", result$selection_rationale))
})

test_that("print methods work correctly", {
  set.seed(123)
  test_mx <- generate_mobility_data(n_classes = 4, seed = 123)
  
  # Test joint tuning print
  result <- auto_tune_joint_parameters(
    mx = test_mx,
    method = "grid",
    n_grid_points = 3,
    n_bootstrap = 5,
    verbose = FALSE
  )
  
  expect_output(print(result), "MONECA Joint Parameter Tuning Results")
  expect_output(print(result), "small.cell.reduction")
  expect_output(print(result), "cut.off")
  
  # Test interaction print
  interaction <- analyze_parameter_interaction(
    mx = test_mx,
    scr = 2,
    cutoff = 1.5,
    n_samples = 5
  )
  
  expect_output(print(interaction), "Parameter Interaction Analysis")
  expect_output(print(interaction), "Interaction strength")
})

test_that("integer constraint utility functions work correctly", {
  # Test ensure_integer_scr_values
  decimal_values <- c(0.5, 1.3, 2.7, 3.0)
  integer_values <- ensure_integer_scr_values(decimal_values, warn_if_rounded = FALSE)
  
  expect_equal(integer_values, c(0L, 1L, 2L, 3L))
  expect_true(is.integer(integer_values))
  
  # Test bounds
  bounded_values <- ensure_integer_scr_values(c(-1, 0, 5, 15), min_value = 0, max_value = 10)
  expect_equal(bounded_values, c(0L, 5L, 10L))
  
  # Test generate_integer_scr_grid
  grid <- generate_integer_scr_grid(c(0, 10), n_points = 5)
  expect_true(all(grid == floor(grid)))  # All integers
  expect_true(all(grid >= 0 & grid <= 10))  # Within bounds
  expect_equal(grid[1], 0)  # Should include min
  expect_equal(grid[length(grid)], 10)  # Should include max
  
  # Test suggest_integer_scr_bounds
  set.seed(123)
  test_mx <- generate_mobility_data(n_classes = 5, seed = 123)
  bounds <- suggest_integer_scr_bounds(test_mx, "moderate")
  
  expect_length(bounds, 2)
  expect_true(all(bounds == floor(bounds)))  # Integer bounds
  expect_true(bounds[1] <= bounds[2])  # Valid range
  expect_equal(bounds[1], 0L)  # Should start at 0
})

test_that("integer constraints are enforced in joint tuning", {
  set.seed(123)
  test_mx <- generate_mobility_data(n_classes = 4, seed = 123)
  
  # Test with non-integer SCR range (should be rounded)
  expect_warning(
    result <- auto_tune_joint_parameters(
      mx = test_mx,
      scr_range = c(0.5, 5.7),  # Non-integer range
      method = "grid",
      n_grid_points = 3,
      n_bootstrap = 5,
      verbose = TRUE
    ),
    "small.cell.reduction"
  )
  
  # Check that result has integer SCR
  expect_true(result$optimal_scr == floor(result$optimal_scr))
})

test_that("moneca_fast works correctly for tuning", {
  set.seed(123)
  test_mx <- generate_mobility_data(n_classes = 5, seed = 123)
  
  # Test fast version
  result_fast <- moneca_fast(
    mx = test_mx,
    segment.levels = 2,
    cut.off = 1,
    small.cell.reduction = 1
  )
  
  expect_true(is.list(result_fast))
  expect_true("segment.list" %in% names(result_fast))
  expect_true("mat.list" %in% names(result_fast))
  
  # Compare with regular moneca (should be similar for first levels)
  result_regular <- moneca(
    test_mx,
    segment.levels = 2,
    cut.off = 1,
    small.cell.reduction = 1
  )
  
  # First level should be identical
  if (length(result_fast$segment.list) > 0 && length(result_regular$segment.list) > 0) {
    expect_equal(
      result_fast$segment.list[[1]],
      result_regular$segment.list[[1]]
    )
  }
})