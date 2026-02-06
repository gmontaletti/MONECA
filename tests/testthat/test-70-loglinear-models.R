# Test Log-Linear Models for Mobility Tables
# ============================================
#
# Comprehensive test suite for fit_mobility_model() and
# compare_mobility_models(), covering all five model types
# (independence, quasi-independence, quasi-symmetry, uniform
# association, moneca topology), input validation, self-consistency
# properties, edge cases, and print methods.

# 1. Setup -----

# Use a 5-class matrix for most tests
test_mx <- get_test_data("small") # 5-class with margins, seed=42
test_seg <- moneca_fast(test_mx, segment.levels = 3)

# 2. Input handling -----

test_that("fit_mobility_model accepts matrix with margins", {
  m <- fit_mobility_model(test_mx, type = "independence")
  expect_s3_class(m, "moneca_loglinear")
  expect_equal(nrow(m$fitted), 5)
  expect_equal(ncol(m$fitted), 5)
})

test_that("fit_mobility_model accepts moneca object", {
  m <- fit_mobility_model(test_seg, type = "independence")
  expect_s3_class(m, "moneca_loglinear")
})

test_that("fit_mobility_model rejects invalid input", {
  expect_error(fit_mobility_model("not a matrix"))
  expect_error(fit_mobility_model(data.frame(x = 1:5)))
})

test_that("fit_mobility_model validates matrix dimensions", {
  # Non-square matrix
  bad_mx <- matrix(1:6, nrow = 2, ncol = 3)
  expect_error(fit_mobility_model(bad_mx))
})

test_that("fit_mobility_model validates type argument", {
  expect_error(fit_mobility_model(test_mx, type = "invalid_type"))
})

test_that("moneca_topology requires moneca object", {
  expect_error(fit_mobility_model(test_mx, type = "moneca_topology"))
})

test_that("moneca_topology requires level", {
  expect_error(
    fit_mobility_model(test_seg, type = "moneca_topology"),
    "level must be specified"
  )
})

test_that("moneca_topology validates level range", {
  expect_error(
    fit_mobility_model(test_seg, type = "moneca_topology", level = 99),
    "level must be between"
  )
})

# 3. Independence model -----

test_that("independence model has correct df", {
  m <- fit_mobility_model(test_mx, type = "independence")
  n <- 5
  # df = n^2 - 2(n-1) - 1 = (n-1)^2
  expect_equal(m$df, (n - 1)^2)
})

test_that("independence model deviance > 0 for structured data", {
  m <- fit_mobility_model(test_mx, type = "independence")
  expect_gt(m$deviance, 0)
})

test_that("independence model returns valid structure", {
  m <- fit_mobility_model(test_mx, type = "independence")
  expect_true(!is.null(m$model))
  expect_equal(m$type, "independence")
  expect_true(is.numeric(m$deviance))
  expect_true(is.numeric(m$df))
  expect_true(is.numeric(m$aic))
  expect_true(is.numeric(m$bic))
  expect_true(is.numeric(m$p_value))
  expect_true(m$p_value >= 0 && m$p_value <= 1)
  expect_true(is.numeric(m$dissimilarity_index))
  expect_true(m$dissimilarity_index >= 0 && m$dissimilarity_index <= 1)
  expect_true(is.numeric(m$coefficients))
  expect_true(is.matrix(m$fitted))
  expect_true(is.matrix(m$residuals))
  expect_true(is.list(m$design_info))
  expect_true(is.matrix(m$original_matrix))
  expect_null(m$segment_level)
})

# 4. Quasi-independence model -----

test_that("quasi-independence model has correct df", {
  m <- fit_mobility_model(test_mx, type = "quasi_independence")
  n <- 5
  # df = (n-1)^2 - n
  expect_equal(m$df, (n - 1)^2 - n)
})

test_that("quasi-independence fits diagonal perfectly", {
  m <- fit_mobility_model(test_mx, type = "quasi_independence")
  # Diagonal residuals should be ~0 (diagonal cells have their own parameters)
  diag_resid <- diag(m$residuals)
  expect_true(all(abs(diag_resid) < 0.01))
})

test_that("quasi-independence improves over independence", {
  m_ind <- fit_mobility_model(test_mx, type = "independence")
  m_qi <- fit_mobility_model(test_mx, type = "quasi_independence")
  expect_lt(m_qi$deviance, m_ind$deviance)
  expect_lt(m_qi$df, m_ind$df)
})

# 5. Quasi-symmetry model -----

test_that("quasi-symmetry model has correct df", {
  m <- fit_mobility_model(test_mx, type = "quasi_symmetry")
  n <- 5
  # df = (n-1)^2 - n - n*(n-1)/2 = (n-1)(n-2)/2
  expected_df <- (n - 1) * (n - 2) / 2
  expect_equal(m$df, expected_df)
})

test_that("quasi-symmetry improves over quasi-independence", {
  m_qi <- fit_mobility_model(test_mx, type = "quasi_independence")
  m_qs <- fit_mobility_model(test_mx, type = "quasi_symmetry")
  expect_lt(m_qs$deviance, m_qi$deviance)
})

# 6. Uniform association model -----

test_that("uniform association model has correct df", {
  m <- fit_mobility_model(test_mx, type = "uniform_association")
  n <- 5
  # df = (n-1)^2 - 1
  expect_equal(m$df, (n - 1)^2 - 1)
})

test_that("uniform association has one association parameter", {
  m <- fit_mobility_model(test_mx, type = "uniform_association")
  # Should have Origin + Destination + score_interaction
  expect_true("score_interaction" %in% names(m$coefficients))
})

test_that("uniform association works with midrank scores", {
  m <- fit_mobility_model(
    test_mx,
    type = "uniform_association",
    scores = "midrank"
  )
  expect_s3_class(m, "moneca_loglinear")
  expect_equal(m$design_info$scores, "midrank")
})

# 7. MONECA topology model -----

test_that("moneca_topology fits correctly", {
  m <- fit_mobility_model(test_seg, type = "moneca_topology", level = 2)
  expect_s3_class(m, "moneca_loglinear")
  expect_equal(m$type, "moneca_topology")
  expect_equal(m$segment_level, 2)
})

test_that("moneca_topology improves over quasi-independence", {
  m_qi <- fit_mobility_model(test_seg, type = "quasi_independence")
  m_top <- fit_mobility_model(test_seg, type = "moneca_topology", level = 2)
  # Topology model adds segment parameters, so deviance should decrease
  # (or at least not increase, for well-structured data it decreases)
  expect_lte(m_top$deviance, m_qi$deviance)
})

test_that("moneca_topology design_info contains segment info", {
  m <- fit_mobility_model(test_seg, type = "moneca_topology", level = 2)
  expect_true("segment_membership" %in% names(m$design_info))
  expect_true("n_segments" %in% names(m$design_info))
  expect_true(m$design_info$n_segments >= 1)
})

# 8. Model comparison -----

test_that("compare_mobility_models works with explicit models", {
  m1 <- fit_mobility_model(test_mx, type = "independence")
  m2 <- fit_mobility_model(test_mx, type = "quasi_independence")
  comp <- compare_mobility_models(m1, m2)

  expect_s3_class(comp, "moneca_model_comparison")
  expect_true(is.data.frame(comp$table))
  expect_equal(nrow(comp$table), 2)
  expect_true("model" %in% names(comp$table))
  expect_true("deviance" %in% names(comp$table))
  expect_true("bic" %in% names(comp$table))
})

test_that("compare_mobility_models table sorted by BIC", {
  m1 <- fit_mobility_model(test_mx, type = "independence")
  m2 <- fit_mobility_model(test_mx, type = "quasi_independence")
  m3 <- fit_mobility_model(test_mx, type = "quasi_symmetry")
  comp <- compare_mobility_models(m1, m2, m3)

  # Table should be sorted by BIC
  bics <- comp$table$bic
  expect_true(all(diff(bics) >= 0))
})

test_that("compare_mobility_models computes LR tests", {
  m1 <- fit_mobility_model(test_mx, type = "independence")
  m2 <- fit_mobility_model(test_mx, type = "quasi_independence")
  comp <- compare_mobility_models(m1, m2)

  expect_true(is.data.frame(comp$lr_tests))
  expect_gt(nrow(comp$lr_tests), 0)
  expect_true("delta_deviance" %in% names(comp$lr_tests))
  expect_true("delta_df" %in% names(comp$lr_tests))
  expect_true("p_value" %in% names(comp$lr_tests))
})

test_that("compare_mobility_models convenience mode fits all models", {
  comp <- compare_mobility_models(mx = test_seg, level = 2)

  expect_s3_class(comp, "moneca_model_comparison")
  expect_equal(nrow(comp$table), 5) # All 5 model types
  expected_types <- c(
    "independence",
    "quasi_independence",
    "quasi_symmetry",
    "uniform_association",
    "moneca_topology"
  )
  expect_true(all(expected_types %in% comp$table$model))
})

test_that("compare_mobility_models convenience mode without topology", {
  comp <- compare_mobility_models(mx = test_mx, include_topology = FALSE)

  expect_equal(nrow(comp$table), 4) # 4 models, no topology
  expect_false("moneca_topology" %in% comp$table$model)
})

test_that("compare_mobility_models rejects non-model objects", {
  expect_error(compare_mobility_models("not a model", "also not"))
})

test_that("compare_mobility_models requires at least 2 models", {
  m1 <- fit_mobility_model(test_mx, type = "independence")
  expect_error(compare_mobility_models(m1), "At least 2 models")
})

# 9. Self-consistency -----

test_that("fitted values sum to observed total", {
  for (type in c(
    "independence",
    "quasi_independence",
    "quasi_symmetry",
    "uniform_association"
  )) {
    m <- fit_mobility_model(test_mx, type = type)
    n <- nrow(test_mx) - 1
    observed_total <- sum(test_mx[1:n, 1:n])
    fitted_total <- sum(m$fitted)
    expect_equal(
      fitted_total,
      observed_total,
      tolerance = 0.01,
      info = paste("type:", type)
    )
  }
})

test_that("fitted values sum to observed total for topology", {
  m <- fit_mobility_model(test_seg, type = "moneca_topology", level = 2)
  n <- nrow(test_seg$mat.list[[1]]) - 1
  observed_total <- sum(test_seg$mat.list[[1]][1:n, 1:n])
  fitted_total <- sum(m$fitted)
  expect_equal(fitted_total, observed_total, tolerance = 0.01)
})

test_that("residuals centered at approximately 0", {
  for (type in c("independence", "quasi_independence")) {
    m <- fit_mobility_model(test_mx, type = type)
    # Mean of Pearson residuals should be approximately 0
    expect_true(abs(mean(m$residuals)) < 1, info = paste("type:", type))
  }
})

test_that("dissimilarity index between 0 and 1", {
  for (type in c(
    "independence",
    "quasi_independence",
    "quasi_symmetry",
    "uniform_association"
  )) {
    m <- fit_mobility_model(test_mx, type = type)
    expect_true(
      m$dissimilarity_index >= 0 && m$dissimilarity_index <= 1,
      info = paste("type:", type)
    )
  }
})

# 10. Edge cases -----

test_that("works with 2x2 matrix", {
  mx_2x2 <- get_minimal_test_data() # 2x2 + margins = 3x3
  m <- fit_mobility_model(mx_2x2, type = "independence")
  expect_s3_class(m, "moneca_loglinear")
  expect_equal(nrow(m$fitted), 2)
  expect_equal(m$df, 1) # (2-1)^2 = 1
})

test_that("works with symmetric matrix", {
  n <- 4
  sym_mx <- matrix(
    c(
      100,
      20,
      10,
      5,
      20,
      80,
      15,
      10,
      10,
      15,
      90,
      20,
      5,
      10,
      20,
      70
    ),
    nrow = n,
    byrow = TRUE
  )
  rownames(sym_mx) <- colnames(sym_mx) <- paste0("C", 1:n)
  # Add margins
  row_totals <- rowSums(sym_mx)
  col_totals <- colSums(sym_mx)
  sym_mx_m <- rbind(
    cbind(sym_mx, row_totals),
    c(col_totals, sum(sym_mx))
  )
  rownames(sym_mx_m) <- colnames(sym_mx_m) <- c(paste0("C", 1:n), "Total")

  m <- fit_mobility_model(sym_mx_m, type = "quasi_symmetry")
  expect_s3_class(m, "moneca_loglinear")
  # For a symmetric matrix, quasi-symmetry should fit very well
  # (deviance should be very small)
  expect_lt(m$deviance, 1)
})

test_that("handles small.cell.reduction", {
  m1 <- fit_mobility_model(
    test_mx,
    type = "independence",
    small.cell.reduction = 0
  )
  # Use a threshold high enough to zero out some cells in the test data
  # (minimum cell in the 5-class test data is 8)
  m2 <- fit_mobility_model(
    test_mx,
    type = "independence",
    small.cell.reduction = 50
  )
  # Models should differ when reduction zeroes out some cells
  expect_true(m1$deviance != m2$deviance)
})

# 11. Equivalence check -----

test_that("independence deviance matches chi-squared test", {
  n <- nrow(test_mx) - 1
  core <- test_mx[1:n, 1:n]

  # GLM-based deviance
  m <- fit_mobility_model(test_mx, type = "independence")

  # chisq.test uses X^2 (Pearson), not G^2 (likelihood ratio)
  # But we can compute G^2 manually from chisq.test expected values
  chi_result <- suppressWarnings(chisq.test(core))
  expected <- chi_result$expected

  # G^2 = 2 * sum(observed * log(observed/expected)) for non-zero cells
  nonzero <- core > 0
  g2_manual <- 2 * sum(core[nonzero] * log(core[nonzero] / expected[nonzero]))

  expect_equal(m$deviance, g2_manual, tolerance = 0.01)
})

# 12. Print methods -----

test_that("print.moneca_loglinear produces output", {
  m <- fit_mobility_model(test_mx, type = "independence")
  output <- capture.output(print(m))
  expect_true(any(grepl("Log-Linear Model", output)))
  expect_true(any(grepl("independence", output)))
  expect_true(any(grepl("Deviance", output)))
})

test_that("print.moneca_loglinear shows segment level for topology", {
  m <- fit_mobility_model(test_seg, type = "moneca_topology", level = 2)
  output <- capture.output(print(m))
  expect_true(any(grepl("Segment level: 2", output)))
})

test_that("print.moneca_model_comparison produces output", {
  comp <- compare_mobility_models(mx = test_mx, include_topology = FALSE)
  output <- capture.output(print(comp))
  expect_true(any(grepl("Model Comparison", output)))
  expect_true(any(grepl("Likelihood-Ratio", output)))
})

test_that("print returns invisible", {
  m <- fit_mobility_model(test_mx, type = "independence")
  expect_invisible(print(m))

  comp <- compare_mobility_models(mx = test_mx, include_topology = FALSE)
  expect_invisible(print(comp))
})
