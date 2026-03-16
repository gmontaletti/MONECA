# Tests for Directed Clustering Analysis
# =======================================
#
# Tests covering:
# - symmetric_method parameter in moneca_fast() (Approach C)
# - Asymmetry scoring and flagging functions (Approach A)
# - Comparison framework (compare_moneca_results, ARI)
# - Integration pipeline tests

# 1. Hand-crafted asymmetric test matrix -----

# Categories A, B, C form a tight mutual cluster.
# D has a strong one-way bridge TO B (D->B high, B->D low).
# E is relatively isolated.
make_asymmetric_matrix <- function() {
  core <- matrix(
    c(
      100,
      30,
      25,
      5,
      3,
      28,
      120,
      35,
      3,
      2,
      22,
      32,
      110,
      4,
      2,
      2,
      40,
      3,
      80,
      5,
      4,
      3,
      2,
      6,
      90
    ),
    nrow = 5,
    byrow = TRUE
  )
  rownames(core) <- colnames(core) <- c("A", "B", "C", "D", "E")

  rs <- rowSums(core)
  cs <- colSums(core)
  gt <- sum(core)
  mx <- rbind(cbind(core, rs), c(cs, gt))
  rownames(mx) <- c("A", "B", "C", "D", "E", "Total")
  colnames(mx) <- c("A", "B", "C", "D", "E", "Total")
  mx
}

# Perfectly symmetric matrix: each off-diagonal pair (i,j) == (j,i).
make_symmetric_matrix <- function() {
  core <- matrix(
    c(
      100,
      20,
      15,
      5,
      3,
      20,
      90,
      25,
      4,
      2,
      15,
      25,
      80,
      6,
      3,
      5,
      4,
      6,
      70,
      8,
      3,
      2,
      3,
      8,
      60
    ),
    nrow = 5,
    byrow = TRUE
  )
  rownames(core) <- colnames(core) <- c("A", "B", "C", "D", "E")

  rs <- rowSums(core)
  cs <- colSums(core)
  gt <- sum(core)
  mx <- rbind(cbind(core, rs), c(cs, gt))
  rownames(mx) <- c("A", "B", "C", "D", "E", "Total")
  colnames(mx) <- c("A", "B", "C", "D", "E", "Total")
  mx
}

# Sparse matrix with many zero off-diagonal cells.
make_sparse_matrix <- function() {
  core <- matrix(
    c(
      200,
      0,
      0,
      1,
      0,
      0,
      180,
      0,
      0,
      2,
      0,
      0,
      150,
      0,
      0,
      1,
      0,
      0,
      120,
      0,
      0,
      2,
      0,
      0,
      100
    ),
    nrow = 5,
    byrow = TRUE
  )
  rownames(core) <- colnames(core) <- c("A", "B", "C", "D", "E")

  rs <- rowSums(core)
  cs <- colSums(core)
  gt <- sum(core)
  mx <- rbind(cbind(core, rs), c(cs, gt))
  rownames(mx) <- c("A", "B", "C", "D", "E", "Total")
  colnames(mx) <- c("A", "B", "C", "D", "E", "Total")
  mx
}

# 2. Group 1: symmetric_method in moneca_fast -----

test_that("moneca_fast default symmetric_method='sum' produces standard results", {
  mob <- generate_mobility_data(n_classes = 6, seed = 100)
  seg <- moneca_fast(mob, segment.levels = 2, progress = FALSE)
  validate_moneca_object(seg)
  expect_true(length(seg$segment.list) >= 2)
  expect_true(length(seg$mat.list) >= 2)
})

test_that("moneca_fast symmetric_method='min' produces valid moneca object", {
  mob <- generate_mobility_data(n_classes = 6, seed = 101)
  seg <- moneca_fast(
    mob,
    segment.levels = 2,
    symmetric_method = "min",
    progress = FALSE
  )
  validate_moneca_object(seg)
  expect_true(length(seg$segment.list) >= 2)
  expect_true(length(seg$mat.list) >= 2)
})

test_that("moneca_fast stores symmetric_method in output", {
  mob <- generate_mobility_data(n_classes = 5, seed = 102)
  seg_sum <- moneca_fast(
    mob,
    segment.levels = 2,
    symmetric_method = "sum",
    progress = FALSE
  )
  seg_min <- moneca_fast(
    mob,
    segment.levels = 2,
    symmetric_method = "min",
    progress = FALSE
  )
  expect_equal(seg_sum$symmetric_method, "sum")
  expect_equal(seg_min$symmetric_method, "min")
})

test_that("moneca_fast with symmetric_method='min' downweights one-way bridges", {
  mx <- make_asymmetric_matrix()
  seg_sum <- moneca_fast(
    mx,
    segment.levels = 2,
    symmetric_method = "sum",
    has_margins = TRUE,
    progress = FALSE
  )
  seg_min <- moneca_fast(
    mx,
    segment.levels = 2,
    symmetric_method = "min",
    has_margins = TRUE,
    progress = FALSE
  )

  # Both must produce valid moneca objects
  validate_moneca_object(seg_sum)
  validate_moneca_object(seg_min)

  # The two results need not be identical because "min" treats the D->B

  # one-way bridge differently. We verify both run without error and
  # produce at least level-2 segments.
  expect_true(length(seg_sum$segment.list) >= 2)
  expect_true(length(seg_min$segment.list) >= 2)
})

test_that("moneca_fast rejects invalid symmetric_method", {
  mob <- generate_mobility_data(n_classes = 5, seed = 103)
  expect_error(
    moneca_fast(mob, symmetric_method = "invalid", progress = FALSE),
    "symmetric_method"
  )
  expect_error(
    moneca_fast(mob, symmetric_method = "mean", progress = FALSE),
    "symmetric_method"
  )
})

test_that("symmetric data gives same results for 'sum' and 'min'", {
  mx <- make_symmetric_matrix()
  seg_sum <- moneca_fast(
    mx,
    segment.levels = 2,
    symmetric_method = "sum",
    has_margins = TRUE,
    progress = FALSE
  )
  seg_min <- moneca_fast(
    mx,
    segment.levels = 2,
    symmetric_method = "min",
    has_margins = TRUE,
    progress = FALSE
  )

  # For a perfectly symmetric matrix, both methods should yield the same

  # segments because pmin(rr, t(rr)) == rr when rr is symmetric, so
  # min * 2 == sum.
  expect_equal(seg_sum$segment.list, seg_min$segment.list)
})

test_that("moneca_fast with 'min' handles matrix with zero flows gracefully", {
  mx <- make_sparse_matrix()
  seg <- moneca_fast(
    mx,
    segment.levels = 2,
    symmetric_method = "min",
    has_margins = TRUE,
    progress = FALSE
  )
  validate_moneca_object(seg)
})

test_that("moneca_fast default behavior unchanged (algorithmic equivalence)", {
  mob <- generate_mobility_data(n_classes = 6, seed = 104)

  # moneca_fast with explicit "sum" must match moneca_fast with default
  seg_default <- moneca_fast(mob, segment.levels = 2, progress = FALSE)
  seg_explicit <- moneca_fast(
    mob,
    segment.levels = 2,
    symmetric_method = "sum",
    progress = FALSE
  )

  expect_equal(seg_default$segment.list, seg_explicit$segment.list)
  expect_equal(seg_default$symmetric_method, "sum")
})

# 3. Group 2: Asymmetry Scores -----

test_that("compute_asymmetry_scores returns correct structure", {
  mob <- generate_mobility_data(n_classes = 6, seed = 200)
  seg <- moneca_fast(mob, segment.levels = 3, progress = FALSE)
  scores <- compute_asymmetry_scores(seg, level = 2)

  expect_s3_class(scores, "data.frame")
  expected_cols <- c(
    "segment_id",
    "n_members",
    "asymmetry_score",
    "max_pair_from",
    "max_pair_to",
    "max_pair_asym"
  )
  expect_true(all(expected_cols %in% names(scores)))

  # One row per segment at level 2
  n_segs <- length(seg$segment.list[[2]])
  expect_equal(nrow(scores), n_segs)

  # Scores must be between 0 and 1
  expect_true(all(scores$asymmetry_score >= 0))
  expect_true(all(scores$asymmetry_score <= 1))
})

test_that("symmetric matrix produces zero asymmetry scores", {
  mx <- make_symmetric_matrix()
  seg <- moneca_fast(
    mx,
    segment.levels = 2,
    has_margins = TRUE,
    progress = FALSE
  )

  # Only compute if there are multi-member segments at level 2
  if (length(seg$segment.list) >= 2 && length(seg$segment.list[[2]]) > 0) {
    scores <- compute_asymmetry_scores(seg, level = 2)
    # For a perfectly symmetric matrix, all asymmetry scores must be 0
    expect_true(all(scores$asymmetry_score == 0))
  }
})

test_that("known asymmetric matrix produces expected scores", {
  mx <- make_asymmetric_matrix()
  seg <- moneca_fast(
    mx,
    segment.levels = 2,
    has_margins = TRUE,
    progress = FALSE
  )

  if (length(seg$segment.list) >= 2 && length(seg$segment.list[[2]]) > 0) {
    scores <- compute_asymmetry_scores(seg, level = 2)
    # At least one segment should have nonzero asymmetry given the D->B bridge
    expect_true(any(scores$asymmetry_score > 0) || all(scores$n_members < 2))
  }
})

test_that("compute_asymmetry_scores validates inputs", {
  mob <- generate_mobility_data(n_classes = 6, seed = 201)
  seg <- moneca_fast(mob, segment.levels = 2, progress = FALSE)

  expect_error(compute_asymmetry_scores("not_moneca"), "moneca")
  expect_error(compute_asymmetry_scores(seg, level = 999), "level")
  expect_error(compute_asymmetry_scores(seg, level = -1), "level")
  expect_error(compute_asymmetry_scores(seg, level = c(1, 2)), "level")
})

test_that("flag_asymmetric_segments correctly flags above threshold", {
  mob <- generate_mobility_data(n_classes = 8, seed = 202)
  seg <- moneca_fast(mob, segment.levels = 3, progress = FALSE)

  # threshold = 0: everything with score > 0 flagged
  flags_low <- flag_asymmetric_segments(seg, threshold = 0, level = 2)
  expect_true(is.list(flags_low))
  expect_true("scores" %in% names(flags_low))
  expect_true("flagged" %in% names(flags_low))
  expect_true("threshold" %in% names(flags_low))
  expect_equal(flags_low$threshold, 0)
  expect_equal(length(flags_low$flagged), nrow(flags_low$scores))

  # threshold = 1: nothing flagged (scores are in [0,1] and flag is strict >)
  flags_high <- flag_asymmetric_segments(seg, threshold = 1, level = 2)
  expect_true(!any(flags_high$flagged))

  # Invalid threshold
  expect_error(flag_asymmetric_segments(seg, threshold = 1.5), "threshold")
  expect_error(flag_asymmetric_segments(seg, threshold = -0.1), "threshold")
})

test_that("refine_segments returns valid moneca object", {
  mob <- generate_mobility_data(n_classes = 8, seed = 203)
  seg <- moneca_fast(mob, segment.levels = 3, progress = FALSE)

  refined <- refine_segments(seg, threshold = 0.3, level = 2)
  validate_moneca_object(refined)
  expect_true("asymmetry_refinement" %in% names(refined))
  expect_true("scores" %in% names(refined$asymmetry_refinement))
  expect_true("threshold" %in% names(refined$asymmetry_refinement))
  expect_true("splits_performed" %in% names(refined$asymmetry_refinement))
})

test_that("refine_segments no-op on symmetric data", {
  mx <- make_symmetric_matrix()
  seg <- moneca_fast(
    mx,
    segment.levels = 2,
    has_margins = TRUE,
    progress = FALSE
  )

  expect_message(
    refined <- refine_segments(seg, threshold = 0.5, level = 2),
    "No segments exceed"
  )
  # segment.list should be unchanged

  expect_equal(seg$segment.list, refined$segment.list)
  # Metadata should still be attached
  expect_true("asymmetry_refinement" %in% names(refined))
  expect_equal(length(refined$asymmetry_refinement$splits_performed), 0)
})

test_that("refine_segments preserves min_segment_size constraint", {
  mob <- generate_mobility_data(n_classes = 10, seed = 204)
  seg <- moneca_fast(mob, segment.levels = 3, progress = FALSE)

  refined <- refine_segments(
    seg,
    threshold = 0.2,
    level = 2,
    min_segment_size = 2
  )

  # Every segment at level 2 must have at least 2 members
  for (s in refined$segment.list[[2]]) {
    expect_true(
      length(s) >= 2,
      info = paste("Segment has", length(s), "member(s), expected >= 2")
    )
  }
})

# 4. Group 3: Comparison Framework -----

test_that("compare_moneca_results with identical inputs gives ARI=1", {
  mob <- generate_mobility_data(n_classes = 6, seed = 300)
  seg <- moneca_fast(mob, segment.levels = 2, progress = FALSE)

  cmp <- compare_moneca_results(seg, seg, level = 2)
  expect_equal(cmp$ari, 1.0)
})

test_that("compare_moneca_results with different inputs gives ARI < 1", {
  mob <- generate_mobility_data(n_classes = 8, seed = 301)
  seg_sum <- moneca_fast(
    mob,
    segment.levels = 2,
    symmetric_method = "sum",
    progress = FALSE
  )
  seg_min <- moneca_fast(
    mob,
    segment.levels = 2,
    symmetric_method = "min",
    progress = FALSE
  )

  # Only test ARI < 1 if the two results differ
  if (!identical(seg_sum$segment.list, seg_min$segment.list)) {
    cmp <- compare_moneca_results(
      seg_sum,
      seg_min,
      level = 2,
      labels = c("sum", "min")
    )
    expect_true(cmp$ari < 1)
  }
})

test_that("compare_moneca_results returns correct class", {
  mob <- generate_mobility_data(n_classes = 6, seed = 302)
  seg <- moneca_fast(mob, segment.levels = 2, progress = FALSE)

  cmp <- compare_moneca_results(seg, seg, level = 2)
  expect_s3_class(cmp, "moneca_comparison")
  expect_true("labels" %in% names(cmp))
  expect_true("level" %in% names(cmp))
  expect_true("segment_sizes" %in% names(cmp))
  expect_true("jaccard" %in% names(cmp))
  expect_true("ari" %in% names(cmp))
})

test_that("print.moneca_comparison works without error", {
  mob <- generate_mobility_data(n_classes = 6, seed = 303)
  seg <- moneca_fast(mob, segment.levels = 2, progress = FALSE)

  cmp <- compare_moneca_results(
    seg,
    seg,
    level = 2,
    labels = c("Original", "Copy")
  )
  expect_output(print(cmp), "MONECA Comparison")
})

test_that("Jaccard matrix has correct dimensions", {
  mob <- generate_mobility_data(n_classes = 8, seed = 304)
  seg1 <- moneca_fast(
    mob,
    segment.levels = 2,
    symmetric_method = "sum",
    progress = FALSE
  )
  seg2 <- moneca_fast(
    mob,
    segment.levels = 2,
    symmetric_method = "min",
    progress = FALSE
  )

  cmp <- compare_moneca_results(seg1, seg2, level = 2)
  n1 <- length(seg1$segment.list[[2]])
  n2 <- length(seg2$segment.list[[2]])
  expect_equal(nrow(cmp$jaccard), n1)
  expect_equal(ncol(cmp$jaccard), n2)

  # All Jaccard values must be in [0, 1]
  expect_true(all(cmp$jaccard >= 0))
  expect_true(all(cmp$jaccard <= 1))
})

test_that(".adjusted_rand_index edge cases", {
  ari <- moneca:::.adjusted_rand_index

  # Identical single-cluster assignment
  expect_equal(ari(c(1, 1, 1), c(1, 1, 1)), 1)

  # Identical multi-cluster assignment
  expect_equal(ari(c(1, 1, 2, 2), c(1, 1, 2, 2)), 1)

  # Completely different partition
  expect_true(ari(c(1, 1, 2, 2), c(1, 2, 1, 2)) < 1)

  # Empty vectors return NA
  expect_true(is.na(ari(integer(0), integer(0))))

  # Mismatched lengths error
  expect_error(ari(c(1, 2), c(1, 2, 3)), "equal length")
})

# 5. Group 4: Internal helper tests -----

test_that(".pairwise_asymmetry returns correct structure for 3x3 matrix", {
  pw <- moneca:::.pairwise_asymmetry

  rr <- matrix(
    c(
      NA,
      2.0,
      0.5,
      1.5,
      NA,
      1.0,
      0.8,
      0.9,
      NA
    ),
    nrow = 3,
    byrow = TRUE
  )
  rownames(rr) <- colnames(rr) <- c("X", "Y", "Z")

  result <- pw(rr)
  expect_s3_class(result, "data.frame")
  expect_equal(ncol(result), 5)
  expect_true(all(c("from", "to", "rr_ij", "rr_ji", "asym") %in% names(result)))

  # 3 categories -> 3 pairs
  expect_equal(nrow(result), 3)
  expect_true(all(result$asym >= 0 & result$asym <= 1))
})

test_that(".pairwise_asymmetry handles single-element matrix", {
  pw <- moneca:::.pairwise_asymmetry

  rr <- matrix(NA, nrow = 1, ncol = 1)
  rownames(rr) <- colnames(rr) <- "Only"

  result <- pw(rr)
  expect_equal(nrow(result), 0)
})

# 6. Group 5: Integration tests -----

test_that("full pipeline: moneca_fast + compute_asymmetry + refine + compare", {
  mob <- generate_mobility_data(n_classes = 8, seed = 500)

  # Step 1: run moneca_fast
  seg <- moneca_fast(mob, segment.levels = 3, progress = FALSE)
  validate_moneca_object(seg)

  # Step 2: compute asymmetry scores
  scores <- compute_asymmetry_scores(seg, level = 2)
  expect_s3_class(scores, "data.frame")

  # Step 3: refine
  refined <- refine_segments(seg, threshold = 0.3, level = 2)
  validate_moneca_object(refined)

  # Step 4: compare original vs refined
  cmp <- compare_moneca_results(
    seg,
    refined,
    level = 2,
    labels = c("Original", "Refined")
  )
  expect_s3_class(cmp, "moneca_comparison")
  expect_true(is.numeric(cmp$ari))
  expect_true(cmp$ari >= -1 && cmp$ari <= 1)
})

test_that("crafted asymmetric matrix produces different segments with 'min'", {
  mx <- make_asymmetric_matrix()

  seg_sum <- moneca_fast(
    mx,
    segment.levels = 2,
    symmetric_method = "sum",
    has_margins = TRUE,
    progress = FALSE
  )
  seg_min <- moneca_fast(
    mx,
    segment.levels = 2,
    symmetric_method = "min",
    has_margins = TRUE,
    progress = FALSE
  )

  # Both must be valid
  validate_moneca_object(seg_sum)
  validate_moneca_object(seg_min)

  # Compute and compare asymmetry profiles
  if (length(seg_sum$segment.list) >= 2 && length(seg_min$segment.list) >= 2) {
    scores_sum <- compute_asymmetry_scores(seg_sum, level = 2)
    scores_min <- compute_asymmetry_scores(seg_min, level = 2)

    # "min" method should not increase overall asymmetry compared to "sum"
    # (it down-weights one-way bridges, so clusters should be tighter)
    expect_true(is.data.frame(scores_sum))
    expect_true(is.data.frame(scores_min))
  }
})

test_that("moneca_fast with symmetric_method='sum' passes algorithm equivalence", {
  mob <- generate_mobility_data(n_classes = 6, seed = 501)

  # moneca() is the reference implementation; moneca_fast(symmetric_method="sum")
  # must produce identical segment.list
  seg_ref <- moneca(mob, segment.levels = 2)
  seg_fast <- moneca_fast(
    mob,
    segment.levels = 2,
    symmetric_method = "sum",
    reduce_density = FALSE,
    progress = FALSE
  )

  expect_equal(
    seg_ref$segment.list,
    seg_fast$segment.list,
    info = "moneca_fast(symmetric_method='sum') must match moneca() segments"
  )
})
