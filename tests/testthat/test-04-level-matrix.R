# Unit Tests: level.matrix() Function
# ====================================
#
# Tests for the level.matrix() function that extracts and labels
# hierarchical mobility matrices from moneca results.

# 1. Shared test fixtures -----

test_data_small <- get_test_data("small", seed = 42)
seg_moneca <- moneca(test_data_small, segment.levels = 3)
seg_fast <- moneca_fast(test_data_small, segment.levels = 3)

# 2. Input validation -----

describe("input validation", {
  it("errors on non-moneca object", {
    expect_error(level.matrix("not_moneca"), "moneca")
  })

  it("errors when level is too high", {
    expect_error(level.matrix(seg_fast, level = 99), "level")
  })

  it("errors when level is zero", {
    expect_error(level.matrix(seg_fast, level = 0), "level")
  })

  it("errors when level is non-integer", {
    expect_error(level.matrix(seg_fast, level = 1.5))
  })
})

# 3. Level 1 extraction -----

describe("level 1", {
  result_l1 <- level.matrix(seg_fast, level = 1)

  it("returns a matrix", {
    expect_true(is.matrix(result_l1))
  })

  it("has row/col names matching original node names (no Total)", {
    orig_names <- rownames(seg_fast$mat.list[[1]])
    orig_names <- orig_names[orig_names != "Total"]
    expect_equal(rownames(result_l1), orig_names)
    expect_equal(colnames(result_l1), orig_names)
  })

  it("does not include Total in default output", {
    expect_false("Total" %in% rownames(result_l1))
    expect_false("Total" %in% colnames(result_l1))
  })

  it("values match mat.list[[1]] without Total row/col", {
    mx_full <- seg_fast$mat.list[[1]]
    total_idx <- which(rownames(mx_full) == "Total")
    mx_core <- mx_full[-total_idx, -total_idx, drop = FALSE]
    expect_equal(
      as.numeric(result_l1),
      as.numeric(mx_core)
    )
  })

  it("include_total = TRUE adds Total row and column back", {
    result_with_total <- level.matrix(seg_fast, level = 1, include_total = TRUE)
    expect_true("Total" %in% rownames(result_with_total))
    expect_true("Total" %in% colnames(result_with_total))
    expect_equal(nrow(result_with_total), nrow(result_l1) + 1)
    expect_equal(ncol(result_with_total), ncol(result_l1) + 1)
  })

  it("has a segment_map attribute that is a data.frame with expected columns", {
    smap <- attr(result_l1, "segment_map")
    expect_true(is.data.frame(smap))
    expect_true(all(
      c("segment", "main_node", "members", "n_members") %in%
        colnames(smap)
    ))
  })

  it("segment_map at level 1 has one row per original node", {
    smap <- attr(result_l1, "segment_map")
    expect_equal(nrow(smap), nrow(result_l1))
    expect_true(all(smap$n_members == 1))
  })
})

# 4. Level 2 and higher extraction -----

describe("level 2 and higher", {
  # Only run level 2+ tests when sufficient levels exist
  n_available_levels <- length(seg_fast$mat.list)

  if (n_available_levels >= 2) {
    result_l2 <- level.matrix(seg_fast, level = 2)
    smap_l2 <- attr(result_l2, "segment_map")

    it("returns a matrix with correct dimensions", {
      # Number of effective groups = cliques at this level + isolates carried
      # from the previous level; equals mat.list rows minus Total row.
      n_groups_l2 <- nrow(seg_fast$mat.list[[2]]) - 1L
      expect_equal(nrow(result_l2), n_groups_l2)
      expect_equal(ncol(result_l2), n_groups_l2)
    })

    it("row/col names are not numeric", {
      expect_false(all(grepl("^[0-9]+$", rownames(result_l2))))
    })

    it("row/col names use main_node+N format for multi-member segments", {
      orig_names <- rownames(seg_fast$mat.list[[1]])
      orig_names <- orig_names[orig_names != "Total"]
      # Strip the +N suffix to recover the base node name
      base_names <- sub("\\+[0-9]+$", "", rownames(result_l2))
      expect_true(all(base_names %in% orig_names))
      base_names_col <- sub("\\+[0-9]+$", "", colnames(result_l2))
      expect_true(all(base_names_col %in% orig_names))
    })

    it("multi-member segments have +N suffix, singletons do not", {
      for (i in seq_len(nrow(smap_l2))) {
        nm <- rownames(result_l2)[i]
        if (smap_l2$n_members[i] > 1L) {
          expected_suffix <- paste0("+", smap_l2$n_members[i] - 1L)
          expect_true(grepl(paste0(expected_suffix, "$"), nm))
        } else {
          expect_false(grepl("\\+[0-9]+$", nm))
        }
      }
    })

    it("segment_map has correct structure", {
      expect_true(is.data.frame(smap_l2))
      expect_true(all(
        c("segment", "main_node", "members", "n_members") %in%
          colnames(smap_l2)
      ))
    })

    it("segment_map has one row per effective group at that level", {
      n_groups_l2 <- nrow(seg_fast$mat.list[[2]]) - 1L
      expect_equal(nrow(smap_l2), n_groups_l2)
    })

    it("n_members sums to total number of original nodes", {
      orig_names <- rownames(seg_fast$mat.list[[1]])
      n_original <- sum(orig_names != "Total")
      expect_equal(sum(smap_l2$n_members), n_original)
    })

    it("include_total = TRUE adds a Total row/col", {
      result_l2_total <- level.matrix(seg_fast, level = 2, include_total = TRUE)
      expect_true("Total" %in% rownames(result_l2_total))
      expect_true("Total" %in% colnames(result_l2_total))
      expect_equal(nrow(result_l2_total), nrow(result_l2) + 1)
      expect_equal(ncol(result_l2_total), ncol(result_l2) + 1)
    })
  }
})

# 5. Consistency between moneca() and moneca_fast() -----

describe("moneca vs moneca_fast", {
  it("level 1 matrices are equal", {
    ml_moneca <- level.matrix(seg_moneca, level = 1)
    ml_fast <- level.matrix(seg_fast, level = 1)
    expect_equal(ml_moneca, ml_fast)
  })

  n_shared_levels <- min(
    length(seg_moneca$mat.list),
    length(seg_fast$mat.list)
  )
  if (n_shared_levels >= 2) {
    it("level 2 matrix values match", {
      ml_moneca <- level.matrix(seg_moneca, level = 2)
      ml_fast <- level.matrix(seg_fast, level = 2)
      expect_equal(unname(ml_moneca), unname(ml_fast))
    })

    it("level 2 segment_map attributes match", {
      ml_moneca <- level.matrix(seg_moneca, level = 2)
      ml_fast <- level.matrix(seg_fast, level = 2)
      smap_moneca <- attr(ml_moneca, "segment_map")
      smap_fast <- attr(ml_fast, "segment_map")
      expect_equal(smap_moneca, smap_fast)
    })
  }
})

# 6. Single-node segments -----

describe("single-node segments", {
  it("main_node equals the member name when segment has one member", {
    # Check across all available levels
    n_levels <- length(seg_fast$mat.list)
    for (lvl in seq_len(n_levels)) {
      result <- level.matrix(seg_fast, level = lvl)
      smap <- attr(result, "segment_map")
      single_rows <- smap[smap$n_members == 1, ]
      if (nrow(single_rows) > 0) {
        expect_equal(single_rows$main_node, single_rows$members)
      }
    }
  })
})

# 7. Default parameter -----

describe("defaults", {
  it("default level is 2 (works without specifying level)", {
    n_levels <- length(seg_fast$mat.list)
    if (n_levels >= 2) {
      result_default <- level.matrix(seg_fast)
      result_explicit <- level.matrix(seg_fast, level = 2)
      expect_equal(result_default, result_explicit)
    }
  })
})
