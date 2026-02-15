# Unit Tests: moneca_segments S3 Class
# =====================================
#
# Tests for the moneca_segments constructor, print method, and accessor
# functions.  Verifies consistency with level.matrix() output and
# cross-checks moneca() vs moneca_fast().

# 1. Shared test fixtures -----

test_data_small <- get_test_data("small", seed = 42)
seg_fast <- moneca_fast(test_data_small, segment.levels = 3)
seg_moneca <- moneca(test_data_small, segment.levels = 3)

# 2. Constructor basics -----

describe("moneca_segments constructor", {
  meta <- moneca_segments(seg_fast)

  it("returns a moneca_segments object", {
    expect_s3_class(meta, "moneca_segments")
  })

  it("has expected top-level components", {
    expect_true(is.list(meta$levels))
    expect_true(is.matrix(meta$weight_matrix))
    expect_true(is.character(meta$original_names))
    expect_true(is.integer(meta$n_levels) || is.numeric(meta$n_levels))
    expect_true(is.numeric(meta$small.cell.reduction))
  })

  it("n_levels matches segment.list length", {
    expect_equal(meta$n_levels, length(seg_fast$segment.list))
  })

  it("original_names excludes Total", {
    expect_false("Total" %in% meta$original_names)
    all_names <- rownames(seg_fast$mat.list[[1]])
    non_total <- all_names[all_names != "Total"]
    expect_equal(meta$original_names, non_total)
  })

  it("weight_matrix has no NA values", {
    expect_false(anyNA(meta$weight_matrix))
  })

  it("errors on non-moneca input", {
    expect_error(moneca_segments("not_moneca"), "moneca")
    expect_error(moneca_segments(list()), "moneca")
  })
})

# 3. Level 1 trivial case -----

describe("level 1 (trivial)", {
  meta <- moneca_segments(seg_fast)
  map1 <- meta$levels[[1]]$map
  groups1 <- meta$levels[[1]]$groups

  it("each node is in its own group", {
    expect_equal(length(groups1), length(meta$original_names))
    for (i in seq_along(groups1)) {
      expect_equal(groups1[[i]], i)
    }
  })

  it("map has one row per node", {
    expect_equal(nrow(map1), length(meta$original_names))
  })

  it("all n_members are 1", {
    expect_true(all(map1$n_members == 1L))
  })

  it("main_node equals the node name", {
    expect_equal(map1$main_node, meta$original_names)
  })

  it("label equals the node name (no suffix)", {
    expect_equal(map1$label, meta$original_names)
    expect_false(any(grepl("\\+", map1$label)))
  })

  it("members equals the node name", {
    expect_equal(map1$members, meta$original_names)
  })
})

# 4. Level 2+ groups -----

describe("level 2+ groups", {
  meta <- moneca_segments(seg_fast)
  n_levels <- meta$n_levels

  if (n_levels >= 2) {
    map2 <- meta$levels[[2]]$map
    groups2 <- meta$levels[[2]]$groups

    it("n_members sums to total number of nodes", {
      expect_equal(sum(map2$n_members), length(meta$original_names))
    })

    it("groups cover all nodes exactly once", {
      all_indices <- sort(unlist(groups2))
      expect_equal(all_indices, seq_along(meta$original_names))
    })

    it("multi-member groups have +N suffix in label", {
      for (i in seq_len(nrow(map2))) {
        if (map2$n_members[i] > 1L) {
          expected_suffix <- paste0("+", map2$n_members[i] - 1L)
          expect_true(
            grepl(paste0(expected_suffix, "$"), map2$label[i]),
            info = paste("Group", i, "label:", map2$label[i])
          )
        } else {
          expect_false(
            grepl("\\+[0-9]+$", map2$label[i]),
            info = paste("Singleton group", i, "should have no suffix")
          )
        }
      }
    })

    it("main_node is a member of the group", {
      for (i in seq_len(nrow(map2))) {
        member_names <- strsplit(map2$members[i], ", ")[[1]]
        expect_true(
          map2$main_node[i] %in% member_names,
          info = paste(
            "main_node",
            map2$main_node[i],
            "should be in members:",
            map2$members[i]
          )
        )
      }
    })

    it("map has expected columns", {
      expect_true(all(
        c("segment", "main_node", "members", "n_members", "label") %in%
          colnames(map2)
      ))
    })
  }
})

# 5. Consistency with level.matrix() -----

describe("consistency with level.matrix()", {
  meta <- moneca_segments(seg_fast)
  n_levels <- meta$n_levels

  it("level 1 map matches level.matrix() segment_map", {
    lm1 <- level.matrix(seg_fast, level = 1)
    smap_lm <- attr(lm1, "segment_map")
    map_meta <- meta$levels[[1]]$map

    expect_equal(smap_lm$segment, map_meta$segment)
    expect_equal(smap_lm$main_node, map_meta$main_node)
    expect_equal(smap_lm$members, map_meta$members)
    expect_equal(smap_lm$n_members, map_meta$n_members)
  })

  if (n_levels >= 2) {
    it("level 2 labels match level.matrix() rownames", {
      lm2 <- level.matrix(seg_fast, level = 2)
      map_meta <- meta$levels[[2]]$map
      expect_equal(rownames(lm2), map_meta$label)
    })

    it("level 2 segment_map matches metadata map (core columns)", {
      lm2 <- level.matrix(seg_fast, level = 2)
      smap_lm <- attr(lm2, "segment_map")
      map_meta <- meta$levels[[2]]$map

      expect_equal(smap_lm$segment, map_meta$segment)
      expect_equal(smap_lm$main_node, map_meta$main_node)
      expect_equal(smap_lm$members, map_meta$members)
      expect_equal(smap_lm$n_members, map_meta$n_members)
    })
  }

  if (n_levels >= 3) {
    it("level 3 labels match level.matrix() rownames", {
      lm3 <- level.matrix(seg_fast, level = 3)
      map_meta <- meta$levels[[3]]$map
      expect_equal(rownames(lm3), map_meta$label)
    })
  }
})

# 6. moneca() vs moneca_fast() cross-check -----

describe("moneca vs moneca_fast metadata", {
  meta_fast <- moneca_segments(seg_fast)
  meta_moneca <- moneca_segments(seg_moneca)

  it("same number of levels", {
    expect_equal(meta_fast$n_levels, meta_moneca$n_levels)
  })

  it("same original_names", {
    expect_equal(meta_fast$original_names, meta_moneca$original_names)
  })

  it("same group structure at each level", {
    for (lvl in seq_len(meta_fast$n_levels)) {
      map_fast <- meta_fast$levels[[lvl]]$map
      map_moneca <- meta_moneca$levels[[lvl]]$map

      expect_equal(
        map_fast$segment,
        map_moneca$segment,
        info = paste("Level", lvl, "segments")
      )
      expect_equal(
        map_fast$main_node,
        map_moneca$main_node,
        info = paste("Level", lvl, "main_node")
      )
      expect_equal(
        map_fast$members,
        map_moneca$members,
        info = paste("Level", lvl, "members")
      )
      expect_equal(
        map_fast$n_members,
        map_moneca$n_members,
        info = paste("Level", lvl, "n_members")
      )
      expect_equal(
        map_fast$label,
        map_moneca$label,
        info = paste("Level", lvl, "label")
      )
    }
  })
})

# 7. Accessor functions -----

describe("get_segment_map()", {
  meta <- moneca_segments(seg_fast)

  it("returns a data.frame", {
    expect_true(is.data.frame(get_segment_map(meta, level = 1)))
  })

  it("errors on invalid level", {
    expect_error(get_segment_map(meta, level = 0), "level")
    expect_error(get_segment_map(meta, level = 99), "level")
  })

  it("errors on non-moneca_segments input", {
    expect_error(get_segment_map("bad", level = 1), "moneca_segments")
  })
})

describe("get_segment_groups()", {
  meta <- moneca_segments(seg_fast)

  it("returns a list", {
    expect_true(is.list(get_segment_groups(meta, level = 1)))
  })

  it("errors on invalid level", {
    expect_error(get_segment_groups(meta, level = 0), "level")
  })
})

describe("get_representative()", {
  meta <- moneca_segments(seg_fast)

  it("returns a character string", {
    rep_name <- get_representative(meta, level = 1, group_id = 1)
    expect_true(is.character(rep_name))
    expect_equal(length(rep_name), 1)
  })

  it("errors on invalid group_id", {
    expect_error(get_representative(meta, level = 1, group_id = 0), "group_id")
    expect_error(
      get_representative(meta, level = 1, group_id = 999),
      "group_id"
    )
  })

  it("errors on invalid level", {
    expect_error(get_representative(meta, level = 99, group_id = 1), "level")
  })
})

describe("get_segment_label()", {
  meta <- moneca_segments(seg_fast)

  it("returns a character string", {
    lbl <- get_segment_label(meta, level = 1, group_id = 1)
    expect_true(is.character(lbl))
    expect_equal(length(lbl), 1)
  })

  it("errors on invalid group_id", {
    expect_error(get_segment_label(meta, level = 1, group_id = 0), "group_id")
  })
})

# 8. Print method -----

describe("print.moneca_segments()", {
  meta <- moneca_segments(seg_fast)

  it("prints without error", {
    expect_output(print(meta), "moneca_segments object")
  })

  it("shows node count", {
    expect_output(print(meta), "Nodes:")
  })

  it("shows level count", {
    expect_output(print(meta), "Levels:")
  })

  it("returns invisibly", {
    expect_invisible(print(meta))
  })
})

# 9. Single-level segmentation -----

describe("single-level segmentation", {
  test_data <- get_test_data("small", seed = 42)
  seg_1level <- moneca_fast(test_data, segment.levels = 1)
  meta_1 <- moneca_segments(seg_1level)

  it("n_levels matches segment.list length", {
    # segment.levels = 1 still produces level 1 (trivial) + level 2 (first segmentation)
    expect_equal(meta_1$n_levels, length(seg_1level$segment.list))
  })

  it("level 1 has each node alone", {
    expect_equal(
      length(meta_1$levels[[1]]$groups),
      length(meta_1$original_names)
    )
  })
})

# 10. Early-stopping case -----

describe("early-stopping", {
  # Use dense data that converges quickly
  test_data_dense <- get_dense_test_data(n_classes = 5, seed = 789)
  seg_early <- moneca_fast(test_data_dense, segment.levels = 5)

  it("actual levels may be fewer than requested", {
    meta_early <- moneca_segments(seg_early)
    expect_lte(meta_early$n_levels, 5)
    expect_gte(meta_early$n_levels, 1)
  })
})

# 11. Medium data -----

describe("medium-size data", {
  test_data_med <- get_test_data("medium", seed = 42)
  seg_med <- moneca_fast(test_data_med, segment.levels = 3)
  meta_med <- moneca_segments(seg_med)

  it("constructs without error", {
    expect_s3_class(meta_med, "moneca_segments")
  })

  it("all levels have groups that cover all nodes", {
    for (lvl in seq_len(meta_med$n_levels)) {
      groups <- meta_med$levels[[lvl]]$groups
      all_idx <- sort(unlist(groups))
      expect_equal(
        all_idx,
        seq_along(meta_med$original_names),
        info = paste("Level", lvl, "should cover all nodes")
      )
    }
  })
})
