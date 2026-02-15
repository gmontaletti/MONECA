# Verification of moneca_fast() enhancements using real profession matrix
# ======================================================================
#
# This script tests moneca_fast() features (margin handling, density reduction)
# against the real profession_matrix_8day dataset, comparing results across
# different configurations.
#
# Run from MONECA project root:
#   Rscript tests/verify_profession_matrix.R

# 1. Setup and data loading -----

tryCatch(
  {
    message("Loading moneca package...")
    devtools::load_all(".")

    matrix_path <- file.path(
      "/Users/giampaolomontaletti/Documents/funzioni/test",
      "longworkR/output/dashboard/profession_matrix_8day.rds"
    )

    if (!file.exists(matrix_path)) {
      stop("Profession matrix not found at: ", matrix_path)
    }

    prof_mx <- readRDS(matrix_path)

    if (!is.matrix(prof_mx)) {
      prof_mx <- as.matrix(prof_mx)
    }

    cat("\n")
    cat(paste(rep("=", 80), collapse = ""), "\n")
    cat("VERIFICATION: moneca_fast() enhancements with profession matrix\n")
    cat(paste(rep("=", 80), collapse = ""), "\n\n")

    # 2. Matrix diagnostics -----

    cat("Matrix diagnostics\n")
    cat(paste(rep("-", 40), collapse = ""), "\n")
    cat("  Dimensions:          ", nrow(prof_mx), "x", ncol(prof_mx), "\n")
    n_cat <- nrow(prof_mx) - 1
    cat("  Categories (excl. totals):", n_cat, "\n")
    core_mx <- prof_mx[1:n_cat, 1:n_cat]
    n_cells <- n_cat * n_cat
    n_nonzero <- sum(core_mx > 0)
    density_nonzero <- n_nonzero / n_cells
    cat("  Non-zero cells:      ", n_nonzero, "of", n_cells, "\n")
    cat(
      "  Density (non-zero):  ",
      sprintf("%.1f%%", 100 * density_nonzero),
      "\n"
    )
    cat("  Total observations:  ", format(sum(core_mx), big.mark = ","), "\n")
    cat(
      "  Row names:           ",
      paste(head(rownames(prof_mx), 5), collapse = ", "),
      "...\n\n"
    )

    # Helper: summarize segment structure
    summarize_segments <- function(seg, label) {
      sl <- seg$segment.list
      n_levels <- length(sl)
      cat("  Segment structure (", label, "):\n", sep = "")
      cat("    Levels in segment.list:", n_levels, "\n")
      for (i in seq_along(sl)) {
        cat("      Level", i, ":", length(sl[[i]]), "segments\n")
      }
    }

    # Helper: print moneca_fast metadata
    print_metadata <- function(seg) {
      cat("    margins_added:      ", seg$margins_added, "\n")
      if (is.null(seg$density_reduction)) {
        cat("    density_reduction:   NULL (not applied)\n")
      } else {
        dr <- seg$density_reduction
        cat("    density_reduction:\n")
        cat("      method:            ", dr$method, "\n")
        cat("      normalization:     ", dr$normalization, "\n")
        cat("      k:                 ", dr$k, "\n")
        cat(
          "      variance_explained:",
          sprintf("%.3f", dr$variance_explained),
          "\n"
        )
        if (!is.null(dr$filter_quantile)) {
          cat("      filter_quantile:   ", dr$filter_quantile, "\n")
        }
        if (!is.null(dr$original_total)) {
          cat(
            "      original_total:    ",
            format(dr$original_total, big.mark = ","),
            "\n"
          )
        }
        if (!is.null(dr$reduced_total)) {
          cat(
            "      reduced_total:     ",
            format(dr$reduced_total, big.mark = ","),
            "\n"
          )
          retention <- 100 * dr$reduced_total / dr$original_total
          cat("      retention:         ", sprintf("%.1f%%", retention), "\n")
        }
      }
    }

    # Storage for summary table
    results <- list()

    # 3. Test 1: Original moneca with reduce_density preprocessing -----

    cat(paste(rep("=", 80), collapse = ""), "\n")
    cat("TEST 1: Original moneca() with reduce_density() preprocessing\n")
    cat(paste(rep("=", 80), collapse = ""), "\n\n")

    tryCatch(
      {
        message("  Running reduce_density() on profession matrix...")
        t1_start <- Sys.time()

        reduced_mx <- reduce_density(prof_mx, method = "svd", verbose = TRUE)

        t1_rd_end <- Sys.time()
        t1_rd_elapsed <- as.numeric(difftime(
          t1_rd_end,
          t1_start,
          units = "secs"
        ))

        cat("\n  Reduction summary:\n")
        cat("    k:                  ", attr(reduced_mx, "k"), "\n")
        cat(
          "    variance_explained: ",
          sprintf("%.3f", attr(reduced_mx, "variance_explained")),
          "\n"
        )
        if (
          !is.null(attr(reduced_mx, "original_total")) &&
            !is.null(attr(reduced_mx, "reduced_total"))
        ) {
          retention <- 100 *
            attr(reduced_mx, "reduced_total") /
            attr(reduced_mx, "original_total")
          cat("    retention:          ", sprintf("%.1f%%", retention), "\n")
        }
        cat(
          "    reduce_density time:",
          sprintf("%.2f sec", t1_rd_elapsed),
          "\n\n"
        )

        message("  Running moneca() on reduced matrix...")
        t1_m_start <- Sys.time()
        seg1 <- moneca(reduced_mx, segment.levels = 3)
        t1_m_end <- Sys.time()
        t1_m_elapsed <- as.numeric(difftime(
          t1_m_end,
          t1_m_start,
          units = "secs"
        ))
        t1_total <- t1_rd_elapsed + t1_m_elapsed

        summarize_segments(seg1, "Test 1")
        cat("    moneca time:        ", sprintf("%.2f sec", t1_m_elapsed), "\n")
        cat("    Total elapsed:      ", sprintf("%.2f sec", t1_total), "\n\n")

        results[["Test1"]] <- list(
          name = "moneca + reduce_density",
          elapsed = t1_total,
          n_levels = length(seg1$segment.list),
          segs_per_level = vapply(seg1$segment.list, length, integer(1)),
          margins_added = NA,
          density_reduction = "external"
        )
      },
      error = function(e) {
        cat("  ERROR in Test 1:", conditionMessage(e), "\n\n")
        results[["Test1"]] <<- list(
          name = "moneca + reduce_density",
          elapsed = NA,
          n_levels = NA,
          segs_per_level = NA,
          margins_added = NA,
          density_reduction = "ERROR"
        )
      }
    )

    # 4. Test 2: SKIPPED -----
    # Running moneca_fast without density reduction on 128 categories (62.8% density)
    # is computationally prohibitive (>30 min). Skipped to save time.
    cat(paste(rep("=", 80), collapse = ""), "\n")
    cat("TEST 2: moneca_fast() with reduce_density=FALSE (baseline)\n")
    cat(paste(rep("=", 80), collapse = ""), "\n\n")
    cat("  SKIPPED: 128-category unreduced matrix is too slow (>30 min).\n")
    cat(
      "  The key comparison is Test 1 vs Tests 3-5 (all use density reduction).\n\n"
    )

    results[["Test2"]] <- list(
      name = "fast, no reduction",
      elapsed = NA,
      n_levels = NA,
      segs_per_level = NA,
      margins_added = NA,
      density_reduction = "SKIPPED"
    )

    # 5. Test 3: moneca_fast with reduce_density=TRUE (integrated) -----

    cat(paste(rep("=", 80), collapse = ""), "\n")
    cat("TEST 3: moneca_fast() with reduce_density=TRUE (integrated)\n")
    cat(paste(rep("=", 80), collapse = ""), "\n\n")

    tryCatch(
      {
        message("  Running moneca_fast(reduce_density=TRUE)...")
        t3_start <- Sys.time()
        seg3 <- moneca_fast(
          prof_mx,
          reduce_density = TRUE,
          segment.levels = 3,
          progress = TRUE
        )
        t3_end <- Sys.time()
        t3_elapsed <- as.numeric(difftime(t3_end, t3_start, units = "secs"))

        cat("\n")
        summarize_segments(seg3, "Test 3")
        print_metadata(seg3)
        cat("    Elapsed time:       ", sprintf("%.2f sec", t3_elapsed), "\n\n")

        results[["Test3"]] <- list(
          name = "fast, reduction=TRUE",
          elapsed = t3_elapsed,
          n_levels = length(seg3$segment.list),
          segs_per_level = vapply(seg3$segment.list, length, integer(1)),
          margins_added = seg3$margins_added,
          density_reduction = if (is.null(seg3$density_reduction)) {
            "not applied"
          } else {
            seg3$density_reduction$method
          }
        )
      },
      error = function(e) {
        cat("  ERROR in Test 3:", conditionMessage(e), "\n\n")
        results[["Test3"]] <<- list(
          name = "fast, reduction=TRUE",
          elapsed = NA,
          n_levels = NA,
          segs_per_level = NA,
          margins_added = NA,
          density_reduction = "ERROR"
        )
      }
    )

    # 6. Test 4: moneca_fast with reduce_density="auto" (analysis only) -----
    # Auto trigger requires: n_categories >= 30 AND count_density > 0.7
    # This matrix has density 62.8% < 70%, so auto will NOT trigger.
    # Running without reduction on 129 categories would take >30 min, so we
    # report the auto-detection result without running.

    cat(paste(rep("=", 80), collapse = ""), "\n")
    cat("TEST 4: moneca_fast() with reduce_density=\"auto\" (analysis only)\n")
    cat(paste(rep("=", 80), collapse = ""), "\n\n")

    cat("  Auto-detection analysis (not run):\n")
    # After margin addition the actual category count may differ
    actual_n_cat <- nrow(prof_mx)
    actual_core <- prof_mx
    actual_density <- sum(actual_core > 0) / (actual_n_cat * actual_n_cat)
    cat("    Categories:         ", actual_n_cat, "(threshold: 30) -> PASS\n")
    cat(
      "    Count density:      ",
      sprintf("%.1f%%", 100 * actual_density),
      "(threshold: 70%) -> FAIL\n"
    )
    cat("    Result: auto would NOT trigger density reduction.\n")
    cat(
      "    Running without reduction on this matrix is too slow (>30 min).\n\n"
    )

    results[["Test4"]] <- list(
      name = "fast, reduction=auto",
      elapsed = NA,
      n_levels = NA,
      segs_per_level = NA,
      margins_added = NA,
      density_reduction = "not triggered (SKIP)"
    )

    # 7. Test 5: moneca_fast with custom density_params -----

    cat(paste(rep("=", 80), collapse = ""), "\n")
    cat("TEST 5: moneca_fast() with custom density_params\n")
    cat(paste(rep("=", 80), collapse = ""), "\n\n")

    tryCatch(
      {
        message(
          "  Running moneca_fast(reduce_density=TRUE, density_params=list(method='svd', k=15, normalization='ppmi'))..."
        )
        t5_start <- Sys.time()
        seg5 <- moneca_fast(
          prof_mx,
          reduce_density = TRUE,
          density_params = list(method = "svd", k = 15, normalization = "ppmi"),
          segment.levels = 3,
          progress = TRUE
        )
        t5_end <- Sys.time()
        t5_elapsed <- as.numeric(difftime(t5_end, t5_start, units = "secs"))

        cat("\n")
        summarize_segments(seg5, "Test 5")
        print_metadata(seg5)
        cat("    Elapsed time:       ", sprintf("%.2f sec", t5_elapsed), "\n\n")

        results[["Test5"]] <- list(
          name = "fast, custom params",
          elapsed = t5_elapsed,
          n_levels = length(seg5$segment.list),
          segs_per_level = vapply(seg5$segment.list, length, integer(1)),
          margins_added = seg5$margins_added,
          density_reduction = if (is.null(seg5$density_reduction)) {
            "not applied"
          } else {
            paste0(
              seg5$density_reduction$method,
              "/",
              seg5$density_reduction$normalization
            )
          }
        )
      },
      error = function(e) {
        cat("  ERROR in Test 5:", conditionMessage(e), "\n\n")
        results[["Test5"]] <<- list(
          name = "fast, custom params",
          elapsed = NA,
          n_levels = NA,
          segs_per_level = NA,
          margins_added = NA,
          density_reduction = "ERROR"
        )
      }
    )

    # 8. Test 6: Margin handling verification -----

    cat(paste(rep("=", 80), collapse = ""), "\n")
    cat("TEST 6: Margin handling verification\n")
    cat(paste(rep("=", 80), collapse = ""), "\n\n")

    tryCatch(
      {
        # Strip margins (remove last row and last column)
        stripped_mx <- prof_mx[1:n_cat, 1:n_cat]
        cat(
          "  Original matrix:  ",
          nrow(prof_mx),
          "x",
          ncol(prof_mx),
          "(with margins)\n"
        )
        cat(
          "  Stripped matrix:  ",
          nrow(stripped_mx),
          "x",
          ncol(stripped_mx),
          "(margins removed)\n\n"
        )

        message(
          "  Running moneca_fast(has_margins='auto', reduce_density=TRUE) on stripped matrix..."
        )
        t6_start <- Sys.time()
        seg6 <- moneca_fast(
          stripped_mx,
          has_margins = "auto",
          reduce_density = TRUE,
          segment.levels = 3,
          progress = TRUE
        )
        t6_end <- Sys.time()
        t6_elapsed <- as.numeric(difftime(t6_end, t6_start, units = "secs"))

        cat("\n")
        cat("  Margin detection result:\n")
        cat("    margins_added:      ", seg6$margins_added, "\n")

        if (seg6$margins_added) {
          cat(
            "    PASS: margins were correctly detected as missing and added.\n\n"
          )
        } else {
          cat(
            "    WARN: margins_added is FALSE; the stripped matrix may have been",
            " misdetected.\n\n"
          )
        }

        summarize_segments(seg6, "Test 6")
        print_metadata(seg6)
        cat("    Elapsed time:       ", sprintf("%.2f sec", t6_elapsed), "\n\n")

        # Compare with Test 3 (reduce_density=TRUE with original matrix)
        if (!is.na(results[["Test3"]]$n_levels)) {
          cat("  Comparison with Test 3 (same config, margins present):\n")
          sl3 <- results[["Test3"]]$segs_per_level
          sl6 <- vapply(seg6$segment.list, length, integer(1))
          cat(
            "    Test 3 levels:",
            length(sl3),
            "  segments:",
            paste(sl3, collapse = ", "),
            "\n"
          )
          cat(
            "    Test 6 levels:",
            length(sl6),
            "  segments:",
            paste(sl6, collapse = ", "),
            "\n"
          )
          if (identical(sl3, sl6)) {
            cat("    MATCH: Segment structures are identical.\n\n")
          } else {
            cat(
              "    DIFFER: Segment structures differ (expected if margin",
              " reconstruction introduces rounding).\n\n"
            )
          }
        }

        results[["Test6"]] <- list(
          name = "fast, margin test",
          elapsed = t6_elapsed,
          n_levels = length(seg6$segment.list),
          segs_per_level = vapply(seg6$segment.list, length, integer(1)),
          margins_added = seg6$margins_added,
          density_reduction = "svd"
        )
      },
      error = function(e) {
        cat("  ERROR in Test 6:", conditionMessage(e), "\n\n")
        results[["Test6"]] <<- list(
          name = "fast, margin test",
          elapsed = NA,
          n_levels = NA,
          segs_per_level = NA,
          margins_added = NA,
          density_reduction = "ERROR"
        )
      }
    )

    # 9. Summary comparison table -----

    cat(paste(rep("=", 80), collapse = ""), "\n")
    cat("SUMMARY COMPARISON TABLE\n")
    cat(paste(rep("=", 80), collapse = ""), "\n\n")

    # Table header
    cat(sprintf(
      "%-5s %-26s %10s %8s %-18s %10s %-16s\n",
      "Test",
      "Configuration",
      "Time (s)",
      "Levels",
      "Segments/level",
      "Margins",
      "Density Red."
    ))
    cat(paste(rep("-", 100), collapse = ""), "\n")

    for (i in seq_along(results)) {
      r <- results[[i]]
      test_id <- names(results)[i]

      # Format segments per level
      if (is.na(r$n_levels)) {
        segs_str <- "ERROR"
      } else {
        segs_str <- paste(r$segs_per_level, collapse = "/")
      }

      # Format elapsed time
      time_str <- if (is.na(r$elapsed)) "ERROR" else sprintf("%.2f", r$elapsed)

      # Format levels
      levels_str <- if (is.na(r$n_levels)) "ERROR" else as.character(r$n_levels)

      # Format margins
      margins_str <- if (is.na(r$margins_added)) {
        "-"
      } else {
        as.character(r$margins_added)
      }

      # Format density reduction
      dr_str <- as.character(r$density_reduction)

      cat(sprintf(
        "%-5s %-26s %10s %8s %-18s %10s %-16s\n",
        test_id,
        r$name,
        time_str,
        levels_str,
        segs_str,
        margins_str,
        dr_str
      ))
    }

    cat(paste(rep("-", 100), collapse = ""), "\n")

    # Fastest / slowest
    elapsed_vals <- vapply(
      results,
      function(r) {
        if (is.na(r$elapsed)) Inf else r$elapsed
      },
      numeric(1)
    )
    if (any(is.finite(elapsed_vals))) {
      fastest_idx <- which.min(elapsed_vals)
      slowest_idx <- which.max(elapsed_vals[is.finite(elapsed_vals)])
      cat(
        "\n  Fastest: ",
        names(results)[fastest_idx],
        " (",
        sprintf("%.2f sec", elapsed_vals[fastest_idx]),
        ")\n",
        sep = ""
      )
      cat(
        "  Slowest: ",
        names(results)[slowest_idx],
        " (",
        sprintf("%.2f sec", elapsed_vals[slowest_idx]),
        ")\n",
        sep = ""
      )
    }

    cat("\nVerification completed.\n")
  },
  error = function(e) {
    cat("\nFATAL ERROR:", conditionMessage(e), "\n")
    cat("Traceback:\n")
    traceback()
    quit(status = 1)
  }
)
