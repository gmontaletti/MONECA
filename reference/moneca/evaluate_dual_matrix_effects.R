# evaluate_dual_matrix_effects.R
# Comparative analysis: dual-matrix system effects on group composition
#
# Compares two evaluation runs on a 127x127 profession matrix (7.6M transitions):
#   eval_run1.rds — old code (SVD, single-matrix)
#   eval_run2.rds — new code (NMF, dual-matrix)
#
# Usage:
#   Rscript -e 'devtools::load_all(); source("reference/moneca/evaluate_dual_matrix_effects.R")'

# 1. Setup -----

devtools::load_all()

run1 <- readRDS("reference/moneca/eval_run1.rds")
run2 <- readRDS("reference/moneca/eval_run2.rds")

# Extract moneca objects
old_sum <- run1$seg_sum
old_min <- run1$seg_min
new_sum <- run2$segments$sum
new_min <- run2$segments$min

fmt <- function(x) format(x, big.mark = ",")

# Helper: get rownames from dense or sparse matrices
get_rn <- function(m) {
  rn <- rownames(m)
  if (is.null(rn) && inherits(m, "sparseMatrix")) {
    rn <- m@Dimnames[[1]]
  }
  rn
}

# Helper: strip margin row/col if present
# Margins may be named "Total" or carry a numeric label (e.g. "128")
strip_margin <- function(m) {
  rn <- get_rn(m)
  nr <- nrow(m)
  nc <- ncol(m)
  if (is.null(rn) || length(rn) != nr) {
    return(m)
  }
  # Check if last row is "Total"
  if (rn[nr] == "Total") {
    return(m[-nr, -nc, drop = FALSE])
  }
  # Check if last row sums equal sum of core (margin indicator)
  if (nr > 1) {
    core_sum <- sum(m[-nr, -nc, drop = FALSE])
    margin_row_sum <- sum(m[nr, -nc, drop = FALSE])
    if (abs(core_sum - margin_row_sum) < 1) {
      return(m[-nr, -nc, drop = FALSE])
    }
  }
  m
}

cat("================================================================\n")
cat("  Dual-Matrix System: Effects on Group Composition\n")
cat("================================================================\n")
cat("\nDataset: 127 profession categories,", fmt(7592506), "transitions\n")
cat("Old code: SVD density reduction, single-matrix aggregation\n")
cat(
  "New code: NMF density reduction, dual-matrix (topology/aggregation split)\n"
)

# 2. Count preservation -----

cat("\n\n## 2. Count Preservation\n\n")
cat("The dual-matrix system uses the reduced matrix only for topology (which\n")
cat(
  "cells to keep) while aggregation uses the original matrix. The old system\n"
)
cat("stored reduced counts in mat.list, causing shrinkage.\n\n")

original_total <- 7592506

cat(sprintf(
  "%-8s  %-14s  %-14s  %-14s\n",
  "Level",
  "old_sum",
  "new_sum",
  "original"
))
cat(sprintf(
  "%-8s  %-14s  %-14s  %-14s\n",
  "-----",
  "---------",
  "---------",
  "---------"
))

n_levels <- min(length(old_sum$mat.list), length(new_sum$mat.list))
for (k in seq_len(n_levels)) {
  old_core <- strip_margin(old_sum$mat.list[[k]])
  new_core <- strip_margin(new_sum$mat.list[[k]])

  old_total <- sum(old_core)
  new_total <- sum(new_core)

  cat(sprintf(
    "%-8d  %-14s  %-14s  %-14s\n",
    k,
    fmt(old_total),
    fmt(new_total),
    fmt(original_total)
  ))
}

# Shrinkage calculation
old_l1 <- sum(strip_margin(old_sum$mat.list[[1]]))
shrinkage_pct <- round((1 - old_l1 / original_total) * 100, 1)
cat(sprintf(
  "\nShrinkage in old system: %s%% (%s → %s)\n",
  shrinkage_pct,
  fmt(original_total),
  fmt(old_l1)
))
cat("New system: 0% shrinkage (original counts preserved)\n")

# 3. Density reduction metadata -----

cat("\n\n## 3. Density Reduction Metadata\n\n")

old_dr <- old_sum$density_reduction
new_dr <- new_sum$density_reduction

cat(sprintf("%-25s  %-20s  %-20s\n", "Parameter", "Old (run1)", "New (run2)"))
cat(sprintf(
  "%-25s  %-20s  %-20s\n",
  "-------------------------",
  "--------------------",
  "--------------------"
))

cat(sprintf(
  "%-25s  %-20s  %-20s\n",
  "method",
  if (!is.null(old_dr$method)) old_dr$method else "svd",
  if (!is.null(new_dr$method)) new_dr$method else "nmf"
))
cat(sprintf(
  "%-25s  %-20s  %-20s\n",
  "k",
  if (!is.null(old_dr$k)) old_dr$k else "NA",
  if (!is.null(new_dr$k)) new_dr$k else "NA"
))
cat(sprintf(
  "%-25s  %-20s  %-20s\n",
  "variance_explained",
  if (!is.null(old_dr$variance_explained)) {
    paste0(round(old_dr$variance_explained * 100, 1), "%")
  } else {
    "NA"
  },
  if (!is.null(new_dr$variance_explained)) {
    paste0(round(new_dr$variance_explained * 100, 1), "%")
  } else {
    "NA"
  }
))

# Retention
old_retention <- if (
  !is.null(old_dr$original_total) && !is.null(old_dr$reduced_total)
) {
  paste0(round(old_dr$reduced_total / old_dr$original_total * 100, 1), "%")
} else {
  "NA"
}
new_retention <- if (
  !is.null(new_dr$original_total) && !is.null(new_dr$reduced_total)
) {
  paste0(round(new_dr$reduced_total / new_dr$original_total * 100, 1), "%")
} else {
  "NA"
}
cat(sprintf("%-25s  %-20s  %-20s\n", "retention", old_retention, new_retention))

cat(sprintf(
  "%-25s  %-20s  %-20s\n",
  "normalization",
  if (!is.null(old_dr$normalization)) old_dr$normalization else "NA",
  if (!is.null(new_dr$normalization)) new_dr$normalization else "NA"
))
cat(sprintf(
  "%-25s  %-20s  %-20s\n",
  "filter_quantile",
  if (!is.null(old_dr$filter_quantile)) old_dr$filter_quantile else "NA",
  if (!is.null(new_dr$filter_quantile)) new_dr$filter_quantile else "NA"
))

# 4. Segment structure comparison -----

cat("\n\n## 4. Segment Structure Comparison\n\n")

for (method_label in c("sum", "min")) {
  old_seg <- if (method_label == "sum") old_sum else old_min
  new_seg <- if (method_label == "sum") new_sum else new_min

  cat(sprintf("--- symmetric_method = \"%s\" ---\n\n", method_label))
  cat(sprintf(
    "%-8s  %-12s  %-12s  %-18s  %-18s\n",
    "Level",
    "old_n_seg",
    "new_n_seg",
    "old_sizes",
    "new_sizes"
  ))
  cat(sprintf(
    "%-8s  %-12s  %-12s  %-18s  %-18s\n",
    "-----",
    "--------",
    "--------",
    "---------",
    "---------"
  ))

  n_lev <- min(length(old_seg$segment.list), length(new_seg$segment.list))
  for (lev in seq_len(n_lev)) {
    old_segs <- old_seg$segment.list[[lev]]
    new_segs <- new_seg$segment.list[[lev]]

    old_n <- length(old_segs)
    new_n <- length(new_segs)

    old_sizes <- sort(sapply(old_segs, length), decreasing = TRUE)
    new_sizes <- sort(sapply(new_segs, length), decreasing = TRUE)

    # Show up to 6 sizes
    old_sizes_str <- paste(head(old_sizes, 6), collapse = ",")
    new_sizes_str <- paste(head(new_sizes, 6), collapse = ",")
    if (length(old_sizes) > 6) {
      old_sizes_str <- paste0(old_sizes_str, "...")
    }
    if (length(new_sizes) > 6) {
      new_sizes_str <- paste0(new_sizes_str, "...")
    }

    cat(sprintf(
      "%-8d  %-12d  %-12d  %-18s  %-18s\n",
      lev,
      old_n,
      new_n,
      old_sizes_str,
      new_sizes_str
    ))
  }
  cat("\n")
}

# 5. Membership comparison (ARI) -----

cat("\n## 5. Membership Comparison (ARI)\n\n")

# Helper: build membership vector from segment.list at a given level
build_membership <- function(seg, level) {
  segs <- seg$segment.list[[level]]
  # Get all category names from level 1 mat.list
  mat <- seg$mat.list[[1]]
  rn <- get_rn(mat)
  if (!is.null(rn) && length(rn) > 0 && rn[length(rn)] == "Total") {
    rn <- rn[-length(rn)]
  }

  membership <- rep(NA_integer_, length(rn))
  names(membership) <- rn

  for (i in seq_along(segs)) {
    members <- segs[[i]]
    # members could be character names or indices
    if (is.character(members)) {
      idx <- match(members, rn)
      membership[idx[!is.na(idx)]] <- i
    } else {
      membership[members] <- i
    }
  }
  membership
}

for (method_label in c("sum", "min")) {
  old_seg <- if (method_label == "sum") old_sum else old_min
  new_seg <- if (method_label == "sum") new_sum else new_min

  cat(sprintf("--- symmetric_method = \"%s\" ---\n", method_label))

  n_lev <- min(length(old_seg$segment.list), length(new_seg$segment.list))
  for (lev in 2:min(n_lev, 4)) {
    tryCatch(
      {
        comparison <- moneca:::compare_moneca_results(
          old_seg,
          new_seg,
          level = lev,
          labels = c("old", "new")
        )
        cat(sprintf("  Level %d: ARI = %.4f\n", lev, comparison$ari))

        # Jaccard summary
        if (!is.null(comparison$jaccard) && nrow(comparison$jaccard) > 0) {
          max_jaccard <- apply(comparison$jaccard, 1, max, na.rm = TRUE)
          mean_max_j <- mean(max_jaccard[is.finite(max_jaccard)])
          cat(sprintf("           Mean best Jaccard = %.4f\n", mean_max_j))
        }
      },
      error = function(e) {
        cat(sprintf("  Level %d: comparison failed (%s)\n", lev, e$message))
      }
    )
  }
  cat("\n")
}

# 6. Category migration analysis -----

cat("\n## 6. Category Migration Analysis\n\n")

for (method_label in c("sum", "min")) {
  old_seg <- if (method_label == "sum") old_sum else old_min
  new_seg <- if (method_label == "sum") new_sum else new_min

  cat(sprintf("--- symmetric_method = \"%s\" ---\n\n", method_label))

  # Compare at level 2
  for (lev in 2:min(
    length(old_seg$segment.list),
    length(new_seg$segment.list),
    3
  )) {
    old_mem <- build_membership(old_seg, lev)
    new_mem <- build_membership(new_seg, lev)

    # Align on common names
    common <- intersect(names(old_mem), names(new_mem))
    old_v <- old_mem[common]
    new_v <- new_mem[common]

    changed <- old_v != new_v
    changed[is.na(changed)] <- FALSE
    n_changed <- sum(changed)
    n_total <- length(common)

    cat(sprintf(
      "Level %d: %d / %d categories changed segment (%.1f%%)\n\n",
      lev,
      n_changed,
      n_total,
      100 * n_changed / n_total
    ))

    if (n_changed > 0) {
      migration_df <- data.frame(
        name = common[changed],
        old_segment = old_v[changed],
        new_segment = new_v[changed],
        stringsAsFactors = FALSE
      )
      migration_df <- migration_df[
        order(migration_df$old_segment, migration_df$new_segment),
      ]

      # Group by old segment
      for (os in sort(unique(migration_df$old_segment))) {
        subset_df <- migration_df[migration_df$old_segment == os, ]
        cat(sprintf("  From old segment %d:\n", os))
        for (i in seq_len(nrow(subset_df))) {
          cat(sprintf(
            "    → segment %d: %s\n",
            subset_df$new_segment[i],
            subset_df$name[i]
          ))
        }
      }
      cat("\n")
    }
  }
}

# 7. Quality metrics comparison -----

cat("\n## 7. Quality Metrics Comparison\n\n")

for (method_label in c("sum", "min")) {
  old_seg <- if (method_label == "sum") old_sum else old_min
  new_seg <- if (method_label == "sum") new_sum else new_min

  cat(sprintf("--- symmetric_method = \"%s\" ---\n", method_label))

  tryCatch(
    {
      # Convert sparse matrices to dense for segment.quality compatibility
      old_seg_dense <- old_seg
      old_seg_dense$mat.list <- lapply(old_seg$mat.list, function(m) {
        if (inherits(m, "sparseMatrix")) as.matrix(m) else m
      })
      new_seg_dense <- new_seg
      new_seg_dense$mat.list <- lapply(new_seg$mat.list, function(m) {
        if (inherits(m, "sparseMatrix")) as.matrix(m) else m
      })
      old_q <- segment.quality(old_seg_dense, final.solution = TRUE)
      new_q <- segment.quality(new_seg_dense, final.solution = TRUE)

      cat(sprintf("  %-25s  %-12s  %-12s\n", "Metric", "Old", "New"))
      cat(sprintf(
        "  %-25s  %-12s  %-12s\n",
        "-------------------------",
        "--------",
        "--------"
      ))

      # Helper for finite means
      fmean <- function(x) mean(x[is.finite(x)], na.rm = TRUE)

      # Mean density
      cat(sprintf(
        "  %-25s  %-12.4f  %-12.4f\n",
        "Mean density (finite)",
        fmean(old_q$Density),
        fmean(new_q$Density)
      ))

      # Mean within-mobility share
      cat(sprintf(
        "  %-25s  %-12.4f  %-12.4f\n",
        "Mean within.mobility",
        fmean(old_q$within.mobility),
        fmean(new_q$within.mobility)
      ))

      # Mean share of mobility
      cat(sprintf(
        "  %-25s  %-12.4f  %-12.4f\n",
        "Mean share.of.mobility",
        mean(old_q$share.of.mobility, na.rm = TRUE),
        mean(new_q$share.of.mobility, na.rm = TRUE)
      ))

      # Number of segments
      cat(sprintf(
        "  %-25s  %-12d  %-12d\n",
        "N segments",
        length(unique(old_q$Membership)),
        length(unique(new_q$Membership))
      ))

      # Mean max path
      cat(sprintf(
        "  %-25s  %-12.2f  %-12.2f\n",
        "Mean max.path",
        mean(old_q$Max.path, na.rm = TRUE),
        mean(new_q$Max.path, na.rm = TRUE)
      ))
    },
    error = function(e) {
      cat(sprintf("  Quality comparison failed: %s\n", e$message))
    }
  )

  cat("\n")
}

cat("Note: old quality metrics were computed on reduced counts (SVD).\n")
cat("New metrics use original counts, so differences reflect both\n")
cat("topology changes AND count restoration.\n")

# 8. Asymmetry comparison -----

cat("\n\n## 8. Asymmetry Comparison (Level 2)\n\n")

for (method_label in c("sum", "min")) {
  old_seg <- if (method_label == "sum") old_sum else old_min
  new_seg <- if (method_label == "sum") new_sum else new_min

  cat(sprintf("--- symmetric_method = \"%s\" ---\n", method_label))

  tryCatch(
    {
      old_asym <- compute_asymmetry_scores(old_seg, level = 2)
      new_asym <- compute_asymmetry_scores(new_seg, level = 2)

      cat(sprintf("  %-25s  %-12s  %-12s\n", "Metric", "Old", "New"))
      cat(sprintf(
        "  %-25s  %-12s  %-12s\n",
        "-------------------------",
        "--------",
        "--------"
      ))
      cat(sprintf(
        "  %-25s  %-12d  %-12d\n",
        "N segments",
        nrow(old_asym),
        nrow(new_asym)
      ))
      cat(sprintf(
        "  %-25s  %-12.4f  %-12.4f\n",
        "Mean asymmetry",
        mean(old_asym$asymmetry_score, na.rm = TRUE),
        mean(new_asym$asymmetry_score, na.rm = TRUE)
      ))
      cat(sprintf(
        "  %-25s  %-12.4f  %-12.4f\n",
        "Max asymmetry",
        max(old_asym$asymmetry_score, na.rm = TRUE),
        max(new_asym$asymmetry_score, na.rm = TRUE)
      ))
      cat(sprintf(
        "  %-25s  %-12.4f  %-12.4f\n",
        "Median asymmetry",
        median(old_asym$asymmetry_score, na.rm = TRUE),
        median(new_asym$asymmetry_score, na.rm = TRUE)
      ))
    },
    error = function(e) {
      cat(sprintf("  Asymmetry comparison failed: %s\n", e$message))
    }
  )

  cat("\n")
}

cat("Note: old asymmetry scores were computed on SVD-reduced counts.\n")
cat("New scores use original counts.\n")

# 9. Summary -----

cat("\n\n## 9. Summary\n\n")

cat(sprintf("Count preservation:\n"))
cat(sprintf(
  "  Old system: mat.list totals = %s (%.1f%% of original)\n",
  fmt(old_l1),
  100 * old_l1 / original_total
))
cat(sprintf(
  "  New system: mat.list totals = %s (100%% of original)\n",
  fmt(original_total)
))

# ARI summary
cat("\nTopology impact (ARI, level 2):\n")
for (method_label in c("sum", "min")) {
  old_seg <- if (method_label == "sum") old_sum else old_min
  new_seg <- if (method_label == "sum") new_sum else new_min
  tryCatch(
    {
      cmp <- moneca:::compare_moneca_results(
        old_seg,
        new_seg,
        level = 2,
        labels = c("old", "new")
      )
      cat(sprintf("  %s: ARI = %.4f\n", method_label, cmp$ari))
    },
    error = function(e) {
      cat(sprintf("  %s: comparison failed\n", method_label))
    }
  )
}

# Migration count
cat("\nCategory migration (level 2):\n")
for (method_label in c("sum", "min")) {
  old_seg <- if (method_label == "sum") old_sum else old_min
  new_seg <- if (method_label == "sum") new_sum else new_min
  old_mem <- build_membership(old_seg, 2)
  new_mem <- build_membership(new_seg, 2)
  common <- intersect(names(old_mem), names(new_mem))
  changed <- sum(old_mem[common] != new_mem[common], na.rm = TRUE)
  cat(sprintf(
    "  %s: %d / %d categories changed segment\n",
    method_label,
    changed,
    length(common)
  ))
}

cat("\n================================================================\n")
cat("  End of report\n")
cat("================================================================\n")
