# Evaluate Directed Clustering Experiment on Real Profession Data
#
# Compares symmetric_method = "sum" (baseline) vs "min" (min-reciprocity)
# on the 126x126 Italian profession mobility matrix (Lombardy, 1.6M transitions).
#
# Also evaluates the dual-matrix system: when reduce_density = TRUE, the reduced
# matrix determines topology while the original matrix is used for aggregation
# and storage in mat.list. This section verifies count preservation.
#
# Output: structured list `eval_results` with timing, comparison, dual-matrix
# diagnostics, and interpretability data. Profvis flame graphs saved as HTML.

# 1. Setup -----

library(moneca)
library(bench)

data_path <- file.path(
  "/Users/giampaolomontaletti/Documents/funzioni",
  "data_pipeline/output/dashboard/profession_matrix_8day.rds"
)

if (!file.exists(data_path)) {
  stop("Data file not found: ", data_path)
}

mx_raw <- readRDS(data_path)
n <- nrow(mx_raw)
mx <- mx_raw[1:(n - 3), 1:(n - 3)] # remove armed forces (last 3 rows/cols)

cat("Matrix dimensions:", nrow(mx), "x", ncol(mx), "\n")
cat("Total transitions:", sum(mx), "\n")
cat("Non-zero cells:", sum(mx > 0), "/", nrow(mx)^2, "\n\n")

# 2. Benchmark timing -----

cat("=== Benchmarking moneca_fast() ===\n")

bm <- bench::mark(
  sum = moneca_fast(
    mx,
    segment.levels = 3,
    progress = FALSE,
    reduce_density = TRUE,
    symmetric_method = "sum"
  ),
  min = moneca_fast(
    mx,
    segment.levels = 3,
    progress = FALSE,
    reduce_density = TRUE,
    symmetric_method = "min"
  ),
  iterations = 3,
  check = FALSE
)

cat("\nBenchmark results:\n")
print(bm[, intersect(
  c("expression", "min", "median", "max", "mem_alloc", "n_gc"),
  names(bm)
)])

# 3. Run both methods (single run for analysis) -----

cat("\n=== Running analysis ===\n")

t_sum <- system.time({
  seg_sum <- moneca_fast(
    mx,
    segment.levels = 3,
    progress = FALSE,
    reduce_density = TRUE,
    symmetric_method = "sum"
  )
})

t_min <- system.time({
  seg_min <- moneca_fast(
    mx,
    segment.levels = 3,
    progress = FALSE,
    reduce_density = TRUE,
    symmetric_method = "min"
  )
})

cat("sum elapsed:", t_sum["elapsed"], "s\n")
cat("min elapsed:", t_min["elapsed"], "s\n")

# 4. Time asymmetry and refinement functions -----

cat("\n=== Timing asymmetry functions ===\n")

t_asym <- system.time({
  asym_scores <- compute_asymmetry_scores(seg_sum, level = 2)
})
cat("compute_asymmetry_scores:", t_asym["elapsed"], "s\n")

t_refine <- system.time({
  seg_refined <- tryCatch(
    refine_segments(seg_sum, threshold = 0.3, level = 2),
    error = function(e) {
      cat("  refine_segments error:", conditionMessage(e), "\n")
      NULL
    }
  )
})
cat("refine_segments:", t_refine["elapsed"], "s\n")

# 5. Profvis (optional, saves HTML) -----

out_dir <- dirname(normalizePath(sys.frame(1)$ofile, mustWork = FALSE))
if (is.null(out_dir) || out_dir == "") {
  out_dir <- file.path(
    "/Users/giampaolomontaletti/Documents/funzioni/MONECA",
    "reference/moneca"
  )
}

if (requireNamespace("profvis", quietly = TRUE)) {
  cat("\n=== Profiling sum method ===\n")
  pv_sum <- profvis::profvis({
    moneca_fast(
      mx,
      segment.levels = 3,
      progress = FALSE,
      reduce_density = TRUE,
      symmetric_method = "sum"
    )
  })
  htmlwidgets::saveWidget(
    pv_sum,
    file.path(out_dir, "profvis_sum.html"),
    selfcontained = TRUE
  )

  cat("=== Profiling min method ===\n")
  pv_min <- profvis::profvis({
    moneca_fast(
      mx,
      segment.levels = 3,
      progress = FALSE,
      reduce_density = TRUE,
      symmetric_method = "min"
    )
  })
  htmlwidgets::saveWidget(
    pv_min,
    file.path(out_dir, "profvis_min.html"),
    selfcontained = TRUE
  )
  cat("Profvis HTML saved to", out_dir, "\n")
} else {
  cat("profvis not installed, skipping profiling\n")
}

# 6. Compare results -----

cat("\n=== Comparing results ===\n")

cmp <- compare_moneca_results(
  seg_sum,
  seg_min,
  level = 2,
  labels = c("sum", "min")
)
print(cmp)

# Also compare at level 3
cmp_l3 <- compare_moneca_results(
  seg_sum,
  seg_min,
  level = 3,
  labels = c("sum", "min")
)
cat("\n--- Level 3 ---\n")
print(cmp_l3)

# 7. Segment structure comparison -----

cat("\n=== Segment structure ===\n")

for (method_name in c("sum", "min")) {
  seg <- if (method_name == "sum") seg_sum else seg_min
  cat("\nMethod:", method_name, "\n")
  for (lv in 2:min(length(seg$segment.list), 3)) {
    sl <- seg$segment.list[[lv]]
    sizes <- vapply(sl, length, integer(1))
    cat(
      "  Level",
      lv,
      ":",
      length(sl),
      "segments, sizes =",
      paste(sort(sizes, decreasing = TRUE), collapse = ", "),
      "\n"
    )
  }
}

# 8. Asymmetry analysis -----

cat("\n=== Asymmetry scores (sum method, level 2) ===\n")

asym_sum <- compute_asymmetry_scores(seg_sum, level = 2)
asym_sum_sorted <- asym_sum[
  order(asym_sum$asymmetry_score, decreasing = TRUE),
]
cat("\nTop 5 most asymmetric segments:\n")
print(head(asym_sum_sorted, 5))

cat("\n=== Asymmetry scores (min method, level 2) ===\n")

asym_min <- compute_asymmetry_scores(seg_min, level = 2)
asym_min_sorted <- asym_min[
  order(asym_min$asymmetry_score, decreasing = TRUE),
]
cat("\nTop 5 most asymmetric segments:\n")
print(head(asym_min_sorted, 5))

# 9. Refinement analysis -----

cat("\n=== Refinement on sum result ===\n")

for (thresh in c(0.5, 0.3)) {
  refined <- tryCatch(
    refine_segments(seg_sum, threshold = thresh, level = 2),
    error = function(e) {
      cat(
        "  refine_segments error at threshold",
        thresh,
        ":",
        conditionMessage(e),
        "\n"
      )
      NULL
    }
  )
  if (!is.null(refined)) {
    n_splits <- length(refined$asymmetry_refinement$splits_performed)
    cat(
      "Threshold",
      thresh,
      ":",
      n_splits,
      "splits performed\n"
    )
    if (n_splits > 0) {
      cat(
        "  Split details:",
        paste(
          names(refined$asymmetry_refinement$splits_performed),
          collapse = ", "
        ),
        "\n"
      )
      cat(
        "  Original segments:",
        length(seg_sum$segment.list[[2]]),
        "-> Refined:",
        length(refined$segment.list[[2]]),
        "\n"
      )
    }
  }
}

# 10. One-way bridge examples -----

cat("\n=== Top one-way bridges (sum method, level 2) ===\n")

# Get category names
cat_names <- rownames(seg_sum$mat.list[[1]])
cat_names <- cat_names[-length(cat_names)]

# For top 3 asymmetric segments, show the bridge pair with profession names
top3 <- head(asym_sum_sorted[asym_sum_sorted$n_members >= 2, ], 3)

for (i in seq_len(nrow(top3))) {
  seg_id <- top3$segment_id[i]
  members <- seg_sum$segment.list[[2]][[seg_id]]
  member_names <- cat_names[members]
  cat(
    "\nSegment",
    seg_id,
    "(",
    length(members),
    "members ):\n",
    "  Members:",
    paste(member_names, collapse = ", "),
    "\n",
    "  Max asymmetric pair:",
    top3$max_pair_from[i],
    "<->",
    top3$max_pair_to[i],
    "(asym =",
    round(top3$max_pair_asym[i], 3),
    ")\n"
  )
}

# 11. Quality metrics -----

cat("\n=== Quality metrics ===\n")

q_sum <- segment.quality(seg_sum)
q_min <- segment.quality(seg_min)

cat("Sum method quality:\n")
print(q_sum)
cat("\nMin method quality:\n")
print(q_min)

# 12. Dual-matrix system evaluation -----
#
# With the dual-matrix system, density reduction only determines topology
# (which categories group together). The original unreduced matrix is used
# for all stored aggregations and metrics, preserving count totals.

cat("\n=== Dual-matrix system: count preservation ===\n")

original_total <- sum(mx)
cat("Original matrix total:", original_total, "\n\n")

# Check mat.list totals for both methods
for (method_name in c("sum", "min")) {
  seg <- if (method_name == "sum") seg_sum else seg_min
  cat("Method:", method_name, "\n")
  for (k in seq_along(seg$mat.list)) {
    mat_k <- seg$mat.list[[k]]
    if (inherits(mat_k, "sparseMatrix")) {
      mat_k <- as.matrix(mat_k)
    }
    nk <- nrow(mat_k) - 1
    level_total <- sum(mat_k[1:nk, 1:nk])
    preserved <- abs(level_total - original_total) < 1
    cat(
      "  mat.list[[",
      k,
      "]] total: ",
      level_total,
      if (preserved) {
        " [OK]"
      } else {
        paste0(" [MISMATCH, delta = ", level_total - original_total, "]")
      },
      "\n",
      sep = ""
    )
  }
  cat("\n")
}

# Compare with a no-reduction run to verify topology divergence
cat("--- Topology comparison: with vs without density reduction ---\n")

seg_no_dr <- moneca_fast(
  mx,
  segment.levels = 3,
  progress = FALSE,
  reduce_density = FALSE,
  symmetric_method = "sum"
)

# Count totals in no-reduction run (should also match)
for (k in seq_along(seg_no_dr$mat.list)) {
  mat_k <- seg_no_dr$mat.list[[k]]
  if (inherits(mat_k, "sparseMatrix")) {
    mat_k <- as.matrix(mat_k)
  }
  nk <- nrow(mat_k) - 1
  cat(
    "  no-reduction mat.list[[",
    k,
    "]] total: ",
    sum(mat_k[1:nk, 1:nk]),
    "\n",
    sep = ""
  )
}

# Compare segment structure: density-reduced vs unreduced
cat("\nSegment counts by level:\n")
cat("  Level | with reduction | without reduction\n")
max_levels <- max(length(seg_sum$segment.list), length(seg_no_dr$segment.list))
for (lv in 2:min(max_levels, 4)) {
  n_with <- if (lv <= length(seg_sum$segment.list)) {
    length(seg_sum$segment.list[[lv]])
  } else {
    NA
  }
  n_without <- if (lv <= length(seg_no_dr$segment.list)) {
    length(seg_no_dr$segment.list[[lv]])
  } else {
    NA
  }
  cat("  ", lv, "     |", n_with, "              |", n_without, "\n")
}

# Compare ARI between reduced and unreduced topologies
cmp_dr <- compare_moneca_results(
  seg_sum,
  seg_no_dr,
  level = 2,
  labels = c("with_reduction", "no_reduction")
)
cat("\nARI (with vs without density reduction, level 2):\n")
print(cmp_dr)

# Density reduction metadata
cat("\nDensity reduction info (sum method):\n")
dr_info <- seg_sum$density_reduction
if (!is.null(dr_info)) {
  cat("  Method:", dr_info$method, "\n")
  cat("  Components (k):", dr_info$k, "\n")
  cat("  Original total:", dr_info$original_total, "\n")
  cat("  Reduced total:", dr_info$reduced_total, "\n")
  cat(
    "  Retention:",
    round(100 * dr_info$reduced_total / dr_info$original_total, 1),
    "%\n"
  )
} else {
  cat("  No density reduction applied\n")
}

# 13. Collect all results -----

eval_results <- list(
  matrix_info = list(
    dims = dim(mx),
    total_transitions = sum(mx),
    nonzero_cells = sum(mx > 0),
    density = sum(mx > 0) / prod(dim(mx))
  ),
  benchmark = bm,
  system_time = list(sum = t_sum, min = t_min),
  comparison_l2 = cmp,
  comparison_l3 = cmp_l3,
  asymmetry = list(sum = asym_sum, min = asym_min),
  refinement = list(
    threshold_0.5 = tryCatch(
      refine_segments(seg_sum, threshold = 0.5, level = 2),
      error = function(e) NULL
    ),
    threshold_0.3 = tryCatch(
      refine_segments(seg_sum, threshold = 0.3, level = 2),
      error = function(e) NULL
    )
  ),
  quality = list(sum = q_sum, min = q_min),
  dual_matrix = list(
    original_total = original_total,
    density_reduction_info = seg_sum$density_reduction,
    comparison_reduced_vs_unreduced = cmp_dr,
    segments_no_reduction = seg_no_dr
  ),
  segments = list(sum = seg_sum, min = seg_min)
)

cat("\n=== Evaluation complete ===\n")
cat("Results stored in 'eval_results'\n")
