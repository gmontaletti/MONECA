# 1. Setup -----
# Companion to profile_moneca_fast.R but driven by the 127x127 Italian
# profession mobility matrix (Lombardy). This fixture is dense enough (>0.7)
# to exercise the reduce_density() auto-trigger and to make clique enumeration
# and dense weight-matrix arithmetic actual hotspots -- complementing the
# synthetic-fixture run that is allocation-bound.
#
# The real matrix lives outside this repo; the script skips gracefully if
# absent.
#
# Run with: Rscript tests/profiling/profile_moneca_fast_real.R

if (requireNamespace("devtools", quietly = TRUE)) {
  suppressMessages(devtools::load_all(".", quiet = TRUE))
} else {
  suppressPackageStartupMessages(library(moneca))
}
cat(sprintf("moneca source: %s\n", utils::packageDescription("moneca")$Version))

has_profvis <- requireNamespace("profvis", quietly = TRUE)
has_bench <- requireNamespace("bench", quietly = TRUE)
has_htmlwidgets <- requireNamespace("htmlwidgets", quietly = TRUE)
has_matrix <- requireNamespace("Matrix", quietly = TRUE)

data_path <- file.path(
  "/Users/giampaolomontaletti/Documents/funzioni",
  "data_pipeline/output/dashboard/profession_matrix_8day.rds"
)
if (!file.exists(data_path)) {
  stop(
    "Real mobility matrix not found at: ",
    data_path,
    "\nThis script requires the Lombardy profession fixture. ",
    "Run profile_moneca_fast.R for the synthetic grid."
  )
}

prof_dir <- file.path("tests", "profiling", "results")
ref_dir <- file.path("reference", "moneca")
dir.create(prof_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(ref_dir, recursive = TRUE, showWarnings = FALSE)

# 2. Fixture -----
mx_raw <- readRDS(data_path)
n <- nrow(mx_raw)
# Drop the last 3 rows/cols (armed forces) to match evaluate_directed_clustering.R.
mx <- mx_raw[1:(n - 3), 1:(n - 3)]
core_nnz <- sum(mx > 0)
core_density <- round(core_nnz / length(mx), 3)
cat(sprintf(
  "Fixture: %d x %d  nnz=%d  density=%.3f  total=%s\n",
  nrow(mx),
  ncol(mx),
  core_nnz,
  core_density,
  format(sum(mx), big.mark = ",")
))

# 3. Scenario grid (same as profile_moneca_fast.R) -----
scenarios <- list(
  baseline = list(),
  sparse = list(use.sparse = TRUE),
  maximal = list(use_maximal_cliques = TRUE),
  min_sym = list(symmetric_method = "min"),
  density_off = list(reduce_density = FALSE),
  deep = list(segment.levels = 5)
)

run_fast <- function(mx, overrides) {
  base_args <- list(mx = mx, segment.levels = 3, progress = FALSE)
  args <- modifyList(base_args, overrides)
  do.call(moneca_fast, args)
}

# 4. profvis runs -----
profvis_paths <- character()
if (has_profvis) {
  cat("\n--- profvis runs (real 127x127) ---\n")
  for (scen_nm in names(scenarios)) {
    tag <- sprintf("real_%s", scen_nm)
    cat(sprintf("  profvis %s ...", tag))
    t0 <- Sys.time()
    p <- profvis::profvis(
      {
        run_fast(mx, scenarios[[scen_nm]])
      },
      simplify = FALSE
    )
    dt <- as.numeric(Sys.time() - t0, units = "secs")
    rds <- file.path(prof_dir, sprintf("profvis_moneca_fast_%s.rds", tag))
    saveRDS(p, rds)
    if (has_htmlwidgets) {
      html <- file.path(
        ref_dir,
        sprintf("profvis_moneca_fast_%s.html", tag)
      )
      htmlwidgets::saveWidget(p, html, selfcontained = TRUE)
      profvis_paths <- c(profvis_paths, html)
    }
    cat(sprintf(" %.2fs\n", dt))
  }
} else {
  cat("profvis not installed -- skipping flame graphs.\n")
}

# 5. bench::mark grid -----
bench_tbl <- NULL
if (has_bench) {
  cat("\n--- bench::mark grid (real 127x127) ---\n")
  rows <- list()
  for (scen_nm in names(scenarios)) {
    ov <- scenarios[[scen_nm]]
    bm <- bench::mark(
      run_fast(mx, ov),
      iterations = 3,
      check = FALSE,
      filter_gc = FALSE,
      memory = TRUE
    )
    row <- data.frame(
      fixture = "real_127",
      scenario = scen_nm,
      min_s = as.numeric(bm$min[[1]]),
      median_s = as.numeric(bm$median[[1]]),
      mem_alloc_b = as.numeric(bm$mem_alloc[[1]]),
      n_itr = bm$n_itr[[1]],
      n_gc = bm$n_gc[[1]],
      stringsAsFactors = FALSE
    )
    rows[[length(rows) + 1]] <- row
    cat(sprintf(
      "  %-12s  median=%6.3fs  mem=%6.1fMB  gc=%d\n",
      scen_nm,
      row$median_s,
      row$mem_alloc_b / 1024^2,
      row$n_gc
    ))
  }
  bench_tbl <- do.call(rbind, rows)
  saveRDS(bench_tbl, file.path(prof_dir, "bench_moneca_fast_real.rds"))
}

# 6. Correctness guard (sparse vs dense parity) -----
cat("\n--- Correctness guard: sparse vs dense parity (real 127x127) ---\n")
if (has_matrix) {
  r_dense <- moneca_fast(
    mx,
    segment.levels = 3,
    progress = FALSE,
    use.sparse = FALSE
  )
  r_sparse <- moneca_fast(
    mx,
    segment.levels = 3,
    progress = FALSE,
    use.sparse = TRUE
  )
  ok_all <- identical(r_dense$segment.list, r_sparse$segment.list)
  cat(sprintf("  overall: sparse == dense -> %s\n", ok_all))
  for (i in seq_along(r_dense$segment.list)) {
    ok <- identical(r_dense$segment.list[[i]], r_sparse$segment.list[[i]])
    nd <- length(r_dense$segment.list[[i]])
    ns <- length(r_sparse$segment.list[[i]])
    cat(sprintf(
      "  level %d: identical=%s  n_segments dense=%d sparse=%d\n",
      i,
      ok,
      nd,
      ns
    ))
  }
}

# 7. Hotspot extraction from profvis samples -----
hotspot_rows <- list()
if (has_profvis) {
  cat("\n--- Top self-time frames (real 127x127) ---\n")
  rds_files <- list.files(
    prof_dir,
    pattern = "^profvis_moneca_fast_real_.*\\.rds$",
    full.names = TRUE
  )
  for (f in rds_files) {
    p <- readRDS(f)
    prof <- p$x$message$prof
    if (is.null(prof) || !nrow(prof)) {
      next
    }
    prof <- as.data.frame(prof)
    prof <- prof[order(prof$time, -prof$depth), ]
    selfs <- prof[!duplicated(prof$time), ]
    n_samples <- nrow(selfs)
    tbl <- sort(table(selfs$label), decreasing = TRUE)
    tag <- sub("^profvis_moneca_fast_", "", sub("\\.rds$", "", basename(f)))
    cat(sprintf("\n[%s]  %d samples\n", tag, n_samples))
    top_k <- head(tbl, 10)
    for (i in seq_along(top_k)) {
      pct <- 100 * top_k[[i]] / n_samples
      cat(sprintf("  %5.1f%%  %s\n", pct, names(top_k)[i]))
    }
    hot_df <- data.frame(
      tag = tag,
      label = names(top_k),
      pct = round(100 * as.numeric(top_k) / n_samples, 1),
      samples = as.integer(top_k),
      total_samples = n_samples,
      stringsAsFactors = FALSE
    )
    hotspot_rows[[tag]] <- hot_df
  }
  if (length(hotspot_rows)) {
    hotspots <- do.call(rbind, hotspot_rows)
    saveRDS(hotspots, file.path(prof_dir, "hotspots_moneca_fast_real.rds"))
  }
}

# 8. Summary -----
if (!is.null(bench_tbl)) {
  cat("\n--- Summary: median time per scenario (real 127x127) ---\n")
  summary_df <- data.frame(
    scenario = bench_tbl$scenario,
    median_s = round(bench_tbl$median_s, 4),
    mem_alloc_mb = round(bench_tbl$mem_alloc_b / 1024^2, 2),
    n_gc = bench_tbl$n_gc,
    stringsAsFactors = FALSE
  )
  baseline_t <- summary_df$median_s[summary_df$scenario == "baseline"]
  summary_df$speedup <- round(baseline_t / summary_df$median_s, 2)
  summary_df <- summary_df[order(summary_df$median_s), ]
  print(summary_df, row.names = FALSE)
  saveRDS(summary_df, file.path(prof_dir, "summary_moneca_fast_real.rds"))
}

cat("\nDone.\n")
