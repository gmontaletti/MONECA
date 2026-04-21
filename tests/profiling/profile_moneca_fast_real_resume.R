# Resume script: finishes the missing pieces from profile_moneca_fast_real.R
# (the density_off + deep profvis runs and the full bench::mark grid) without
# redoing the four scenarios that already produced .rds artifacts.

if (requireNamespace("devtools", quietly = TRUE)) {
  suppressMessages(devtools::load_all(".", quiet = TRUE))
} else {
  suppressPackageStartupMessages(library(moneca))
}
cat(sprintf("moneca source: %s\n", utils::packageDescription("moneca")$Version))

prof_dir <- file.path("tests", "profiling", "results")
ref_dir <- file.path("reference", "moneca")

mx_raw <- readRDS(file.path(
  "/Users/giampaolomontaletti/Documents/funzioni",
  "data_pipeline/output/dashboard/profession_matrix_8day.rds"
))
n <- nrow(mx_raw)
mx <- mx_raw[1:(n - 3), 1:(n - 3)]
cat(sprintf(
  "Fixture: %d x %d  nnz=%d\n",
  nrow(mx),
  ncol(mx),
  sum(mx > 0)
))

run_fast <- function(mx, overrides) {
  base_args <- list(mx = mx, segment.levels = 3, progress = FALSE)
  args <- modifyList(base_args, overrides)
  do.call(moneca_fast, args)
}

scenarios <- list(
  baseline = list(),
  sparse = list(use.sparse = TRUE),
  maximal = list(use_maximal_cliques = TRUE),
  min_sym = list(symmetric_method = "min"),
  density_off = list(reduce_density = FALSE),
  deep = list(segment.levels = 5)
)

# 1. Fill in any missing profvis runs -----
for (scen_nm in names(scenarios)) {
  tag <- sprintf("real_%s", scen_nm)
  rds <- file.path(prof_dir, sprintf("profvis_moneca_fast_%s.rds", tag))
  if (file.exists(rds)) {
    cat(sprintf("  already have profvis %s -- skip\n", tag))
    next
  }
  cat(sprintf("  profvis %s ...", tag))
  t0 <- Sys.time()
  p <- profvis::profvis(
    {
      run_fast(mx, scenarios[[scen_nm]])
    },
    simplify = FALSE
  )
  dt <- as.numeric(Sys.time() - t0, units = "secs")
  saveRDS(p, rds)
  html <- file.path(ref_dir, sprintf("profvis_moneca_fast_%s.html", tag))
  htmlwidgets::saveWidget(p, html, selfcontained = TRUE)
  cat(sprintf(" %.2fs\n", dt))
}

# 2. bench::mark grid -----
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
  rows[[length(rows) + 1]] <- data.frame(
    fixture = "real_127",
    scenario = scen_nm,
    min_s = as.numeric(bm$min[[1]]),
    median_s = as.numeric(bm$median[[1]]),
    mem_alloc_b = as.numeric(bm$mem_alloc[[1]]),
    n_itr = bm$n_itr[[1]],
    n_gc = bm$n_gc[[1]],
    stringsAsFactors = FALSE
  )
  cat(sprintf(
    "  %-12s  median=%7.3fs  mem=%7.1fMB  gc=%d\n",
    scen_nm,
    rows[[length(rows)]]$median_s,
    rows[[length(rows)]]$mem_alloc_b / 1024^2,
    rows[[length(rows)]]$n_gc
  ))
}
bench_tbl <- do.call(rbind, rows)
saveRDS(bench_tbl, file.path(prof_dir, "bench_moneca_fast_real.rds"))

# 3. Correctness guard -----
cat("\n--- Correctness guard (real 127x127) ---\n")
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
cat(sprintf(
  "  overall: sparse == dense -> %s\n",
  identical(r_dense$segment.list, r_sparse$segment.list)
))
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

# 4. Hotspots -----
cat("\n--- Top self-time frames (real 127x127) ---\n")
rds_files <- list.files(
  prof_dir,
  pattern = "^profvis_moneca_fast_real_.*\\.rds$",
  full.names = TRUE
)
hotspot_rows <- list()
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
  hotspot_rows[[tag]] <- data.frame(
    tag = tag,
    label = names(top_k),
    pct = round(100 * as.numeric(top_k) / n_samples, 1),
    samples = as.integer(top_k),
    total_samples = n_samples,
    stringsAsFactors = FALSE
  )
}
if (length(hotspot_rows)) {
  saveRDS(
    do.call(rbind, hotspot_rows),
    file.path(prof_dir, "hotspots_moneca_fast_real.rds")
  )
}

# 5. Summary -----
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
cat("\n--- Summary ---\n")
print(summary_df, row.names = FALSE)
saveRDS(summary_df, file.path(prof_dir, "summary_moneca_fast_real.rds"))
cat("\nDone.\n")
