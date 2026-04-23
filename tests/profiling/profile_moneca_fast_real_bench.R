# Minimal resume: bench::mark grid only for the real fixture. No profvis.
# Chunked into one bench call per scenario so that if any one run takes
# disproportionately long the earlier results are already on disk.

suppressMessages(devtools::load_all(".", quiet = TRUE))
prof_dir <- file.path("tests", "profiling", "results")

mx_raw <- readRDS(file.path(
  "/Users/giampaolomontaletti/Documents/funzioni",
  "data_pipeline/output/dashboard/profession_matrix_8day.rds"
))
n <- nrow(mx_raw)
mx <- mx_raw[1:(n - 3), 1:(n - 3)]

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

out <- file.path(prof_dir, "bench_moneca_fast_real.rds")
rows <- if (file.exists(out)) readRDS(out) else NULL
done <- if (is.null(rows)) character() else rows$scenario

for (scen_nm in names(scenarios)) {
  if (scen_nm %in% done) {
    cat(sprintf("  already have bench %s -- skip\n", scen_nm))
    next
  }
  cat(sprintf("  bench %s ...", scen_nm))
  flush.console()
  t0 <- Sys.time()
  bm <- bench::mark(
    run_fast(mx, scenarios[[scen_nm]]),
    iterations = 3,
    check = FALSE,
    filter_gc = FALSE,
    memory = TRUE
  )
  dt <- as.numeric(Sys.time() - t0, units = "secs")
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
  rows <- rbind(rows, row)
  saveRDS(rows, out)
  cat(sprintf(
    " median=%7.3fs mem=%6.1fMB gc=%d (elapsed %.1fs)\n",
    row$median_s,
    row$mem_alloc_b / 1024^2,
    row$n_gc,
    dt
  ))
}

# Correctness + summary
cat("\n--- Correctness guard ---\n")
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

summary_df <- data.frame(
  scenario = rows$scenario,
  median_s = round(rows$median_s, 4),
  mem_alloc_mb = round(rows$mem_alloc_b / 1024^2, 2),
  n_gc = rows$n_gc,
  stringsAsFactors = FALSE
)
baseline_t <- summary_df$median_s[summary_df$scenario == "baseline"]
summary_df$speedup <- round(baseline_t / summary_df$median_s, 2)
summary_df <- summary_df[order(summary_df$median_s), ]
cat("\n--- Summary ---\n")
print(summary_df, row.names = FALSE)
saveRDS(summary_df, file.path(prof_dir, "summary_moneca_fast_real.rds"))

cat("\nDone.\n")
