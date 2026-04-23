# 1. Setup -----
# Focused profiling of moneca_fast() across scales and configuration knobs.
# Produces profvis flame graphs, a bench::mark grid, and a per-scenario
# hotspot summary. Companion report lives at
# reference/moneca/profiling_moneca_fast.md.
#
# Run with: Rscript tests/profiling/profile_moneca_fast.R
# Requires: profvis, bench (both optional-guarded).

# Prefer the in-tree source over any installed version so profiling always
# exercises the current branch. Falls back to library() when devtools is absent.
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

prof_dir <- file.path("tests", "profiling", "results")
ref_dir <- file.path("reference", "moneca")
dir.create(prof_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(ref_dir, recursive = TRUE, showWarnings = FALSE)

# 2. Fixtures -----
set.seed(42)
scales <- list(
  small = generate_mobility_data(n_classes = 60, seed = 101),
  medium = generate_mobility_data(n_classes = 120, seed = 202),
  large = generate_mobility_data(n_classes = 300, seed = 303)
)

fixture_info <- data.frame(
  scale = names(scales),
  dim = vapply(scales, function(m) sprintf("%d x %d", nrow(m), ncol(m)), ""),
  core_nnz = vapply(
    scales,
    function(m) {
      core <- m[-nrow(m), -ncol(m)]
      sum(core > 0)
    },
    integer(1)
  ),
  core_density = vapply(
    scales,
    function(m) {
      core <- m[-nrow(m), -ncol(m)]
      round(sum(core > 0) / length(core), 3)
    },
    numeric(1)
  ),
  stringsAsFactors = FALSE
)
cat("Fixtures:\n")
print(fixture_info, row.names = FALSE)

# 3. Scenario grid -----
# Each scenario is a named list of extra args to moneca_fast().
# Common args (progress=FALSE, segment.levels=3) are added in run_fast().
scenarios <- list(
  baseline = list(),
  sparse = list(use.sparse = TRUE),
  maximal = list(use_maximal_cliques = TRUE),
  min_sym = list(symmetric_method = "min"),
  density_off = list(reduce_density = FALSE),
  deep = list(segment.levels = 5)
)

run_fast <- function(mx, overrides) {
  base_args <- list(
    mx = mx,
    segment.levels = 3,
    progress = FALSE
  )
  args <- modifyList(base_args, overrides)
  do.call(moneca_fast, args)
}

# 4. profvis runs -----
profvis_paths <- character()
if (has_profvis) {
  cat("\n--- profvis runs ---\n")
  for (scale_nm in names(scales)) {
    mx <- scales[[scale_nm]]
    for (scen_nm in names(scenarios)) {
      tag <- sprintf("%s_%s", scale_nm, scen_nm)
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
  }
} else {
  cat("profvis not installed -- skipping flame graphs.\n")
}

# 5. bench::mark grid -----
# One bench::mark call per (scale, scenario). Avoids NSE pitfalls when passing
# multiple expressions built from a list of overrides.
bench_tbl <- NULL
if (has_bench) {
  cat("\n--- bench::mark grid ---\n")
  rows <- list()
  for (scale_nm in names(scales)) {
    mx <- scales[[scale_nm]]
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
        scale = scale_nm,
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
        "  %-8s %-12s  median=%6.3fs  mem=%6.1fMB\n",
        scale_nm,
        scen_nm,
        row$median_s,
        row$mem_alloc_b / 1024^2
      ))
    }
  }
  bench_tbl <- do.call(rbind, rows)
  saveRDS(bench_tbl, file.path(prof_dir, "bench_moneca_fast.rds"))
} else {
  cat("bench not installed -- skipping benchmark grid.\n")
}

# 6. Correctness guard (sparse vs dense parity) -----
cat("\n--- Correctness guard: sparse vs dense parity ---\n")
if (has_matrix) {
  for (scale_nm in names(scales)) {
    mx <- scales[[scale_nm]]
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
    ok <- identical(r_dense$segment.list, r_sparse$segment.list)
    cat(sprintf("  %s: sparse == dense -> %s\n", scale_nm, ok))
  }
} else {
  cat("Matrix not installed -- sparse parity check skipped.\n")
}

# 7. Hotspot extraction from profvis samples -----
# profvis objects embed the Rprof sample trace at p$x$message$prof.
# Compute self-time per function label as a quick textual summary.
hotspot_rows <- list()
if (has_profvis) {
  cat("\n--- Top self-time frames (per profvis run) ---\n")
  rds_files <- list.files(
    prof_dir,
    pattern = "^profvis_moneca_fast_.*\\.rds$",
    full.names = TRUE
  )
  for (f in rds_files) {
    p <- readRDS(f)
    prof <- p$x$message$prof
    if (is.null(prof) || !nrow(prof)) {
      next
    }
    # Each sample at a given `time` is one Rprof interval; deepest frame on
    # that sample is the self-time owner.
    prof <- as.data.frame(prof)
    prof <- prof[order(prof$time, -prof$depth), ]
    selfs <- prof[!duplicated(prof$time), ]
    n_samples <- nrow(selfs)
    tbl <- sort(table(selfs$label), decreasing = TRUE)
    tag <- sub("^profvis_moneca_fast_", "", sub("\\.rds$", "", basename(f)))
    cat(sprintf("\n[%s]  %d samples\n", tag, n_samples))
    top_k <- head(tbl, 8)
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
    saveRDS(hotspots, file.path(prof_dir, "hotspots_moneca_fast.rds"))
  }
}

# 8. Summary table (by scale, scenarios ordered by median time) -----
if (!is.null(bench_tbl)) {
  cat("\n--- Summary: median time per (scale, scenario) ---\n")
  summary_df <- data.frame(
    scale = bench_tbl$scale,
    scenario = bench_tbl$scenario,
    median_s = round(bench_tbl$median_s, 4),
    mem_alloc_mb = round(bench_tbl$mem_alloc_b / 1024^2, 2),
    stringsAsFactors = FALSE
  )
  summary_df <- summary_df[
    order(summary_df$scale, summary_df$median_s),
  ]
  print(summary_df, row.names = FALSE)
  saveRDS(summary_df, file.path(prof_dir, "summary_moneca_fast.rds"))
}

cat("\nDone. Artifacts:\n")
cat(sprintf("  %s/\n", prof_dir))
cat(sprintf("  %s/ (profvis_moneca_fast_*.html)\n", ref_dir))
