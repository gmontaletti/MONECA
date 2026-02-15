# Test: Confronto workflow completi di riduzione densità
# ======================================================
#
# Confronta tre workflow:
# 1. Originale → moneca (weight.matrix con cut.off=1 chiamato internamente)
# 2. reduce_density(SVD) → moneca
# 3. reduce_density(NMF) → moneca
#
# Nota: moneca() chiama internamente weight.matrix() con cut.off=1 di default,
# quindi il filtraggio RR >= 1 avviene sempre come parte dell'algoritmo.
#
# Valuta: riduzione densità cumulativa e tempi di calcolo

devtools::load_all(".")

# 1. Genera dati di test -----
set.seed(123)
cat("=== Test: Confronto Workflow Completi ===\n\n")

test_data <- generate_mobility_data(
  n_classes = 60,
  n_total = 50000,
  immobility_strength = 0.5,
  class_clustering = 0.3,
  noise_level = 0.2,
  seed = 123
)

n <- nrow(test_data) - 1
core_original <- test_data[1:n, 1:n]

cat("Matrice originale:\n")
cat("  Dimensioni:", n, "x", n, "\n")
cat("  Celle totali:", n * n, "\n")
cat("  Celle non-zero (conteggi):", sum(core_original > 0), "\n")
cat(
  "  Densità conteggi:",
  round(100 * sum(core_original > 0) / (n * n), 1),
  "%\n\n"
)

# Funzione helper per calcolare densità della weight matrix (come fa moneca internamente)
calc_wm_density <- function(mx, label = "") {
  wm <- weight.matrix(mx, cut.off = 1, diagonal = FALSE)
  core_wm <- wm[1:(nrow(wm) - 1), 1:(ncol(wm) - 1)]
  core_wm[is.na(core_wm)] <- 0
  celle <- sum(core_wm > 0)
  dens <- 100 * celle / length(core_wm)
  if (label != "") {
    cat(sprintf(
      "  %s - Celle con RR >= 1: %d (densità rete: %.1f%%)\n",
      label,
      celle,
      dens
    ))
  }
  list(celle_nonzero = celle, densita = dens)
}

cat("Densità della rete (dopo weight.matrix interno a moneca):\n")
wm_orig <- calc_wm_density(test_data, "Originale")

# 2. WORKFLOW 1: Originale → moneca -----
cat("\n")
cat(paste(rep("=", 70), collapse = ""), "\n")
cat("WORKFLOW 1: Originale → moneca\n")
cat("(moneca chiama internamente weight.matrix con cut.off=1)\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

cat("Input:\n")
cat("  Celle conteggi > 0:", sum(core_original > 0), "\n")
cat(
  "  Densità conteggi:",
  round(100 * sum(core_original > 0) / (n * n), 1),
  "%\n"
)
cat("  Celle RR >= 1 (rete):", wm_orig$celle_nonzero, "\n")
cat("  Densità rete:", round(wm_orig$densita, 1), "%\n\n")

cat("Esecuzione moneca...\n")
t1_start <- Sys.time()
seg1 <- moneca(test_data, segment.levels = 2, small.cell.reduction = 100)
t1_end <- Sys.time()
tempo_wf1 <- as.numeric(difftime(t1_end, t1_start, units = "secs"))
cat("  Tempo:", round(tempo_wf1, 2), "sec\n")
cat(
  "  Segmenti livello 1:",
  length(unique(segment.membership(seg1, level = 1))),
  "\n"
)

# 3. WORKFLOW 2: reduce_density(SVD) → moneca -----
cat("\n")
cat(paste(rep("=", 70), collapse = ""), "\n")
cat("WORKFLOW 2: reduce_density(SVD) → moneca\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

# Step 1: reduce_density SVD
cat("Step 1 - reduce_density(SVD):\n")
t2a_start <- Sys.time()
reduced_svd <- reduce_density(
  test_data,
  method = "svd",
  k = "auto",
  variance_target = 0.75,
  verbose = FALSE,
  seed = 42
)
t2a_end <- Sys.time()
tempo_svd <- as.numeric(difftime(t2a_end, t2a_start, units = "secs"))

core_svd <- reduced_svd[1:n, 1:n]
wm_svd <- calc_wm_density(reduced_svd, "")
cat("  Tempo:", round(tempo_svd, 3), "sec\n")
cat("  Celle conteggi > 0:", sum(core_svd > 0), "\n")
cat("  Densità conteggi:", round(100 * sum(core_svd > 0) / (n * n), 1), "%\n")
cat("  Celle RR >= 1 (rete):", wm_svd$celle_nonzero, "\n")
cat("  Densità rete:", round(wm_svd$densita, 1), "%\n\n")

# Step 2: moneca
cat("Step 2 - moneca:\n")
t2b_start <- Sys.time()
seg2 <- moneca(reduced_svd, segment.levels = 2, small.cell.reduction = 100)
t2b_end <- Sys.time()
tempo_moneca_svd <- as.numeric(difftime(t2b_end, t2b_start, units = "secs"))
tempo_wf2 <- tempo_svd + tempo_moneca_svd
cat("  Tempo moneca:", round(tempo_moneca_svd, 2), "sec\n")
cat("  Tempo totale:", round(tempo_wf2, 2), "sec\n")
cat(
  "  Segmenti livello 1:",
  length(unique(segment.membership(seg2, level = 1))),
  "\n"
)

# 4. WORKFLOW 3: reduce_density(NMF) → moneca -----
cat("\n")
cat(paste(rep("=", 70), collapse = ""), "\n")
cat("WORKFLOW 3: reduce_density(NMF) → moneca\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

# Step 1: reduce_density NMF
cat("Step 1 - reduce_density(NMF):\n")
t3a_start <- Sys.time()
reduced_nmf <- reduce_density(
  test_data,
  method = "nmf",
  k = 20,
  filter_quantile = "auto",
  verbose = FALSE,
  seed = 42
)
t3a_end <- Sys.time()
tempo_nmf <- as.numeric(difftime(t3a_end, t3a_start, units = "secs"))

core_nmf <- reduced_nmf[1:n, 1:n]
wm_nmf <- calc_wm_density(reduced_nmf, "")
cat("  Tempo:", round(tempo_nmf, 3), "sec\n")
cat("  Celle conteggi > 0:", sum(core_nmf > 0), "\n")
cat("  Densità conteggi:", round(100 * sum(core_nmf > 0) / (n * n), 1), "%\n")
cat("  Celle RR >= 1 (rete):", wm_nmf$celle_nonzero, "\n")
cat("  Densità rete:", round(wm_nmf$densita, 1), "%\n\n")

# Step 2: moneca
cat("Step 2 - moneca:\n")
t3b_start <- Sys.time()
seg3 <- moneca(reduced_nmf, segment.levels = 2, small.cell.reduction = 100)
t3b_end <- Sys.time()
tempo_moneca_nmf <- as.numeric(difftime(t3b_end, t3b_start, units = "secs"))
tempo_wf3 <- tempo_nmf + tempo_moneca_nmf
cat("  Tempo moneca:", round(tempo_moneca_nmf, 2), "sec\n")
cat("  Tempo totale:", round(tempo_wf3, 2), "sec\n")
cat(
  "  Segmenti livello 1:",
  length(unique(segment.membership(seg3, level = 1))),
  "\n"
)

# 5. TABELLA RIASSUNTIVA -----
cat("\n\n")
cat(paste(rep("=", 85), collapse = ""), "\n")
cat("TABELLA RIASSUNTIVA\n")
cat(paste(rep("=", 85), collapse = ""), "\n\n")

cat(sprintf(
  "%-40s %14s %14s %14s\n",
  "",
  "WF1: Originale",
  "WF2: SVD",
  "WF3: NMF"
))
cat(paste(rep("-", 85), collapse = ""), "\n")

cat("DENSITÀ CONTEGGI (celle > 0):\n")
cat(sprintf(
  "  %-38s %14d %14d %14d\n",
  "Celle non-zero",
  sum(core_original > 0),
  sum(core_svd > 0),
  sum(core_nmf > 0)
))
cat(sprintf(
  "  %-38s %13.1f%% %13.1f%% %13.1f%%\n",
  "Densità",
  100 * sum(core_original > 0) / (n * n),
  100 * sum(core_svd > 0) / (n * n),
  100 * sum(core_nmf > 0) / (n * n)
))

cat("\nDENSITÀ RETE (celle con RR >= 1, usata da moneca):\n")
cat(sprintf(
  "  %-38s %14d %14d %14d\n",
  "Celle RR >= 1",
  wm_orig$celle_nonzero,
  wm_svd$celle_nonzero,
  wm_nmf$celle_nonzero
))
cat(sprintf(
  "  %-38s %13.1f%% %13.1f%% %13.1f%%\n",
  "Densità rete",
  wm_orig$densita,
  wm_svd$densita,
  wm_nmf$densita
))

cat(paste(rep("-", 85), collapse = ""), "\n")

cat("TEMPI:\n")
cat(sprintf(
  "  %-38s %14s %13.2fs %13.2fs\n",
  "reduce_density",
  "-",
  tempo_svd,
  tempo_nmf
))
cat(sprintf(
  "  %-38s %13.2fs %13.2fs %13.2fs\n",
  "moneca",
  tempo_wf1,
  tempo_moneca_svd,
  tempo_moneca_nmf
))
cat(sprintf(
  "  %-38s %13.2fs %13.2fs %13.2fs\n",
  "TOTALE",
  tempo_wf1,
  tempo_wf2,
  tempo_wf3
))

cat(paste(rep("-", 85), collapse = ""), "\n")

cat(sprintf(
  "  %-38s %14d %14d %14d\n",
  "Segmenti identificati (liv. 1)",
  length(unique(segment.membership(seg1, level = 1))),
  length(unique(segment.membership(seg2, level = 1))),
  length(unique(segment.membership(seg3, level = 1)))
))

# 6. ANALISI -----
cat("\n")
cat(paste(rep("=", 85), collapse = ""), "\n")
cat("ANALISI\n")
cat(paste(rep("=", 85), collapse = ""), "\n\n")

cat("EFFETTO SULLA DENSITÀ DELLA RETE (quella che conta per moneca):\n\n")

cat(sprintf("  WF1 (Originale):  %.1f%% densità rete\n", wm_orig$densita))
cat(sprintf(
  "  WF2 (SVD):        %.1f%% densità rete (%+.1f punti %%)\n",
  wm_svd$densita,
  wm_svd$densita - wm_orig$densita
))
cat(sprintf(
  "  WF3 (NMF):        %.1f%% densità rete (%+.1f punti %%)\n",
  wm_nmf$densita,
  wm_nmf$densita - wm_orig$densita
))

cat("\nCONCLUSIONI:\n\n")

if (wm_svd$densita > wm_orig$densita) {
  cat("  1. SVD AUMENTA la densità della rete!\n")
  cat("     Lo smoothing della ricostruzione modifica i valori delle celle,\n")
  cat("     facendo sì che più celle superino la soglia RR >= 1.\n\n")
} else {
  cat("  1. SVD riduce leggermente la densità della rete.\n\n")
}

if (wm_nmf$densita < wm_orig$densita) {
  cat("  2. NMF RIDUCE la densità della rete.\n")
  cat("     Azzerando le celle rumorose PRIMA di weight.matrix,\n")
  cat("     meno celle possono avere RR >= 1.\n\n")
} else {
  cat("  2. NMF non riduce significativamente la densità della rete.\n\n")
}

cat(
  "  3. La densità della rete (RR >= 1) è ciò che determina il numero di archi\n"
)
cat("     nel grafo analizzato da moneca, e quindi i tempi di calcolo.\n\n")

cat("  4. reduce_density() agisce sui CONTEGGI prima che moneca calcoli i\n")
cat(
  "     rischi relativi. L'effetto sulla densità della rete dipende da come\n"
)
cat("     la modifica dei conteggi influenza il rapporto osservato/atteso.\n")
