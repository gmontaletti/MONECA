# Test: Confronto tra metodi di riduzione della densità
# =====================================================
#
# Questo test confronta tre approcci per ridurre la densità:
# 1. reduce_density() con SVD
# 2. reduce_density() con NMF
# 3. weight.matrix() con cut.off = 1 (filtra celle con rischio relativo < 1)
#
# Obiettivo: capire le differenze in termini di:
# - Densità risultante
# - Preservazione dei valori
# - Effetto sui tempi di calcolo MONECA

devtools::load_all(".")

# 1. Genera dati di test -----
set.seed(123)
cat("=== Test: Confronto metodi di riduzione densità ===\n\n")

# Matrice 60x60 con densità medio-alta
test_data <- generate_mobility_data(
  n_classes = 60,
  n_total = 50000,
  immobility_strength = 0.5,
  class_clustering = 0.3,
  noise_level = 0.2,
  seed = 123
)

n <- nrow(test_data) - 1 # esclude riga/colonna totali
core_original <- test_data[1:n, 1:n]

cat("Matrice originale:\n")
cat("  Dimensioni:", n, "x", n, "\n")
cat("  Totale osservazioni:", sum(core_original), "\n")
cat("  Celle non-zero:", sum(core_original > 0), "su", n * n, "\n")
cat("  Densità:", round(100 * sum(core_original > 0) / (n * n), 1), "%\n\n")

# 2. Metodo 1: reduce_density() con SVD -----
cat("--- Metodo 1: reduce_density() con SVD ---\n")
t1_start <- Sys.time()
reduced_svd <- reduce_density(
  test_data,
  method = "svd",
  k = "auto",
  variance_target = 0.75,
  verbose = TRUE,
  seed = 42
)
t1_end <- Sys.time()

core_svd <- reduced_svd[1:n, 1:n]
cat(
  "  Tempo riduzione:",
  round(difftime(t1_end, t1_start, units = "secs"), 2),
  "sec\n"
)
cat("  Celle non-zero:", sum(core_svd > 0), "\n")
cat("  Densità:", round(100 * sum(core_svd > 0) / (n * n), 1), "%\n")
cat(
  "  Osservazioni mantenute:",
  sum(core_svd),
  "(",
  round(100 * sum(core_svd) / sum(core_original), 1),
  "%)\n\n"
)

# 3. Metodo 2: reduce_density() con NMF -----
cat("--- Metodo 2: reduce_density() con NMF ---\n")
nmf_available <- requireNamespace("RcppML", quietly = TRUE)
if (nmf_available) {
  t2_start <- Sys.time()
  reduced_nmf <- reduce_density(
    test_data,
    method = "nmf",
    k = 20,
    filter_quantile = "auto",
    verbose = TRUE,
    seed = 42
  )
  t2_end <- Sys.time()

  core_nmf <- reduced_nmf[1:n, 1:n]
  cat(
    "  Tempo riduzione:",
    round(difftime(t2_end, t2_start, units = "secs"), 2),
    "sec\n"
  )
  cat("  Celle non-zero:", sum(core_nmf > 0), "\n")
  cat("  Densità:", round(100 * sum(core_nmf > 0) / (n * n), 1), "%\n")
  cat(
    "  Osservazioni mantenute:",
    sum(core_nmf),
    "(",
    round(100 * sum(core_nmf) / sum(core_original), 1),
    "%)\n\n"
  )
} else {
  cat("  SKIPPED: RcppML non installato\n")
  cat("  Installa con: install.packages('RcppML')\n\n")
  reduced_nmf <- NULL
  core_nmf <- NULL
}

# 4. Metodo 3: weight.matrix() con cut.off = 1 -----
cat("--- Metodo 3: weight.matrix() con cut.off = 1 ---\n")
t3_start <- Sys.time()
wm_cutoff <- weight.matrix(test_data, cut.off = 1, diagonal = FALSE)
t3_end <- Sys.time()

core_wm <- wm_cutoff[1:n, 1:n]
# Sostituisci NA con 0 per il conteggio
core_wm[is.na(core_wm)] <- 0

cat(
  "  Tempo calcolo:",
  round(difftime(t3_end, t3_start, units = "secs"), 2),
  "sec\n"
)
cat("  Celle non-zero (RR >= 1):", sum(core_wm > 0), "\n")
cat("  Densità:", round(100 * sum(core_wm > 0) / (n * n), 1), "%\n")
cat("  Nota: weight.matrix restituisce rischi relativi, non conteggi\n\n")

# Per confronto: calcoliamo quante celle della matrice ORIGINALE hanno RR >= 1
wm_original <- weight.matrix(test_data, cut.off = 0, diagonal = FALSE) # nessun cutoff
core_wm_all <- wm_original[1:n, 1:n]
core_wm_all[is.na(core_wm_all)] <- 0
celle_rr_ge_1 <- sum(core_wm_all >= 1)
cat("  Celle originali con RR >= 1:", celle_rr_ge_1, "\n\n")

# 5. Confronto: quale metodo mantiene valori originali? -----
cat("=== Confronto preservazione valori originali ===\n\n")

# SVD: uses reconstruction as filter, preserves original values
valori_uguali_svd <- sum(core_svd > 0 & core_svd == core_original)
valori_diversi_svd <- sum(core_svd > 0 & core_svd != core_original)
cat("SVD:\n")
cat("  Celle con valore UGUALE all'originale:", valori_uguali_svd, "\n")
cat("  Celle con valore DIVERSO dall'originale:", valori_diversi_svd, "\n")
if (valori_diversi_svd == 0) {
  cat(
    "  -> SVD PRESERVA i valori originali (filtra in base alla ricostruzione)\n\n"
  )
} else {
  cat("  -> Nota: alcune celle hanno valori diversi\n\n")
}

# NMF: i valori dovrebbero essere originali
if (!is.null(core_nmf)) {
  valori_uguali_nmf <- sum(core_nmf > 0 & core_nmf == core_original)
  valori_diversi_nmf <- sum(core_nmf > 0 & core_nmf != core_original)
  cat("NMF:\n")
  cat("  Celle con valore UGUALE all'originale:", valori_uguali_nmf, "\n")
  cat("  Celle con valore DIVERSO dall'originale:", valori_diversi_nmf, "\n")
  if (valori_diversi_nmf == 0) {
    cat("  -> NMF PRESERVA i valori originali (filtra solo)\n\n")
  } else {
    cat("  -> Nota: alcune celle hanno valori diversi\n\n")
  }
} else {
  cat("NMF: SKIPPED (RcppML non disponibile)\n\n")
}

# 6. Test tempi MONECA -----
cat("=== Test tempi di calcolo MONECA ===\n\n")

# MONECA su matrice originale
cat("MONECA su matrice originale...\n")
t_orig_start <- Sys.time()
seg_orig <- moneca(test_data, segment.levels = 2, small.cell.reduction = 100)
t_orig_end <- Sys.time()
tempo_orig <- difftime(t_orig_end, t_orig_start, units = "secs")
cat("  Tempo:", round(tempo_orig, 2), "sec\n\n")

# MONECA su matrice ridotta SVD
cat("MONECA su matrice ridotta SVD...\n")
t_svd_start <- Sys.time()
seg_svd <- moneca(reduced_svd, segment.levels = 2, small.cell.reduction = 100)
t_svd_end <- Sys.time()
tempo_svd <- difftime(t_svd_end, t_svd_start, units = "secs")
cat("  Tempo:", round(tempo_svd, 2), "sec\n")
cat(
  "  Speedup:",
  round(as.numeric(tempo_orig) / as.numeric(tempo_svd), 1),
  "x\n\n"
)

# MONECA su matrice ridotta NMF
if (!is.null(reduced_nmf)) {
  cat("MONECA su matrice ridotta NMF...\n")
  t_nmf_start <- Sys.time()
  seg_nmf <- moneca(reduced_nmf, segment.levels = 2, small.cell.reduction = 100)
  t_nmf_end <- Sys.time()
  tempo_nmf <- difftime(t_nmf_end, t_nmf_start, units = "secs")
  cat("  Tempo:", round(tempo_nmf, 2), "sec\n")
  cat(
    "  Speedup:",
    round(as.numeric(tempo_orig) / as.numeric(tempo_nmf), 1),
    "x\n\n"
  )
} else {
  cat("MONECA su matrice ridotta NMF: SKIPPED\n\n")
  tempo_nmf <- NA
}

# 7. Tabella riassuntiva -----
cat("=== TABELLA RIASSUNTIVA ===\n\n")
cat(sprintf(
  "%-25s %12s %12s %12s %12s\n",
  "Metodo",
  "Celle>0",
  "Densità%",
  "Valori orig",
  "MONECA sec"
))
cat(paste(rep("-", 75), collapse = ""), "\n")
cat(sprintf(
  "%-25s %12d %12.1f %12s %12.2f\n",
  "Originale",
  sum(core_original > 0),
  100 * sum(core_original > 0) / (n * n),
  "100%",
  as.numeric(tempo_orig)
))
cat(sprintf(
  "%-25s %12d %12.1f %12s %12.2f\n",
  "reduce_density (SVD)",
  sum(core_svd > 0),
  100 * sum(core_svd > 0) / (n * n),
  "Preservati",
  as.numeric(tempo_svd)
))
if (!is.null(core_nmf)) {
  cat(sprintf(
    "%-25s %12d %12.1f %12s %12.2f\n",
    "reduce_density (NMF)",
    sum(core_nmf > 0),
    100 * sum(core_nmf > 0) / (n * n),
    "Preservati",
    as.numeric(tempo_nmf)
  ))
} else {
  cat(sprintf(
    "%-25s %12s %12s %12s %12s\n",
    "reduce_density (NMF)",
    "N/A",
    "N/A",
    "Preservati",
    "N/A"
  ))
}
cat(sprintf(
  "%-25s %12d %12.1f %12s %12s\n",
  "weight.matrix (RR>=1)",
  sum(core_wm > 0),
  100 * sum(core_wm > 0) / (n * n),
  "N/A (RR)",
  "N/A"
))

cat("\n=== CONCLUSIONI ===\n\n")
cat(
  "1. reduce_density(SVD): Riduce densità filtrando celle in base a ricostruzione SVD\n"
)
cat("   - I valori NON-ZERO sono esattamente i conteggi originali\n")
cat(
  "   - Riduzione densità dipende da k, variance_target, e filter_quantile\n\n"
)

cat(
  "2. reduce_density(NMF): Riduce densità filtrando celle in base a ricostruzione NMF\n"
)
cat("   - I valori NON-ZERO sono esattamente i conteggi originali\n")
cat("   - Riduzione densità dipende da k e filter_quantile\n\n")

cat("3. weight.matrix(cut.off=1): Filtra in base al rischio relativo\n")
cat("   - Non riduce la densità della matrice di input\n")
cat("   - Agisce DOPO la conversione a rischi relativi\n")
cat("   - Le celle con RR < 1 diventano 0 nella matrice dei pesi\n")
cat(
  "   - Criterio diverso: 'mobilità sotto le aspettative' vs 'rumore statistico'\n\n"
)

cat("DIFFERENZA CHIAVE:\n")
cat("- reduce_density() agisce sui CONTEGGI grezzi prima di weight.matrix()\n")
cat("- weight.matrix(cut.off) agisce sui RISCHI RELATIVI calcolati\n")
cat(
  "- Sono complementari: si può usare reduce_density() E POI weight.matrix()\n"
)
