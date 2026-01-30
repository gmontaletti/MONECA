# Test: Confronto scenari di riduzione densità
# =============================================
#
# Confronta i seguenti scenari:
# 1. SVD -> moneca (cut.off default = 1)
# 2. NMF -> moneca (cut.off default = 1)
# 3. SVD -> moneca (cut.off = 0.1)
# 4. NMF -> moneca (cut.off = 0.1)

devtools::load_all(".")

# 1. Genera dati di test -----
set.seed(123)
cat("=== Test: Confronto scenari riduzione densità ===\n\n")

# Matrice 60x60
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
cat("  Totale osservazioni:", sum(core_original), "\n")
cat("  Celle non-zero:", sum(core_original > 0), "su", n * n, "\n")
cat("  Densità:", round(100 * sum(core_original > 0) / (n * n), 1), "%\n\n")

# 2. Applica riduzione densità -----
cat("--- Preprocessing con SVD ---\n")
reduced_svd <- reduce_density(
  test_data,
  method = "svd",
  k = "auto",
  variance_target = 0.75,
  verbose = TRUE,
  seed = 42
)

cat("\n--- Preprocessing con NMF ---\n")
reduced_nmf <- reduce_density(
  test_data,
  method = "nmf",
  k = 20,
  filter_quantile = "auto",
  verbose = TRUE,
  seed = 42
)

# 3. Funzione helper per analizzare risultati MONECA -----
analyze_moneca <- function(seg, name, input_matrix) {
  # Calcola weight.matrix separatamente per avere controllo
  # sul cut.off usato
  wm <- weight.matrix(input_matrix, cut.off = 0, diagonal = FALSE)

  n <- nrow(input_matrix) - 1
  core_wm <- wm[1:n, 1:n]
  core_wm[is.na(core_wm)] <- 0

  # Conta archi con RR >= 1 (default moneca)
  n_edges_rr1 <- sum(core_wm >= 1)
  # Conta archi con RR >= 0.1
  n_edges_rr01 <- sum(core_wm >= 0.1)

  density_rr1 <- 100 * n_edges_rr1 / (n * n)
  density_rr01 <- 100 * n_edges_rr01 / (n * n)

  # Membership al livello 1
  memb <- tryCatch(
    segment.membership(seg, level = 1),
    error = function(e) NULL
  )
  n_segments <- if (!is.null(memb)) length(unique(memb)) else NA

  list(
    name = name,
    n_edges_rr1 = n_edges_rr1,
    n_edges_rr01 = n_edges_rr01,
    density_rr1 = density_rr1,
    density_rr01 = density_rr01,
    n_segments = n_segments
  )
}

# 4. Scenario 1: SVD -> moneca (cut.off = 1) -----
cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("SCENARIO 1: SVD -> moneca (cut.off = 1, default)\n")
cat(paste(rep("=", 60), collapse = ""), "\n\n")

t1_start <- Sys.time()
seg_svd_default <- moneca(
  reduced_svd,
  segment.levels = 3,
  small.cell.reduction = 100,
  cut.off = 1
)
t1_end <- Sys.time()
t1 <- difftime(t1_end, t1_start, units = "secs")

res1 <- analyze_moneca(seg_svd_default, "SVD + cut.off=1", reduced_svd)
cat("  Tempo MONECA:", round(t1, 2), "sec\n")
cat("  Archi nella rete (RR >= 1):", res1$n_edges_rr1, "\n")
cat("  Densità rete (RR >= 1):", round(res1$density_rr1, 1), "%\n")
cat("  Segmenti (livello 1):", res1$n_segments, "\n")

# 5. Scenario 2: NMF -> moneca (cut.off = 1) -----
cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("SCENARIO 2: NMF -> moneca (cut.off = 1, default)\n")
cat(paste(rep("=", 60), collapse = ""), "\n\n")

t2_start <- Sys.time()
seg_nmf_default <- moneca(
  reduced_nmf,
  segment.levels = 3,
  small.cell.reduction = 100,
  cut.off = 1
)
t2_end <- Sys.time()
t2 <- difftime(t2_end, t2_start, units = "secs")

res2 <- analyze_moneca(seg_nmf_default, "NMF + cut.off=1", reduced_nmf)
cat("  Tempo MONECA:", round(t2, 2), "sec\n")
cat("  Archi nella rete (RR >= 1):", res2$n_edges_rr1, "\n")
cat("  Densità rete (RR >= 1):", round(res2$density_rr1, 1), "%\n")
cat("  Segmenti (livello 1):", res2$n_segments, "\n")

# 6. Scenario 3: SVD -> moneca (cut.off = 0.1) -----
cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("SCENARIO 3: SVD -> moneca (cut.off = 0.1)\n")
cat(paste(rep("=", 60), collapse = ""), "\n\n")

t3_start <- Sys.time()
seg_svd_low <- moneca(
  reduced_svd,
  segment.levels = 3,
  small.cell.reduction = 100,
  cut.off = 0.1
)
t3_end <- Sys.time()
t3 <- difftime(t3_end, t3_start, units = "secs")

res3 <- analyze_moneca(seg_svd_low, "SVD + cut.off=0.1", reduced_svd)
cat("  Tempo MONECA:", round(t3, 2), "sec\n")
cat("  Archi nella rete (RR >= 0.1):", res3$n_edges_rr01, "\n")
cat("  Densità rete (RR >= 0.1):", round(res3$density_rr01, 1), "%\n")
cat("  Segmenti (livello 1):", res3$n_segments, "\n")

# 7. Scenario 4: NMF -> moneca (cut.off = 0.1) -----
cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("SCENARIO 4: NMF -> moneca (cut.off = 0.1)\n")
cat(paste(rep("=", 60), collapse = ""), "\n\n")

t4_start <- Sys.time()
seg_nmf_low <- moneca(
  reduced_nmf,
  segment.levels = 3,
  small.cell.reduction = 100,
  cut.off = 0.1
)
t4_end <- Sys.time()
t4 <- difftime(t4_end, t4_start, units = "secs")

res4 <- analyze_moneca(seg_nmf_low, "NMF + cut.off=0.1", reduced_nmf)
cat("  Tempo MONECA:", round(t4, 2), "sec\n")
cat("  Archi nella rete (RR >= 0.1):", res4$n_edges_rr01, "\n")
cat("  Densità rete (RR >= 0.1):", round(res4$density_rr01, 1), "%\n")
cat("  Segmenti (livello 1):", res4$n_segments, "\n")

# 8. Scenario di riferimento: originale -> moneca -----
cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("RIFERIMENTO: Originale -> moneca (cut.off = 1)\n")
cat(paste(rep("=", 60), collapse = ""), "\n\n")

t0_start <- Sys.time()
seg_orig <- moneca(
  test_data,
  segment.levels = 3,
  small.cell.reduction = 100,
  cut.off = 1
)
t0_end <- Sys.time()
t0 <- difftime(t0_end, t0_start, units = "secs")

res0 <- analyze_moneca(seg_orig, "Originale + cut.off=1", test_data)
cat("  Tempo MONECA:", round(t0, 2), "sec\n")
cat("  Archi nella rete (RR >= 1):", res0$n_edges_rr1, "\n")
cat("  Densità rete (RR >= 1):", round(res0$density_rr1, 1), "%\n")
cat("  Segmenti (livello 1):", res0$n_segments, "\n")

# 9. Tabella riassuntiva -----
cat("\n\n")
cat(paste(rep("=", 80), collapse = ""), "\n")
cat("TABELLA RIASSUNTIVA\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")

cat(sprintf(
  "%-30s %12s %12s %12s %12s\n",
  "Scenario",
  "Archi RR>=1",
  "Densità%",
  "Segmenti",
  "Tempo(sec)"
))
cat(paste(rep("-", 82), collapse = ""), "\n")

# Riferimento
cat(sprintf(
  "%-30s %12d %12.1f %12d %12.2f\n",
  "Originale (cut.off=1)",
  res0$n_edges_rr1,
  res0$density_rr1,
  res0$n_segments,
  as.numeric(t0)
))

# Scenari con cut.off = 1 (RR >= 1)
cat(sprintf(
  "%-30s %12d %12.1f %12d %12.2f\n",
  "1. SVD -> moneca (cut.off=1)",
  res1$n_edges_rr1,
  res1$density_rr1,
  res1$n_segments,
  as.numeric(t1)
))

cat(sprintf(
  "%-30s %12d %12.1f %12d %12.2f\n",
  "2. NMF -> moneca (cut.off=1)",
  res2$n_edges_rr1,
  res2$density_rr1,
  res2$n_segments,
  as.numeric(t2)
))

cat("\n")
cat(sprintf(
  "%-30s %12s %12s %12s %12s\n",
  "Scenario",
  "Archi RR>=0.1",
  "Densità%",
  "Segmenti",
  "Tempo(sec)"
))
cat(paste(rep("-", 82), collapse = ""), "\n")

# Scenari con cut.off = 0.1 (RR >= 0.1)
cat(sprintf(
  "%-30s %12d %12.1f %12d %12.2f\n",
  "3. SVD -> moneca (cut.off=0.1)",
  res3$n_edges_rr01,
  res3$density_rr01,
  res3$n_segments,
  as.numeric(t3)
))

cat(sprintf(
  "%-30s %12d %12.1f %12d %12.2f\n",
  "4. NMF -> moneca (cut.off=0.1)",
  res4$n_edges_rr01,
  res4$density_rr01,
  res4$n_segments,
  as.numeric(t4)
))

# 10. Dettagli sulla riduzione pre-MONECA -----
cat("\n\n")
cat(paste(rep("=", 80), collapse = ""), "\n")
cat("DETTAGLI RIDUZIONE PRE-MONECA\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")

core_svd <- reduced_svd[1:n, 1:n]
core_nmf <- reduced_nmf[1:n, 1:n]

cat("Matrice originale:\n")
cat("  Celle non-zero:", sum(core_original > 0), "\n")
cat(
  "  Densità conteggi:",
  round(100 * sum(core_original > 0) / (n * n), 1),
  "%\n\n"
)

cat("Dopo SVD:\n")
cat("  Celle non-zero:", sum(core_svd > 0), "\n")
cat("  Densità conteggi:", round(100 * sum(core_svd > 0) / (n * n), 1), "%\n")
cat("  Filter quantile:", attr(reduced_svd, "filter_quantile"), "\n")
cat("  k componenti:", attr(reduced_svd, "k"), "\n")
cat(
  "  Varianza spiegata:",
  round(attr(reduced_svd, "variance_explained") * 100, 1),
  "%\n\n"
)

cat("Dopo NMF:\n")
cat("  Celle non-zero:", sum(core_nmf > 0), "\n")
cat("  Densità conteggi:", round(100 * sum(core_nmf > 0) / (n * n), 1), "%\n")
cat("  Filter quantile:", attr(reduced_nmf, "filter_quantile"), "\n")
cat("  k componenti:", attr(reduced_nmf, "k"), "\n\n")

# 11. Analisi effetto cut.off -----
cat(paste(rep("=", 80), collapse = ""), "\n")
cat("ANALISI EFFETTO CUT.OFF\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")

cat("Con cut.off = 1 (default):\n")
cat("  - Solo celle con RR >= 1 diventano archi nella rete\n")
cat("  - Seleziona mobilità SUPERIORE alle aspettative\n\n")

cat("Con cut.off = 0.1:\n")
cat("  - Anche celle con RR >= 0.1 diventano archi\n")
cat("  - Include mobilità anche INFERIORE alle aspettative\n")
cat("  - Rete più densa, più archi da analizzare\n\n")

cat("Effetto combinato reduce_density() + cut.off:\n")
cat("  - reduce_density() filtra PRIMA sul rumore statistico (conteggi)\n")
cat("  - cut.off filtra DOPO sul significato sostantivo (RR)\n")
cat("  - Sono complementari e non si sostituiscono\n")
