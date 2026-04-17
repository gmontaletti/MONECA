# Dual-Matrix System: Effects on Group Composition

**Date**: 2026-03-17
**Dataset**: 127 profession categories, 7,592,506 transitions
**Package**: moneca v1.2.0.9000, branch `experimental/directed-clustering`

## 1. Introduction

This report documents the effects of two concurrent changes in `moneca_fast()` on clustering output, evaluated on a 127x127 profession transition matrix from Lombardy labor market data:

1. **Default density reduction method changed from SVD to NMF**. The SVD method explained 96.2% of variance with k=16 components. The NMF method retains a comparable proportion of counts (94.9% vs 94.8%) but does not produce a variance-explained metric.

2. **Dual-matrix aggregation split**. The old system used the density-reduced matrix for both topology detection (which cells to retain) and count aggregation at each hierarchical level. The new system uses the reduced matrix only for topology, while aggregation is performed on the original matrix. This eliminates count shrinkage across levels.

Both runs use `symmetric_method = "sum"` and `symmetric_method = "min"` with `small.cell.reduction = 0`, `segment.levels = 4`, and default density reduction parameters.

## 2. Count Preservation

The dual-matrix system preserves 100% of the original transition counts in `mat.list` at every segmentation level. The old system suffered 5.2% shrinkage because the reduced matrix (with zeroed-out low-signal cells) replaced the original at each level.

| Level | Old (SVD) | New (NMF + dual) | Original |
|------:|----------:|------------------:|---------:|
| 1 | 7,194,581 | 7,592,506 | 7,592,506 |
| 2 | 7,194,581 | 7,592,506 | 7,592,506 |
| 3 | 7,194,581 | 7,592,506 | 7,592,506 |
| 4 | 7,194,581 | 7,592,506 | 7,592,506 |

The old system retained 94.8% of the original total (7,194,581 / 7,592,506). The new system retains 100%.

## 3. Density Reduction Parameters

| Parameter | Old (SVD) | New (NMF) |
|:----------|:---------:|:---------:|
| method | svd | nmf |
| k | 16 | 16 |
| variance_explained | 96.2% | N/A |
| retention | 94.8% | 94.9% |
| normalization | none | none |
| filter_quantile | 0.25 | 0.25 |

Both runs use the same number of components (k=16) and the same quantile filter (0.25). The retention rates are nearly identical, indicating that the two methods discard a similar proportion of low-signal cells. The NMF method does not produce a variance-explained metric because it operates under non-negativity constraints rather than an orthogonal decomposition.

## 4. Segment Structure

### symmetric_method = "sum"

| Level | Old segments | New segments | Old sizes (top 6) | New sizes (top 6) |
|------:|:------------:|:------------:|:-------------------|:------------------|
| 1 | 127 | 127 | 1,1,1,1,1,1... | 1,1,1,1,1,1... |
| 2 | 34 | 35 | 5,5,4,4,3,3... | 5,4,3,3,3,3... |
| 3 | 17 | 20 | 15,14,10,6,5,4... | 13,10,8,7,6,6... |
| 4 | 12 | 11 | 24,18,11,8,7,7... | 17,17,12,11,9,7... |

### symmetric_method = "min"

| Level | Old segments | New segments | Old sizes (top 6) | New sizes (top 6) |
|------:|:------------:|:------------:|:-------------------|:------------------|
| 1 | 127 | 127 | 1,1,1,1,1,1... | 1,1,1,1,1,1... |
| 2 | 33 | 31 | 4,4,4,4,3,3... | 4,4,4,3,3,3... |
| 3 | 20 | 21 | 11,9,9,7,7,6... | 14,7,6,5,5,5... |
| 4 | 12 | 14 | 20,15,12,10,10,7... | 22,15,9,8,8,7... |

The total number of segments at level 2 changes modestly: +1 for sum, -2 for min. At deeper levels the differences are larger (sum: +3 at level 3, -1 at level 4; min: +1 at level 3, +2 at level 4). Size distributions shift slightly, with the new system producing somewhat more evenly distributed segments at intermediate levels.

## 5. Membership Agreement (ARI)

### symmetric_method = "sum"

| Level | ARI | Mean best Jaccard |
|------:|:---:|:-----------------:|
| 2 | 0.4787 | 0.7123 |
| 3 | 0.3736 | 0.5109 |
| 4 | 0.3781 | 0.4821 |

### symmetric_method = "min"

| Level | ARI | Mean best Jaccard |
|------:|:---:|:-----------------:|
| 2 | 0.7319 | 0.7182 |
| 3 | 0.4133 | 0.5193 |
| 4 | 0.5531 | 0.5848 |

The Adjusted Rand Index (ARI) measures global partition similarity (1.0 = identical, 0.0 = random). The mean best Jaccard measures how well individual segments in the old partition match their closest counterpart in the new partition.

The min-reciprocity method shows higher agreement at all levels (ARI 0.73 at level 2 vs 0.48 for sum), indicating that min-reciprocity clustering is more robust to the underlying change in density reduction and aggregation. The sum method exhibits more structural reorganization, particularly at levels 3 and 4.

## 6. Category Migration

### symmetric_method = "sum", level 2

61 / 127 categories changed segment (48.0%).

| Origin segment | Destination | Categories |
|:--------------:|:-----------:|:-----------|
| 2 | 7 | Specialisti delle scienze gestionali, commerciali e bancarie |
| 2 | 12 | Tecnici dei rapporti con i mercati |
| 3 | 2 | Agricoltori e operai agricoli specializzati |
| 4 | 3 | Personale non qualificato delle miniere e delle cave |
| 4 | 3 | Personale non qualificato delle costruzioni e professioni assimilate |
| 5 | 6 | Tecnici di apparecchiature ottiche e audio-video |
| 6 | 5 | Docenti universitari (ordinari e associati) |
| 6 | 5 | Docenti di scuola secondaria, post-secondaria e professioni assimilate |
| 6 | 5 | Altri specialisti dell'educazione e della formazione |
| 7 | 11 | Specialisti in discipline religiose e teologiche |
| 7 | 11 | Docenti di scuola primaria, pre-primaria e professioni assimilate |
| 9 | 4 | Analisti e specialisti nella progettazione di applicazioni |
| 9 | 4 | Tecnici informatici, telematici e delle telecomunicazioni |
| 9 | 4 | Addetti alle macchine d'ufficio |
| 9 | 4 | Addetti all'archiviazione e conservazione della documentazione |
| 10 | 25 | Insegnanti nella formazione professionale, istruttori, allenatori, atleti e professioni assimilate |
| 10 | 25 | Professioni qualificate nei servizi ricreativi, culturali ed assimilati |
| 11 | 12 | Ingegneri e professioni assimilate |
| 11 | 12 | Tecnici dell'organizzazione e dell'amministrazione delle attività produttive |
| 12 | 9 | Operai addetti a macchinari in impianti per la produzione in serie di articoli in legno |
| 12 | 9 | Personale non qualificato nella manifattura |
| 12 | 22 | Personale non qualificato addetto allo spostamento e alla consegna merci |
| 14 | 15 | Conduttori di impianti per la trasformazione dei minerali |
| 14 | 15 | Operai addetti all'assemblaggio di prodotti industriali |
| 14 | 15 | Operai addetti a macchine confezionatrici di prodotti industriali |
| 15 | 17 | Tecnici della salute |
| 15 | 17 | Tecnici dei servizi sociali |
| 15 | 17 | Professioni qualificate nei servizi sanitari e sociali |
| 15 | 17 | Professioni qualificate nei servizi personali ed assimilati |
| 15 | 17 | Personale non qualificato nei servizi di istruzione e sanitari |
| 16 | 18 | Fabbri ferrai costruttori di utensili ed assimilati |
| 16 | 18 | Operatori di impianti per la trasformazione e lavorazione a caldo dei metalli |
| 16 | 18 | Operai addetti a macchine automatiche e semiautomatiche per lavorazioni metalliche e per prodotti minerali |
| 17 | 16 | Specialisti in discipline artistico-espressive |
| 17 | 16 | Tecnici dei servizi ricreativi |
| 18 | 7 | Tecnici delle attività finanziarie ed assicurative |
| 18 | 7 | Addetti alla gestione economica, contabile e finanziaria |
| 21 | 32 | Esercenti ed addetti nelle attività di ristorazione |
| 22 | 21 | Tecnici in campo ingegneristico |
| 22 | 21 | Tecnici della gestione dei processi produttivi di beni e servizi |
| 23 | 14 | Conduttori di veicoli a motore e a trazione animale |
| 24 | 23 | Tecnici della conduzione di impianti produttivi in continuo e dell'esercizio di reti idriche ed energetiche |
| 24 | 23 | Conduttori di macchinari per la fabbricazione di articoli in gomma e materie plastiche |
| 25 | 23 | Operatori di catene di montaggio automatizzate e di robot industriali |
| 26 | 24 | Professioni qualificate nei servizi di sicurezza, vigilanza e custodia |
| 26 | 24 | Personale non qualificato addetto ai servizi di custodia di edifici, attrezzature e beni |
| 26 | 35 | Assistenti di viaggio e professioni assimilate |
| 27 | 26 | Esercenti delle vendite |
| 27 | 26 | Addetti alle vendite |
| 28 | 27 | Addetti all'accoglienza e all'informazione della clientela |
| 29 | 30 | Addetti alla gestione amministrativa della logistica |
| 30 | 29 | Conduttori di impianti per la produzione di energia termica e di vapore, per il recupero dei rifiuti e per il trattamento e la distribuzione delle acque |
| 30 | 29 | Personale non qualificato nei servizi di pulizia di uffici, alberghi, navi, ristoranti, aree pubbliche e veicoli |
| 31 | 28 | Tecnici della distribuzione commerciale e professioni assimilate |
| 31 | 28 | Altre professioni qualificate nelle attività commerciali |
| 31 | 35 | Professioni tecniche delle attività turistiche, ricettive ed assimilate |
| 32 | 10 | Artigiani ed operai specializzati del tessile e dell'abbigliamento |
| 32 | 10 | Operatori di macchinari e di impianti per la raffinazione del gas e dei prodotti petroliferi, per la chimica di base e la chimica fine e per la fabbricazione di prodotti derivati dalla chimica |
| 32 | 10 | Operai addetti a macchinari dell'industria tessile, delle confezioni ed assimilati |
| 32 | 31 | Artigiani ed operai specializzati delle lavorazioni alimentari |
| 32 | 31 | Operai addetti a macchinari fissi per l'industria alimentare |

### symmetric_method = "sum", level 3

72 / 127 categories changed segment (56.7%). The full migration table is omitted for brevity; the pattern at level 3 reflects further disaggregation of the level 2 changes, with additional splits within the metalwork, construction, education, and service segments.

### symmetric_method = "min", level 2

62 / 127 categories changed segment (48.8%).

| Origin segment | Destination | Categories |
|:--------------:|:-----------:|:-----------|
| 2 | 6 | Specialisti delle scienze gestionali, commerciali e bancarie |
| 2 | 12 | Tecnici dei rapporti con i mercati |
| 3 | 2 | Personale non qualificato delle miniere e delle cave |
| 3 | 2 | Personale non qualificato delle costruzioni e professioni assimilate |
| 4 | 5 | Tecnici di apparecchiature ottiche e audio-video |
| 4 | 5 | Artigiani ed operai specializzati dell'industria dello spettacolo |
| 5 | 3 | Docenti universitari (ordinari e associati) |
| 5 | 3 | Docenti di scuola secondaria, post-secondaria e professioni assimilate |
| 5 | 3 | Altri specialisti dell'educazione e della formazione |
| 6 | 4 | Agricoltori e operai agricoli specializzati |
| 6 | 4 | Conduttori di macchine agricole |
| 8 | 30 | Artigiani ed operai specializzati delle lavorazioni alimentari |
| 8 | 30 | Operai addetti a macchinari fissi per l'industria alimentare |
| 9 | 6 | Tecnici dell'organizzazione e dell'amministrazione delle attività produttive |
| 9 | 6 | Tecnici delle attività finanziarie ed assicurative |
| 9 | 6 | Addetti alla gestione economica, contabile e finanziaria |
| 11 | 13 | Brillatori, tagliatori di pietre, coltivatori di saline e professioni assimilate |
| 11 | 13 | Artigiani ed operai specializzati addetti alle costruzioni e al mantenimento di strutture edili |
| 12 | 8 | Operatori di macchinari e di impianti per la raffinazione del gas e dei prodotti petroliferi, per la chimica di base e la chimica fine e per la fabbricazione di prodotti derivati dalla chimica |
| 12 | 14 | Fabbri ferrai costruttori di utensili ed assimilati |
| 12 | 14 | Operatori di impianti per la trasformazione e lavorazione a caldo dei metalli |
| 12 | 14 | Operai addetti a macchine automatiche e semiautomatiche per lavorazioni metalliche e per prodotti minerali |
| 13 | 15 | Conduttori di impianti per la trasformazione dei minerali |
| 13 | 15 | Operai addetti a macchine confezionatrici di prodotti industriali |
| 14 | 16 | Tecnici della salute |
| 14 | 16 | Tecnici dei servizi sociali |
| 14 | 16 | Professioni qualificate nei servizi sanitari e sociali |
| 14 | 16 | Professioni qualificate nei servizi personali ed assimilati |
| 15 | 11 | Personale non qualificato nella manifattura |
| 16 | 5 | Tecnici dei servizi ricreativi |
| 16 | 17 | Specialisti in discipline artistico-espressive |
| 16 | 17 | Tecnici dei servizi culturali |
| 17 | 5 | Artigiani e operai specializzati dell'installazione e della manutenzione di attrezzature elettriche ed elettroniche |
| 17 | 25 | Artigiani ed operai specializzati addetti alle rifiniture delle costruzioni |
| 17 | 25 | Attrezzisti, operai e artigiani del trattamento del legno ed assimilati |
| 19 | 20 | Esercenti nelle attività ricettive |
| 19 | 20 | Esercenti ed addetti nelle attività di ristorazione |
| 20 | 11 | Operai addetti a macchinari in impianti per la produzione in serie di articoli in legno |
| 20 | 11 | Personale non qualificato addetto allo spostamento e alla consegna merci |
| 21 | 9 | Conduttori di veicoli a motore e a trazione animale |
| 22 | 21 | Operatori di catene di montaggio automatizzate e di robot industriali |
| 22 | 21 | Conduttori di macchinari per la fabbricazione di articoli in gomma e materie plastiche |
| 22 | 28 | Conduttori di macchinari per tipografia e stampa su carta e cartone |
| 22 | 28 | Operai addetti all'assemblaggio di prodotti industriali |
| 23 | 19 | Tecnici in campo ingegneristico |
| 24 | 19 | Tecnici informatici, telematici e delle telecomunicazioni |
| 24 | 26 | Addetti alle macchine d'ufficio |
| 24 | 26 | Addetti all'archiviazione e conservazione della documentazione |
| 25 | 22 | Esercenti delle vendite |
| 25 | 22 | Addetti alle vendite |
| 26 | 23 | Addetti al controllo di documenti e allo smistamento e recapito della posta |
| 26 | 23 | Professioni qualificate nei servizi di sicurezza, vigilanza e custodia |
| 26 | 23 | Personale non qualificato addetto ai servizi di custodia di edifici, attrezzature e beni |
| 27 | 24 | Addetti agli sportelli e ai movimenti di denaro |
| 27 | 24 | Addetti all'accoglienza e all'informazione della clientela |
| 28 | 27 | Tecnici del trasporto aereo, navale e ferroviario |
| 28 | 27 | Addetti alla gestione amministrativa della logistica |
| 30 | 12 | Tecnici della distribuzione commerciale e professioni assimilate |
| 30 | 12 | Altre professioni qualificate nelle attività commerciali |
| 32 | 29 | Artigiani ed operai specializzati addetti alla pulizia ed all'igiene degli edifici |
| 32 | 29 | Artigiani ed operai specializzati della lavorazione del cuoio, delle pelli e delle calzature ed assimilati |
| 33 | 31 | Operatori della cura estetica |

## 7. Quality Metrics

Quality metrics are computed at the final segmentation level using `segment.quality(final.solution = TRUE)`. Mean density is computed over finite values only (excluding single-node segments where density is undefined).

### symmetric_method = "sum"

| Metric | Old (SVD) | New (NMF + dual) |
|:-------|:---------:|:-----------------:|
| Mean density (finite) | 0.5267 | 0.6304 |
| Mean within.mobility | 0.4032 | 0.4153 |
| Mean share.of.mobility | 0.0344 | 0.0345 |
| N segments | 29 | 29 |
| Mean max.path | 1.38 | 1.14 |

### symmetric_method = "min"

| Metric | Old (SVD) | New (NMF + dual) |
|:-------|:---------:|:-----------------:|
| Mean density (finite) | 0.4728 | 0.6289 |
| Mean within.mobility | 0.3827 | 0.4382 |
| Mean share.of.mobility | 0.0333 | 0.0312 |
| N segments | 30 | 32 |
| Mean max.path | 1.07 | 1.09 |

The old quality metrics were computed on reduced counts (SVD). New metrics use original counts, so differences reflect both topology changes and count restoration. Density increases for both methods (+0.10 for sum, +0.16 for min), indicating that groups are internally denser when original counts are preserved. The decrease in mean max.path for sum (1.38 to 1.14) suggests that the new topology produces more compact segments.

## 8. Asymmetry Scores (Level 2)

Asymmetry scores are computed via `compute_asymmetry_scores()` at level 2. The score ranges from 0 (perfectly symmetric inter-category flows within a segment) to 1 (maximally asymmetric).

### symmetric_method = "sum"

| Metric | Old (SVD) | New (NMF + dual) |
|:-------|:---------:|:-----------------:|
| N segments | 34 | 35 |
| Mean asymmetry | 0.2167 | 0.1064 |
| Max asymmetry | 1.0000 | 0.5668 |
| Median asymmetry | 0.0864 | 0.0777 |

### symmetric_method = "min"

| Metric | Old (SVD) | New (NMF + dual) |
|:-------|:---------:|:-----------------:|
| N segments | 33 | 31 |
| Mean asymmetry | 0.0973 | 0.0744 |
| Max asymmetry | 0.4505 | 0.1559 |
| Median asymmetry | 0.0726 | 0.0756 |

The old asymmetry scores were computed on SVD-reduced counts; new scores use original counts.

The most notable change is in the sum method: mean asymmetry drops from 0.22 to 0.11 (halved), and the maximum drops from 1.00 to 0.57. This indicates that the SVD reduction introduced systematic distortion in flow symmetry, which inflated asymmetry scores. The min method, which already enforces reciprocity at the adjacency level, shows a smaller but consistent reduction (mean 0.10 to 0.07, max 0.45 to 0.16).

## 9. Summary

- **Count preservation**: verified at all 4 levels. New system retains 100% of original counts (7,592,506) vs 94.8% (7,194,581) in the old system.
- **Topology impact**: moderate for min-reciprocity (ARI = 0.73 at level 2), substantial for sum (ARI = 0.48 at level 2). Agreement decreases at deeper levels for both methods.
- **Category migration**: approximately 48% of categories changed segment at level 2 for both methods (61/127 for sum, 62/127 for min). At level 3, migration increases to 56.7% for sum.
- **Asymmetry reduction**: mean asymmetry halved for sum method (0.22 to 0.11); max asymmetry reduced from 1.00 to 0.57. Min method shows consistent but smaller reduction (mean 0.10 to 0.07).
- **Quality improvement**: mean density (finite) increased from 0.53 to 0.63 (sum) and from 0.47 to 0.63 (min). Within-mobility share increased for both methods.
- **Structural stability**: min-reciprocity clustering is more stable across the change (higher ARI, lower asymmetry in both old and new), consistent with its design as a more conservative symmetrization.
