# Log-Linear Models for Mobility Tables
# ======================================
#
# Functions to fit standard log-linear models to mobility tables and
# test whether MONECA-identified segments explain mobility structure
# beyond standard baselines.

#' @importFrom stats glm poisson pchisq coef residuals fitted deviance df.residual AIC
#' @importFrom stats as.formula model.matrix
NULL

# 1. Internal Helpers -----

#' Convert a square matrix to long format
#'
#' Takes a square core matrix (no margins) and returns a data frame with
#' Origin, Destination, and Freq columns suitable for log-linear modelling.
#'
#' @param mx A square numeric matrix with row and column names.
#' @return A data frame with columns Origin (factor), Destination (factor),
#'   and Freq (numeric).
#' @keywords internal
.matrix_to_long <- function(mx) {
  categories <- rownames(mx)
  n <- nrow(mx)

  origin <- rep(categories, each = n)
  destination <- rep(categories, times = n)
  freq <- as.vector(t(mx))

  data.frame(
    Origin = factor(origin, levels = categories),
    Destination = factor(destination, levels = categories),
    Freq = freq,
    stringsAsFactors = FALSE
  )
}

#' Build design for independence model
#'
#' Returns data frame with Origin and Destination as factors. R handles
#' dummy coding for the Poisson GLM.
#'
#' @param df Long-format data frame from \code{.matrix_to_long}.
#' @param categories Character vector of category names.
#' @return The input data frame with factors set.
#' @keywords internal
.build_design_independence <- function(df, categories) {
  df$Origin <- factor(df$Origin, levels = categories)
  df$Destination <- factor(df$Destination, levels = categories)
  df
}

#' Build design for quasi-independence model
#'
#' Adds n binary indicator columns for diagonal cells (one per category).
#'
#' @param df Long-format data frame from \code{.matrix_to_long}.
#' @param categories Character vector of category names.
#' @return Data frame with added diagonal indicator columns.
#' @keywords internal
.build_design_quasi_independence <- function(df, categories) {
  df <- .build_design_independence(df, categories)

  for (cat in categories) {
    col_name <- paste0("diag_", make.names(cat))
    df[[col_name]] <- as.integer(
      df$Origin == cat & df$Destination == cat
    )
  }

  df
}

#' Build design for quasi-symmetry model
#'
#' Starts with quasi-independence columns, then adds symmetric pair
#' indicators. For each unordered pair \{i, j\} where i < j, adds column
#' \code{symm_i_j} = 1 when (Origin == i & Dest == j) OR
#' (Origin == j & Dest == i). This gives n + n*(n-1)/2 extra columns
#' beyond Origin + Destination.
#'
#' @param df Long-format data frame from \code{.matrix_to_long}.
#' @param categories Character vector of category names.
#' @return Data frame with diagonal and symmetry indicator columns.
#' @keywords internal
.build_design_quasi_symmetry <- function(df, categories) {
  df <- .build_design_quasi_independence(df, categories)

  # Create symmetric pair factor: for each unordered pair {i,j}, cells
  # (i,j) and (j,i) share the same level. Diagonal cells are assigned to
  # a self-referencing pair (e.g., "CatA::CatA") to avoid collinearity
  # with the individual diagonal indicators.
  # Using a factor is more numerically stable than n*(n-1)/2 binary columns.
  orig <- as.character(df$Origin)
  dest <- as.character(df$Destination)
  symm_pair <- ifelse(
    orig <= dest,
    paste0(orig, "::", dest),
    paste0(dest, "::", orig)
  )
  df$symm_pair <- factor(symm_pair)

  df
}

#' Build design for uniform association model
#'
#' Adds a single numeric column \code{score_interaction} = row_score *
#' col_score. Scores are assigned based on the \code{scores} parameter.
#'
#' @param df Long-format data frame from \code{.matrix_to_long}.
#' @param categories Character vector of category names.
#' @param scores Character string: \code{"integer"} for 1, 2, ..., n or
#'   \code{"midrank"} for midrank values based on marginal distribution.
#' @return Data frame with added score_interaction column.
#' @keywords internal
.build_design_uniform_association <- function(
  df,
  categories,
  scores = "integer"
) {
  df <- .build_design_independence(df, categories)

  n <- length(categories)

  if (scores == "integer") {
    score_vals <- seq_len(n)
    names(score_vals) <- categories
  } else if (scores == "midrank") {
    # Midrank scores based on marginal distribution of origin
    freq_by_origin <- tapply(df$Freq, df$Origin, sum)
    cum_freq <- cumsum(freq_by_origin)
    total <- sum(freq_by_origin)
    # Midrank = (cumulative frequency - 0.5 * category frequency) / total
    score_vals <- (cum_freq - 0.5 * freq_by_origin) / total
  } else {
    stop("scores must be 'integer' or 'midrank'")
  }

  row_scores <- score_vals[as.character(df$Origin)]
  col_scores <- score_vals[as.character(df$Destination)]
  df$score_interaction <- as.numeric(row_scores) * as.numeric(col_scores)

  df
}

#' Build design for MONECA topology model
#'
#' Adds diagonal indicators and a segment-pair factor encoding within-segment
#' and between-segment relationships based on MONECA segment membership.
#' Uses a single factor variable for numerical stability with large matrices.
#'
#' @param df Long-format data frame from \code{.matrix_to_long}.
#' @param categories Character vector of category names.
#' @param segment_membership Named character vector mapping category name
#'   to segment label (e.g., \code{"seg_1"}, \code{"seg_2"}).
#' @return Data frame with diagonal indicators and a seg_pair factor column.
#' @keywords internal
.build_design_moneca_topology <- function(df, categories, segment_membership) {
  df <- .build_design_independence(df, categories)

  # Diagonal indicators (same as quasi-independence)
  for (cat in categories) {
    col_name <- paste0("diag_", make.names(cat))
    df[[col_name]] <- as.integer(
      df$Origin == cat & df$Destination == cat
    )
  }

  # Map origin and destination to their segment labels
  orig_seg <- segment_membership[as.character(df$Origin)]
  dest_seg <- segment_membership[as.character(df$Destination)]

  # Build segment-pair factor: encodes the unordered segment pair for
  # each cell. Diagonal cells are assigned to their within-segment group
  # (the individual diagonal indicators capture the extra diagonal effect).
  # Within-segment off-diagonal cells get their segment label, and
  # between-segment cells get "seg_a:seg_b".
  seg_pair <- ifelse(
    orig_seg == dest_seg,
    paste0("within:", orig_seg),
    ifelse(
      orig_seg < dest_seg,
      paste0(orig_seg, ":", dest_seg),
      paste0(dest_seg, ":", orig_seg)
    )
  )
  df$seg_pair <- factor(seg_pair)

  df
}

#' Compute dissimilarity index between observed and fitted values
#'
#' The dissimilarity index measures the proportion of cases that would
#' need to be reclassified for the model to fit exactly.
#'
#' @param observed Numeric vector of observed counts.
#' @param fitted Numeric vector of fitted (expected) counts.
#' @return A single numeric value between 0 and 1.
#' @keywords internal
.dissimilarity_index <- function(observed, fitted) {
  total <- sum(observed)
  if (total == 0) {
    return(0)
  }
  sum(abs(observed - fitted)) / (2 * total)
}

#' Extract segment membership from a moneca object
#'
#' Returns a named character vector mapping category names to segment labels
#' at a given hierarchical level.
#'
#' @param moneca_obj A moneca object.
#' @param level Integer specifying the hierarchical level.
#' @return A named character vector where names are category names and values
#'   are segment labels (e.g., \code{"seg_1"}).
#' @keywords internal
.get_segment_membership <- function(moneca_obj, level) {
  # Get category names from mat.list[[1]] (excluding margin row)
  # Use as.matrix() to handle sparse matrices from density reduction
  mat1 <- as.matrix(moneca_obj$mat.list[[1]])
  categories <- rownames(mat1)
  n <- length(categories) - 1
  categories <- categories[1:n]

  # Validate level
  if (level < 1 || level > length(moneca_obj$segment.list)) {
    stop(sprintf(
      "level must be between 1 and %d (available levels in this moneca object)",
      length(moneca_obj$segment.list)
    ))
  }

  # segment.list[[level]] is a list of integer vectors
  seg_list <- moneca_obj$segment.list[[level]]
  membership <- rep(NA_character_, n)
  names(membership) <- categories

  for (s in seq_along(seg_list)) {
    membership[seg_list[[s]]] <- paste0("seg_", s)
  }

  # Categories not in any multi-member segment are grouped as isolates.
  # Giving each singleton its own segment would create a near-saturated
  # model for large matrices, so they share a single "isolates" label.
  unassigned <- is.na(membership)
  if (any(unassigned)) {
    membership[unassigned] <- "isolates"
  }

  membership
}

# 2. Exported Functions -----

#' Fit Log-Linear Models to Mobility Tables
#'
#' Fits standard log-linear models to mobility tables for statistical
#' testing of mobility structure. Supports independence, quasi-independence,
#' quasi-symmetry, uniform association, and MONECA topology models.
#'
#' @param mx A mobility matrix with margins (totals row/column), OR a moneca
#'   object. If a moneca object, the matrix is extracted from
#'   \code{mat.list[[1]]}.
#' @param type Character string specifying the model type. One of:
#'   \describe{
#'     \item{\code{"independence"}}{No origin-destination association. Baseline
#'       with 2(n-1) parameters.}
#'     \item{\code{"quasi_independence"}}{Accounts for diagonal immobility
#'       cells. Adds n diagonal parameters. Standard mobility table baseline.}
#'     \item{\code{"quasi_symmetry"}}{Tests whether mobility O->D equals D->O.
#'       Adds n diagonal and n*(n-1)/2 symmetric pair parameters.}
#'     \item{\code{"uniform_association"}}{Single-parameter association model
#'       using row/column scores.}
#'     \item{\code{"moneca_topology"}}{Tests whether MONECA segments explain
#'       mobility structure beyond diagonal effects. Requires a moneca object
#'       or explicit segment membership.}
#'   }
#' @param level Integer specifying the hierarchical level for MONECA topology
#'   model. Required when \code{type = "moneca_topology"} and \code{mx} is a
#'   moneca object. Default is NULL.
#' @param scores Character string specifying score type for uniform association
#'   model: \code{"integer"} (default) or \code{"midrank"}.
#' @param small.cell.reduction Numeric. Cells with counts below this value
#'   are set to zero before fitting. Default is 0 (no reduction).
#'
#' @return An object of class \code{"moneca_loglinear"} containing:
#'   \describe{
#'     \item{model}{The \code{glm} fit object}
#'     \item{type}{Model type string}
#'     \item{deviance}{Residual deviance (G-squared)}
#'     \item{df}{Residual degrees of freedom}
#'     \item{aic}{Akaike Information Criterion}
#'     \item{bic}{Bayesian Information Criterion}
#'     \item{p_value}{P-value from chi-squared goodness-of-fit test}
#'     \item{dissimilarity_index}{Proportion of cases misclassified}
#'     \item{coefficients}{Named coefficient vector}
#'     \item{fitted}{Fitted values reshaped as n x n matrix}
#'     \item{residuals}{Pearson residuals reshaped as n x n matrix}
#'     \item{design_info}{List describing the design matrix construction}
#'     \item{original_matrix}{The input mobility matrix (with margins)}
#'     \item{segment_level}{Level used (moneca_topology only, NULL otherwise)}
#'   }
#'
#' @details
#' All models are fit as Poisson log-linear models using \code{glm()}.
#' The deviance statistic (G-squared) tests the goodness of fit relative
#' to the saturated model. A non-significant p-value indicates the model
#' adequately describes the observed mobility patterns.
#'
#' The BIC is computed using the number of cells (n-squared) as the sample
#' size, which is the standard R approach for GLMs. For contingency table
#' comparisons based on total count N, users should refer to the deviance
#' and degrees of freedom directly.
#'
#' The dissimilarity index indicates the proportion of individuals that
#' would need to be reclassified for the model to fit exactly. Values
#' below 0.05 indicate good fit; below 0.03 indicates excellent fit.
#'
#' @examples
#' # Generate test data and run MONECA
#' mx <- generate_mobility_data(n_classes = 5, seed = 42)
#' seg <- moneca_fast(mx, segment.levels = 3)
#'
#' # Fit different models
#' m_ind <- fit_mobility_model(mx, type = "independence")
#' m_qi <- fit_mobility_model(mx, type = "quasi_independence")
#'
#' # Fit MONECA topology model
#' m_top <- fit_mobility_model(seg, type = "moneca_topology", level = 2)
#'
#' # Compare models
#' compare_mobility_models(m_ind, m_qi, m_top)
#'
#' @seealso \code{\link{compare_mobility_models}} for model comparisons,
#'   \code{\link{moneca}} for the main clustering algorithm
#'
#' @export
fit_mobility_model <- function(
  mx,
  type = c(
    "independence",
    "quasi_independence",
    "quasi_symmetry",
    "uniform_association",
    "moneca_topology"
  ),
  level = NULL,
  scores = c("integer", "midrank"),
  small.cell.reduction = 0
) {
  type <- match.arg(type)
  scores <- match.arg(scores)

  # 1. Validate and extract matrix -----
  is_moneca <- inherits(mx, "moneca")
  original_moneca <- NULL

  if (is_moneca) {
    original_moneca <- mx
    original_matrix <- as.matrix(mx$mat.list[[1]])
  } else if (is.matrix(mx)) {
    original_matrix <- mx
  } else {
    stop("mx must be a matrix with margins or a moneca object")
  }

  # Validate square matrix (with margins)
  if (nrow(original_matrix) != ncol(original_matrix)) {
    stop("Input matrix must be square (including margins row/column)")
  }

  if (nrow(original_matrix) < 3) {
    stop("Matrix must have at least 2 categories (3 rows including margins)")
  }

  # Extract core matrix (remove margins row/col)
  n <- nrow(original_matrix) - 1
  core <- original_matrix[1:n, 1:n]

  # Apply small cell reduction
  if (small.cell.reduction > 0) {
    core[core < small.cell.reduction] <- 0
  }

  # 2. Validate MONECA topology requirements -----
  segment_membership <- NULL
  segment_level <- NULL

  if (type == "moneca_topology") {
    if (is.null(original_moneca)) {
      stop(
        "type = 'moneca_topology' requires mx to be a moneca object. ",
        "Run moneca() or moneca_fast() first."
      )
    }
    if (is.null(level)) {
      stop(
        "level must be specified when type = 'moneca_topology'. ",
        "Choose a level from 1 to ",
        length(original_moneca$segment.list),
        "."
      )
    }
    segment_membership <- .get_segment_membership(original_moneca, level)
    segment_level <- level
  }

  # 3. Convert to long format -----
  categories <- rownames(core)
  if (is.null(categories)) {
    categories <- paste0("Cat", seq_len(n))
    rownames(core) <- categories
    colnames(core) <- categories
  }

  df <- .matrix_to_long(core)

  # 4. Build design matrix -----
  design_df <- switch(
    type,
    "independence" = .build_design_independence(df, categories),
    "quasi_independence" = .build_design_quasi_independence(df, categories),
    "quasi_symmetry" = .build_design_quasi_symmetry(df, categories),
    "uniform_association" = .build_design_uniform_association(
      df,
      categories,
      scores
    ),
    "moneca_topology" = .build_design_moneca_topology(
      df,
      categories,
      segment_membership
    )
  )

  # 5. Fit Poisson log-linear model -----
  # Drop all-zero numeric columns (e.g., singleton within-segment indicators)
  predictor_cols <- setdiff(
    names(design_df),
    c("Freq", "Origin", "Destination")
  )
  zero_cols <- vapply(
    predictor_cols,
    function(col) {
      is.numeric(design_df[[col]]) && all(design_df[[col]] == 0)
    },
    logical(1)
  )
  if (any(zero_cols)) {
    design_df <- design_df[,
      !names(design_df) %in% predictor_cols[zero_cols],
      drop = FALSE
    ]
  }

  # Build formula: Freq ~ all other columns
  predictor_cols <- setdiff(names(design_df), "Freq")
  formula_str <- paste("Freq ~", paste(predictor_cols, collapse = " + "))
  model_formula <- as.formula(formula_str)

  model <- tryCatch(
    suppressWarnings(
      glm(model_formula, data = design_df, family = poisson())
    ),
    error = function(e) {
      n_params <- length(predictor_cols)
      stop(
        sprintf(
          "GLM fitting failed for %s model (%d predictors, %d cells). %s\n%s",
          type,
          n_params,
          nrow(design_df),
          "This typically occurs with large matrices (100+ categories).",
          conditionMessage(e)
        ),
        call. = FALSE
      )
    }
  )

  # 6. Extract results -----
  dev <- deviance(model)
  df_resid <- df.residual(model)
  p_val <- pchisq(dev, df_resid, lower.tail = FALSE)
  aic_val <- AIC(model)
  bic_val <- AIC(model, k = log(nrow(design_df)))

  model_coefficients <- coef(model)
  fitted_vals <- fitted(model)
  pearson_resid <- residuals(model, type = "pearson")

  # Reshape fitted values and residuals to n x n matrix
  fitted_matrix <- matrix(fitted_vals, nrow = n, ncol = n, byrow = TRUE)
  rownames(fitted_matrix) <- categories
  colnames(fitted_matrix) <- categories

  resid_matrix <- matrix(pearson_resid, nrow = n, ncol = n, byrow = TRUE)
  rownames(resid_matrix) <- categories
  colnames(resid_matrix) <- categories

  # Compute dissimilarity index
  di <- .dissimilarity_index(as.vector(t(core)), fitted_vals)

  # Build design info
  design_info <- list(
    type = type,
    n_categories = n,
    n_cells = n^2,
    n_parameters = length(model_coefficients),
    predictor_columns = predictor_cols
  )

  if (type == "moneca_topology") {
    design_info$segment_membership <- segment_membership
    design_info$n_segments <- length(unique(segment_membership))
  }

  if (type == "uniform_association") {
    design_info$scores <- scores
  }

  # 7. Construct return object -----
  result <- list(
    model = model,
    type = type,
    deviance = dev,
    df = df_resid,
    aic = aic_val,
    bic = bic_val,
    p_value = p_val,
    dissimilarity_index = di,
    coefficients = model_coefficients,
    fitted = fitted_matrix,
    residuals = resid_matrix,
    design_info = design_info,
    original_matrix = original_matrix,
    segment_level = segment_level
  )

  class(result) <- "moneca_loglinear"
  result
}


#' Compare Log-Linear Models for Mobility Tables
#'
#' Compares multiple log-linear model fits for a mobility table, producing
#' a summary table with fit statistics and likelihood-ratio tests between
#' nested models.
#'
#' @param ... One or more \code{moneca_loglinear} objects to compare.
#' @param mx Optional. A mobility matrix with margins or a moneca object.
#'   If provided, fits all standard models automatically for comparison.
#' @param level Integer specifying the hierarchical level for the MONECA
#'   topology model. Used when \code{mx} is a moneca object.
#' @param include_topology Logical. Whether to include the MONECA topology
#'   model when fitting all models via \code{mx}. Default is TRUE. Set to
#'   FALSE if \code{mx} is a plain matrix without segment information.
#'
#' @return An object of class \code{"moneca_model_comparison"} containing:
#'   \describe{
#'     \item{table}{Data frame with columns: model, deviance, df, aic, bic,
#'       p_value, dissimilarity}
#'     \item{lr_tests}{Data frame of pairwise likelihood-ratio tests between
#'       nested models, with columns: model_1, model_2, delta_deviance,
#'       delta_df, p_value}
#'     \item{models}{Named list of the fitted model objects}
#'   }
#'
#' @details
#' When \code{mx} is provided, the function fits independence,
#' quasi-independence, quasi-symmetry, and uniform association models
#' automatically. If \code{mx} is a moneca object and
#' \code{include_topology = TRUE}, a MONECA topology model is also fit.
#'
#' Likelihood-ratio tests are computed for known nesting relationships:
#' \itemize{
#'   \item Independence is nested in quasi-independence
#'   \item Independence is nested in uniform association
#'   \item Quasi-independence is nested in quasi-symmetry
#'   \item Quasi-independence is nested in MONECA topology
#' }
#'
#' Models passed via \code{...} are combined with any automatically fitted
#' models. Duplicate model types are resolved by keeping the explicitly
#' passed version.
#'
#' @examples
#' mx <- generate_mobility_data(n_classes = 5, seed = 42)
#' seg <- moneca_fast(mx, segment.levels = 3)
#'
#' # Compare all standard models
#' comp <- compare_mobility_models(mx = seg, level = 2)
#' print(comp)
#'
#' # Compare specific models
#' m1 <- fit_mobility_model(mx, type = "independence")
#' m2 <- fit_mobility_model(mx, type = "quasi_independence")
#' compare_mobility_models(m1, m2)
#'
#' @seealso \code{\link{fit_mobility_model}} for fitting individual models
#'
#' @export
compare_mobility_models <- function(
  ...,
  mx = NULL,
  level = NULL,
  include_topology = TRUE
) {
  # 1. Collect models from ... -----
  dots <- list(...)
  passed_models <- list()

  for (i in seq_along(dots)) {
    obj <- dots[[i]]
    if (!inherits(obj, "moneca_loglinear")) {
      stop(sprintf(
        "Argument %d is not a moneca_loglinear object (class: %s)",
        i,
        paste(class(obj), collapse = ", ")
      ))
    }
    # Use a unique key: append level for topology models to allow
    # comparing multiple topology levels
    key <- obj$type
    if (key == "moneca_topology" && !is.null(obj$segment_level)) {
      key <- paste0(key, "_L", obj$segment_level)
    }
    passed_models[[key]] <- obj
  }

  # 2. Fit standard models if mx is provided -----
  auto_models <- list()

  if (!is.null(mx)) {
    standard_types <- c(
      "independence",
      "quasi_independence",
      "quasi_symmetry",
      "uniform_association"
    )

    for (type in standard_types) {
      if (!(type %in% names(passed_models))) {
        auto_models[[type]] <- fit_mobility_model(mx, type = type)
      }
    }

    # Fit topology model if applicable
    if (include_topology && inherits(mx, "moneca")) {
      if (is.null(level)) {
        # Default to highest level with multiple segments
        level <- length(mx$segment.list)
      }
      if (!("moneca_topology" %in% names(passed_models))) {
        auto_models[["moneca_topology"]] <- fit_mobility_model(
          mx,
          type = "moneca_topology",
          level = level
        )
      }
    }
  }

  # Combine: passed models override auto-fitted ones
  all_models <- c(auto_models, passed_models)

  if (length(all_models) < 2) {
    stop(
      "At least 2 models are required for comparison. ",
      "Either pass multiple models via ... or provide mx to fit automatically."
    )
  }

  # 3. Build comparison table -----
  model_names <- names(all_models)

  comp_table <- data.frame(
    model = model_names,
    deviance = vapply(all_models, function(m) m$deviance, numeric(1)),
    df = vapply(all_models, function(m) m$df, numeric(1)),
    aic = vapply(all_models, function(m) m$aic, numeric(1)),
    bic = vapply(all_models, function(m) m$bic, numeric(1)),
    p_value = vapply(all_models, function(m) m$p_value, numeric(1)),
    dissimilarity = vapply(
      all_models,
      function(m) m$dissimilarity_index,
      numeric(1)
    ),
    stringsAsFactors = FALSE
  )

  # Sort by BIC
  comp_table <- comp_table[order(comp_table$bic), ]
  rownames(comp_table) <- NULL

  # 4. Compute likelihood-ratio tests for nested pairs -----
  # Define static nesting relationships, then expand for topology variants
  nesting <- list(
    c("independence", "quasi_independence"),
    c("independence", "uniform_association"),
    c("quasi_independence", "quasi_symmetry")
  )
  # Add quasi_independence -> each moneca_topology variant
  topo_keys <- grep("^moneca_topology", model_names, value = TRUE)
  for (tk in topo_keys) {
    nesting <- c(nesting, list(c("quasi_independence", tk)))
  }

  lr_tests <- data.frame(
    model_1 = character(0),
    model_2 = character(0),
    delta_deviance = numeric(0),
    delta_df = integer(0),
    p_value = numeric(0),
    stringsAsFactors = FALSE
  )

  for (pair in nesting) {
    simpler <- pair[1]
    complex <- pair[2]

    if (simpler %in% model_names && complex %in% model_names) {
      m_simpler <- all_models[[simpler]]
      m_complex <- all_models[[complex]]

      delta_g2 <- m_simpler$deviance - m_complex$deviance
      delta_df <- m_simpler$df - m_complex$df

      # Only compute if the nesting is valid (more parameters = fewer df)
      if (delta_df > 0 && delta_g2 > 0) {
        p <- pchisq(delta_g2, delta_df, lower.tail = FALSE)

        lr_tests <- rbind(
          lr_tests,
          data.frame(
            model_1 = simpler,
            model_2 = complex,
            delta_deviance = delta_g2,
            delta_df = delta_df,
            p_value = p,
            stringsAsFactors = FALSE
          )
        )
      }
    }
  }

  # 5. Return comparison object -----
  result <- list(
    table = comp_table,
    lr_tests = lr_tests,
    models = all_models
  )

  class(result) <- "moneca_model_comparison"
  result
}

# 3. S3 Print Methods -----

#' Print Method for moneca_loglinear Objects
#'
#' Displays a formatted summary of a log-linear model fit for a mobility
#' table, including deviance, degrees of freedom, p-value, and fit indices.
#'
#' @param x A \code{moneca_loglinear} object.
#' @param digits Integer specifying the number of significant digits for
#'   numeric output. Default is 3.
#' @param ... Additional arguments (ignored).
#' @return Invisibly returns the object.
#' @export
print.moneca_loglinear <- function(x, digits = 3, ...) {
  cat("Log-Linear Model for Mobility Table\n")
  cat("====================================\n")
  n <- nrow(x$fitted)
  cat(sprintf("Type: %s\n", x$type))
  cat(sprintf("Categories: %d\n", n))
  if (!is.null(x$segment_level)) {
    cat(sprintf("Segment level: %d\n", x$segment_level))
  }
  cat(sprintf("\nDeviance (G2): %.*f\n", digits, x$deviance))
  cat(sprintf("Df: %d\n", x$df))
  cat(sprintf("P-value: %.*g\n", digits, x$p_value))
  cat(sprintf("Dissimilarity index: %.*f\n", digits, x$dissimilarity_index))
  cat(sprintf("AIC: %.*f\n", digits, x$aic))
  cat(sprintf("BIC: %.*f\n", digits, x$bic))
  invisible(x)
}


#' Print Method for moneca_model_comparison Objects
#'
#' Displays a formatted comparison of multiple log-linear model fits,
#' including a fit statistics table and likelihood-ratio tests for nested
#' models.
#'
#' @param x A \code{moneca_model_comparison} object.
#' @param digits Integer specifying the number of significant digits for
#'   numeric output. Default is 3.
#' @param ... Additional arguments (ignored).
#' @return Invisibly returns the object.
#' @export
print.moneca_model_comparison <- function(x, digits = 3, ...) {
  cat("Log-Linear Model Comparison\n")
  cat("===========================\n\n")

  # Print fit table
  tbl <- x$table
  tbl$deviance <- round(tbl$deviance, digits)
  tbl$aic <- round(tbl$aic, digits)
  tbl$bic <- round(tbl$bic, digits)
  tbl$p_value <- signif(tbl$p_value, digits)
  tbl$dissimilarity <- round(tbl$dissimilarity, digits)
  print(tbl, row.names = FALSE)

  # Print LR tests if available
  if (nrow(x$lr_tests) > 0) {
    cat("\nLikelihood-Ratio Tests:\n")
    lr <- x$lr_tests
    lr$delta_deviance <- round(lr$delta_deviance, digits)
    lr$p_value <- signif(lr$p_value, digits)
    print(lr, row.names = FALSE)
  }

  invisible(x)
}
