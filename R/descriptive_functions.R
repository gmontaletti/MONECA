#' Print Method for moneca Objects (Enhanced)
#'
#' Displays comprehensive mobility analysis results from moneca clustering with
#' clear, interpretable statistics and improved labeling. Output is structured to
#' follow the natural flow of analysis from overall patterns to detailed metrics.
#'
#' @param x A moneca object returned by \code{\link{moneca}}.
#' @param small.cell.reduction Numeric threshold for small cell handling. If NULL,
#'   uses the value from the moneca object.
#' @param show.degree.stats Logical. If TRUE (default), displays detailed degree
#'   distribution statistics for each hierarchical level.
#' @param digits Integer. Number of decimal places for numeric output (default 1).
#' @param ... Additional arguments (currently unused).
#'
#' @return Invisibly returns NULL. This function is called for its side effect of
#'   printing a formatted summary.
#'
#' @details
#' The output is organized into four main sections that follow the analysis workflow:
#'
#' \strong{Section 1: OVERALL MOBILITY PATTERNS}
#' \itemize{
#'   \item \strong{Overall Population Mobility Rate}: Percentage of the total
#'     population that experiences any form of mobility (moves from origin to a
#'     different destination). Calculated as: (total off-diagonal cells) / (grand total).
#'     A rate of 20% means 80% remain in their origin position.
#'   \item \strong{Average Mobility Concentration}: Mean percentage of mobility
#'     captured by significant pathways (edges with relative risk > 1) across all
#'     segmentation levels. Higher values indicate mobility is concentrated in
#'     fewer, stronger pathways rather than dispersed randomly.
#' }
#'
#' \strong{Section 2: HIERARCHICAL SEGMENTATION ANALYSIS}
#'
#' \emph{Internal Mobility Within Segments (percent):}
#' \itemize{
#'   \item Shows the percentage of mobility that remains within segment boundaries
#'     at each level. Level 1 represents original categories (always 100% as each
#'     category is its own segment). Higher levels show increasing aggregation.
#'   \item Values decrease at higher levels as segments combine different origin
#'     categories, naturally reducing internal cohesion.
#'   \item High values (>70%) indicate segments successfully capture mobility patterns.
#' }
#'
#' \emph{Mobility Concentration in Significant Pathways by Level (percent):}
#' \itemize{
#'   \item Percentage of total mobility flowing through edges with relative risk > 1.
#'   \item Indicates how much mobility follows stronger-than-expected pathways
#'     versus random distribution.
#'   \item Higher values suggest clearer mobility structure; lower values indicate
#'     more dispersed patterns.
#' }
#'
#' \emph{Network Structure by Level:}
#' \itemize{
#'   \item \strong{Active Segments/Classes}: Number of nodes in the network.
#'     Decreases at higher levels as categories aggregate into segments.
#'   \item \strong{Significant Edges}: Count of mobility pathways with relative
#'     risk > 1. These represent stronger-than-expected mobility connections.
#'   \item \strong{Network Density}: Proportion of possible edges that exist.
#'     Calculated as: (actual edges) / (possible edges). Range 0-1.
#'   \item \strong{Isolated Segments}: Number of segments with no significant
#'     connections to other segments. Isolates represent positions that exchange
#'     members primarily internally or have only weak external connections.
#' }
#'
#' \strong{Section 3: DETAILED WEIGHTED DEGREE DISTRIBUTIONS (optional)}
#'
#' Shows distribution statistics (Min, Q1, Median, Mean, Q3, Max) for:
#' \itemize{
#'   \item \strong{Total Weighted Connections}: Sum of weighted incoming and outgoing
#'     connections (strength) for each node. Represents the total volume of mobility
#'     flows, not just the count of connections.
#'   \item \strong{Outward Mobility Strength}: Sum of weights on outgoing edges from
#'     each origin. High out-strength indicates positions that send large volumes
#'     to other destinations, weighted by relative risk.
#'   \item \strong{Inward Mobility Strength}: Sum of weights on incoming edges to
#'     each destination. High in-strength indicates positions that receive large
#'     volumes from other origins, weighted by relative risk.
#'   \item \strong{Edge Weight Distribution}: Distribution of relative risk values
#'     for significant edges. Values > 1 indicate mobility above expected levels.
#'     Higher values represent stronger mobility pathways.
#' }
#'
#' @section Technical Terms:
#' \describe{
#'   \item{\strong{Isolate}}{A node (category or segment) with no significant
#'     connections to other nodes. In mobility analysis, isolates represent
#'     positions that either have very high internal retention or only weak
#'     exchange with other positions. Not necessarily problematic - may indicate
#'     genuinely distinct positions.}
#'   \item{\strong{Relative Risk}}{Ratio of observed to expected mobility under
#'     independence. Values > 1 indicate mobility exceeds random expectation;
#'     < 1 indicates below expectation. Used as edge weights in the network.}
#'   \item{\strong{Network Density}}{Proportion of all possible connections that
#'     actually exist. In a fully connected network (density = 1), every position
#'     has significant mobility to every other position. Low density indicates
#'     mobility is channeled through specific pathways.}
#'   \item{\strong{Degree/Strength}}{In weighted networks, we use \strong{strength}
#'     instead of simple degree: the sum of edge weights connected to a node.
#'     In-strength = sum of incoming edge weights, out-strength = sum of outgoing
#'     edge weights, total strength = sum of both. High strength nodes handle large
#'     volumes of mobility flow, not just many connections.}
#'   \item{\strong{Segmentation Level}}{Hierarchical aggregation level. Level 1 =
#'     original categories, Level 2 = first aggregation based on cliques, etc.
#'     Higher levels represent coarser groupings.}
#' }
#'
#' @examples
#' # Generate data and run analysis
#' mobility_data <- generate_mobility_data(n_classes = 6, seed = 42)
#' seg <- moneca(mobility_data, segment.levels = 3)
#'
#' # Print comprehensive summary with all statistics
#' print(seg)
#'
#' # Hide detailed degree distributions for cleaner output
#' print(seg, show.degree.stats = FALSE)
#'
#' # Show more decimal places for precision
#' print(seg, digits = 2)
#'
#' @seealso \code{\link{moneca}} for the main analysis function,
#'   \code{\link{segment.quality}} for detailed segment metrics,
#'   \code{vignette("moneca-introduction")} for methodology details
#' @export
print.moneca <- function(
  x,
  small.cell.reduction = x$small.cell.reduction,
  show.degree.stats = TRUE,
  digits = 1,
  ...
) {
  segments <- x

  # Calculate overall mobility rate
  mx <- segments$mat.list[[1]]
  l <- ncol(mx)
  total.total <- mx[l, l]
  # Calculate off-diagonal sum (excluding diagonal = those who actually move)
  mx.no.margins <- mx[-l, -l]

  # Handle matrix conversion and prevent diag() scalar interpretation issue
  if (!is.matrix(mx.no.margins)) {
    mx.no.margins <- as.matrix(mx.no.margins)
  }

  # For 1x1 matrices, diag() would try to create an identity matrix of size n
  # Instead, directly use the single value as both the sum and diagonal
  if (nrow(mx.no.margins) == 1 && ncol(mx.no.margins) == 1) {
    diagonal_sum <- as.numeric(mx.no.margins)
  } else {
    diagonal_sum <- sum(diag(mx.no.margins))
  }

  off.diagonal.sum <- sum(mx.no.margins) - diagonal_sum
  total.mobility <- off.diagonal.sum / total.total

  # Calculate diagonal mobility function
  diag.mobility <- function(mx) {
    l <- ncol(mx)

    # Ensure mx is a matrix
    if (!is.matrix(mx)) {
      mx <- as.matrix(mx)
    }

    # Handle edge case of 1x1 matrix after removing margins
    if (nrow(mx) == 1 || ncol(mx) == 1) {
      return(1) # Single element has perfect internal mobility
    }

    mx.dia <- diag(mx)
    mx.dia <- mx.dia[-l]
    internal.mobility <- sum(mx.dia) / sum(mx[-l, -l])
    return(internal.mobility)
  }

  diagonal.mobility <- unlist(lapply(segments$mat.list, diag.mobility))

  # Calculate mobility in significant edges
  mob.in.edges <- vector(length = length(segments$mat.list))

  for (i in seq_along(segments$mat.list)) {
    # Check if we have segments at this level
    if (i > length(segments$segment.list)) {
      # No segments at this level, set to NA or 1
      mob.in.edges[i] <- 1
    } else {
      wm <- weight.matrix(
        mx = segments$mat.list[[i]],
        cut.off = 1,
        diagonal = TRUE,
        symmetric = FALSE,
        small.cell.reduction = 0
      )
      mm <- segments$mat.list[[i]]
      l <- nrow(mm)

      if (l <= 2) {
        mob.in.edges[i] <- 1
      } else {
        mm.reduced <- mm[-l, -l]
        if (!is.matrix(mm.reduced)) {
          mm.reduced <- as.matrix(mm.reduced)
        }
        row.margin <- rowSums(mm.reduced)
        mm.reduced[is.na(wm)] <- 0
        rs <- rowSums(mm.reduced)
        mob.in.edges[i] <- sum(rs) / sum(row.margin)
      }
    }
  }

  # Calculate degree statistics
  segment.degree <- function(mx, small.cell.reduction) {
    mx.1 <- weight.matrix(
      mx,
      cut.off = 1,
      small.cell.reduction = small.cell.reduction,
      symmetric = FALSE
    )
    mx.1[is.na(mx.1)] <- 0

    gra.diag.null <- moneca_graph_from_adjacency(
      mx.1,
      weighted = TRUE,
      mode = "directed",
      diag = FALSE
    )
    gra.diag.true <- moneca_graph_from_adjacency(
      mx.1,
      weighted = TRUE,
      mode = "directed"
    )
    gra.diag.null <- simplify(
      gra.diag.null,
      remove.loops = TRUE,
      remove.multiple = FALSE
    )

    # Use strength (weighted degree) instead of simple degree
    deg.all <- strength(gra.diag.null, mode = "all")
    deg.out <- strength(gra.diag.null, mode = "out")
    deg.in <- strength(gra.diag.null, mode = "in")
    nodes <- vcount(gra.diag.null)
    dens <- moneca_graph_density(gra.diag.null)
    isolates <- sum(deg.all == 0)

    mx.asym <- weight.matrix(
      mx,
      cut.off = 1,
      small.cell.reduction = small.cell.reduction,
      symmetric = FALSE
    )
    weight.stat <- summary(mx.asym[!is.na(mx.asym)])

    out <- list(
      degree.all = summary(deg.all),
      deg.out = summary(deg.out),
      deg.in = summary(deg.in),
      weight.stat = weight.stat,
      deg.sum = ecount(gra.diag.null),
      nodes = nodes,
      density = dens,
      isolates = isolates
    )
    return(out)
  }

  degree.stats <- lapply(
    segments$mat.list,
    segment.degree,
    small.cell.reduction
  )

  # Format output with improved labels
  cat("\n")
  cat(
    "================================================================================\n"
  )
  cat(
    "                        moneca MOBILITY ANALYSIS RESULTS                        \n"
  )
  cat(
    "================================================================================\n\n"
  )

  # Overall statistics
  cat("OVERALL MOBILITY PATTERNS\n")
  cat("-", strrep("-", 78), "\n", sep = "")
  cat(sprintf(
    "%-50s %6.1f%%\n",
    "Overall Population Mobility Rate:",
    round(total.mobility * 100, digits)
  ))
  cat(sprintf(
    "%-50s %6.1f%%\n",
    "Average Mobility Concentration (all levels):",
    round(mean(mob.in.edges) * 100, digits)
  ))
  cat("\n")

  # Hierarchical level statistics
  cat("HIERARCHICAL SEGMENTATION ANALYSIS\n")
  cat("-", strrep("-", 78), "\n", sep = "")

  # Create summary table for levels
  n_levels <- length(diagonal.mobility)
  level_names <- paste("Level", 1:n_levels)

  # Internal mobility
  cat("\nInternal Mobility Within Segments (%):\n")
  internal_mob <- round(diagonal.mobility * 100, digits)
  names(internal_mob) <- level_names
  print(internal_mob, quote = FALSE)

  # Mobility concentration by level
  cat("\nMobility Concentration in Significant Pathways by Level (%):\n")
  mob_concentration <- round(mob.in.edges * 100, digits)
  names(mob_concentration) <- level_names
  print(mob_concentration, quote = FALSE)

  # Network statistics table
  cat("\nNetwork Structure by Level:\n")
  cat(sprintf("%-30s", ""))
  cat(sprintf("%12s", level_names), "\n")
  cat("-", strrep("-", 78), "\n", sep = "")

  # Extract statistics for each level
  edges <- sapply(degree.stats, function(x) x$deg.sum)
  nodes <- sapply(degree.stats, function(x) x$nodes)
  density <- sapply(degree.stats, function(x) x$density)
  isolates <- sapply(degree.stats, function(x) x$isolates)

  cat(sprintf("%-30s", "Active Segments/Classes:"))
  cat(sprintf("%12d", nodes), "\n")

  cat(sprintf("%-30s", "Significant Edges:"))
  cat(sprintf("%12d", edges), "\n")

  cat(sprintf("%-30s", "Network Density:"))
  cat(sprintf("%12.3f", density), "\n")

  cat(sprintf("%-30s", "Isolated Segments:"))
  cat(sprintf("%12d", isolates), "\n")

  # Optional degree statistics
  if (show.degree.stats) {
    cat("\n")
    cat("DETAILED WEIGHTED DEGREE DISTRIBUTIONS (STRENGTH)\n")
    cat("-", strrep("-", 78), "\n", sep = "")

    # Create matrices for degree statistics
    l <- length(degree.stats)
    all.mat <- matrix(ncol = 6, nrow = l)
    out.mat <- matrix(ncol = 6, nrow = l)
    in.mat <- matrix(ncol = 6, nrow = l)
    weight.mat <- matrix(ncol = 6, nrow = l)

    for (i in 1:l) {
      all.mat[i, ] <- degree.stats[[i]]$degree.all
      out.mat[i, ] <- degree.stats[[i]]$deg.out
      in.mat[i, ] <- degree.stats[[i]]$deg.in
      weight.mat[i, ] <- degree.stats[[i]]$weight.stat
    }

    stat_names <- c("Min", "Q1", "Median", "Mean", "Q3", "Max")

    # Total strength
    cat("\nTotal Weighted Connections (Strength In + Out):\n")
    colnames(all.mat) <- stat_names
    rownames(all.mat) <- level_names
    print(round(all.mat, 2))

    # Out strength
    cat("\nOutward Mobility Strength (Weighted Out-Degree):\n")
    colnames(out.mat) <- stat_names
    rownames(out.mat) <- level_names
    print(round(out.mat, 2))

    # In strength
    cat("\nInward Mobility Strength (Weighted In-Degree):\n")
    colnames(in.mat) <- stat_names
    rownames(in.mat) <- level_names
    print(round(in.mat, 2))

    # Edge weights
    cat("\nEdge Weight Distribution (Relative Risk Values):\n")
    colnames(weight.mat) <- stat_names
    rownames(weight.mat) <- level_names
    print(round(weight.mat, 2))
  }

  cat("\n")
  cat(
    "================================================================================\n"
  )

  # Add interpretation guide
  if (interactive()) {
    cat("\nFor detailed interpretation of these statistics, see:\n")
    cat("?print.moneca or vignette('moneca-statistics')\n")
  }

  invisible(NULL)
}

#' Print Method for Descriptive MONECA Statistics
#'
#' Internal function that formats and displays the detailed statistics calculated
#' by the \code{print.moneca} method. This function creates formatted tables showing
#' network properties and mobility statistics across hierarchical levels.
#'
#' @param x A descriptive.moneca object containing calculated statistics.
#' @param ... Additional arguments (currently unused).
#'
#' @return NULL (called for side effects - printing formatted output).
#'
#' @details
#' This internal function is responsible for the formatted display of MONECA
#' analysis results. It creates several summary tables:
#' \itemize{
#'   \item Degree distributions (all, in, out) for each level
#'   \item Edge weight distributions for each level
#'   \item Diagonal mobility percentages
#'   \item Network density and connectivity statistics
#' }
#'
#' @keywords internal
print.descriptive.moneca <- function(x, ...) {
  # Out matrice

  l <- length(x$diagonal.mobility)
  stat <- x$degree.stats

  all.mat <- matrix(ncol = 6, nrow = l)
  out.mat <- matrix(ncol = 6, nrow = l)
  in.mat <- matrix(ncol = 6, nrow = l)
  weight.mat <- matrix(ncol = 6, nrow = l)
  deg.sum <- vector(length = l)
  nodes <- vector(length = l)
  density <- vector(length = l)
  isolates <- vector(length = l)

  for (i in 1:l) {
    all.mat[i, ] <- stat[[i]]$degree.all
    out.mat[i, ] <- stat[[i]]$deg.out
    in.mat[i, ] <- stat[[i]]$deg.in
    weight.mat[i, ] <- stat[[i]]$weight.stat
    deg.sum[i] <- stat[[i]]$deg.sum
    nodes[i] <- stat[[i]]$nodes
    density[i] <- stat[[i]]$density
    isolates[i] <- stat[[i]]$isolates
  }

  rownames(all.mat) <- paste(
    "        ",
    format("All degrees:", width = 12),
    format(1:l, width = 5, justify = "left"),
    ". Level  ",
    sep = ""
  )
  rownames(out.mat) <- paste(
    "        ",
    format("Out degrees:", width = 12),
    format(1:l, width = 5, justify = "left"),
    ". Level  ",
    sep = ""
  )
  rownames(in.mat) <- paste(
    "        ",
    format("In degrees:", width = 12),
    format(1:l, width = 5, justify = "left"),
    ". Level ",
    sep = ""
  )
  rownames(weight.mat) <- paste(
    "        ",
    format("Weights:", width = 12),
    format(1:l, width = 5, justify = "left"),
    ". Level  ",
    sep = ""
  )

  res.mat <- round(rbind(all.mat, out.mat, in.mat, weight.mat), 3)
  colnames(res.mat) <- c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max.")

  diag.mobil <- round(x$diagonal.mobility * 100, 1)
  names(diag.mobil) <- paste(1:l, ". Level", sep = "")

  # Print

  cat(
    format(
      "General descriptives of moneca analysis:",
      width = 90,
      justify = "centre"
    ),
    "\n",
    "\n",
    format("Total mobility:", width = 40, justify = "left"),
    format(round(x$total.mobility * 100, 1), justify = "right"),
    "%",
    "\n",
    format("Share of mobility within edges:", width = 40, justify = "left"),
    paste(
      format(round(x$samlet.oversandsynlighed * 100, 1), justify = "right"),
      "%",
      sep = ""
    ),
    "\n",
    "\n",
    format(
      "Share of mobility within the diagonal for each level:",
      width = 50,
      justify = "left"
    ),
    "\n"
  )
  print(noquote(format(diag.mobil, width = 15, justify = "centre")))
  cat(
    "\n",
    format(
      "Number of small cell reduced edges  for each level:",
      width = 40,
      justify = "left"
    ),
    format(deg.sum),
    "\n"
  )
  cat(
    "\n",
    format("Number of nodes for each level:", width = 40, justify = "left"),
    format(nodes),
    "\n"
  )
  cat(
    "\n",
    format("Density for each level:", width = 40, justify = "left"),
    format(round(density, 3)),
    "\n"
  )
  cat(
    "\n",
    format("Number of isolates for each level:", width = 40, justify = "left"),
    format(isolates),
    "\n"
  )
  # cat("\n", "\n", format("Degree statistics per level:", width=90, justify="centre"), "\n")
  # print(res.mat)
}

#######################################################################################################
# Node descriptives

#' Vertex mobility
#'
#' Calculate mobility metrics for each vertex in the network.
#' @param segments A moneca object from moneca().
#' @return A data frame with mobility metrics for each vertex.
#' @export

vertex.mobility <- function(segments) {
  mat <- segments$mat.list[[1]]

  number.of.levels <- length(segments$segment.list)
  l <- nrow(mat)
  mat <- mat[-l, -l]

  mobil.out <- mat / rowSums(mat)
  mobil.in <- t(t(mat) / colSums(mat))

  out.mat <- matrix(nrow = nrow(mat), ncol = number.of.levels)
  in.mat <- matrix(nrow = nrow(mat), ncol = number.of.levels)

  rownames(out.mat) <- rownames(mat)
  rownames(in.mat) <- rownames(mat)

  for (level in 1:number.of.levels) {
    segments <- unlist(segments$segment.list[level], recursive = FALSE)
    for (i in 1:length(segments)) {
      mobil.out[segments[[i]], segments[[i]]] <- 0
      mobil.in[segments[[i]], segments[[i]]] <- 0
    }

    out.mat[, level] <- rowSums(mobil.out)
    in.mat[, level] <- colSums(mobil.in)
  }

  out.mat <- 1 - out.mat
  in.mat <- 1 - in.mat

  out.mat <- cbind(out.mat, out.mat[, number.of.levels] - out.mat[, 1])
  in.mat <- cbind(in.mat, in.mat[, number.of.levels] - in.mat[, 1])

  colnames(out.mat) <- c(1:number.of.levels, "Exp. mobility")
  colnames(in.mat) <- c(1:number.of.levels, "Exp. mobility")

  return(list(out.mobility = out.mat, in.mat = in.mat))
}


####################################################################
# Segment quality

#' Evaluate Segment Quality Metrics
#'
#' Calculates comprehensive quality metrics for each segment across all hierarchical
#' levels of a moneca analysis. This function provides detailed statistics about
#' segment cohesion, size, network properties, and mobility patterns.
#'
#' @param segments A moneca object returned by \code{\link{moneca}}.
#' @param final.solution Logical indicating whether to return only the final
#'   (most aggregated) solution for each unique segment. Default is FALSE, which
#'   returns metrics for all levels.
#' @param segment_naming Character string or data frame specifying how to name segments.
#'   Used to create readable segment labels in the output. Options include:
#'   \itemize{
#'     \item "auto" (default): Use simple "Segment X" format
#'     \item "concat": Concatenate member names
#'     \item "pattern": Use pattern-based naming
#'     \item "custom": Custom naming strategy
#'     \item Data frame: Must have columns "name" and "segment_label" for custom mapping
#'   }
#'   When using a data frame, the function will match original category names to
#'   custom labels and use them for segment naming. This creates a "segment_label"
#'   column in the output that can be used by plotting functions.
#'
#' @return A data frame with the following columns for each hierarchical level:
#'   \describe{
#'     \item{Membership}{Segment membership identifier in format "level.segment"
#'       (e.g., "2.1" means level 2, segment 1)}
#'     \item{[Level]: Segment}{Integer segment identifier at each level}
#'     \item{[Level]: within.mobility}{Proportion of mobility that stays within
#'       the segment (diagonal/total). Higher values indicate more cohesive segments.
#'       Range: 0-1, where 1 means all mobility is internal.}
#'     \item{[Level]: share.of.mobility}{Segment's share of total mobility in the
#'       system. Indicates the relative importance/size of the segment.
#'       Range: 0-1, values sum to 1 across segments.}
#'     \item{[Level]: Density}{Network density within the segment (proportion of
#'       possible edges that exist). Higher values indicate more interconnected nodes.
#'       Range: 0-1, where 1 means fully connected.}
#'     \item{[Level]: Nodes}{Number of original categories/nodes in the segment}
#'     \item{[Level]: Max.path}{Maximum shortest path length (diameter) within
#'       the segment. Lower values indicate more compact segments.}
#'     \item{[Level]: share.of.total}{Segment's share of total population/observations
#'       based on marginal totals. Differs from share.of.mobility by considering
#'       population size rather than flow volume.}
#'   }
#'
#'   When \code{final.solution = TRUE}, returns a simplified data frame with only
#'   the most aggregated metrics for each unique segment.
#'
#' @details
#' The function evaluates multiple aspects of segmentation quality:
#'
#' \strong{Cohesion Metrics:}
#' \itemize{
#'   \item \code{within.mobility}: Key quality indicator - proportion of mobility
#'     contained within segment boundaries. Values > 0.7 suggest strong segments.
#'   \item \code{Density}: How interconnected nodes are within each segment.
#'     Dense segments (> 0.5) indicate tight communities.
#' }
#'
#' \strong{Size Metrics:}
#' \itemize{
#'   \item \code{share.of.mobility}: Relative importance based on flow volume
#'   \item \code{share.of.total}: Relative size based on population
#'   \item \code{Nodes}: Absolute size in terms of categories
#' }
#'
#' \strong{Structure Metrics:}
#' \itemize{
#'   \item \code{Max.path}: Network diameter - smaller values indicate more
#'     compact, well-connected segments
#' }
#'
#' The output is ordered by the final level's segment sizes for easier interpretation.
#'
#' @examples
#' # Generate data and run MONECA
#' mobility_data <- generate_mobility_data(n_classes = 8, seed = 123)
#' seg <- moneca(mobility_data, segment.levels = 3)
#'
#' # Get detailed quality metrics for all levels
#' quality_full <- segment.quality(seg)
#' print(quality_full)
#'
#' # Get only final solution summary
#' quality_final <- segment.quality(seg, final.solution = TRUE)
#' print(quality_final)
#'
#' # Use custom segment naming with final solution
#' custom_names <- data.frame(
#'   name = c("Class1", "Class2", "Class3"),
#'   segment_label = c("Executive", "Professional", "Technical"),
#'   stringsAsFactors = FALSE
#' )
#' quality_labeled <- segment.quality(seg, final.solution = TRUE, segment_naming = custom_names)
#' print(quality_labeled)
#'
#' \dontrun{
#' # Visualize segment quality
#' plot_segment_quality(seg)
#' }
#'
#' @seealso
#' \code{\link{moneca}} for the main analysis function,
#' \code{\link{plot_segment_quality}} for graphical representation,
#' \code{\link{segment.membership}} for segment assignments
#'
#' @export

segment.quality <- function(
  segments,
  final.solution = FALSE,
  segment_naming = "auto"
) {
  mat <- segments$mat.list[[1]]
  l <- nrow(mat)
  mat <- mat[-l, -l]
  # Use the minimum of mat.list and segment.list lengths to avoid index out of bounds
  number.of.levels <- min(
    length(segments$mat.list),
    length(segments$segment.list)
  )

  segment.qual.onelevel <- function(segments, level) {
    names <- rownames(segments$mat.list[[1]])
    names <- names[-length(names)]

    # Check if level exists in both lists
    if (
      level > length(segments$segment.list) || level > length(segments$mat.list)
    ) {
      # Return empty data frame with correct structure
      return(data.frame(
        Segment = rep(NA, length(names)),
        within.mobility = rep(NA, length(names)),
        share.of.mobility = rep(NA, length(names)),
        Density = rep(NA, length(names)),
        Nodes = rep(NA, length(names)),
        Max.path = rep(NA, length(names)),
        share.of.total = rep(NA, length(names))
      ))
    }

    seg.list.level <- segments$segment.list[[level]]
    mat.level <- segments$mat.list[[level]]
    totals.level <- (mat.level[nrow(mat.level), ] +
      mat.level[, nrow(mat.level)]) /
      2
    totals.level <- totals.level[-length(totals.level)]
    mat.level <- mat.level[-nrow(mat.level), -nrow(mat.level)]

    edge.matrix <- segment.edges(
      segments,
      cut.off = 1,
      level = 0,
      small.cell.reduction = 5,
      segment.reduction = 0
    )
    net.edge <- moneca_graph_from_adjacency(edge.matrix, weighted = TRUE) # Tjek mode og den slags på det her svin

    # Segment
    seg <- rep(NA, length(names))
    for (i in 1:length(seg.list.level)) {
      seg[seg.list.level[[i]]] <- i
    }
    # Quality (or within mobility)
    if (is.matrix(mat.level) && nrow(mat.level) > 0) {
      level.qual <- round(
        diag(mat.level) / ((rowSums(mat.level) + colSums(mat.level)) / 2),
        3
      )
    } else if (length(mat.level) == 1) {
      # Handle single segment case
      level.qual <- 1 # Perfect within-mobility for single segment
    } else {
      level.qual <- numeric(0)
    }
    quality <- rep(NA, length(names))
    for (i in seq_along(seg.list.level)) {
      if (i <= length(level.qual)) {
        quality[seg.list.level[[i]]] <- level.qual[i]
      }
    }
    # Share of mobility
    if (is.matrix(mat.level) && nrow(mat.level) > 0) {
      level.size <- round(
        ((rowSums(mat.level) + colSums(mat.level)) / 2) /
          sum(colSums(mat.level)),
        3
      )
    } else if (length(mat.level) == 1) {
      level.size <- 1 # Single segment has 100% of mobility
    } else {
      level.size <- numeric(0)
    }
    size <- rep(NA, length(names))
    for (i in seq_along(seg.list.level)) {
      if (i <= length(level.size)) {
        size[seg.list.level[[i]]] <- level.size[i]
      }
    }
    # Density
    level.density <- rep(NA, length(names))
    for (i in 1:length(seg.list.level)) {
      level.density[seg.list.level[[i]]] <- moneca_graph_density(
        net.edge -
          which(((1:vcount(net.edge) %in% seg.list.level[[i]]) == FALSE))
      )
    }
    # Nodes
    nodes <- rep(NA, length(names))
    for (i in 1:length(seg.list.level)) {
      nodes[seg.list.level[[i]]] <- length(seg.list.level[[i]])
    }
    # Max path length
    max.path <- rep(NA, length(names))
    for (i in 1:length(seg.list.level)) {
      max.path[seg.list.level[[i]]] <- diameter(
        net.edge -
          which(((1:vcount(net.edge) %in% seg.list.level[[i]]) == FALSE)),
        weights = NA
      )
    }

    # Share of total size
    share.of.total <- rep(NA, length(names))
    for (i in 1:length(seg.list.level)) {
      share.of.total[seg.list.level[[i]]] <- (totals.level / sum(totals.level))[
        i
      ]
    }

    out.frame <- data.frame(
      Segment = seg,
      within.mobility = quality,
      share.of.mobility = size,
      Density = level.density,
      Nodes = nodes,
      Max.path = max.path,
      share.of.total = round(share.of.total, 3)
    )
    colnames(out.frame) <- paste(level, ": ", colnames(out.frame), sep = "")
    out.frame
  }

  qual.list <- lapply(
    1:number.of.levels,
    segment.qual.onelevel,
    segments = segments
  )
  out.mat <- do.call(cbind, qual.list)
  out.mat <- cbind(Membership = segment.membership(segments)[, 2], out.mat)
  rownames(out.mat) <- rownames(mat)
  order.mat <- out.mat[, grep("share.of.total", colnames(out.mat))]
  order.mat <- order.mat[, ncol(order.mat):1]
  out.mat <- out.mat[do.call(order, -order.mat), ]

  if (final.solution == TRUE) {
    small.mat <- out.mat[duplicated(out.mat$Membership) == FALSE, ]
    small.mat[sapply(small.mat, is.nan)] <- Inf
    tsm <- as.matrix(small.mat)[, -1, drop = FALSE] # Keep matrix dimensions
    collapse.mat <- function(row, n) tail(na.omit(row), n)

    # Check if we have any rows
    if (nrow(tsm) > 0) {
      # Apply collapse.mat only if we have data
      if (ncol(tsm) >= 7) {
        tsm <- as.data.frame(t(apply(tsm, 1, collapse.mat, n = 7)))
        colnames(tsm) <- c(
          "Membership",
          "within.mobility",
          "share.of.mobility",
          "Density",
          "Nodes",
          "Max.path",
          "share.of.total"
        )

        # Convert numeric columns back to numeric (apply converts everything to character)
        numeric_cols <- c(
          "within.mobility",
          "share.of.mobility",
          "Density",
          "Nodes",
          "Max.path",
          "share.of.total"
        )
        for (col in numeric_cols) {
          if (col %in% colnames(tsm)) {
            tsm[[col]] <- as.numeric(tsm[[col]])
          }
        }
      } else {
        # If we have fewer than 7 columns, just use what we have
        tsm <- as.data.frame(tsm)
      }
      tsm$Membership <- small.mat$Membership

      # Add Segment column that plotting functions expect (same as Membership for final solution)
      tsm$Segment <- tsm$Membership

      # Add proper segment naming from canonical metadata
      meta <- if (!is.null(segments$segment_metadata)) {
        segments$segment_metadata
      } else {
        moneca_segments(segments)
      }

      membership_parts <- strsplit(as.character(tsm$Membership), "\\.")
      segment_labels <- character(nrow(tsm))
      for (i in seq_along(membership_parts)) {
        parts <- membership_parts[[i]]
        if (length(parts) >= 2) {
          level_num <- as.integer(parts[1])
          segment_num <- as.integer(parts[2])
          if (is.na(level_num) || is.na(segment_num)) {
            segment_labels[i] <- paste("Segment", i)
          } else if (level_num >= 1 && level_num <= meta$n_levels) {
            map_df <- meta$levels[[level_num]]$map
            if (segment_num >= 1 && segment_num <= nrow(map_df)) {
              segment_labels[i] <- map_df$label[segment_num]
            } else {
              segment_labels[i] <- paste("Segment", segment_num)
            }
          } else {
            segment_labels[i] <- paste("Segment", segment_num)
          }
        } else {
          segment_labels[i] <- paste("Segment", i)
        }
      }

      # Override with custom dataframe if provided
      if (is.data.frame(segment_naming)) {
        if (all(c("name", "segment_label") %in% colnames(segment_naming))) {
          for (i in seq_len(nrow(tsm))) {
            lbl <- segment_labels[i]
            base_name <- sub("\\+[0-9]+$", "", lbl)
            custom_idx <- match(base_name, segment_naming$name)
            if (!is.na(custom_idx)) {
              segment_labels[i] <- segment_naming$segment_label[custom_idx]
            }
          }
        }
      }

      tsm$segment_label <- segment_labels

      out.mat <- tsm
    }
  }

  #colnames(out.mat) <- c("Membership", "Within mobility", "Share of mobility", "Density", "Nodes", "Max.path", "Share of total size")
  out.mat
}


#####################################################################
#### According to first level

#' First level summary
#'
#' @keywords internal

first.level.summary <- function(
  segments,
  small.cell.reduction = segments$small.cell.reduction
) {
  n.1.edges <- vector(length = length(segments$segment.list))
  sum.1 <- list()

  for (i in seq(segments$segment.list)) {
    # Antal 1.levels edges pr. level
    seg <- segment.edges(
      segments,
      level = seq(segments$segment.list),
      segment.reduction = 1:i,
      diagonal = NULL
    )
    n.1.edges[i] <- sum(seg > 0)
    sum.1[[i]] <- summary(seg[seg > 0])
  }

  n.1.edges # Number of 1. edges on each level
  sum.1 # Summary of degrees on each level

  ###### Longest path and density
  m <- weight.matrix(
    segments$mat.list[[1]],
    cut.off = 1,
    small.cell.reduction = small.cell.reduction,
    symmetric = FALSE
  )
  m[is.na(m)] <- 0
  net.path <- moneca_graph_from_adjacency(
    m,
    mode = "directed",
    weighted = TRUE,
    diag = FALSE
  ) # Her er diag = NULL erstattet med FALSE
  sp <- moneca_shortest_paths(net.path, weights = NA)

  des <- list()
  for (niv in 2:length(segments$segment.list)) {
    max.path.global <- vector(length = length(segments$segment.list[[niv]]))
    clust <- vector(length = length(segments$segment.list[[niv]]))
    max.clust <- vector(length = length(segments$segment.list[[niv]]))
    density <- vector(length = length(segments$segment.list[[niv]]))
    size <- vector(length = length(segments$segment.list[[niv]]))
    seg.niv <- segments$segment.list[[niv]]
    max.path <- vector(length = length(segments$segment.list[[niv]]))
    av.path <- vector(length = length(segments$segment.list[[niv]]))
    for (i in 1:length(seg.niv)) {
      seg <- seg.niv[[i]]
      max.path.global[i] <- max(sp[seg, seg])
      h <- 1:vcount(net.path)
      density[i] <- moneca_graph_density(net.path - h[-seg])
      clust[i] <- max(moneca_components(net.path - h[-seg])$no)
      max.clust[i] <- max(moneca_components(net.path - h[-seg])$csize)
      size[i] <- length(seg)
      max.path[i] <- max(moneca_shortest_paths(
        net.path - h[-seg],
        weights = NA
      ))
      av.path[i] <- moneca_average_path_length(net.path - h[-seg])
    }
    des[[niv]] <- cbind(
      size,
      max.path.global,
      max.path,
      density = round(density, 3),
      clusters = clust,
      max.clust,
      av.path
    )
  }

  ###################################################
  ### Amount of mobility left in 1. level edges on subsequent levels

  mx <- segments$mat.list[[1]]
  l <- ncol(mx)
  mx.s <- mx[-l, -l]
  m <- weight.matrix(
    segments$mat.list[[1]],
    cut.off = 1,
    small.cell.reduction = small.cell.reduction,
    symmetric = FALSE,
    diagonal = TRUE
  )
  mx.share <- mx.s / sum(mx.s)
  mx.share[is.na(m)] <- 0

  segment.list <- segments$segment.list
  share <- vector(length = length(segments$segment.list))

  for (niv in seq(segments$segment.list)) {
    seg.niv <- segment.list[[niv]]
    for (segment in 1:length(seg.niv)) {
      seg <- seg.niv[[segment]]
      mx.share[seg, seg] <- 0
    }
    share[niv] <- sum(mx.share)
  }
  share

  # output

  out <- list(n.1.edges = n.1.edges, sum.1 = sum.1, des = des, share = share)
  class(out) <- "first_level_summary"
  return(out)
}

#' Print first level summary
#'
#' @param x A first_level_summary object.
#' @param ... Additional arguments (unused).
#' @return Invisibly returns the object.
#' @export

print.first_level_summary <- function(x, ...) {
  l.seq <- 1:length(x$n.1.edges)
  n.1.edges <- paste("Level ", l.seq, ": ", x$n.1.edges, sep = "")
  sum.1 <- x$sum.1
  des <- x$des[-1]
  share <- paste("Level ", l.seq, ": ", round(x$share, 3) * 100, "%", sep = "")
  names(sum.1) <- paste("Level", l.seq, ": ", sep = "")
  names(des) <- paste("Level", l.seq[-1:-2], ": ", sep = "")

  cat("\n", "\n", "Descriptives of each level according to level 1:", "\n")
  cat(
    "\n",
    "\n",
    "Amount of mobility left in 1. level edges on subsequent levels",
    "\n"
  )
  print(noquote(format(share, width = 15)))
  cat("\n", "\n", "Number of edges pr. level", "\n")
  print(noquote(format(n.1.edges, width = 15)))

  cat("\n", "\n", "Summary of degrees on each level", "\n", "\n")
  print(print(sum.1))
  cat(
    "\n",
    "Maximal paths and density for each segment on each level:",
    "\n",
    "\n"
  )
  print(des)
}

#' Extract and Label Hierarchical Mobility Matrix
#'
#' Extracts the mobility matrix at a given hierarchical level from a moneca
#' result, strips the "Total" margin (unless \code{include_total = TRUE}), and
#' labels rows and columns by the most central node within each segment.
#' Centrality is measured as the sum of incoming and outgoing relative-risk
#' weights within the segment subgraph.
#'
#' @param segments A moneca object returned by \code{\link{moneca}} or
#'   \code{\link{moneca_fast}}.
#' @param level Integer. The hierarchical level to extract (default 2). Level 1
#'   returns the original input matrix.
#' @param include_total Logical. If \code{TRUE}, keep the "Total" row and column
#'   in the returned matrix (default \code{FALSE}).
#'
#' @return A matrix with rows and columns labeled by the most central node in
#'   each segment. The matrix carries a \code{"segment_map"} attribute: a
#'   \code{data.frame} with columns \code{segment} (integer index),
#'   \code{main_node} (label chosen for that segment), \code{members}
#'   (comma-separated string of all member names), and \code{n_members} (number
#'   of members in the segment).
#'
#' @export
#'
#' @examples
#' mob <- generate_mobility_data(n_classes = 6, n_total = 500, seed = 42)
#' seg <- moneca_fast(mob, segment.levels = 2)
#' mx2 <- level.matrix(seg, level = 2)
#' mx2
#' attr(mx2, "segment_map")
level.matrix <- function(segments, level = 2, include_total = FALSE) {
  # 1. Validate input -----
  if (!inherits(segments, "moneca")) {
    stop(
      "'segments' must be a moneca object returned by moneca() or moneca_fast().",
      call. = FALSE
    )
  }

  n_levels <- length(segments$mat.list)
  if (
    !is.numeric(level) ||
      length(level) != 1 ||
      level != as.integer(level) ||
      level < 1 ||
      level > n_levels
  ) {
    stop(
      "'level' must be a single integer between 1 and ",
      n_levels,
      " (available levels in this moneca object).",
      call. = FALSE
    )
  }
  level <- as.integer(level)

  # 2. Extract matrix and strip Total margin -----
  mx_full <- segments$mat.list[[level]]

  # At level 1 the margin row/col is named "Total"; at higher levels

  # it carries a numeric name (the grand total).  Detect by name first,

  # then fall back to checking whether the last row/col is the margin.
  total_rows <- which(rownames(mx_full) == "Total")
  total_cols <- which(colnames(mx_full) == "Total")

  if (
    length(total_rows) == 0 && length(total_cols) == 0 && nrow(mx_full) >= 2
  ) {
    nr <- nrow(mx_full)
    nc <- ncol(mx_full)
    core_col_sums <- colSums(mx_full[-nr, -nc, drop = FALSE])
    core_row_sums <- rowSums(mx_full[-nr, -nc, drop = FALSE])
    last_row_vals <- mx_full[nr, -nc]
    last_col_vals <- mx_full[-nr, nc]
    if (
      isTRUE(all.equal(
        as.vector(core_col_sums),
        as.vector(last_row_vals),
        check.attributes = FALSE
      )) &&
        isTRUE(all.equal(
          as.vector(core_row_sums),
          as.vector(last_col_vals),
          check.attributes = FALSE
        ))
    ) {
      total_rows <- nr
      total_cols <- nc
    }
  }

  has_total <- length(total_rows) > 0 || length(total_cols) > 0

  # Work on matrix without Total
  drop_idx <- union(total_rows, total_cols)
  if (length(drop_idx) > 0) {
    mx <- mx_full[-drop_idx, -drop_idx, drop = FALSE]
  } else {
    mx <- mx_full
  }

  # 3. Original node names (excluding Total) -----
  all_names <- rownames(segments$mat.list[[1]])
  total_idx_l1 <- which(all_names == "Total")
  if (length(total_idx_l1) > 0) {
    original_names <- all_names[-total_idx_l1]
  } else {
    original_names <- all_names
  }

  # 4. Level 1: return with trivial segment_map -----
  if (level == 1) {
    segment_map <- data.frame(
      segment = seq_along(original_names),
      main_node = original_names,
      members = original_names,
      n_members = rep(1L, length(original_names)),
      stringsAsFactors = FALSE
    )
    attr(mx, "segment_map") <- segment_map

    if (include_total && has_total) {
      mx <- mx_full
    }
    return(mx)
  }

  # 5. Get canonical metadata and apply labels -----
  meta <- if (!is.null(segments$segment_metadata)) {
    segments$segment_metadata
  } else {
    moneca_segments(segments)
  }

  map_df <- meta$levels[[level]]$map
  main_nodes <- map_df$label
  segment_map <- map_df[,
    c("segment", "main_node", "members", "n_members"),
    drop = FALSE
  ]

  rownames(mx) <- main_nodes
  colnames(mx) <- main_nodes

  attr(mx, "segment_map") <- segment_map

  # 8. Re-add Total row/col if requested -----
  if (include_total && has_total) {
    nr <- nrow(mx_full)
    nc <- ncol(mx_full)
    full_labels_row <- character(nr)
    full_labels_col <- character(nc)

    non_total_rows <- setdiff(seq_len(nr), total_rows)
    non_total_cols <- setdiff(seq_len(nc), total_cols)

    full_labels_row[non_total_rows] <- main_nodes
    full_labels_col[non_total_cols] <- main_nodes

    if (length(total_rows) > 0) {
      full_labels_row[total_rows] <- "Total"
    }
    if (length(total_cols) > 0) {
      full_labels_col[total_cols] <- "Total"
    }

    rownames(mx_full) <- full_labels_row
    colnames(mx_full) <- full_labels_col
    attr(mx_full, "segment_map") <- segment_map
    return(mx_full)
  }

  mx
}
