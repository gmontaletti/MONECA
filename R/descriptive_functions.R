#' Print Method for MONECA Objects
#'
#' Provides a comprehensive summary of MONECA analysis results including mobility
#' statistics, network properties, and segmentation quality measures across all
#' hierarchical levels.
#'
#' @param x A MONECA object returned by \code{\link{moneca}}.
#' @param small.cell.reduction Numeric threshold for small cell handling. If NULL,
#'   uses the value from the MONECA object.
#' @param ... Additional arguments (currently unused).
#'
#' @return Invisibly returns a list of descriptive statistics, but primarily called
#'   for its side effect of printing a formatted summary.
#'
#' @details
#' The print method calculates and displays several key statistics:
#' 
#' \strong{Mobility Statistics:}
#' \itemize{
#'   \item Total mobility rate (proportion of off-diagonal movement)
#'   \item Diagonal mobility (immobility) by level
#'   \item Mobility captured by significant edges
#' }
#' 
#' \strong{Network Properties:}
#' \itemize{
#'   \item Node degrees (in, out, total) by level
#'   \item Network density by level
#'   \item Number of isolated nodes
#'   \item Edge weight distributions
#' }
#' 
#' \strong{Segmentation Quality:}
#' \itemize{
#'   \item Number of segments per level
#'   \item Proportion of mobility within vs. between segments
#'   \item Network coherence measures
#' }
#'
#' @examples
#' # Generate data and run analysis
#' mobility_data <- generate_mobility_data(n_classes = 6, seed = 42)
#' seg <- moneca(mobility_data, segment.levels = 3)
#' 
#' # Print comprehensive summary
#' print(seg)
#' 
#' # The summary includes mobility rates, network statistics, and
#' # segmentation quality measures for each hierarchical level
#'
#' @seealso \code{\link{moneca}}, \code{\link{summary.moneca}}
#' @export
print.moneca <- function(x, small.cell.reduction=x$small.cell.reduction, ...){
  segments <- x
  
  # Hvor mange procent flytter sig i det hele taget?
  mx            <- segments$mat.list[[1]]
  l             <- ncol(mx)
  total.total   <- mx[l,l]
  total.mobile  <- sum(mx[-l,-l])
  total.mobility      <- total.mobile/total.total
  total.mobility # Til output
  
  # Procent af den samlede mobilitet i diagonalen?
  diag.mobility        <- function(mx){
    l                    <- ncol(mx)
    mx.dia               <- diag(mx)
    mx.dia               <- mx.dia[-l]
    internal.mobility    <- sum(mx.dia)/sum(mx[-l,-l])
    return(internal.mobility)
  }
  diagonal.mobility <- unlist(lapply(segments$mat.list, diag.mobility))
  diagonal.mobility
  
  ## Hvor meget af den samlede mobilitet ligger i det oversandsynlige?
  # Sandsynlighedsmatricen
  mob.in.edges     <- vector(length=length(segments$segment.list))
  for (i in seq(segments$segment.list)){
    wm               <- weight.matrix(mx=segments$mat.list[[i]], cut.off=1, diagonal=TRUE, symmetric=FALSE, small.cell.reduction=0)
    mm               <- segments$mat.list[[i]]
    l                <- nrow(mm)
    
    # Handle case where matrix is too small
    if (l <= 2) {
      mob.in.edges[i] <- 1  # All mobility is in edges for tiny matrices
    } else {
      mm.reduced       <- mm[-l,-l]
      # Ensure mm.reduced is a matrix
      if (!is.matrix(mm.reduced)) {
        mm.reduced <- as.matrix(mm.reduced)
      }
      row.margin       <- rowSums(mm.reduced)
      mm.reduced[is.na(wm)]    <- 0
      rs               <- rowSums(mm.reduced)
      mob.in.edges[i]  <- sum(rs)/sum(row.margin)
    }
  }
  
  samlet.oversandsynlighed <- mob.in.edges
  
  # Degree mål
  
  segment.degree <- function(mx, small.cell.reduction){
  # Brug weight matrix istedet  
    mx.1 <- weight.matrix(mx, cut.off=1, small.cell.reduction=small.cell.reduction, symmetric=FALSE)
    mx.1[is.na(mx.1)]  <- 0   
    gra.diag.null <- moneca_graph_from_adjacency(mx.1, weighted=TRUE, mode="directed", diag = FALSE)
    gra.diag.true <- moneca_graph_from_adjacency(mx.1, weighted=TRUE, mode="directed")
    gra.diag.null <- simplify(gra.diag.null, remove.loops=TRUE, remove.multiple = FALSE)
    
    deg.all                <- degree(gra.diag.null, mode="all")
    deg.out                <- degree(gra.diag.null, mode="out")
    deg.in                 <- degree(gra.diag.null, mode="in")
    nodes                  <- vcount(gra.diag.null)
    dens                   <- moneca_graph_density(gra.diag.null)
    isolates               <- sum(deg.all == 0)
    # Average weight
    #w <- E(gra.diag.null)$weight
    #weight.stat <- summary(w)
    mx.asym                <- weight.matrix(mx, cut.off=1, small.cell.reduction=small.cell.reduction, symmetric=FALSE)
    weight.stat            <- summary(mx.asym[is.na(mx.asym)==FALSE])
    
    out <-list(degree.all=summary(deg.all), deg.out=summary(deg.out), deg.in=summary(deg.in), weight.stat=weight.stat, deg.sum=ecount(gra.diag.null), nodes=nodes, density=dens, isolates=isolates)
    return(out)
  }
  
  degree.stats <- lapply(segments$mat.list,segment.degree, small.cell.reduction)
  
  # Resultatet
  
  out <- list(total.mobility=total.mobility, samlet.oversandsynlighed=samlet.oversandsynlighed, diagonal.mobility=diagonal.mobility, degree.stats=degree.stats)
  
  
  # Pretty printing
  class(out) <- "descriptive.moneca"

  MONECA:::print.descriptive.moneca(out)
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
print.descriptive.moneca <- function(x, ...){
  
  # Out matrice
  
  l             <- length(x$diagonal.mobility)
  stat          <- x$degree.stats
  
  all.mat       <- matrix(ncol=6, nrow=l)
  out.mat       <- matrix(ncol=6, nrow=l)
  in.mat        <- matrix(ncol=6, nrow=l)
  weight.mat    <- matrix(ncol=6, nrow=l)
  deg.sum       <- vector(length=l)
  nodes         <- vector(length=l)
  density       <- vector(length=l)
  isolates      <- vector(length=l)
  
  for (i in 1:l){
    all.mat[i,]     <- stat[[i]]$degree.all
    out.mat[i,]     <- stat[[i]]$deg.out
    in.mat[i,]      <- stat[[i]]$deg.in
    weight.mat[i, ] <- stat[[i]]$weight.stat
    deg.sum[i]      <- stat[[i]]$deg.sum
    nodes[i]        <- stat[[i]]$nodes
    density[i]      <- stat[[i]]$density
    isolates[i]     <- stat[[i]]$isolates
    }
    
  rownames(all.mat)    <- paste("        ", format("All degrees:", width=12), format(1:l, width=5, justify="left"), ". Level  ", sep="")
  rownames(out.mat)    <- paste("        ", format("Out degrees:", width=12),format(1:l, width=5, justify="left"), ". Level  ", sep="")
  rownames(in.mat)     <- paste("        ", format("In degrees:", width=12),format(1:l, width=5, justify="left"), ". Level ", sep="")
  rownames(weight.mat) <- paste("        ", format("Weights:", width=12),format(1:l, width=5, justify="left"), ". Level  ", sep="")
  
  
  res.mat              <- round(rbind(all.mat, out.mat, in.mat, weight.mat),3)
  colnames(res.mat)    <- c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max.")
  
  diag.mobil           <- round(x$diagonal.mobility*100, 1)
  names(diag.mobil)    <- paste(1:l, ". Level", sep="")
   
  # Print
  
  cat(format("General descriptives of moneca analysis:",   width=90, justify="centre"),"\n", "\n", 
      format("Total mobility:", width=40, justify="left"), format(round(x$total.mobility*100,1), justify="right"), "%","\n",
      format("Share of mobility within edges:", width=40, justify="left"), paste(format(round(x$samlet.oversandsynlighed*100,1), justify="right"), "%", sep=""),"\n", 
      "\n", format("Share of mobility within the diagonal for each level:", width=50, justify="left"), "\n")
      print(noquote(format(diag.mobil, width=15, justify="centre")))
      cat("\n", format("Number of small cell reduced edges  for each level:", width=40, justify="left"), format(deg.sum), "\n")
  cat("\n", format("Number of nodes for each level:", width=40, justify="left"), format(nodes), "\n")
  cat("\n", format("Density for each level:", width=40, justify="left"), format(round(density, 3)), "\n")
  cat("\n", format("Number of isolates for each level:", width=40, justify="left"), format(isolates), "\n")
  # cat("\n", "\n", format("Degree statistics per level:", width=90, justify="centre"), "\n")
  # print(res.mat)
}

#######################################################################################################
# Node descriptives

#' Vertex mobility
#' 
#' @export


vertex.mobility <- function(segments){
mat <- segments$mat.list[[1]]

number.of.levels <- length(segments$segment.list)
l      <- nrow(mat)
mat    <- mat[-l,-l]

mobil.out <- mat/rowSums(mat)
mobil.in  <- t(t(mat)/colSums(mat))

out.mat <- matrix(nrow=nrow(mat), ncol=number.of.levels)
in.mat  <- matrix(nrow=nrow(mat), ncol=number.of.levels)

rownames(out.mat) <- rownames(mat)
rownames(in.mat)  <- rownames(mat) 


for(level in 1:number.of.levels){

segments <- unlist(segments$segment.list[level], recursive=FALSE)  
  for (i in 1:length(segments)){
    mobil.out[segments[[i]], segments[[i]]]  <- 0
    mobil.in[segments[[i]], segments[[i]]]   <- 0
  }

out.mat[,level]     <- rowSums(mobil.out)
in.mat[, level]     <- colSums(mobil.in)
}


out.mat <- 1 - out.mat
in.mat  <- 1 - in.mat

out.mat   <- cbind(out.mat, out.mat[,number.of.levels] - out.mat[,1])
in.mat    <- cbind(in.mat, in.mat[,number.of.levels] - in.mat[,1])

colnames(out.mat) <- c(1:number.of.levels, "Exp. mobility")
colnames(in.mat) <- c(1:number.of.levels, "Exp. mobility")

return(list(out.mobility=out.mat, in.mat=in.mat))
}


####################################################################
# Segment quality

#' Evaluate Segment Quality Metrics
#' 
#' Calculates comprehensive quality metrics for each segment across all hierarchical 
#' levels of a MONECA analysis. This function provides detailed statistics about 
#' segment cohesion, size, network properties, and mobility patterns.
#' 
#' @param segments A MONECA object returned by \code{\link{moneca}}.
#' @param final.solution Logical indicating whether to return only the final 
#'   (most aggregated) solution for each unique segment. Default is FALSE, which
#'   returns metrics for all levels.
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
#' # Identify high-quality segments (high cohesion, reasonable size)
#' good_segments <- quality_full[quality_full[,"2 : within.mobility"] > 0.7 & 
#'                              quality_full[,"2 : Nodes"] > 1, ]
#' 
#' # Visualize segment quality
#' \dontrun{
#' plot_segment_quality(seg)
#' }
#' 
#' @seealso 
#' \code{\link{moneca}} for the main analysis function,
#' \code{\link{plot_segment_quality}} for graphical representation,
#' \code{\link{segment.membership}} for segment assignments
#' 
#' @export

segment.quality <- function(segments, final.solution = FALSE){
  mat <- segments$mat.list[[1]]
  l   <- nrow(mat)
  mat <- mat[-l, -l]
  # Use the minimum of mat.list and segment.list lengths to avoid index out of bounds
  number.of.levels <- min(length(segments$mat.list), length(segments$segment.list))
  
  segment.qual.onelevel <- function(segments, level){
    
    names <- rownames(segments$mat.list[[1]])
    names <- names[-length(names)]
    
    # Check if level exists in both lists
    if (level > length(segments$segment.list) || level > length(segments$mat.list)) {
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
    mat.level      <- segments$mat.list[[level]]
    totals.level   <- (mat.level[nrow(mat.level),] + mat.level[, nrow(mat.level)]) / 2
    totals.level   <- totals.level[-length(totals.level)]
    mat.level      <- mat.level[-nrow(mat.level), -nrow(mat.level)]
    
    edge.matrix        <- segment.edges(segments, cut.off = 1 ,  level = 0, small.cell.reduction = 5, segment.reduction = 0)
    net.edge           <- moneca_graph_from_adjacency(edge.matrix, weighted = TRUE) # Tjek mode og den slags på det her svin
      
    # Segment
    seg                <- rep(NA, length(names))
    for (i in 1 : length(seg.list.level)) seg[seg.list.level[[i]]] <- i
    # Quality (or within mobility)
    if (is.matrix(mat.level) && nrow(mat.level) > 0) {
      level.qual        <- round(diag(mat.level)/((rowSums(mat.level) + colSums(mat.level))/2),3)
    } else if (length(mat.level) == 1) {
      # Handle single segment case
      level.qual <- 1  # Perfect within-mobility for single segment
    } else {
      level.qual <- numeric(0)
    }
    quality            <- rep(NA, length(names))
    for (i in seq_along(seg.list.level)) {
      if (i <= length(level.qual)) {
        quality[seg.list.level[[i]]] <- level.qual[i]
      }
    }
    # Share of mobility
    if (is.matrix(mat.level) && nrow(mat.level) > 0) {
      level.size        <- round(((rowSums(mat.level) + colSums(mat.level))/2)/sum(colSums(mat.level)),3)
    } else if (length(mat.level) == 1) {
      level.size <- 1  # Single segment has 100% of mobility
    } else {
      level.size <- numeric(0)
    }
    size               <- rep(NA, length(names))
    for (i in seq_along(seg.list.level)) {
      if (i <= length(level.size)) {
        size[seg.list.level[[i]]] <- level.size[i]
      }
    }
    # Density
    level.density     <- rep(NA, length(names))
    for (i in 1 : length(seg.list.level)) level.density[seg.list.level[[i]]] <- moneca_graph_density(net.edge - which(((1:vcount(net.edge) %in% seg.list.level[[i]]) == FALSE)))
    # Nodes
    nodes              <- rep(NA, length(names))
    for (i in 1 : length(seg.list.level)) nodes[seg.list.level[[i]]] <- length(seg.list.level[[i]])
    # Max path length
    max.path           <- rep(NA, length(names))
    for (i in 1 : length(seg.list.level)) max.path[seg.list.level[[i]]] <- diameter(net.edge - which(((1:vcount(net.edge) %in% seg.list.level[[i]]) == FALSE)), weights = NA)
    
    # Share of total size
    share.of.total     <- rep(NA, length(names))
    for (i in 1 : length(seg.list.level)) share.of.total[seg.list.level[[i]]] <- (totals.level / sum(totals.level))[i]
    
    out.frame             <- data.frame(Segment = seg, within.mobility = quality, share.of.mobility = size, Density = level.density, Nodes = nodes, Max.path = max.path, share.of.total = round(share.of.total, 3))
    colnames(out.frame)   <- paste(level, ": ", colnames(out.frame), sep = "") 
    out.frame
    
  }
  
  qual.list          <- lapply(1:number.of.levels, segment.qual.onelevel, segments = segments)
  out.mat            <- do.call(cbind , qual.list)
  out.mat            <- cbind(Membership = segment.membership(segments)[,2], out.mat)
  rownames(out.mat)  <- rownames(mat)
  order.mat          <- out.mat[, grep("share.of.total", colnames(out.mat))]
  order.mat          <- order.mat[, ncol(order.mat) : 1]
  out.mat            <- out.mat[do.call(order, -order.mat),]  
    
  if (final.solution == TRUE){
    small.mat       <- out.mat[duplicated(out.mat$Membership) == FALSE,]
    small.mat[sapply(small.mat, is.nan)] <- Inf
    tsm             <- as.matrix(small.mat)[, -1]
    collapse.mat    <- function(row, n) tail(na.omit(row), n)
    
    # Check if we have any rows
    if (nrow(tsm) > 0) {
      # Apply collapse.mat only if we have data
      if (ncol(tsm) >= 7) {
        tsm             <- as.data.frame(t(apply(tsm, 1, collapse.mat, n = 7)))
        colnames(tsm)   <- c("Membership", "Within mobility", "Share of mobility", "Density", "Nodes", "Max.path", "Share of total size")
      } else {
        # If we have fewer than 7 columns, just use what we have
        tsm <- as.data.frame(tsm)
      }
      tsm$Membership  <- small.mat$Membership
      out.mat         <- tsm
    }
  } 
  
  #colnames(out.mat) <- c("Membership", "Within mobility", "Share of mobility", "Density", "Nodes", "Max.path", "Share of total size")
  out.mat
  
  
}




#####################################################################
#### According to first level

#' First level summary
#' 
#' @export

first.level.summary <- function(segments, small.cell.reduction=segments$small.cell.reduction){

n.1.edges      <- vector(length=length(segments$segment.list))
sum.1          <- list()

for (i in seq(segments$segment.list)){
  # Antal 1.levels edges pr. level
  seg               <- segment.edges(segments, level=seq(segments$segment.list), segment.reduction=1:i, diagonal=NULL)
  n.1.edges[i]      <- sum(seg>0)
  sum.1[[i]]        <- summary(seg[seg>0])
}

n.1.edges # Number of 1. edges on each level
sum.1     # Summary of degrees on each level


###### Longest path and density
m        <- weight.matrix(segments$mat.list[[1]], cut.off=1, small.cell.reduction=small.cell.reduction, symmetric=FALSE)
m[is.na(m)] <- 0
net.path <- moneca_graph_from_adjacency(m, mode="directed", weighted=TRUE, diag=FALSE) # Her er diag = NULL erstattet med FALSE
sp       <- moneca_shortest_paths(net.path, weights=NA)

des <- list()
for(niv in 2:length(segments$segment.list)){
  max.path.global <- vector(length=length(segments$segment.list[[niv]]))
  clust         <- vector(length=length(segments$segment.list[[niv]]))
  max.clust     <- vector(length=length(segments$segment.list[[niv]]))
  density       <- vector(length=length(segments$segment.list[[niv]]))
  size          <- vector(length=length(segments$segment.list[[niv]]))
  seg.niv       <- segments$segment.list[[niv]]
  max.path      <- vector(length=length(segments$segment.list[[niv]]))
  av.path       <- vector(length=length(segments$segment.list[[niv]]))
  for (i in 1:length(seg.niv)){
    seg          <- seg.niv[[i]]
    max.path.global[i] <- max(sp[seg,seg])
    h            <- 1:vcount(net.path)
    density[i]   <- moneca_graph_density(net.path - h[-seg])
    clust[i]     <- max(moneca_components(net.path - h[-seg])$no)
    max.clust[i] <- max(moneca_components(net.path - h[-seg])$csize)
    size[i]      <- length(seg)
    max.path[i]  <- max(moneca_shortest_paths(net.path - h[-seg], weights=NA))
    av.path[i]   <- moneca_average_path_length(net.path - h[-seg])
    
  }
    des[[niv]]   <- cbind(size,max.path.global, max.path, density=round(density,3), clusters=clust, max.clust, av.path)
}

###################################################
### Amount of mobility left in 1. level edges on subsequent levels

mx                 <- segments$mat.list[[1]]
l                  <- ncol(mx)
mx.s               <- mx[-l,-l]
m                  <- weight.matrix(segments$mat.list[[1]], cut.off=1, small.cell.reduction=small.cell.reduction, symmetric=FALSE, diagonal=TRUE)
mx.share           <- mx.s/sum(mx.s)
mx.share[is.na(m)] <- 0


segment.list      <- segments$segment.list
share             <- vector(length=length(segments$segment.list))

for (niv in seq(segments$segment.list)){
seg.niv           <- segment.list[[niv]]
for (segment in 1:length(seg.niv)){
seg               <- seg.niv[[segment]]
mx.share[seg,seg] <- 0
}
share[niv]        <- sum(mx.share)
}
share

# output

out <- list(n.1.edges=n.1.edges, sum.1=sum.1, des=des, share=share)
class(out) <- "first_level_summary"
return(out)
}

#' Print first level summary
#' 
#' @export

print.first_level_summary <- function(x, ...){
  l.seq     <- 1:length(x$n.1.edges)
  n.1.edges <- paste("Level ",l.seq,": ", x$n.1.edges, sep="") 
  sum.1     <- x$sum.1
  des       <- x$des[-1]
  share     <- paste("Level ",l.seq,": ", round(x$share,3)*100, "%", sep="") 
  names(sum.1)   <- paste("Level", l.seq,": ", sep="")
  names(des)     <- paste("Level", l.seq[-1:-2],": ", sep="")
  
  cat("\n", "\n", "Descriptives of each level according to level 1:", "\n")
  cat("\n", "\n", "Amount of mobility left in 1. level edges on subsequent levels", "\n")
  print(noquote(format(share, width=15)))
  cat("\n", "\n", "Number of edges pr. level", "\n")
  print(noquote(format(n.1.edges, width=15)))
  
  cat("\n", "\n", "Summary of degrees on each level", "\n", "\n")
  print(print(sum.1))
  cat("\n", "Maximal paths and density for each segment on each level:", "\n", "\n")
  print(des)
}

