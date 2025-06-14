###################################

#' Find Segments in Mobility Networks
#' 
#' Identifies discrete groups or segments based on a weighted network matrix using a 
#' clique-based algorithm. This function implements the core segmentation algorithm
#' that iteratively assigns nodes to segments based on network ties.
#' 
#' @param mat A weighted adjacency matrix representing mobility flows or relationships.
#'   Should include row and column names representing the categories/classes.
#' @param cliques A list of cliques (complete subgraphs) in the network, typically
#'   obtained from \code{igraph::cliques()}.
#' @param cut.off Numeric threshold for minimum weight or relative risk to be 
#'   considered a network tie. Default is 1.
#' @param mode Character string specifying how to handle asymmetric relationships:
#'   \itemize{
#'     \item "symmetric" (default): Standard symmetric treatment
#'     \item "Mutual": Only mutual ties (bidirectional) are considered
#'     \item "Unmutual": Unidirectional ties are allowed
#'   }
#' @param delete.upper.tri Logical indicating whether to process only the lower 
#'   triangle of the matrix for efficiency. Default is TRUE.
#' 
#' @return A list with two components:
#'   \describe{
#'     \item{membership}{A factor indicating segment membership for each node}
#'     \item{cliques}{A list where each element contains the indices of nodes 
#'       belonging to that segment}
#'   }
#' 
#' @details
#' The algorithm works by iteratively examining the strongest remaining ties in the
#' network and assigning nodes to segments based on clique membership. It uses a
#' greedy approach that prioritizes stronger connections and larger existing segments.
#' 
#' @seealso \code{\link{moneca}} for the main analysis function
#' @export

find.segments <- function(mat, cliques, cut.off = 1, mode = "symmetric", delete.upper.tri = TRUE){
  
  ##################
  # Matrice modificering
  
  if(identical(mode, "Mutual")){
    mat[mat < cut.off]   <- NA
    mat                  <- mat + t(mat)
  }
  
  if(identical(mode, "Unmutual")){
    mat[mat < cut.off]  <- 0
    mat                 <- mat + t(mat)
    mat[mat==0]         <- NA
  }
  
  if(identical(delete.upper.tri, TRUE)) {
    mat[upper.tri(mat)] <- NA
  }
    
  # Her defineres output vectoren:
  gruppe               <- vector(mode="numeric",length=nrow(mat))
  names(gruppe)        <- rownames(mat)
  # Her laves den matrice der skal "slettes i"
  max.mat              <- mat
  
  ############################
  # Her defineres hjælpefunktionen
  bejler.test           <- function(cliques, potentiel.klike){
    klike.match         <- vector(length=length(cliques))
    for (j in 1:length(cliques)){
      kl                <- cliques[[j]]
      klike.match[[j]]  <- all(potentiel.klike %in% kl)
    }
    any(klike.match)
  }
  
  #############################################################
  # Progress bar
  loop.length           <-  sum(mat > cut.off, na.rm=TRUE)
  
  # Skip if no edges meet the cut-off criterion
  if (loop.length == 0) {
    # Return empty result
    out <- list()
    out$membership   <- as.factor(rep(1, nrow(mat)))
    out$cliques      <- list(1:nrow(mat))
    return(out)
  }
  
  pb                    <- txtProgressBar(min = 1, max = max(loop.length, 2), style=3)
  for (i in 1:loop.length){
    
    setTxtProgressBar(pb, i, label=paste(round(i/loop.length*100, 0), "% ready!"))
    ###########################################################
    
    max.ind             <- which(max.mat==max(max.mat, na.rm=TRUE), arr.ind=TRUE)[1,]
    #gruppe[max.ind]  <- i
    max.mat[max.ind[1], max.ind[2]] <- NA
    
    gruppe.tilbejles    <- gruppe[max.ind] # Hvilken gruppe bejler den til?
    bejler              <- max.ind 
    
    ### Find den tilbejlede gruppe
    
    if(sum(gruppe.tilbejles)==0){
      gruppe[max.ind] <- i  
    }
    if(sum(gruppe.tilbejles)!=0){
      
      gruppe.tilbejles <- gruppe.tilbejles[gruppe.tilbejles!=0] 
      gruppe.medlemmer <- which(gruppe %in% gruppe.tilbejles)   # Det her skal ske før vi ved med sikkerhed hvilken af grupperne der er den rigtige
      gruppe.stoerrelse <- table(gruppe[gruppe.medlemmer])       # Her findes den største af grupperne
      gruppe.tildeles  <- as.numeric(names(gruppe.stoerrelse))[which.max(gruppe.stoerrelse)[1]]        # Her skal den finde den største af grupperne - Den tager det første element - så hvis der er to der er lige store så vælger den "tilfældigt" den største - som også vil være den mest cohesive???
      
      #### Test cliques
      potentiel.klike  <- unique(sort(c(gruppe.medlemmer, bejler)))
      
      test <- bejler.test(cliques, potentiel.klike)              # Her tester vi om den potentielle cliques er en faktisk klike
      if (test==TRUE){
        gruppe[potentiel.klike] <- gruppe.tildeles             
      }
    }
  }
  sub <- gruppe[gruppe==0] 
  gruppe[gruppe==0] <- 1:length(sub) + max(gruppe)
  g <- as.factor(gruppe) 
  levels(g) <- 1:nlevels(g)
  
  # Her laver vi de nye cliques
  l        <- levels(g)
  ud.list  <- list()
  for ( i in 1:length(l)) ud.list[[i]] <- which(g == l[i])
  
  out <- list()
  out$membership   <- g
  out$cliques      <- ud.list
  return(out)
}

#' MONECA - Mobility Network Clustering Analysis
#'
#' Main function for performing hierarchical clustering analysis on mobility tables.
#' MONECA creates weighted networks from mobility data and uses cliques to identify
#' discrete and nested clusters of positions with high internal mobility.
#'
#' @param mx A mobility table (square matrix) with row and column totals in the last
#'   row/column. Row names should identify the categories/classes.
#' @param segment.levels Integer specifying the number of hierarchical segmentation 
#'   levels to compute. Default is 3. The algorithm may return fewer levels if no
#'   further meaningful segmentation is possible.
#' @param cut.off Numeric threshold for the minimum relative risk to be considered
#'   a significant tie. Default is 1 (no mobility above random expectation required).
#' @param mode Character string specifying edge mode ("symmetric", "Mutual", or 
#'   "Unmutual"). Currently not fully implemented - uses symmetric mode.
#' @param delete.upper.tri Logical indicating whether to use only lower triangle 
#'   for efficiency. Default is TRUE.
#' @param small.cell.reduction Numeric value to handle small cell counts. Cells with
#'   counts below this threshold are set to 0. Default is 0 (no reduction).
#' 
#' @return An object of class "moneca" containing:
#'   \describe{
#'     \item{segment.list}{A list of segment memberships for each hierarchical level.
#'       Each element is a list of vectors containing the original row indices.}
#'     \item{mat.list}{A list of aggregated mobility matrices for each level, where
#'       rows/columns represent segments instead of original categories.}
#'     \item{small.cell.reduction}{The small cell reduction parameter used.}
#'   }
#' 
#' @details
#' MONECA implements an iterative algorithm that:
#' \enumerate{
#'   \item Converts the mobility table to a relative risk matrix
#'   \item Identifies network cliques based on the threshold
#'   \item Groups nodes into segments using the clique structure
#'   \item Aggregates the mobility table by segments
#'   \item Repeats the process for the specified number of levels
#' }
#' 
#' The algorithm stops early if no further segmentation is possible (e.g., all
#' nodes collapse into a single segment).
#' 
#' @examples
#' # Generate synthetic mobility data
#' mobility_data <- generate_mobility_data(n_classes = 6, seed = 42)
#' 
#' # Run MONECA analysis
#' seg <- moneca(mobility_data, segment.levels = 3)
#' print(seg)
#' 
#' # Examine segment membership
#' membership <- segment.membership(seg)
#' print(membership)
#' 
#' # Visualize with modern plotting
#' \dontrun{
#' plot_moneca_ggraph(seg, node_color = "segment", title = "MONECA Clustering")
#' }
#' 
#' @references
#' Toubøl, J., & Larsen, A. G. (2017). Mapping the Social Class Structure: 
#' From Occupational Mobility to Social Class Categories Using Network Analysis.
#' Sociology, 51(6), 1257-1276.
#' 
#' @seealso 
#' \code{\link{find.segments}} for the core segmentation algorithm,
#' \code{\link{weight.matrix}} for relative risk calculation,
#' \code{\link{plot_moneca_ggraph}} for modern visualization,
#' \code{\link{segment.membership}} for extracting memberships
#' 
#' @export

moneca <- function(mx=mx, segment.levels=3, cut.off=1, mode="symmetric", delete.upper.tri=TRUE, small.cell.reduction=0){
  
  # Find segmentsne på baggrund af en matrice
  # Det er her find.segments options skal angives  
  make.segments   <- function(mx, cut.off=1, mode=mode, delete.upper.tri=delete.upper.tri, small.cell.reduction=small.cell.reduction){
    
#     l               <- nrow(mx)
#     mx.1_exp        <- as.array(mx[,l]) %*% t(as.array(mx[l,]) / mx[l,l])
#     mx.1_net        <- mx/mx.1_exp
#     mx.1            <- mx.1_net[-l,-l]
#     mx.1i           <- as.matrix(mx.1)
#     mx.1i[mx.1i < cut.off]  <- NA                # Her er cutoff pointet - det skal op i options senere
#     mx.1i           <- mx.1i + t(mx.1i)
#     diag(mx.1i)     <- NA

    # Ensure mx is a matrix
    if (!is.matrix(mx)) {
      mx <- as.matrix(mx)
    }
    mx.1i           <- weight.matrix(mx, cut.off, small.cell.reduction=small.cell.reduction)
    
    # Ensure matrix is symmetric for undirected graph
    # Replace NAs with 0 for graph creation
    mx.1i.graph     <- mx.1i
    mx.1i.graph[is.na(mx.1i.graph)] <- 0
    
    gra.1ii         <- moneca_graph_from_adjacency(adjmatrix=mx.1i.graph, mode="undirected", weighted=TRUE, diag=FALSE)
    klike           <- cliques(gra.1ii)
    clust.1         <- find.segments(mx.1i, klike, cut.off=cut.off)
    
    return(clust.1)
  }
  
  segment.matrix  <- function(mx, segments){
    
    grupper.1       <- c(segments$membership, length(segments$membership)+1)
    mx.2_r          <- rowsum(mx, grupper.1)
    mx.2_r_t        <- t(mx.2_r)
    mx.2_rc_t       <- rowsum(mx.2_r_t, grupper.1)
    mx.2g           <- t(mx.2_rc_t)
    return(mx.2g)
  }
  
  level.down <- function(niv.nu, niv.ned){
    # nak isolates
    a               <- unlist(lapply(niv.nu, length))
    niv.nu          <- niv.nu[a>1]
    
    ud <- list()
    
    # Return empty list if no valid segments
    if(length(niv.nu) == 0) {
      return(ud)
    }
    
    for(i in 1:length(niv.nu)){
      d                 <- niv.nu[[i]]
      ud[[i]]           <- unlist(niv.ned[d])
    }
    
    return(ud)
  }
  
  create.segments <- function(out.put, mx){
    
    seg.list        <- list()
    seg.list[[1]]   <- as.list(1:(nrow(mx)-1))
    
    niv.nu          <- out.put[[1]]$segments$cliques
    # nak isolates
    a               <- unlist(lapply(niv.nu, length))
    seg.list[[2]]   <- niv.nu[a>1]
    
    # Adjust for actual number of levels available
    actual.levels <- min(segment.levels, length(out.put))
    
    if (actual.levels > 1) {
      for (n in 2:actual.levels){
      
      nu  <- n
      ned <- n
      
      # Check if level exists in output
      if(nu > length(out.put)) break
      
      niv.nu     <- out.put[[nu]]$segments$cliques
      niv.ned    <- out.put[[n-1]]$segments$cliques
      
      for (i in 1:(n-1)){
        ned <- ned-1
        niv.ned <- out.put[[ned]]$segments$cliques
        niv.nu  <- level.down(niv.nu, niv.ned)  
      }
      # Only add non-empty levels
      if (length(niv.nu) > 0) {
        seg.list[[n+1]] <- niv.nu
      }
    }
    }
    return(seg.list)
  }
  
  
  # Her finder vi segmentsne
  mat.list        <- list()
  mat.list[[1]]   <- mx
  segments        <- make.segments(mx, cut.off=cut.off, mode=mode, delete.upper.tri=delete.upper.tri, small.cell.reduction=small.cell.reduction)
  mx.2g           <- segment.matrix(mx, segments)
  mat.list[[2]]   <- mx.2g
  out.put         <- list()
  out.put[[1]]    <- list(segments=segments, mat=mx.2g)
  
  if (segment.levels > 1) {
    for (i in 2:segment.levels){
      segments        <- make.segments(mx.2g, cut.off=cut.off, mode=mode, delete.upper.tri=delete.upper.tri, small.cell.reduction=small.cell.reduction)
      mx.2g           <- segment.matrix(mx.2g, segments)
      mat.list[[i+1]] <- mx.2g
      out.put[[i]]    <- list(segments=segments, mat=mx.2g)
      
      # Stop if only one segment remains
      if(length(segments$cliques) <= 1) {
        break
      }
    }
  }
  
  
  # Her laves segmentsne
  segment.list    <- create.segments(out.put, mx)
  
  # Her laves output
  
  out <- list(segment.list=segment.list, mat.list=mat.list, small.cell.reduction=small.cell.reduction)
  
  class(out) <- "moneca"
  
  return(out)
}

#' Calculate Relative Risk Weight Matrix
#' 
#' Converts a mobility table into a relative risk matrix by comparing observed
#' mobility flows to expected flows under independence. This matrix forms the
#' basis for network construction in MONECA analysis.
#' 
#' @param mx A mobility table (square matrix) with row and column totals in the 
#'   last row/column.
#' @param cut.off Numeric threshold for minimum relative risk. Values below this
#'   are set to NA. Default is 1.
#' @param symmetric Logical indicating whether to force the matrix to be symmetric
#'   by adding it to its transpose. Default is TRUE.
#' @param diagonal Controls diagonal values. If NULL (default), diagonal is set 
#'   to NA. Otherwise, diagonal values are preserved.
#' @param small.cell.reduction Numeric value for handling small cells. Cells with
#'   counts below this threshold are set to 0 before calculating relative risks.
#'   Default is 0.
#' 
#' @return A matrix of relative risks where:
#'   \itemize{
#'     \item Values > 1 indicate mobility above expected levels
#'     \item Values < 1 indicate mobility below expected levels  
#'     \item Values below cut.off are set to NA
#'   }
#' 
#' @details
#' The relative risk for cell (i,j) is calculated as:
#' \deqn{RR_{ij} = O_{ij} / E_{ij}}
#' where \eqn{O_{ij}} is the observed count and \eqn{E_{ij}} is the expected
#' count under independence: \eqn{E_{ij} = (n_i * n_j) / N}
#' 
#' @examples
#' # Create a simple mobility table
#' mob_table <- matrix(c(100, 20, 10, 130,
#'                       15, 80, 25, 120,  
#'                       5,  10, 50,  65,
#'                       120, 110, 85, 315), 
#'                     nrow = 4, byrow = TRUE)
#' rownames(mob_table) <- colnames(mob_table) <- c("A", "B", "C", "Total")
#' 
#' # Calculate relative risk matrix
#' rr_matrix <- weight.matrix(mob_table, cut.off = 1.5)
#' 
#' @seealso \code{\link{moneca}} for the main analysis function
#' @export

weight.matrix <- function(mx, cut.off = 1, symmetric = TRUE, diagonal = NULL, small.cell.reduction = 0){

  # Input validation and conversion
  if (is.null(mx)) {
    stop("Input cannot be NULL")
  }
  
  # Convert to matrix if needed
  if (!is.matrix(mx)) {
    # Try to convert data.frame, array, or other structures to matrix
    tryCatch({
      mx <- as.matrix(mx)
    }, error = function(e) {
      stop("Input must be convertible to a matrix: ", e$message)
    })
  }
  
  # Check if resulting matrix is valid
  if (!is.matrix(mx)) {
    stop("Input must be a matrix")
  }
  
  l               <- nrow(mx)
  
  # Check if matrix has at least 2 rows/columns
  if (l < 2) {
    stop("Matrix must have at least 2 rows and columns")
  }
  
  # Check if matrix is square
  if (nrow(mx) != ncol(mx)) {
    stop("Matrix must be square (same number of rows and columns)")
  }
  
  o.r.s           <- mx[-l, l]
  o.c.s           <- mx[l, -l]
  total.total     <- mx[l,l]
  row.share       <- o.r.s/total.total
  col.share       <- o.c.s/total.total
  total.mobility  <- sum(mx[-l,-l])
#   r.s             <- rowSums(mx[-l, -l])
#   c.s             <- colSums(mx[-l, -l])
#   mx.1_exp        <- o.r.s %*% t(o.c.s)/sum(o.r.s)    # Unweighted Relative Risks
  mx.1_exp        <- row.share %*% t(col.share)*total.mobility
  mx.red          <- mx[-l,-l]
  mx.red[mx.red < small.cell.reduction] <- 0
  mx.1_net        <- mx.red/mx.1_exp
  mx.1            <- mx.1_net
  mx.1i           <- as.matrix(mx.1)
  mx.1i[mx.1i < cut.off]  <- NA               
  if (identical(symmetric, TRUE))    mx.1i           <- mx.1i + t(mx.1i)
  if(is.null(diagonal)) diag(mx.1i)     <- NA
  
  return(mx.1i)  
}




#########################################################################
# Plotting

#' Generate Colors for Segments
#' 
#' Creates a grayscale color scheme for MONECA segments based on internal
#' mobility rates. Darker colors indicate higher immobility (lower internal mobility).
#' 
#' @param segments A MONECA object returned by \code{\link{moneca}}.
#' 
#' @return A list of color vectors, one for each hierarchical level.
#' 
#' @details
#' This function calculates grayscale colors where the intensity reflects
#' the immobility rate within each segment. Segments with higher immobility
#' (more stable positions) receive darker colors.
#' 
#' @export

segment.colors <- function(segments){
  n.segments <- length(segments$segment.list)
  
  
  segment.grey  <- function(segments, level){
    
    mat               <- segments$mat.list[[level]]
    l.seg             <- length(segments$segment.list[[level]])
    diag.mat          <- diag(mat)[-nrow(mat)]
    row.mat           <- mat[-nrow(mat), nrow(mat)]
    value             <- round((1-diag.mat/row.mat)*100)
    color             <- paste("grey", value, sep="")
    return(color)
  }
  
  colors <- list() 
  for (i in 1:n.segments) colors[[i]] <- segment.grey(segments, i)
  
  groups <- unlist(lapply(segments$segment.list, length))
  
  for ( i in 1:length(groups)) colors[[i]]  <-  colors[[i]][1:groups[i]]
  
  return(colors)
}


########################################################################
# 

#' Layout matrix
#' 
#' A matrix with the coordinates of the segments
#' @param segments a segment object
#' @param attraction the distance between the segment points for each level.
#' @param area.size the size of the plot area - see \link{layout.fruchterman.reingold}
#' @param level the included levels
#' @param mode the mode
#' @export
# old attraction attraction=c(200, 100, 15, 5, 3)
layout.matrix <- function(segments, attraction=c(320, 40, 10, 4, 2), level=seq(segments$segment.list), mode = "directed", weight.adjustment = 1, start.temp = 20, niter = 10000, tie.adjustment = 0.4, ...){
  
  seg              <- segments
  seg$segment.list <- segments$segment.list[level]
  seg$mat.list     <- segments$mat.list[level]
  
#   mx              <- segments$mat.list[[1]]
#   l               <- nrow(mx)
#   mx.1_exp        <- as.array(mx[,l]) %*% t(as.array(mx[l,]) / mx[l,l])
#   mx.1_net        <- mx/mx.1_exp
#   mx.1            <- mx.1_net[-l,-l]
#   mx.attract      <- mx.1
#   
  # Validate input matrix
  mx_input <- segments$mat.list[[1]]
  if (is.null(mx_input)) {
    stop("segments$mat.list[[1]] is NULL - the MONECA object appears to be incomplete or corrupted. Please re-run the moneca() function to generate a valid segments object.")
  }
  if (!is.matrix(mx_input)) {
    mx_input <- as.matrix(mx_input)
  }
  
  mx.attract      <- weight.matrix(mx_input, cut.off = 0, diagonal=TRUE, symmetric=FALSE)
  mx.attract      <- mx.attract ^ tie.adjustment
  
  gra.lay         <- moneca_graph_from_adjacency(mx.attract, mode="directed", weighted=TRUE)
  
  
  assign.attraction <- function(mx.attract, segment, attract){
    for (i in 1:length(segment)) mx.attract[segment[[i]],segment[[i]]] <- attract
    return(mx.attract)
  }
  
  for (i in length(seg$segment.list):2){
    segment       <- seg$segment.list[[i]]
    mx.attract    <- assign.attraction(mx.attract, segment, attraction[i-1])
  }
  
  diag(mx.attract) <- 0
  gra.lay          <- moneca_graph_from_adjacency(mx.attract, mode=mode, weighted=TRUE)

  # wm               <- weight.matrix(segments)
  # a                <- rowSums(wm)
  # b                <- colSums(wm)
  # start            <- cbind(max(a) - a, max(b)- b)
  # start            <- norm_coords(start, xmin = -100, xmax = 100, ymin = -100, ymax = 100)
  # 
  layout           <- layout_with_fr(gra.lay, weights=E(gra.lay)$weight*weight.adjustment, niter = niter, start.temp = start.temp, ...)
  layout[, 1:2]    <- moneca_norm_coords(layout[, 1:2], xmin = 1, xmax = 10^10, ymin = 1, ymax = 10^10)
  layout
}

#' Extract Segment Edge Matrix
#' 
#' Creates an adjacency matrix representing edges between segments based on
#' mobility flows. This function is used for network visualization and analysis.
#' 
#' @param segments A MONECA object returned by \code{\link{moneca}}.
#' @param cut.off Numeric threshold for minimum relative risk to include an edge.
#'   Default is 1.
#' @param mode Character string specifying the graph mode ("directed" or "undirected").
#'   Default is "directed".
#' @param level Integer vector specifying which hierarchical levels to include.
#'   Default includes all levels.
#' @param segment.reduction Integer vector specifying levels for which to remove
#'   internal segment edges. Default includes all levels.
#' @param method Character string specifying edge filtering method:
#'   \itemize{
#'     \item "all" (default): Include all edges above threshold
#'     \item "top.out": Keep only the top outgoing edges per node
#'     \item "top.in": Keep only the top incoming edges per node
#'   }
#' @param top Integer specifying how many top edges to keep when using 
#'   "top.out" or "top.in" methods. Default is 3.
#' @param diagonal Controls diagonal values. If NULL (default), diagonal is zeroed.
#' @param small.cell.reduction Numeric threshold for small cell handling.
#' 
#' @return A square matrix representing edge weights between nodes/segments.
#' 
#' @seealso \code{\link{plot_moneca_ggraph}}, \code{\link{moneca.plot}}
#' @export
segment.edges <- function(segments, cut.off=1, mode="directed", level=NULL, segment.reduction=NULL, method="all", top=3, diagonal=NULL, small.cell.reduction=0){
  
  # Always use the first matrix which contains the original data with totals
  mx                     <- segments$mat.list[[1]]
  
  # Validate that the matrix exists and has proper structure
  if (is.null(mx) || !is.matrix(mx)) {
    stop("segments$mat.list[[1]] must be a valid matrix")
  }
  
  # Set default values after validation
  if (is.null(level)) {
    level <- seq(segments$segment.list)
  }
  if (is.null(segment.reduction)) {
    segment.reduction <- seq(segments$segment.list)
  }
  
  seg                    <- segments
  seg$segment.list <- segments$segment.list[level]
  seg$mat.list     <- segments$mat.list[level]
  
#   
#   l               <- nrow(mx)
#   mx.1_exp        <- as.array(mx[,l]) %*% t(as.array(mx[l,]) / mx[l,l])
#   mx.1_net        <- mx/mx.1_exp
#   mx.1            <- mx.1_net[-l,-l]
#   mx.edges        <- mx.1
#   
#   # Cut reduction
#   mx.edges[mx.edges < cut] <- 0
#   
#   # Diagonal
#   diag(mx.edges)           <- 0
  mx.edges <- weight.matrix(mx, cut.off=cut.off, symmetric=FALSE, diagonal=diagonal, small.cell.reduction=small.cell.reduction) # Sæt tilbage
  mx.edges[is.na(mx.edges)] <- 0
    
  # Segment reduction
  if(identical(segment.reduction, 0)==FALSE){
  segments <- unlist(seg$segment.list[segment.reduction], recursive=FALSE)  
    for (i in 1:length(segments)){
      mx.edges[segments[[i]], segments[[i]]] <- 0
    }
  }
  
  if (identical(method, "top.out")){
    mx.sort <- matrix(nrow=top, ncol=ncol(mx.edges))
    mx.sort[1:top,] <- apply(mx.edges, 1, sort, decreasing=TRUE)[1:top,]
    
    for (i in 1:(nrow(mx.edges))){
      mx.edges[i,][(mx.edges[i,] %in% mx.sort[,i])==FALSE] <- 0
        }
  }
    
  if (identical(method, "top.in")){
    mx.sort <- matrix(nrow=top, ncol=ncol(mx.edges))
    mx.sort[1:top,] <- apply(mx.edges, 2, sort, decreasing=TRUE)[1:top,]
      
      for (i in 1:(nrow(mx.edges))){
        mx.edges[,i][(mx.edges[,i] %in% mx.sort[,i])==FALSE] <- 0
      }
      
  }

  
  
  
  # 80% reduction of percentage of all mobility
  # 5 strongest edges
  #gra.edges <- graph.adjacency(mx.edges, mode=mode, weighted=TRUE, diag=NULL)
  return(mx.edges)
}


#' Legacy Network Plot for MONECA Results
#' 
#' Creates a network visualization of MONECA segmentation results using base
#' graphics and igraph. For modern visualizations, use \code{\link{plot_moneca_ggraph}}.
#' 
#' @param segments A MONECA object returned by \code{\link{moneca}}.
#' @param layout A matrix of node coordinates, typically from \code{\link{layout.matrix}}.
#' @param edges An adjacency matrix of edges, typically from \code{\link{segment.edges}}.
#' @param mode Character string specifying graph mode ("directed" or "undirected").
#' @param level Integer vector of hierarchical levels to visualize.
#' @param vertex.size Numeric value for vertex size. Default is 5.
#' @param vertex.frame.color Color for vertex borders. Default is "black".
#' @param edge.curved Logical for curved edges. Default is FALSE.
#' @param vertex.color Color for vertices. Default is "grey50".
#' @param vertex.label.color Color for vertex labels. Default is "black".
#' @param vertex.label.cex Size multiplier for labels. Default is 0.5.
#' @param vertex.label.dist Distance of labels from vertices. Default is 0.12.
#' @param edge.arrow.size Size of edge arrows. Default is 0.1.
#' @param mark.col Color for segment markers. Default is NULL.
#' @param mark.expand Expansion factor for segment boundaries. Default is 10.
#' @param border.col Color for segment borders. Default is "black".
#' @param edge.width Width of edges. Default is 1.
#' @param edge.color Color for edges. Can be a color name or matrix. Default is "black".
#' 
#' @return NULL (creates a plot as side effect).
#' 
#' @details
#' This function provides backward compatibility with earlier versions of MONECA.
#' For new analyses, consider using \code{\link{plot_moneca_ggraph}} which offers
#' more modern styling and customization options.
#' 
#' @seealso \code{\link{plot_moneca_ggraph}} for modern plotting
#' @export

moneca.plot <- function(segments,
                       layout             = NULL,
                       edges              = NULL,
                       mode               = "directed",
                       level             = NULL,
                       vertex.size        = 5,
                       vertex.frame.color = "black",
                       edge.curved        = FALSE,
                       vertex.color       = "grey50",
                       vertex.label.color = "black",
                       vertex.label.cex   = 0.5,
                       vertex.label.dist  = 0.12,
                       edge.arrow.size = 0.1,
                       mark.col        = NULL, 
                       mark.expand     = 10, 
                       border.col      = "black",
                       edge.width      = 1,
                       edge.color      = "black"){
  
  # Validate segments object before proceeding
  if (is.null(segments)) {
    stop("segments object is NULL")
  }
  if (!inherits(segments, "moneca")) {
    stop("segments must be a moneca object created by the moneca() function")
  }
  if (is.null(segments$mat.list) || length(segments$mat.list) == 0) {
    stop("segments$mat.list is empty - the MONECA object appears to be incomplete. Please re-run the moneca() function.")
  }
  if (is.null(segments$mat.list[[1]])) {
    stop("segments$mat.list[[1]] is NULL - the MONECA object appears to be incomplete. Please re-run the moneca() function.")
  }
  
  # Set default level if not provided, after validation
  if (is.null(level)) {
    level <- seq(segments$segment.list)
  }
  
  # Calculate layout if not provided, after validation
  if (is.null(layout)) {
    layout <- layout.matrix(segments)
  }
  
  # Calculate edges first if not provided, before modifying segments
  if (is.null(edges)) {
    edges <- segment.edges(segments)
  }
  
  level                 <- level[level != 1]
  seg                    <- segments
  seg$segment.list       <- segments$segment.list[level]
  seg$mat.list           <- segments$mat.list[level]
  segments               <- unlist(seg$segment.list, recursive=FALSE)
  
  mat.edges              <- edges
  gra.edges              <- moneca_graph_from_adjacency(mat.edges, mode = mode, weighted = TRUE, diag = FALSE)                     
  el                     <- moneca_get_edgelist(gra.edges, names=FALSE)
  # Edge colors
  if(is.matrix(edge.color) == TRUE){
    mat.color            <- edge.color
    mat.color[mat.edges == 0] <- NA
    e.colors             <- vector(length=nrow(el))
    for (i in 1:nrow(el)){
      e.colors[i]        <- mat.color[el[i,1], el[i,2]]
    }
    gra.edges            <- set.edge.attribute(gra.edges, "color", index=E(gra.edges), e.colors)
  }
  
  plot(gra.edges, vertex.size=vertex.size, vertex.label=V(gra.edges)$name, vertex.frame.color=vertex.frame.color, layout=layout, edge.curved=edge.curved, vertex.color=vertex.color, vertex.label.color=vertex.label.color, vertex.label.family="sans", vertex.label.cex=vertex.label.cex, vertex.label.dist=vertex.label.dist, vertex.label.degree=pi/2, edge.arrow.size=edge.arrow.size, mark.groups=segments, mark.border=border.col, mark.col=NULL, mark.expand=10, edge.width=1, edge.color=edge.color)
 }


###############################################################################
#### Segment membership

#' Extract Segment Membership Information
#' 
#' Returns a data frame showing which segment each original category belongs to
#' across the specified hierarchical levels of a MONECA analysis.
#' 
#' @param segments A MONECA object returned by \code{\link{moneca}}.
#' @param level Integer vector specifying which hierarchical levels to include.
#'   Default includes all available levels.
#' 
#' @return A data frame with two columns:
#'   \describe{
#'     \item{name}{Character vector of original category names}
#'     \item{membership}{Character vector indicating segment membership, formatted
#'       as "level.segment" (e.g., "2.1" for level 2, segment 1)}
#'   }
#' 
#' @details
#' The membership strings indicate both the hierarchical level and the specific
#' segment within that level. For example, "2.3" means the category belongs to
#' segment 3 at level 2 of the hierarchy.
#' 
#' @examples
#' # Generate data and run analysis
#' mob_data <- generate_mobility_data(n_classes = 5, seed = 42)
#' seg <- moneca(mob_data, segment.levels = 3)
#' 
#' # Get membership information
#' membership <- segment.membership(seg)
#' print(membership)
#' 
#' # Get membership for specific levels only
#' membership_level2 <- segment.membership(seg, level = 2)
#' 
#' @seealso \code{\link{moneca}}, \code{\link{plot_moneca_ggraph}}
#' @export

segment.membership <- function(segments, level=seq(segments$segment.list)){

org.name <- rownames(segments$mat.list[[1]])
org.name <- org.name[-length(org.name)]

position <- vector(length=length(org.name))

# Ensure we only process levels that actually exist
actual.levels <- intersect(level, seq_along(segments$segment.list))

for (niv in actual.levels){
seg.niv <- segments$segment.list[[niv]]
# Skip if this level is empty
if (length(seg.niv) > 0) {
  for (i in 1:length(seg.niv)){
    position[seg.niv[[i]]] <- rep(paste(niv, i, sep="."), length(seg.niv[[i]]))
  }
}
}

out.mat <- data.frame(name=org.name, membership=position)
out.mat
}  
#   out <- list()
#   
#   for (i in level){
#   niv     <- level[i]
#   seg     <- segments$segment.list[[niv]]
#   node    <- unlist(seg)
#   l       <- 1:length(seg)
# 
#   member <- vector()  
#   for( u in l){
#   member  <- c(member,rep(u, length(seg[[u]])))
#  }
# 
#   out[[i]]  <- data.frame(node,member)
#  }
# 
#   return(out)
# }
#   

#' force.segments
#' 
#' Create a two-level segment-object with a forced solutionz
#' 
#' @export

force.segments <-function(segments, variable){

new.seg                       <- list()
new.seg$segment.list          <- list()
new.seg$mat.list              <- list()
new.seg$small.cell.reduction  <- segments$small.cell.reduction

# Første level

new.seg$segment.list[[1]]  <- segments$segment.list[[1]]
new.seg$mat.list[[1]]      <- segments$mat.list[[1]]
mxa                        <- segments$mat.list[[1]]

# Andet level
out.list <- list()
for(i in 1:nlevels(as.factor(variable))){
  var <- as.factor(variable)
  levs <- levels(var)
  out.list[[i]]<- which(var == levs[i])  
}

new.seg$segment.list[[2]] <- out.list

grupper.1       <- c(variable, length(out.list)+1)
mx.2_r          <- rowsum(mxa, grupper.1)
mx.2_r_t        <- t(mx.2_r)
mx.2_rc_t       <- rowsum(mx.2_r_t, grupper.1)
mx.2g           <- t(mx.2_rc_t)

new.seg$mat.list[[2]] <- mx.2g

return(new.seg)
}


