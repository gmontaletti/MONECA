#' MONECA - Mobility Network Clustering Analysis
#'
#' Main function for performing hierarchical clustering analysis on mobility tables.
#' MONECA creates weighted networks from mobility data and uses cliques to identify
#' discrete and nested clusters of positions with high internal mobility.
#'
#' This is the original antongrau/MONECA implementation with progress bar functionality. 
#' For memory-optimized version, use moneca_fast().
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
#' @param auto_tune Logical indicating whether to automatically tune the 
#'   small.cell.reduction parameter. When TRUE, uses optimization methods to
#'   select the best value. Default is FALSE.
#' @param tune_method Character string specifying the auto-tuning method when
#'   auto_tune is TRUE. Options are "stability" (default), "quality", or 
#'   "performance". See \code{\link{auto_tune_small_cell_reduction}} for details.
#' @param tune_verbose Logical indicating whether to print verbose messages
#'   during auto-tuning. Default is FALSE.
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
#' # Run moneca analysis
#' seg <- moneca(mobility_data, segment.levels = 3)
#' print(seg)
#' 
#' # Run moneca with auto-tuning for optimal small.cell.reduction
#' seg_tuned <- moneca(mobility_data, segment.levels = 3, 
#'                     auto_tune = TRUE, tune_method = "stability", 
#'                     tune_verbose = TRUE)
#' print(seg_tuned)
#' 
#' # Examine segment membership
#' membership <- segment.membership(seg)
#' print(membership)
#' 
#' @references
#' Toubøl, J., & Larsen, A. G. (2017). Mapping the Social Class Structure: 
#' From Occupational Mobility to Social Class Categories Using Network Analysis.
#' Sociology, 51(6), 1257-1276.
#' 
#' @seealso 
#' \code{\link{moneca_fast}} for the memory-optimized version,
#' \code{\link{find.segments}} for the core segmentation algorithm,
#' \code{\link{weight.matrix}} for relative risk calculation,
#' \code{\link{plot_moneca_ggraph}} for modern visualization,
#' \code{\link{segment.membership}} for extracting memberships
#' 
#' @export

moneca <- function(mx=mx, segment.levels=3, cut.off=1, mode="symmetric", delete.upper.tri=TRUE, small.cell.reduction=0, auto_tune = FALSE, tune_method = "stability", tune_verbose = FALSE){
  
  # Find segments based on a matrix
  # This is where find.segments options should be specified  
  make.segments   <- function(mx, cut.off=1, mode=mode, delete.upper.tri=delete.upper.tri, small.cell.reduction=small.cell.reduction){
    
    # Ensure mx is a matrix
    if (!is.matrix(mx)) {
      mx <- as.matrix(mx)
    }
    mx.1i           <- weight.matrix(mx, cut.off, small.cell.reduction=small.cell.reduction, 
                                     auto_tune=auto_tune, tune_method=tune_method, tune_verbose=tune_verbose)
    
    # Ensure matrix is symmetric for undirected graph
    # Replace NAs with 0 for graph creation
    mx.1i.graph     <- mx.1i
    mx.1i.graph[is.na(mx.1i.graph)] <- 0
    
    # Use modern igraph functions via compatibility layer
    gra.1ii         <- moneca_graph_from_adjacency(adjmatrix=mx.1i.graph, mode="undirected", weighted=TRUE, diag=FALSE)
    clique           <- moneca_cliques(gra.1ii)
    clust.1         <- find.segments(mx.1i, clique, cut.off=cut.off)
    
    return(clust.1)
  }
  
  segment.matrix  <- function(mx, segments){
    
    groups.1       <- c(segments$membership, length(segments$membership)+1)
    mx.2_r          <- rowsum(mx, groups.1)
    mx.2_r_t        <- t(mx.2_r)
    mx.2_rc_t       <- rowsum(mx.2_r_t, groups.1)
    mx.2g           <- t(mx.2_rc_t)
    return(mx.2g)
  }
  
  level.down <- function(level.current, level.below){
    # remove isolates
    a               <- unlist(lapply(level.current, length))
    level.current          <- level.current[a>1]
    
    ud <- list()
    
    # Return empty list if no valid segments
    if(length(level.current) == 0) {
      return(ud)
    }
    
    for(i in 1:length(level.current)){
      d                 <- level.current[[i]]
      ud[[i]]           <- unlist(level.below[d])
    }
    
    return(ud)
  }
  
  create.segments <- function(out.put, mx){
    
    seg.list        <- list()
    seg.list[[1]]   <- as.list(1:(nrow(mx)-1))
    
    level.current          <- out.put[[1]]$segments$cliques
    # remove isolates
    a               <- unlist(lapply(level.current, length))
    seg.list[[2]]   <- level.current[a>1]
    
    # Adjust for actual number of levels available
    actual.levels <- min(segment.levels, length(out.put))
    
    if (actual.levels > 1) {
      for (n in 2:actual.levels){
      
      current  <- n
      below <- n
      
      # Check if level exists in output
      if(current > length(out.put)) break
      
      level.current     <- out.put[[current]]$segments$cliques
      level.below    <- out.put[[n-1]]$segments$cliques
      
      for (i in 1:(n-1)){
        below <- below-1
        level.below <- out.put[[below]]$segments$cliques
        level.current  <- level.down(level.current, level.below)  
      }
      # Only add non-empty levels
      if (length(level.current) > 0) {
        seg.list[[n+1]] <- level.current
      }
    }
    }
    return(seg.list)
  }
  
  
  # Find segments
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
  
  
  # Create segments
  segment.list    <- create.segments(out.put, mx)
  
  # Create output
  
  out <- list(segment.list=segment.list, mat.list=mat.list, small.cell.reduction=small.cell.reduction)
  
  class(out) <- "moneca"
  
  return(out)
}