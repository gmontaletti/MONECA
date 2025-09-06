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
  # Matrix modification
  
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
    
  # Define output vector
  group               <- vector(mode="numeric",length=nrow(mat))
  names(group)        <- rownames(mat)
  max.mat             <- mat
  
  # Simple clique test function (original bejler.test approach)
  clique.test <- function(potential.clique){
    if (length(potential.clique) < 2) return(TRUE)  # Single nodes are trivially cliques
    
    # Test if potential.clique is subset of any clique
    for (cl in cliques) {
      if (all(potential.clique %in% cl)) {
        return(TRUE)
      }
    }
    return(FALSE)
  }
  
  # Progress bar
  loop.length <- sum(mat > cut.off, na.rm=TRUE)
  
  # Skip if no edges meet the cut-off criterion
  if (loop.length == 0) {
    out <- list()
    out$membership   <- as.factor(rep(1, nrow(mat)))
    out$cliques      <- list(1:nrow(mat))
    return(out)
  }
  
  pb <- txtProgressBar(min = 1, max = max(loop.length, 2), style=3)
  
  # Track milestones for percentage messages
  milestones <- c(25, 50, 75, 100)
  milestone_reached <- rep(FALSE, length(milestones))
  
  for (i in 1:loop.length){
    
    # Update progress bar
    percent_complete <- round(i/loop.length*100, 0)
    setTxtProgressBar(pb, i)
    
    # Print percentage message at key milestones
    for (j in seq_along(milestones)) {
      if (percent_complete >= milestones[j] && !milestone_reached[j]) {
        cat(paste("\n", percent_complete, "% ready!\n", sep=""))
        milestone_reached[j] <- TRUE
        break  # Only print one message per iteration
      }
    }
    
    # Find maximum value and set to NA
    max.ind             <- which(max.mat==max(max.mat, na.rm=TRUE), arr.ind=TRUE)[1,]
    max.mat[max.ind[1], max.ind[2]] <- NA
    
    group.candidates    <- group[max.ind] # Which group is it seeking to join?
    candidate              <- max.ind 
    
    ### Find the target group
    
    if(sum(group.candidates)==0){
      group[max.ind] <- i  
    }
    if(sum(group.candidates)!=0){
      
      group.candidates <- group.candidates[group.candidates!=0] 
      group.members <- which(group %in% group.candidates)   
      group.size <- table(group[group.members])       
      group.assigned  <- as.numeric(names(group.size))[which.max(group.size)[1]]        
      
      #### Test cliques with simple function
      potential.clique  <- unique(sort(c(group.members, candidate)))
      
      test <- clique.test(potential.clique)
      if (test==TRUE){
        group[potential.clique] <- group.assigned             
      }
    }
  }
  
  # Clean up progress bar
  close(pb)
  
  sub <- group[group==0] 
  group[group==0] <- 1:length(sub) + max(group)
  g <- as.factor(group) 
  levels(g) <- 1:nlevels(g)
  
  # Create new cliques
  l        <- levels(g)
  ud.list  <- list()
  for ( i in 1:length(l)) ud.list[[i]] <- which(g == l[i])
  
  out <- list()
  out$membership   <- g
  out$cliques      <- ud.list
  return(out)
}

#' MONECA Fast - Memory-Optimized Mobility Network Clustering Analysis
#'
#' Memory-optimized version of the main function for performing hierarchical clustering 
#' analysis on mobility tables. MONECA creates weighted networks from mobility data and 
#' uses cliques to identify discrete and nested clusters of positions with high internal mobility.
#' 
#' This version includes intelligent memory management for dense graphs and optimized
#' clique enumeration to prevent memory issues with large datasets.
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
#' **Algorithm Features:**
#' \itemize{
#'   \item Simple and memory-efficient clique testing using direct subset checking
#'   \item Intelligent dense graph detection with fallback to maximal cliques
#'   \item Streamlined implementation based on the original bejler.test approach
#'   \item Maintains algorithmic correctness while minimizing memory overhead
#' }
#' 
#' @examples
#' # Generate synthetic mobility data
#' mobility_data <- generate_mobility_data(n_classes = 6, seed = 42)
#' 
#' # Run moneca analysis (fast version)
#' seg <- moneca_fast(mobility_data, segment.levels = 3)
#' print(seg)
#' 
#' # Run with auto-tuning for optimal parameters
#' seg_tuned <- moneca_fast(mobility_data, segment.levels = 3, 
#'                          auto_tune = TRUE, tune_method = "stability")
#' print(seg_tuned)
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
#' \code{\link{moneca}} for the standard version with progress bar,
#' \code{\link{find.segments}} for the core segmentation algorithm,
#' \code{\link{weight.matrix}} for relative risk calculation,
#' \code{\link{plot_moneca_ggraph}} for modern visualization,
#' \code{\link{segment.membership}} for extracting memberships
#' 
#' @export

moneca_fast <- function(mx=mx, segment.levels=3, cut.off=1, mode="symmetric", delete.upper.tri=TRUE, small.cell.reduction=0, auto_tune = FALSE, tune_method = "stability", tune_verbose = FALSE){
  
  # Input validation
  if (is.null(mx)) {
    stop("Input matrix cannot be NULL")
  }
  
  if (!is.matrix(mx)) {
    mx <- as.matrix(mx)
  }
  
  # Find segments based on a matrix
  make.segments   <- function(mx, cut.off=1, mode=mode, delete.upper.tri=delete.upper.tri, small.cell.reduction=small.cell.reduction){
    
    # Ensure mx is a matrix
    if (!is.matrix(mx)) {
      mx <- as.matrix(mx)
    }
    mx.1i           <- weight.matrix(mx, cut.off, small.cell.reduction=small.cell.reduction, 
                                     auto_tune=auto_tune, tune_method=tune_method, tune_verbose=tune_verbose)
    
    # Check for empty weight matrix early
    if (all(is.na(mx.1i))) {
      # Return trivial segmentation
      n_nodes <- nrow(mx.1i)
      return(list(
        membership = as.factor(rep(1, n_nodes)),
        cliques = list(1:n_nodes)
      ))
    }
    
    # Ensure matrix is symmetric for undirected graph
    mx.1i.graph     <- mx.1i
    mx.1i.graph[is.na(mx.1i.graph)] <- 0
    
    # Intelligent clique enumeration for dense graphs (simplified)
    non_zero_edges <- sum(mx.1i.graph > 0)
    max_nodes <- nrow(mx.1i.graph)
    edge_density <- non_zero_edges / (max_nodes * (max_nodes - 1) / 2)
    
    if (edge_density > 0.7 && max_nodes > 20) {
      warning("Dense graph detected. Using max cliques instead of all cliques.")
      gra.1ii         <- moneca_graph_from_adjacency(adjmatrix=mx.1i.graph, mode="undirected", weighted=TRUE, diag=FALSE)
      clique          <- moneca_max_cliques(gra.1ii)  # Use max cliques instead of all cliques
    } else {
      gra.1ii         <- moneca_graph_from_adjacency(adjmatrix=mx.1i.graph, mode="undirected", weighted=TRUE, diag=FALSE)
      clique          <- moneca_cliques(gra.1ii)
    }
    
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
  
  
  # MEMORY OPTIMIZATION: Find segments with better memory management
  mat.list        <- list()
  mat.list[[1]]   <- mx
  
  # First level segmentation
  segments        <- make.segments(mx, cut.off=cut.off, mode=mode, delete.upper.tri=delete.upper.tri, small.cell.reduction=small.cell.reduction)
  mx.2g           <- segment.matrix(mx, segments)
  mat.list[[2]]   <- mx.2g
  out.put         <- list()
  out.put[[1]]    <- list(segments=segments, mat=mx.2g)
  
  # MEMORY OPTIMIZATION: Process remaining levels with memory cleanup
  if (segment.levels > 1) {
    current_matrix <- mx.2g
    
    for (i in 2:segment.levels){
      # MEMORY OPTIMIZATION: Force garbage collection before each iteration
      gc(verbose = FALSE)
      
      segments        <- make.segments(current_matrix, cut.off=cut.off, mode=mode, delete.upper.tri=delete.upper.tri, small.cell.reduction=small.cell.reduction)
      
      # Stop early if no meaningful segmentation is possible
      if(length(segments$cliques) <= 1) {
        break
      }
      
      # MEMORY OPTIMIZATION: Replace current matrix instead of keeping all versions
      new_matrix      <- segment.matrix(current_matrix, segments)
      
      # Only keep the matrix if we're not at the last level or if it will be used
      mat.list[[i+1]] <- new_matrix
      out.put[[i]]    <- list(segments=segments, mat=new_matrix)
      
      # MEMORY OPTIMIZATION: Update current matrix for next iteration
      current_matrix  <- new_matrix
      
      # Additional memory pressure check
      if (i > 5 && object.size(out.put) > 100 * 1024 * 1024) { # 100MB threshold
        warning("Memory usage is high. Stopping early to prevent system instability.")
        break
      }
    }
    
    # MEMORY OPTIMIZATION: Clear temporary objects
    if (exists("current_matrix")) rm(current_matrix)
    if (exists("new_matrix")) rm(new_matrix)
    gc(verbose = FALSE)
  }
  
  
  # MEMORY OPTIMIZATION: Create segments with final cleanup
  segment.list    <- create.segments(out.put, mx)
  
  # MEMORY OPTIMIZATION: Clear intermediate objects before creating final output
  rm(out.put)
  gc(verbose = FALSE)
  
  # Create output
  out <- list(segment.list=segment.list, mat.list=mat.list, small.cell.reduction=small.cell.reduction)
  
  class(out) <- "moneca"
  
  # MEMORY OPTIMIZATION: Final cleanup
  gc(verbose = FALSE)
  
  return(out)
}

#' Calculate Relative Risk Weight Matrix
#' 
#' Converts a mobility table into a relative risk matrix by comparing observed
#' mobility flows to expected flows under independence. This matrix forms the
#' basis for network construction in moneca analysis.
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
#' @param auto_tune Logical indicating whether to automatically tune the 
#'   small.cell.reduction parameter. When TRUE, uses optimization methods to
#'   select the best value. Default is FALSE.
#' @param tune_method Character string specifying the auto-tuning method when
#'   auto_tune is TRUE. Options are "stability" (default), "quality", or 
#'   "performance". See \code{\link{auto_tune_small_cell_reduction}} for details.
#' @param tune_verbose Logical indicating whether to print verbose messages
#'   during auto-tuning. Default is FALSE.
#' 
#' @return A matrix of relative risks where:
#'   \itemize{
#'     \item Values > 1 indicate mobility above expected levels
#'     \item Values < 1 indicate mobility below expected levels  
#'     \item Values below cut.off are set to NA
#'   }
#'   When auto_tune is TRUE, the matrix includes additional attributes:
#'   \itemize{
#'     \item \code{auto_tuning}: Complete results from auto-tuning process
#'     \item \code{auto_tuned}: Logical flag indicating auto-tuning was used
#'     \item \code{tuned_small_cell_reduction}: The selected optimal value
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
#' # Use automatic tuning for small.cell.reduction parameter
#' rr_matrix_tuned <- weight.matrix(mob_table, auto_tune = TRUE, 
#'                                  tune_method = "stability", tune_verbose = TRUE)
#' 
#' # Check tuning results
#' if (attr(rr_matrix_tuned, "auto_tuned", exact = TRUE)) {
#'   optimal_value <- attr(rr_matrix_tuned, "tuned_small_cell_reduction", exact = TRUE)
#'   message("Optimal small.cell.reduction: ", optimal_value)
#' }
#' 
#' @seealso \code{\link{moneca}} for the main analysis function, 
#'   \code{\link{auto_tune_small_cell_reduction}} for auto-tuning details
#' @export

weight.matrix <- function(mx, cut.off = 1, symmetric = TRUE, diagonal = NULL, 
                          small.cell.reduction = 0, auto_tune = FALSE, 
                          tune_method = "stability", tune_verbose = FALSE){

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
  
  # Auto-tune small.cell.reduction parameter if requested
  tuning_info <- NULL
  if (auto_tune) {
    if (tune_verbose) {
      message("Auto-tuning small.cell.reduction parameter using method: ", tune_method)
    }
    
    # Call auto-tuning function
    tune_result <- auto_tune_small_cell_reduction(
      mx = mx,
      cut.off = cut.off,
      method = tune_method,
      verbose = tune_verbose
    )
    
    # Use the optimal parameter
    small.cell.reduction <- tune_result$optimal_value
    tuning_info <- tune_result
    
    if (tune_verbose) {
      message("Selected optimal small.cell.reduction: ", small.cell.reduction)
    }
  }
  
  # MEMORY OPTIMIZATION: Calculate components efficiently with minimal copying
  o.r.s           <- mx[-l, l]
  o.c.s           <- mx[l, -l]
  total.total     <- mx[l,l]
  total.mobility  <- sum(mx[-l,-l])
  
  # MEMORY OPTIMIZATION: Calculate expected matrix in-place
  mx.1_exp        <- (o.r.s/total.total) %*% t(o.c.s/total.total) * total.mobility
  
  # MEMORY OPTIMIZATION: Work directly with the sub-matrix
  mx.1i           <- mx[-l,-l]
  
  # Apply small cell reduction in-place
  if (small.cell.reduction > 0) {
    mx.1i[mx.1i < small.cell.reduction] <- 0
  }
  
  # Calculate relative risks in-place
  mx.1i           <- mx.1i / mx.1_exp
  
  # MEMORY OPTIMIZATION: Clear temporary objects
  rm(mx.1_exp, o.r.s, o.c.s)
  gc(verbose = FALSE)
  
  # Apply symmetric transformation if needed
  if (identical(symmetric, TRUE)) {
    mx.1i <- mx.1i + t(mx.1i)
  }
  
  # Apply cut-off and diagonal settings
  mx.1i[mx.1i < cut.off] <- NA               
  if(is.null(diagonal)) diag(mx.1i) <- NA
  
  # Add tuning information as attributes if available
  if (!is.null(tuning_info)) {
    attr(mx.1i, "auto_tuning") <- tuning_info
    attr(mx.1i, "auto_tuned") <- TRUE
    attr(mx.1i, "tuned_small_cell_reduction") <- small.cell.reduction
  }
  
  return(mx.1i)  
}




#########################################################################
# Plotting

#' Generate Colors for Segments
#' 
#' Creates a grayscale color scheme for MONECA segments based on internal
#' mobility rates. Darker colors indicate higher immobility (lower internal mobility).
#' 
#' @param segments A moneca object returned by \code{\link{moneca}}.
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
    stop("segments$mat.list[[1]] is NULL - the moneca object appears to be incomplete or corrupted. Please re-run the moneca() function to generate a valid segments object.")
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
  
  # Validate layout coordinates before normalization
  if (any(is.na(layout[, 1:2])) || any(is.infinite(layout[, 1:2]))) {
    warning("Layout contains NA or infinite coordinates, using fallback layout")
    # Create simple circular layout as fallback
    n_nodes <- nrow(layout)
    angles <- seq(0, 2*pi, length.out = n_nodes + 1)[1:n_nodes]
    layout[, 1] <- cos(angles)
    layout[, 2] <- sin(angles)
  }
  
  # Check if all coordinates are the same (which can cause viewport issues)
  if (length(unique(layout[, 1])) == 1 || length(unique(layout[, 2])) == 1) {
    warning("Layout coordinates are degenerate, adding jitter")
    layout[, 1] <- layout[, 1] + runif(nrow(layout), -0.1, 0.1)
    layout[, 2] <- layout[, 2] + runif(nrow(layout), -0.1, 0.1)
  }
  
  # Normalize coordinates with more reasonable bounds
  layout[, 1:2]    <- moneca_norm_coords(layout[, 1:2], xmin = 0, xmax = 100, ymin = 0, ymax = 100)
  
  # Final validation
  if (any(is.na(layout[, 1:2])) || any(is.infinite(layout[, 1:2]))) {
    stop("Layout normalization failed - please check input data")
  }
  
  layout
}

#' Extract Segment Edge Matrix
#' 
#' Creates an adjacency matrix representing edges between segments based on
#' mobility flows. This function is used for network visualization and analysis.
#' 
#' @param segments A moneca object returned by \code{\link{moneca}}.
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
#' @param segments A moneca object returned by \code{\link{moneca}}.
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
    stop("segments$mat.list is empty - the moneca object appears to be incomplete. Please re-run the moneca() function.")
  }
  if (is.null(segments$mat.list[[1]])) {
    stop("segments$mat.list[[1]] is NULL - the moneca object appears to be incomplete. Please re-run the moneca() function.")
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
#' across the specified hierarchical levels of a moneca analysis.
#' 
#' @param segments A moneca object returned by \code{\link{moneca}}.
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

#' Enhanced Segment Membership with Meaningful Names
#' 
#' Returns segment membership information with an additional column containing 
#' meaningful segment names derived from the constituent nodes. This function 
#' extends \code{\link{segment.membership}} by adding intelligent naming 
#' strategies for aggregated segments.
#' 
#' @param segments A moneca object returned by \code{\link{moneca}}.
#' @param level Integer vector specifying which hierarchical levels to include.
#'   Default includes all available levels.
#' @param naming_strategy Character string specifying the naming approach:
#'   \itemize{
#'     \item "auto" (default): Automatic naming based on segment composition
#'     \item "concat": Concatenate node names with separator
#'     \item "pattern": Use pattern recognition for common suffixes/prefixes
#'     \item "custom": Use user-provided names from custom_names parameter
#'   }
#' @param custom_names Named list providing custom names for specific segments.
#'   Format: list("level.segment" = "Custom Name"), e.g., list("2.1" = "Blue Collar")
#' @param separator Character string used to separate names when concatenating.
#'   Default is " + ".
#' @param max_concat_length Maximum number of names to concatenate before 
#'   switching to pattern-based naming. Default is 2.
#' @param pattern_rules Enhanced pattern rules list with priorities and semantic
#'   categories. Default includes comprehensive occupational and industry patterns
#'   (avoiding generic social class terms).
#' 
#' @return A data frame with three columns:
#'   \describe{
#'     \item{name}{Character vector of original category names}
#'     \item{membership}{Character vector indicating segment membership, formatted
#'       as "level.segment" (e.g., "2.1" for level 2, segment 1)}
#'     \item{segment_label}{Character vector with meaningful segment names derived
#'       from constituent nodes or user-provided names}
#'   }
#' 
#' @details
#' The function applies different naming strategies based on segment composition:
#' \enumerate{
#'   \item \strong{Individual nodes}: Keep original name unchanged
#'   \item \strong{Small segments} (<= max_concat_length): Concatenate names
#'   \item \strong{Large segments}: Apply pattern recognition or use generic names
#'   \item \strong{Custom names}: Override automatic naming with user-provided names
#' }
#' 
#' The enhanced pattern recognition system uses weighted scoring to identify
#' occupational hierarchies, industry sectors, and employment types. It creates
#' semantically meaningful names like "Executive_Leadership", "Professional_Services", 
#' or "Technical_Specialists" based on segment composition, avoiding generic terms.
#' 
#' @examples
#' # Generate data and run analysis
#' mob_data <- generate_mobility_data(n_classes = 6, seed = 42,
#'   class_names = c("Upper_Class", "Professional", "Manager", 
#'                   "Skilled_Worker", "Service_Worker", "Manual_Worker"))
#' seg <- moneca(mob_data, segment.levels = 3)
#' 
#' # Get enhanced membership with automatic naming
#' enhanced <- segment.membership.enhanced(seg)
#' print(enhanced)
#' 
#' # Use concatenation strategy
#' concat_membership <- segment.membership.enhanced(seg, 
#'                                                 naming_strategy = "concat")
#' 
#' # Use custom names for specific segments
#' custom_names <- list("2.1" = "Blue_Collar", "3.1" = "All_Classes")
#' custom_membership <- segment.membership.enhanced(seg, 
#'                                                 naming_strategy = "custom",
#'                                                 custom_names = custom_names)
#' 
#' @seealso 
#' \code{\link{segment.membership}} for basic membership information,
#' \code{\link{moneca}} for the main analysis function
#' 
#' @export

segment.membership.enhanced <- function(segments, 
                                      level = seq(segments$segment.list),
                                      naming_strategy = "auto",
                                      custom_names = NULL,
                                      separator = " + ",
                                      max_concat_length = 2,
                                      pattern_rules = NULL) {
  
  # Handle NULL naming_strategy
  if (is.null(naming_strategy) || is.na(naming_strategy)) {
    naming_strategy <- "auto"
  }
  
  # Validate inputs
  if (is.null(segments)) {
    stop("segments object is NULL")
  }
  if (!inherits(segments, "moneca")) {
    stop("segments must be a moneca object created by the moneca() function")
  }
  if (is.null(segments$mat.list) || length(segments$mat.list) == 0) {
    stop("segments$mat.list is empty - the moneca object appears to be incomplete. Please re-run the moneca() function.")
  }
  if (is.null(segments$mat.list[[1]])) {
    stop("segments$mat.list[[1]] is NULL - the moneca object appears to be incomplete. Please re-run the moneca() function.")
  }
  
  # Get basic membership information
  basic_membership <- segment.membership(segments, level = level)
  
  # Get original node names
  org.names <- rownames(segments$mat.list[[1]])[-nrow(segments$mat.list[[1]])]
  
  # Set default enhanced pattern rules if not provided
  if (is.null(pattern_rules)) {
    pattern_rules <- create_enhanced_pattern_rules()
  }
  
  # Initialize level_name column
  level_names <- character(nrow(basic_membership))
  
  # Process each unique membership
  unique_memberships <- unique(basic_membership$membership)
  # Filter out empty strings and invalid memberships
  unique_memberships <- unique_memberships[unique_memberships != "" & 
                                         unique_memberships != "FALSE" & 
                                         !is.na(unique_memberships)]
  
  for (membership_id in unique_memberships) {
    # Find nodes with this membership
    node_indices <- which(basic_membership$membership == membership_id)
    node_names <- basic_membership$name[node_indices]
    
    # Generate level name based on strategy
    level_name <- generate_level_name(
      node_names = node_names,
      membership_id = membership_id,
      naming_strategy = naming_strategy,
      custom_names = custom_names,
      separator = separator,
      max_concat_length = max_concat_length,
      pattern_rules = pattern_rules
    )
    
    # Assign level name to all nodes with this membership
    level_names[node_indices] <- level_name
  }
  
  # Handle nodes without membership (empty string, FALSE, or NA)
  invalid_membership_indices <- which(basic_membership$membership == "" | 
                                     basic_membership$membership == "FALSE" | 
                                     is.na(basic_membership$membership))
  if (length(invalid_membership_indices) > 0) {
    level_names[invalid_membership_indices] <- basic_membership$name[invalid_membership_indices]
  }
  
  # Create enhanced membership data frame
  enhanced_membership <- data.frame(
    name = basic_membership$name,
    membership = basic_membership$membership,
    segment_label = level_names,
    stringsAsFactors = FALSE
  )
  
  return(enhanced_membership)
}

#' Generate Level Name Based on Strategy
#' 
#' Internal helper function to generate meaningful level names for segments.
#' 
#' @param node_names Character vector of node names in the segment
#' @param membership_id Character string of the membership ID (e.g., "2.1")
#' @param naming_strategy Character string specifying naming approach
#' @param custom_names Named list of custom names
#' @param separator Character string for concatenation
#' @param max_concat_length Maximum nodes to concatenate
#' @param pattern_rules Named list of pattern replacement rules
#' 
#' @return Character string with the generated level name
#' @keywords internal

generate_level_name <- function(node_names, membership_id, naming_strategy, 
                               custom_names, separator, max_concat_length, 
                               pattern_rules) {
  
  # Robust handling of naming_strategy parameter
  # Convert to character and handle all edge cases
  if (is.null(naming_strategy) || length(naming_strategy) == 0) {
    naming_strategy <- "auto"
  } else {
    # Try to convert to character
    naming_strategy <- as.character(naming_strategy)
    
    # Check if conversion resulted in NA or empty
    if (length(naming_strategy) == 0 || is.na(naming_strategy[1])) {
      naming_strategy <- "auto"
    } else {
      # Take first element if vector
      naming_strategy <- naming_strategy[1]
      
      # Final check for NA after subsetting
      if (is.na(naming_strategy)) {
        naming_strategy <- "auto"
      }
    }
  }
  
  # Strategy 1: Custom names override everything
  if (naming_strategy == "custom" && !is.null(custom_names) && 
      membership_id %in% names(custom_names)) {
    return(custom_names[[membership_id]])
  }
  
  # Strategy 2: Single node - keep original name
  if (length(node_names) == 1) {
    return(node_names[1])
  }
  
  # Strategy 3: Concatenation for small groups
  if (naming_strategy == "concat" || 
      (naming_strategy == "auto" && length(node_names) <= max_concat_length)) {
    return(paste(node_names, collapse = separator))
  }
  
  # Strategy 4: Enhanced pattern recognition
  if (naming_strategy == "pattern" || naming_strategy == "auto") {
    pattern_name <- apply_enhanced_pattern_rules(node_names, pattern_rules, membership_id)
    if (!is.null(pattern_name)) {
      return(pattern_name)
    }
  }
  
  # Fallback: Generic group name
  level_num <- as.numeric(strsplit(membership_id, "\\.")[[1]][1])
  segment_num <- strsplit(membership_id, "\\.")[[1]][2]
  
  if (length(node_names) > max_concat_length) {
    return(paste0("Group_", level_num, "_", segment_num, " (", length(node_names), " nodes)"))
  } else {
    return(paste(node_names, collapse = separator))
  }
}

#' Create Enhanced Pattern Rules for Segment Naming
#' 
#' Creates a comprehensive pattern library for intelligent segment naming,
#' focusing on occupational levels, industries, and employment types while
#' avoiding generic social class terms.
#' 
#' @return List of enhanced pattern rules with priorities and semantic categories
#' @keywords internal

create_enhanced_pattern_rules <- function() {
  list(
    # Executive and Leadership (Priority 10)
    "executive" = list(
      patterns = c("CEO", "Executive", "Director", "Chief", "President"),
      name = "Executive_Leadership",
      priority = 10,
      category = "leadership"
    ),
    
    # Senior Management (Priority 9)
    "senior_management" = list(
      patterns = c("Senior.*Manager", "Vice.*President", "General.*Manager"),
      name = "Senior_Management",
      priority = 9,
      category = "management"
    ),
    
    # Management (Priority 8)
    "management" = list(
      patterns = c("Manager", "Management", "Supervisor", "Head", "Lead"),
      name = "Management_Level",
      priority = 8,
      category = "management"
    ),
    
    # Professional Services (Priority 7)
    "professional" = list(
      patterns = c("Professional", "Consultant", "Specialist", "Analyst"),
      name = "Professional_Services",
      priority = 7,
      category = "professional"
    ),
    
    # Technical and Engineering (Priority 7)
    "technical" = list(
      patterns = c("Engineer", "Technical", "Technician", "Developer", "Programmer"),
      name = "Technical_Specialists",
      priority = 7,
      category = "technical"
    ),
    
    # Healthcare (Priority 6)
    "healthcare" = list(
      patterns = c("Doctor", "Nurse", "Medical", "Health", "Physician"),
      name = "Healthcare_Professionals",
      priority = 6,
      category = "healthcare"
    ),
    
    # Education (Priority 6)
    "education" = list(
      patterns = c("Teacher", "Educator", "Professor", "Academic", "Instructor"),
      name = "Education_Sector",
      priority = 6,
      category = "education"
    ),
    
    # Finance and Banking (Priority 6)
    "finance" = list(
      patterns = c("Finance", "Bank", "Accountant", "Insurance", "Investment"),
      name = "Financial_Services",
      priority = 6,
      category = "finance"
    ),
    
    # Sales and Marketing (Priority 5)
    "sales" = list(
      patterns = c("Sales", "Marketing", "Account", "Business.*Development"),
      name = "Sales_Marketing",
      priority = 5,
      category = "sales"
    ),
    
    # Administrative (Priority 5)
    "administrative" = list(
      patterns = c("Admin", "Clerk", "Secretary", "Assistant", "Coordinator"),
      name = "Administrative_Support", 
      priority = 5,
      category = "administrative"
    ),
    
    # Skilled Workers (Priority 4)
    "skilled_worker" = list(
      patterns = c("Skilled", "Craft", "Trade", "Artisan", "Mechanic"),
      name = "Skilled_Workers",
      priority = 4,
      category = "skilled"
    ),
    
    # Service Workers (Priority 4)
    "service" = list(
      patterns = c("Service", "Customer", "Hospitality", "Retail", "Food"),
      name = "Service_Industries",
      priority = 4,
      category = "service"
    ),
    
    # Manual Labor (Priority 3)
    "manual" = list(
      patterns = c("Manual", "Labor", "Factory", "Production", "Warehouse"),
      name = "Manual_Labor",
      priority = 3,
      category = "manual"
    ),
    
    # Public Sector (Priority 5)
    "public_sector" = list(
      patterns = c("Public", "Government", "Civil.*Service", "Municipal"),
      name = "Public_Sector",
      priority = 5,
      category = "public"
    ),
    
    # Self-employed (Priority 4)
    "self_employed" = list(
      patterns = c("Self.*employed", "Entrepreneur", "Freelance", "Independent"),
      name = "Self_Employed",
      priority = 4,
      category = "employment_type"
    )
  )
}

#' Apply Enhanced Pattern Rules for Segment Naming
#' 
#' Advanced pattern recognition system that uses weighted scoring, semantic
#' analysis, and intelligent fallbacks to generate meaningful segment names.
#' 
#' @param node_names Character vector of node names in the segment
#' @param pattern_rules Enhanced pattern rules from create_enhanced_pattern_rules()
#' @param membership_id Character string of membership ID for fallback naming
#' 
#' @return Character string with intelligently generated name, or NULL if no pattern found
#' @keywords internal

apply_enhanced_pattern_rules <- function(node_names, pattern_rules, membership_id) {
  
  if (length(node_names) == 0) return(NULL)
  
  # Score all patterns
  pattern_scores <- list()
  
  for (rule_name in names(pattern_rules)) {
    rule <- pattern_rules[[rule_name]]
    total_score <- 0
    
    # Check each pattern in the rule
    for (pattern in rule$patterns) {
      matches <- sum(grepl(pattern, node_names, ignore.case = TRUE))
      if (matches > 0) {
        # Weight by priority and coverage
        coverage <- matches / length(node_names)
        score <- rule$priority * coverage * matches
        total_score <- total_score + score
      }
    }
    
    if (total_score > 0) {
      pattern_scores[[rule_name]] <- list(
        score = total_score,
        name = rule$name,
        category = rule$category,
        coverage = sum(sapply(rule$patterns, function(p) sum(grepl(p, node_names, ignore.case = TRUE)))) / length(node_names)
      )
    }
  }
  
  # Find best pattern match
  if (length(pattern_scores) > 0) {
    # Sort by score
    sorted_scores <- pattern_scores[order(sapply(pattern_scores, function(x) x$score), decreasing = TRUE)]
    
    best_match <- sorted_scores[[1]]
    
    # Use best match if it covers at least 30% of nodes
    if (best_match$coverage >= 0.3) {
      # Check for compound patterns (multiple high-scoring categories)
      if (length(sorted_scores) >= 2) {
        second_best <- sorted_scores[[2]]
        
        # If second best is also strong (>50% of best score), create compound name
        if (second_best$score > best_match$score * 0.5 && 
            second_best$coverage >= 0.2 && 
            best_match$category != second_best$category) {
          
          # Create compound name, ordering by hierarchy
          categories <- c(best_match$name, second_best$name)
          # Reorder based on typical hierarchy
          if (any(grepl("Executive|Leadership", categories)) && any(grepl("Management", categories))) {
            return("Executive_Management")
          } else if (any(grepl("Professional", categories)) && any(grepl("Technical", categories))) {
            return("Professional_Technical")
          } else {
            return(paste(categories[1:2], collapse = "_"))
          }
        }
      }
      
      return(best_match$name)
    }
  }
  
  # Enhanced fallback: extract meaningful terms
  meaningful_name <- extract_meaningful_terms(node_names)
  if (!is.null(meaningful_name)) {
    return(meaningful_name)
  }
  
  # Final fallback using semantic analysis
  return(generate_semantic_fallback(node_names, membership_id))
}

#' Extract Meaningful Terms from Node Names
#' 
#' Analyzes node names to extract the most semantically meaningful terms
#' for generating descriptive segment names.
#' 
#' @param node_names Character vector of node names
#' @return Character string with meaningful term-based name, or NULL
#' @keywords internal

extract_meaningful_terms <- function(node_names) {
  if (length(node_names) == 0) return(NULL)
  
  # Clean and split node names into words, preserving meaningful separators
  cleaned_names <- gsub("[^A-Za-z0-9_.]", " ", node_names)
  all_words <- unlist(strsplit(cleaned_names, "[\\s_]+"))
  all_words <- all_words[nchar(all_words) >= 3]  # Filter short words
  all_words <- tolower(all_words)
  
  # Remove numbers and purely numeric words
  all_words <- all_words[!grepl("^\\d+$", all_words)]
  
  # Remove common stopwords that aren't meaningful for occupation names
  stopwords <- c("and", "the", "for", "with", "other", "all", "new", "old", "not", "any")
  all_words <- all_words[!all_words %in% stopwords]
  
  if (length(all_words) == 0) return(NULL)
  
  # Find most frequent meaningful words
  word_freq <- table(all_words)
  
  # Prioritize occupational terms
  occupational_indicators <- c("work", "job", "employ", "occupation", "profession", "career", "industry", "sector")
  
  # Find most frequent non-generic terms
  frequent_words <- names(word_freq)[word_freq >= max(1, length(node_names) * 0.3)]
  
  if (length(frequent_words) > 0) {
    # Prioritize longer, more specific terms
    best_words <- frequent_words[order(nchar(frequent_words), decreasing = TRUE)]
    
    # Take up to 2 most meaningful words, prioritizing longer ones
    selected_words <- head(best_words, 2)
    selected_words <- sapply(selected_words, function(w) {
      # Capitalize first letter properly
      paste0(toupper(substr(w, 1, 1)), substr(w, 2, nchar(w)))
    })
    
    return(paste(selected_words, collapse = "_"))
  }
  
  return(NULL)
}

#' Generate Semantic Fallback Name
#' 
#' Creates intelligent fallback names using segment composition analysis
#' when no clear patterns are detected.
#' 
#' @param node_names Character vector of node names
#' @param membership_id Character string of membership ID
#' @return Character string with semantic fallback name
#' @keywords internal

generate_semantic_fallback <- function(node_names, membership_id) {
  
  # Analyze segment composition
  n_nodes <- length(node_names)
  
  # Create descriptive name based on composition
  if (n_nodes == 1) {
    return(node_names[1])
  } else if (n_nodes <= 3) {
    return(paste(node_names, collapse = "_"))
  } else {
    # For larger segments, create meaningful descriptive names
    
    # Check for numeric patterns in names (might indicate levels)
    has_numbers <- any(grepl("\\d", node_names))
    
    # Check for common word patterns
    common_terms <- extract_meaningful_terms(node_names)
    
    if (!is.null(common_terms)) {
      return(paste0(common_terms, "_Group"))
    } else if (has_numbers) {
      return(paste0("Level_", gsub("\\.", "_", membership_id)))
    } else {
      # Use position-based naming that's more descriptive
      level_num <- as.numeric(strsplit(membership_id, "\\.")[[1]][1])
      segment_num <- strsplit(membership_id, "\\.")[[1]][2]
      
      segment_descriptors <- c("Alpha", "Beta", "Gamma", "Delta", "Epsilon", "Zeta", "Eta", "Theta")
      
      if (as.numeric(segment_num) <= length(segment_descriptors)) {
        descriptor <- segment_descriptors[as.numeric(segment_num)]
        return(paste0("Segment_", descriptor, "_L", level_num))
      } else {
        return(paste0("Mixed_Group_", level_num, "_", segment_num))
      }
    }
  }
}


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

# First level

new.seg$segment.list[[1]]  <- segments$segment.list[[1]]
new.seg$mat.list[[1]]      <- segments$mat.list[[1]]
mxa                        <- segments$mat.list[[1]]

# Second level
out.list <- list()
for(i in 1:nlevels(as.factor(variable))){
  var <- as.factor(variable)
  levs <- levels(var)
  out.list[[i]]<- which(var == levs[i])  
}

new.seg$segment.list[[2]] <- out.list

groups.1       <- c(variable, length(out.list)+1)
mx.2_r          <- rowsum(mxa, groups.1)
mx.2_r_t        <- t(mx.2_r)
mx.2_rc_t       <- rowsum(mx.2_r_t, groups.1)
mx.2g           <- t(mx.2_rc_t)

new.seg$mat.list[[2]] <- mx.2g

return(new.seg)
}


#' Find Best Level With Structure and Names
#' 
#' @keywords internal
find_best_level_with_structure <- function(segments, enhanced_membership, max_level) {
  best_level <- 2  # Default fallback
  best_score <- -1
  
  # Evaluate each level for both network structure and name availability
  for (level in 2:max_level) {
    if (level <= length(segments$mat.list)) {
      mat <- segments$mat.list[[level]]
      n_segments <- nrow(mat) - 1  # Exclude total row
      
      # Skip levels with too few segments
      if (n_segments < 2) next
      
      # Check network connectivity
      wm <- weight.matrix(mat, cut.off = 1, diagonal = FALSE)
      wm[is.na(wm)] <- 0
      n_edges <- sum(wm > 0) / 2
      
      # Check if enhanced membership has data for this level
      level_data <- enhanced_membership[enhanced_membership$level == level, ]
      has_enhanced_data <- nrow(level_data) > 0
      
      # Calculate composite score
      # Prefer levels with 2-5 segments, some connectivity, and enhanced names
      segment_score <- if (n_segments >= 2 && n_segments <= 5) {
        1.0
      } else if (n_segments <= 8) {
        0.7
      } else {
        0.3
      }
      
      connectivity_score <- if (n_edges > 0) 1.0 else 0.1
      name_score <- if (has_enhanced_data) 1.0 else 0.5
      
      # Combined score favoring structure and connectivity
      score <- segment_score * 0.5 + connectivity_score * 0.3 + name_score * 0.2
      
      if (score > best_score) {
        best_score <- score
        best_level <- level
      }
    }
  }
  
  return(best_level)
}

#' Extract Named Matrices
#' 
#' @keywords internal
extract_named_matrices <- function(segments, max_level, enhanced_membership) {
  
  # Extract original matrix (level 1)
  original_matrix <- segments$mat.list[[1]]
  
  # Extract max level matrix
  max_level_matrix <- segments$mat.list[[max_level]]
  
  # Get names for max level segments
  max_level_membership <- enhanced_membership[enhanced_membership$level == max_level, ]
  
  # Create mapping of segment numbers to enhanced names
  segment_mapping <- unique(max_level_membership[, c("membership", "level_name")])
  
  # Apply names to max level matrix
  if (nrow(max_level_matrix) - 1 == nrow(segment_mapping)) {
    # Matrix includes total row/column
    new_names <- c(segment_mapping$level_name[order(segment_mapping$membership)], "Total")
    rownames(max_level_matrix) <- new_names
    colnames(max_level_matrix) <- new_names
  }
  
  list(
    original = original_matrix,
    max_level = max_level_matrix,
    level = max_level
  )
}

#' Compute Graph Metrics Comparison
#' 
#' @keywords internal
compute_metrics_comparison <- function(segments, max_level) {
  
  metrics_list <- list()
  
  for (level in 1:max_level) {
    # Get matrix for this level
    mat <- segments$mat.list[[level]]
    
    # Remove total row/column
    mat_clean <- mat[-nrow(mat), -ncol(mat)]
    
    # Create weighted adjacency matrix
    wm <- weight.matrix(mat, cut.off = 1, diagonal = FALSE)
    wm[is.na(wm)] <- 0
    
    # Create graph
    g <- moneca_graph_from_adjacency(wm, mode = "directed", weighted = TRUE, diag = FALSE)
    
    # Compute metrics
    metrics_list[[level]] <- data.frame(
      level = level,
      n_nodes = igraph::vcount(g),
      n_edges = igraph::ecount(g),
      density = igraph::graph.density(g),
      reciprocity = igraph::reciprocity(g),
      transitivity = igraph::transitivity(g),
      diameter = ifelse(igraph::is_connected(g, mode = "weak"), 
                       igraph::diameter(g, directed = TRUE), 
                       NA),
      avg_path_length = igraph::mean_distance(g, directed = TRUE),
      modularity = tryCatch({
        comm <- igraph::cluster_walktrap(g)
        igraph::modularity(g, igraph::membership(comm))
      }, error = function(e) NA),
      avg_degree = mean(igraph::degree(g)),
      avg_strength = mean(igraph::strength(g)),
      centralization = igraph::centralization.degree(g)$centralization
    )
  }
  
  do.call(rbind, metrics_list)
}

#' Compute Centrality Metrics
#' 
#' @keywords internal
compute_centrality_metrics <- function(segments, level, enhanced_membership) {
  
  # Get matrix and create graph
  mat <- segments$mat.list[[level]]
  mat_clean <- mat[-nrow(mat), -ncol(mat)]
  
  wm <- weight.matrix(mat, cut.off = 1, diagonal = FALSE)
  wm[is.na(wm)] <- 0
  
  g <- moneca_graph_from_adjacency(wm, mode = "directed", weighted = TRUE, diag = FALSE)
  
  # Get segment names - the enhanced membership doesn't use numeric levels
  # Instead, we'll work directly with the membership data
  level_membership <- enhanced_membership
  
  # Clean up membership data - remove FALSE values and convert to numeric
  valid_membership <- level_membership[level_membership$membership != "FALSE" & 
                                      !is.na(level_membership$membership), ]
  
  if (nrow(valid_membership) == 0) {
    return(data.frame(
      segment = character(),
      degree_in = numeric(),
      degree_out = numeric(),
      degree_total = numeric(),
      strength_in = numeric(),
      strength_out = numeric(),
      strength_total = numeric(),
      betweenness = numeric(),
      closeness_in = numeric(),
      closeness_out = numeric(),
      eigenvector = numeric(),
      pagerank = numeric(),
      composition = character(),
      stringsAsFactors = FALSE
    ))
  }
  
  # Convert membership to numeric and create clean segment mapping
  valid_membership$membership_clean <- as.numeric(as.character(valid_membership$membership))
  segment_names <- unique(valid_membership[, c("membership_clean", "level_name")])
  segment_names <- segment_names[order(segment_names$membership_clean), ]
  
  # Check if we have nodes
  n_nodes <- igraph::vcount(g)
  if (n_nodes == 0 || nrow(segment_names) == 0) {
    return(data.frame(
      segment = character(),
      degree_in = numeric(),
      degree_out = numeric(),
      degree_total = numeric(),
      strength_in = numeric(),
      strength_out = numeric(),
      strength_total = numeric(),
      betweenness = numeric(),
      closeness_in = numeric(),
      closeness_out = numeric(),
      eigenvector = numeric(),
      pagerank = numeric(),
      composition = character(),
      stringsAsFactors = FALSE
    ))
  }
  
  # Ensure we have the right number of segments as graph nodes
  n_segments <- nrow(segment_names)
  if (n_segments != n_nodes) {
    # Try to match based on available data
    n_segments <- min(n_segments, n_nodes)
    segment_names <- head(segment_names, n_segments)
  }
  
  # Compute various centrality measures
  centrality_df <- data.frame(
    segment = segment_names$level_name,
    degree_in = igraph::degree(g, mode = "in")[1:n_segments],
    degree_out = igraph::degree(g, mode = "out")[1:n_segments],
    degree_total = igraph::degree(g, mode = "all")[1:n_segments],
    strength_in = igraph::strength(g, mode = "in")[1:n_segments],
    strength_out = igraph::strength(g, mode = "out")[1:n_segments],
    strength_total = igraph::strength(g, mode = "all")[1:n_segments],
    betweenness = igraph::betweenness(g, directed = TRUE)[1:n_segments],
    closeness_in = igraph::closeness(g, mode = "in")[1:n_segments],
    closeness_out = igraph::closeness(g, mode = "out")[1:n_segments],
    eigenvector = igraph::eigen_centrality(g, directed = TRUE)$vector[1:n_segments],
    pagerank = igraph::page_rank(g, directed = TRUE)$vector[1:n_segments]
  )
  
  # Add composition information
  segment_composition <- sapply(1:nrow(segment_names), function(i) {
    members <- valid_membership[valid_membership$membership_clean == segment_names$membership_clean[i], "name"]
    paste(members, collapse = ", ")
  })
  
  centrality_df$composition <- segment_composition
  
  # Sort by total strength (most central segments first)
  centrality_df <- centrality_df[order(centrality_df$strength_total, decreasing = TRUE), ]
  
  return(centrality_df)
}

#' Compute Mobility Metrics
#' 
#' @keywords internal
compute_mobility_metrics <- function(segments, level, enhanced_membership) {
  
  # Get matrix
  mat <- segments$mat.list[[level]]
  n <- nrow(mat) - 1  # Exclude total row
  
  # Get segment names - the enhanced membership doesn't use numeric levels
  # Instead, we'll work directly with the membership data
  level_membership <- enhanced_membership
  
  # Clean up membership data - remove FALSE values and convert to numeric
  valid_membership <- level_membership[level_membership$membership != "FALSE" & 
                                      !is.na(level_membership$membership), ]
  
  if (nrow(valid_membership) == 0) {
    return(data.frame(
      segment = character(),
      total_outflow = numeric(),
      total_inflow = numeric(),
      net_flow = numeric(),
      immobility_rate = numeric(),
      mobility_rate = numeric(),
      upward_mobility = numeric(),
      downward_mobility = numeric(),
      lateral_mobility = numeric(),
      relative_mobility_share = numeric(),
      stringsAsFactors = FALSE
    ))
  }
  
  # Convert membership to numeric and create clean segment mapping
  valid_membership$membership_clean <- as.numeric(as.character(valid_membership$membership))
  segment_names <- unique(valid_membership[, c("membership_clean", "level_name")])
  segment_names <- segment_names[order(segment_names$membership_clean), ]
  
  # Ensure we match the matrix dimensions
  n_segments <- min(nrow(segment_names), n)
  segment_names <- head(segment_names, n_segments)
  
  # Check if we have segments
  if (n == 0 || nrow(segment_names) == 0) {
    return(data.frame(
      segment = character(),
      total_outflow = numeric(),
      total_inflow = numeric(),
      net_flow = numeric(),
      immobility_rate = numeric(),
      mobility_rate = numeric(),
      upward_mobility = numeric(),
      downward_mobility = numeric(),
      lateral_mobility = numeric(),
      relative_mobility_share = numeric(),
      stringsAsFactors = FALSE
    ))
  }
  
  # Initialize metrics data frame
  mobility_metrics <- data.frame(
    segment = segment_names$level_name,
    total_outflow = numeric(n_segments),
    total_inflow = numeric(n_segments),
    net_flow = numeric(n_segments),
    immobility_rate = numeric(n_segments),
    mobility_rate = numeric(n_segments),
    upward_mobility = numeric(n_segments),
    downward_mobility = numeric(n_segments),
    lateral_mobility = numeric(n_segments)
  )
  
  # Compute metrics for each segment
  for (i in 1:n_segments) {
    # Total flows
    mobility_metrics$total_outflow[i] <- sum(mat[i, -c(i, ncol(mat))])
    mobility_metrics$total_inflow[i] <- sum(mat[-c(i, nrow(mat)), i])
    mobility_metrics$net_flow[i] <- mobility_metrics$total_inflow[i] - mobility_metrics$total_outflow[i]
    
    # Mobility rates
    row_total <- mat[i, ncol(mat)]
    mobility_metrics$immobility_rate[i] <- if(row_total > 0) mat[i, i] / row_total else 0
    mobility_metrics$mobility_rate[i] <- 1 - mobility_metrics$immobility_rate[i]
    
    # Directional mobility (assuming lower index = higher status)
    if (i > 1) {
      mobility_metrics$upward_mobility[i] <- sum(mat[i, 1:(i-1)])
    }
    if (i < n_segments) {
      mobility_metrics$downward_mobility[i] <- sum(mat[i, (i+1):n_segments])
    }
    mobility_metrics$lateral_mobility[i] <- mat[i, i]
  }
  
  # Add relative mobility index
  total_mobility <- sum(mat[-nrow(mat), -ncol(mat)]) - sum(diag(mat[-nrow(mat), -ncol(mat)]))
  mobility_metrics$relative_mobility_share <- 
    (mobility_metrics$total_outflow + mobility_metrics$total_inflow) / total_mobility
  
  return(mobility_metrics)
}

#' Generate Summary Report as Data Frame
#' 
#' @keywords internal
generate_summary_report <- function(results, segments, max_level, analysis_level = max_level) {
  
  # Initialize report components
  report_rows <- list()
  
  # Basic information
  report_rows[[length(report_rows) + 1]] <- data.frame(
    category = "Basic Information",
    metric = "Analysis Date",
    value = as.character(Sys.Date()),
    details = NA,
    stringsAsFactors = FALSE
  )
  
  report_rows[[length(report_rows) + 1]] <- data.frame(
    category = "Basic Information",
    metric = "Maximum Segmentation Level",
    value = as.character(max_level),
    details = NA,
    stringsAsFactors = FALSE
  )
  
  report_rows[[length(report_rows) + 1]] <- data.frame(
    category = "Basic Information",
    metric = "Analysis Level Used",
    value = as.character(analysis_level),
    details = "Level used for centrality/mobility analysis",
    stringsAsFactors = FALSE
  )
  
  # Matrix dimensions
  report_rows[[length(report_rows) + 1]] <- data.frame(
    category = "Matrix Dimensions",
    metric = "Original Matrix Size",
    value = paste(dim(results$matrices$original), collapse = " x "),
    details = paste(nrow(results$matrices$original) - 1, "categories"),
    stringsAsFactors = FALSE
  )
  
  report_rows[[length(report_rows) + 1]] <- data.frame(
    category = "Matrix Dimensions",
    metric = paste("Level", max_level, "Matrix Size"),
    value = paste(dim(results$matrices$max_level), collapse = " x "),
    details = paste(nrow(results$matrices$max_level) - 1, "segments"),
    stringsAsFactors = FALSE
  )
  
  # Network consolidation
  consolidation_ratio <- results$metrics_comparison$n_nodes[1] / results$metrics_comparison$n_nodes[max_level]
  report_rows[[length(report_rows) + 1]] <- data.frame(
    category = "Network Structure",
    metric = "Consolidation Ratio",
    value = paste0(round(consolidation_ratio, 2), ":1"),
    details = paste(results$metrics_comparison$n_nodes[1], "->", results$metrics_comparison$n_nodes[max_level]),
    stringsAsFactors = FALSE
  )
  
  # Graph metrics at analysis level
  analysis_metrics <- results$metrics_comparison[results$metrics_comparison$level == analysis_level, ]
  
  if (nrow(analysis_metrics) > 0) {
    report_rows[[length(report_rows) + 1]] <- data.frame(
      category = "Graph Metrics",
      metric = "Network Density",
      value = round(analysis_metrics$density, 4),
      details = paste("Level", analysis_level),
      stringsAsFactors = FALSE
    )
    
    report_rows[[length(report_rows) + 1]] <- data.frame(
      category = "Graph Metrics",
      metric = "Reciprocity",
      value = round(analysis_metrics$reciprocity, 4),
      details = paste("Level", analysis_level),
      stringsAsFactors = FALSE
    )
    
    report_rows[[length(report_rows) + 1]] <- data.frame(
      category = "Graph Metrics",
      metric = "Transitivity",
      value = round(analysis_metrics$transitivity, 4),
      details = paste("Level", analysis_level),
      stringsAsFactors = FALSE
    )
    
    report_rows[[length(report_rows) + 1]] <- data.frame(
      category = "Graph Metrics",
      metric = "Average Degree",
      value = round(analysis_metrics$avg_degree, 2),
      details = paste("Level", analysis_level),
      stringsAsFactors = FALSE
    )
  }
  
  # Most central segments
  if (nrow(results$centrality_report) > 0) {
    top_n <- min(5, nrow(results$centrality_report))
    for (i in 1:top_n) {
      report_rows[[length(report_rows) + 1]] <- data.frame(
        category = "Top Central Segments",
        metric = paste("Rank", i, "by Strength"),
        value = results$centrality_report$segment[i],
        details = paste("Strength:", round(results$centrality_report$strength_total[i], 2)),
        stringsAsFactors = FALSE
      )
    }
  }
  
  # Mobility patterns
  if (nrow(results$mobility_report) > 0) {
    highest_mobility_idx <- which.max(results$mobility_report$mobility_rate)
    lowest_mobility_idx <- which.min(results$mobility_report$mobility_rate)
    
    if (length(highest_mobility_idx) > 0) {
      report_rows[[length(report_rows) + 1]] <- data.frame(
        category = "Mobility Patterns",
        metric = "Highest Mobility Segment",
        value = results$mobility_report$segment[highest_mobility_idx],
        details = paste("Rate:", round(results$mobility_report$mobility_rate[highest_mobility_idx], 3)),
        stringsAsFactors = FALSE
      )
    }
    
    if (length(lowest_mobility_idx) > 0) {
      report_rows[[length(report_rows) + 1]] <- data.frame(
        category = "Mobility Patterns",
        metric = "Lowest Mobility Segment",
        value = results$mobility_report$segment[lowest_mobility_idx],
        details = paste("Rate:", round(results$mobility_report$mobility_rate[lowest_mobility_idx], 3)),
        stringsAsFactors = FALSE
      )
    }
    
    # Average mobility
    avg_mobility <- mean(results$mobility_report$mobility_rate)
    report_rows[[length(report_rows) + 1]] <- data.frame(
      category = "Mobility Patterns",
      metric = "Average Mobility Rate",
      value = round(avg_mobility, 3),
      details = paste("Level", analysis_level),
      stringsAsFactors = FALSE
    )
    
    # Net flow information
    max_inflow_idx <- which.max(results$mobility_report$net_flow)
    max_outflow_idx <- which.min(results$mobility_report$net_flow)
    
    if (length(max_inflow_idx) > 0 && results$mobility_report$net_flow[max_inflow_idx] > 0) {
      report_rows[[length(report_rows) + 1]] <- data.frame(
        category = "Flow Patterns",
        metric = "Largest Net Inflow",
        value = results$mobility_report$segment[max_inflow_idx],
        details = paste("Net flow:", round(results$mobility_report$net_flow[max_inflow_idx], 0)),
        stringsAsFactors = FALSE
      )
    }
    
    if (length(max_outflow_idx) > 0 && results$mobility_report$net_flow[max_outflow_idx] < 0) {
      report_rows[[length(report_rows) + 1]] <- data.frame(
        category = "Flow Patterns",
        metric = "Largest Net Outflow",
        value = results$mobility_report$segment[max_outflow_idx],
        details = paste("Net flow:", round(results$mobility_report$net_flow[max_outflow_idx], 0)),
        stringsAsFactors = FALSE
      )
    }
  }
  
  # Combine all rows into a single data frame
  report_df <- do.call(rbind, report_rows)
  
  # Add row numbers for reference
  report_df <- cbind(row_id = 1:nrow(report_df), report_df)
  
  return(report_df)
}


#' Determine Optimal Analysis Level
#' 
#' @keywords internal
determine_analysis_level <- function(moneca_results, enhanced_membership, level_selection) {
  
  if (is.numeric(level_selection)) {
    # Use specific level if provided
    max_level <- length(moneca_results$mat.list)
    if (level_selection > max_level) {
      warning(paste("Requested level", level_selection, "exceeds maximum level", max_level, 
                   ". Using level", max_level))
      return(max_level)
    }
    return(level_selection)
  }
  
  if (level_selection == "max") {
    return(length(moneca_results$mat.list))
  }
  
  if (level_selection == "auto") {
    # For auto selection, always use the level where enhanced membership has meaningful names
    # This is typically the final/maximum level where segmentation is complete
    
    # Get levels with meaningful names (not just "FALSE" or simple numbers)
    meaningful_levels <- enhanced_membership[
      enhanced_membership$membership != "FALSE" & 
      !is.na(enhanced_membership$level_name) &
      enhanced_membership$level_name != "" &
      !grepl("^[0-9]+$", enhanced_membership$level_name), ]
    
    if (nrow(meaningful_levels) > 0) {
      # Extract level numbers from membership strings
      level_nums <- as.numeric(sapply(strsplit(meaningful_levels$membership, "\\."), `[`, 1))
      level_nums <- level_nums[!is.na(level_nums)]
      
      if (length(level_nums) > 0) {
        # Use the level with meaningful names (should be the final level)
        return(max(level_nums))
      }
    }
    
    # Fallback to maximum level
    return(length(moneca_results$mat.list))
  }
  
  stop("level_selection must be 'auto', 'max', or a numeric level")
}

#' Extract Segment Matrix
#' 
#' @keywords internal
extract_segment_matrix <- function(moneca_results, enhanced_membership, level) {
  
  # Get the matrix at the specified level
  mat <- moneca_results$mat.list[[level]]
  
  if (is.null(mat)) {
    stop(paste("No matrix available at level", level))
  }
  
  # Get segment names for this level from enhanced membership
  level_membership <- enhanced_membership[
    enhanced_membership$membership != "FALSE" & 
    !is.na(enhanced_membership$membership), ]
  
  # Extract level number from membership and filter
  level_membership$level_num <- as.numeric(sapply(strsplit(level_membership$membership, "\\."), `[`, 1))
  level_data <- level_membership[level_membership$level_num == level, ]
  
  # For the final level with meaningful names, we need to handle the case where 
  # all classes are grouped into segments represented in the level_data
  if (nrow(level_data) > 0) {
    # Get unique segments with their names
    unique_segments <- unique(level_data[, c("membership", "level_name")])
    unique_segments <- unique_segments[order(unique_segments$membership), ]
    
    # The matrix dimensions represent the final segments
    n_segments <- nrow(mat) - 1  # Exclude totals row
    
    # Check if we have a matrix with multiple segments but only one level_name
    if (n_segments == 1 && nrow(unique_segments) == 1) {
      # True single segment case: matrix is 2x2 (segment + total)
      segment_name <- unique_segments$level_name[1]
      dimnames(mat) <- list(c(segment_name, "Total"), c(segment_name, "Total"))
    } else if (nrow(unique_segments) == 1 && n_segments > 1) {
      # Matrix has multiple segments but enhanced membership shows single consolidated name
      # Keep original segment identifiers with the consolidated name as prefix
      base_name <- unique_segments$level_name[1]
      segment_names <- paste0(base_name, "_Seg", rownames(mat)[1:n_segments])
      dimnames(mat) <- list(c(segment_names, "Total"), c(segment_names, "Total"))
    } else if (nrow(unique_segments) > 0) {
      # Multiple distinct segments with different names
      segment_names <- unique_segments$level_name[1:min(nrow(unique_segments), n_segments)]
      
      # If we have fewer names than segments, supplement with segment IDs
      if (length(segment_names) < n_segments) {
        original_names <- rownames(mat)[1:n_segments]
        additional_names <- paste0("Segment_", original_names[(length(segment_names) + 1):n_segments])
        segment_names <- c(segment_names, additional_names)
      }
      
      dimnames(mat) <- list(c(segment_names, "Total"), c(segment_names, "Total"))
    }
  }
  
  return(mat)
}

#' Compute Segment Mobility Metrics
#' 
#' @keywords internal
compute_segment_mobility_metrics <- function(segment_matrix, enhanced_membership) {
  
  n <- nrow(segment_matrix) - 1  # Exclude totals row
  
  if (n <= 0) {
    return(data.frame(
      segment = character(),
      total_outflow = numeric(),
      total_inflow = numeric(),
      net_flow = numeric(),
      mobility_rate = numeric(),
      immobility_rate = numeric(),
      upward_mobility = numeric(),
      downward_mobility = numeric(),
      lateral_mobility = numeric(),
      relative_mobility_share = numeric(),
      stringsAsFactors = FALSE
    ))
  }
  
  # Get segment names from matrix
  segment_names <- rownames(segment_matrix)[1:n]
  if (is.null(segment_names)) {
    segment_names <- paste("Segment", 1:n)
  }
  
  # Initialize metrics data frame
  mobility_metrics <- data.frame(
    segment = segment_names,
    total_outflow = numeric(n),
    total_inflow = numeric(n),
    net_flow = numeric(n),
    mobility_rate = numeric(n),
    immobility_rate = numeric(n),
    upward_mobility = numeric(n),
    downward_mobility = numeric(n),
    lateral_mobility = numeric(n),
    relative_mobility_share = numeric(n),
    stringsAsFactors = FALSE
  )
  
  # Calculate total mobility for relative shares
  total_mobility <- sum(segment_matrix[1:n, 1:n]) - sum(diag(segment_matrix[1:n, 1:n]))
  
  # Compute metrics for each segment
  for (i in 1:n) {
    # Total flows
    mobility_metrics$total_outflow[i] <- sum(segment_matrix[i, 1:n]) - segment_matrix[i, i]
    mobility_metrics$total_inflow[i] <- sum(segment_matrix[1:n, i]) - segment_matrix[i, i]
    mobility_metrics$net_flow[i] <- mobility_metrics$total_inflow[i] - mobility_metrics$total_outflow[i]
    
    # Mobility rates
    row_total <- segment_matrix[i, n + 1]  # Total from last column
    if (row_total > 0) {
      mobility_metrics$immobility_rate[i] <- segment_matrix[i, i] / row_total
      mobility_metrics$mobility_rate[i] <- 1 - mobility_metrics$immobility_rate[i]
    } else {
      mobility_metrics$immobility_rate[i] <- 0
      mobility_metrics$mobility_rate[i] <- 0
    }
    
    # Directional mobility (assuming lower index = higher status)
    mobility_metrics$upward_mobility[i] <- if(i > 1) sum(segment_matrix[i, 1:(i-1)]) else 0
    mobility_metrics$downward_mobility[i] <- if(i < n) sum(segment_matrix[i, (i+1):n]) else 0
    mobility_metrics$lateral_mobility[i] <- segment_matrix[i, i]
    
    # Relative mobility share
    total_segment_mobility <- mobility_metrics$total_outflow[i] + mobility_metrics$total_inflow[i]
    mobility_metrics$relative_mobility_share[i] <- if(total_mobility > 0) total_segment_mobility / total_mobility else 0
  }
  
  return(mobility_metrics)
}

#' Compute Segment Centrality Metrics
#' 
#' @keywords internal
compute_segment_centrality_metrics <- function(segment_matrix, enhanced_membership) {
  
  n <- nrow(segment_matrix) - 1  # Exclude totals row
  
  if (n <= 0) {
    return(data.frame(
      segment = character(),
      degree_in = numeric(),
      degree_out = numeric(),
      degree_total = numeric(),
      strength_in = numeric(),
      strength_out = numeric(),
      strength_total = numeric(),
      betweenness = numeric(),
      closeness_in = numeric(),
      closeness_out = numeric(),
      eigenvector = numeric(),
      pagerank = numeric(),
      stringsAsFactors = FALSE
    ))
  }
  
  # Get segment names
  segment_names <- rownames(segment_matrix)[1:n]
  if (is.null(segment_names)) {
    segment_names <- paste("Segment", 1:n)
  }
  
  # Create weighted adjacency matrix (excluding totals)
  adj_matrix <- segment_matrix[1:n, 1:n]
  
  # Convert to igraph object
  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop("igraph package is required for centrality calculations")
  }
  
  # Use compatibility function for igraph
  g <- moneca_graph_from_adjacency(adj_matrix, mode = "directed", weighted = TRUE)
  
  # Compute centrality measures with error handling
  safe_centrality <- function(func, ...) {
    tryCatch(func(...), error = function(e) rep(0, n))
  }
  
  centrality_metrics <- data.frame(
    segment = segment_names,
    degree_in = safe_centrality(igraph::degree, g, mode = "in"),
    degree_out = safe_centrality(igraph::degree, g, mode = "out"),
    degree_total = safe_centrality(igraph::degree, g, mode = "all"),
    strength_in = safe_centrality(igraph::strength, g, mode = "in"),
    strength_out = safe_centrality(igraph::strength, g, mode = "out"),
    strength_total = safe_centrality(igraph::strength, g, mode = "all"),
    betweenness = safe_centrality(igraph::betweenness, g, directed = TRUE),
    closeness_in = safe_centrality(igraph::closeness, g, mode = "in"),
    closeness_out = safe_centrality(igraph::closeness, g, mode = "out"),
    stringsAsFactors = FALSE
  )
  
  # Eigenvector centrality and PageRank (these return different structures)
  eigen_result <- tryCatch(igraph::eigen_centrality(g, directed = TRUE), 
                          error = function(e) list(vector = rep(0, n)))
  pagerank_result <- tryCatch(igraph::page_rank(g, directed = TRUE), 
                             error = function(e) list(vector = rep(0, n)))
  
  centrality_metrics$eigenvector <- eigen_result$vector
  centrality_metrics$pagerank <- pagerank_result$vector
  
  # Sort by total strength (most central first)
  centrality_metrics <- centrality_metrics[order(centrality_metrics$strength_total, decreasing = TRUE), ]
  
  return(centrality_metrics)
}

#' Generate Segment Network Plot
#' 
#' @keywords internal
generate_segment_plot <- function(segment_matrix, enhanced_membership, 
                                 mobility_metrics, centrality_metrics,
                                 plot_layout, plot_node_color, node_size_metric) {
  
  n <- nrow(segment_matrix) - 1  # Exclude totals row
  
  if (n <= 0 || !requireNamespace("ggplot2", quietly = TRUE) || 
      !requireNamespace("ggraph", quietly = TRUE) || !requireNamespace("tidygraph", quietly = TRUE)) {
    # Return a simple placeholder plot
    if (requireNamespace("ggplot2", quietly = TRUE)) {
      return(ggplot2::ggplot() + 
             ggplot2::labs(title = "Cannot generate plot: insufficient data or missing packages") +
             ggplot2::theme_void())
    } else {
      return(NULL)
    }
  }
  
  # Create adjacency matrix for network (excluding totals row and column)
  adj_matrix <- segment_matrix[1:n, 1:n]
  
  # Create igraph object and convert to tidygraph
  g <- moneca_graph_from_adjacency(adj_matrix, mode = "directed", weighted = TRUE)
  
  # Create tidygraph object
  tg <- tidygraph::as_tbl_graph(g)
  
  # Add node attributes - get names from matrix (excluding totals)
  segment_names <- rownames(segment_matrix)[1:n]
  if (is.null(segment_names)) {
    segment_names <- paste("Segment", 1:n)
  }
  
  # Ensure we have exactly n names for n graph nodes
  if (length(segment_names) != n) {
    segment_names <- segment_names[1:n]
  }
  
  # Get the actual number of vertices in the graph
  n_vertices <- igraph::vcount(g)
  
  # Debug: ensure everything matches
  if (length(segment_names) != n_vertices) {
    warning(paste("Mismatch: segment_names length", length(segment_names), "vs graph vertices", n_vertices))
    segment_names <- segment_names[1:min(length(segment_names), n_vertices)]
    if (length(segment_names) < n_vertices) {
      segment_names <- c(segment_names, paste("Node", (length(segment_names) + 1):n_vertices))
    }
  }
  
  # Create node data frame with exact length matching graph vertices
  node_data <- data.frame(
    name = segment_names[1:n_vertices], 
    stringsAsFactors = FALSE
  )
  
  # Add mobility metrics - only for matching segments
  if (nrow(mobility_metrics) > 0) {
    # Ensure mobility metrics match the node data
    mobility_subset <- mobility_metrics[mobility_metrics$segment %in% node_data$name, ]
    if (nrow(mobility_subset) > 0) {
      node_data <- merge(node_data, mobility_subset, by.x = "name", by.y = "segment", all.x = TRUE)
    }
  }
  
  # Add centrality metrics - only for matching segments
  if (nrow(centrality_metrics) > 0) {
    # Ensure centrality metrics match the node data
    centrality_subset <- centrality_metrics[centrality_metrics$segment %in% node_data$name, ]
    if (nrow(centrality_subset) > 0) {
      node_data <- merge(node_data, centrality_subset, by.x = "name", by.y = "segment", all.x = TRUE)
    }
  }
  
  # Ensure node_data has exactly n_vertices rows
  if (nrow(node_data) != n_vertices) {
    # Pad or trim to match exactly
    if (nrow(node_data) > n_vertices) {
      node_data <- node_data[1:n_vertices, ]
    } else {
      # Add missing rows
      missing_rows <- n_vertices - nrow(node_data)
      additional_data <- data.frame(
        name = paste("Node", (nrow(node_data) + 1):n_vertices),
        stringsAsFactors = FALSE
      )
      # Add NA columns for all other columns in node_data
      for (col in names(node_data)[names(node_data) != "name"]) {
        additional_data[[col]] <- NA
      }
      node_data <- rbind(node_data, additional_data)
    }
  }
  
  # Update tidygraph with node attributes
  tg <- tg %>%
    tidygraph::activate(nodes) %>%
    dplyr::mutate(name = node_data$name)
  
  # Add additional attributes one by one to avoid length mismatches
  for (col in names(node_data)[names(node_data) != "name"]) {
    if (!is.null(node_data[[col]])) {
      tg <- tg %>%
        tidygraph::activate(nodes) %>%
        dplyr::mutate(!!col := node_data[[col]])
    }
  }
  
  # Create the plot
  p <- ggraph::ggraph(tg, layout = plot_layout) +
    ggraph::geom_edge_arc(
      ggplot2::aes(width = weight, alpha = weight),
      arrow = ggplot2::arrow(length = ggplot2::unit(3, "mm"), type = "closed"),
      end_cap = ggraph::circle(3, "mm"),
      start_cap = ggraph::circle(3, "mm"),
      color = "grey60"
    ) +
    ggraph::scale_edge_width_continuous(range = c(0.3, 2), guide = "none") +
    ggraph::scale_edge_alpha_continuous(range = c(0.3, 0.8), guide = "none")
  
  # Determine node size
  size_value <- switch(node_size_metric,
    "strength" = if("strength_total" %in% names(node_data)) node_data$strength_total else 3,
    "mobility_rate" = if("mobility_rate" %in% names(node_data)) node_data$mobility_rate * 10 else 3,
    "population" = 3,  # Would need population data
    "constant" = 3,
    3  # default
  )
  
  # Determine node color
  color_aes <- switch(plot_node_color,
    "mobility_rate" = if("mobility_rate" %in% names(node_data)) "mobility_rate" else NULL,
    "centrality" = if("strength_total" %in% names(node_data)) "strength_total" else NULL,
    "net_flow" = if("net_flow" %in% names(node_data)) "net_flow" else NULL,
    "segment" = "name",
    NULL
  )
  
  if (!is.null(color_aes)) {
    p <- p + ggraph::geom_node_point(ggplot2::aes_string(color = color_aes), size = size_value)
  } else {
    p <- p + ggraph::geom_node_point(size = size_value, color = "steelblue")
  }
  
  # Add node labels
  p <- p + ggraph::geom_node_label(ggplot2::aes(label = name), 
                                  size = 3, repel = TRUE, alpha = 0.8)
  
  # Customize colors based on metric
  if (plot_node_color == "mobility_rate") {
    p <- p + ggplot2::scale_color_gradient(low = "darkblue", high = "red", 
                                          name = "Mobility\nRate")
  } else if (plot_node_color == "centrality") {
    p <- p + ggplot2::scale_color_gradient(low = "lightblue", high = "darkred", 
                                          name = "Centrality\n(Strength)")
  } else if (plot_node_color == "net_flow") {
    p <- p + ggplot2::scale_color_gradient2(low = "red", mid = "white", high = "blue",
                                           name = "Net Flow")
  } else if (plot_node_color == "segment") {
    p <- p + ggplot2::scale_color_discrete(name = "Segment")
  }
  
  # Final styling
  p <- p +
    ggplot2::labs(
      title = "Segment Mobility Network",
      subtitle = paste("Layout:", plot_layout, "| Node color:", plot_node_color, "| Node size:", node_size_metric),
      caption = "Edge width represents mobility flow strength"
    ) +
    ggraph::theme_graph(base_family = "sans") +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 10),
      legend.position = "right"
    )
  
  return(p)
}


#' Generate Segment Membership Dataframe from MONECA Results
#'
#' Creates a dataframe showing segment membership for each row of the original mobility matrix
#' across all segmentation levels.
#'
#' @param moneca_results A moneca object returned by \code{\link{moneca}} or \code{moneca_fast}.
#' @return A dataframe with:
#'   \itemize{
#'     \item name: The name of the row from the original matrix
#'     \item index: The index of the row in the original matrix  
#'     \item level_X: For each segmentation level X (excluding level 1), the segment
#'       assignment. If a row is not assigned to any segment at a level, it shows
#'       the previous level's assignment or the original index.
#'   }
#' @export
#' @examples
#' # Generate example data
#' mx <- generate_mobility_data(n_classes = 5, immobility_strength = 1.5)
#' 
#' # Run moneca analysis
#' seg <- moneca_fast(mx, segment.levels = 3)
#' 
#' # Generate membership dataframe
#' membership_df <- segment.membership.dataframe(seg)
#' print(membership_df)
segment.membership.dataframe <- function(moneca_results) {
  # Validate input
  if (!inherits(moneca_results, "moneca")) {
    stop("Input must be a moneca object from moneca() or moneca_fast()")
  }
  
  # Get the original matrix (excluding sum row/column)
  original_matrix <- moneca_results$mat.list[[1]]
  n_rows <- nrow(original_matrix) - 1  # Exclude sum row
  row_names <- rownames(original_matrix)[1:n_rows]
  
  # Initialize dataframe
  df <- data.frame(
    name = row_names,
    index = 1:n_rows,
    stringsAsFactors = FALSE
  )
  
  # Get segment list
  segment_list <- moneca_results$segment.list
  n_levels <- length(segment_list)
  
  # Skip first level (which is just individual indices)
  if (n_levels > 1) {
    # Process each level starting from level 2
    for (level in 2:n_levels) {
      # Initialize column with previous assignment or index
      if (level == 2) {
        # For level 2, default is the index with l2. prefix
        level_assignment <- paste0("l2.", 1:n_rows)
      } else {
        # For higher levels, default is the previous level's assignment
        # Extract the numeric part after the prefix
        prev_vals <- sub("^l[0-9]+\\.", "", df[[paste0("level_", level - 1)]])
        level_assignment <- paste0("l", level, ".", prev_vals)
      }
      
      # Get segments for this level
      segments_at_level <- segment_list[[level]]
      
      # Assign segment numbers to rows that belong to segments
      for (seg_num in seq_along(segments_at_level)) {
        segment_members <- segments_at_level[[seg_num]]
        # Update assignment for members of this segment with lx. prefix
        level_assignment[segment_members] <- paste0("l", level, ".", seg_num)
      }
      
      # Add to dataframe as character
      df[[paste0("level_", level)]] <- as.character(level_assignment)
    }
  }
  
  # Create the final column with concatenated id and all levels
  # Start with the index
  id_col <- paste0("id_", df$index)
  
  # Add all level columns if they exist
  level_cols <- grep("^level_[0-9]+$", names(df), value = TRUE)
  if (length(level_cols) > 0) {
    for (col in level_cols) {
      id_col <- paste0(id_col, "_", df[[col]])
    }
  }
  
  df$id_full <- id_col
  
  return(df)
}

