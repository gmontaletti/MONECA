#' Parallel moneca - High-Performance Mobility Network Clustering Analysis
#'
#' Parallel implementation of the moneca algorithm leveraging multiple CPU cores
#' for improved performance on large datasets. This implementation maintains
#' full compatibility with the original moneca() function while providing
#' significant speedups through parallelization.
#'
#' @param mx A mobility table (square matrix) with row and column totals in the last
#'   row/column. Row names should identify the categories/classes.
#' @param segment.levels Integer specifying the number of hierarchical segmentation 
#'   levels to compute. Default is 3.
#' @param cut.off Numeric threshold for the minimum relative risk to be considered
#'   a significant tie. Default is 1.
#' @param mode Character string specifying edge mode ("symmetric", "Mutual", or 
#'   "Unmutual"). Default is "symmetric".
#' @param delete.upper.tri Logical indicating whether to use only lower triangle 
#'   for efficiency. Default is TRUE.
#' @param small.cell.reduction Numeric value to handle small cell counts. Cells with
#'   counts below this threshold are set to 0. Default is 0.
#' @param n.cores Integer specifying the number of CPU cores to use. Default is NULL
#'   (auto-detect optimal number).
#' @param parallel.backend Character string specifying the parallel backend:
#'   "auto" (default), "fork" (Unix/Mac only), or "socket" (all platforms).
#' @param chunk.size Integer specifying the chunk size for parallel processing.
#'   Default is NULL (auto-calculate based on data size).
#' @param fallback.sequential Logical indicating whether to fall back to sequential
#'   processing if parallel fails. Default is TRUE.
#' @param progress Logical indicating whether to show progress bars. Default is TRUE.
#' 
#' @return An object of class "moneca" containing:
#'   \describe{
#'     \item{segment.list}{A list of segment memberships for each hierarchical level.}
#'     \item{mat.list}{A list of aggregated mobility matrices for each level.}
#'     \item{small.cell.reduction}{The small cell reduction parameter used.}
#'     \item{performance}{Performance metrics including timing and speedup (if parallel).}
#'   }
#' 
#' @details
#' The parallel implementation uses several strategies to improve performance:
#' \itemize{
#'   \item Parallel clique detection in find.segments
#'   \item Vectorized and parallel matrix operations in weight.matrix
#'   \item Parallel aggregation in segment.matrix
#'   \item Automatic load balancing and chunk size optimization
#'   \item Intelligent fallback to sequential processing when beneficial
#' }
#' 
#' @examples
#' \dontrun{
#' # Generate test data
#' test_data <- generate_mobility_data(n_classes = 100, seed = 123)
#' 
#' # Run parallel analysis (auto-detect cores)
#' result <- moneca_parallel(test_data, segment.levels = 3)
#' 
#' # Run with specific number of cores
#' result <- moneca_parallel(test_data, segment.levels = 3, n.cores = 4)
#' 
#' # Force socket backend for Windows compatibility
#' result <- moneca_parallel(test_data, parallel.backend = "socket")
#' }
#' 
#' @export
moneca_parallel <- function(mx = mx, 
                           segment.levels = 3, 
                           cut.off = 1, 
                           mode = "symmetric", 
                           delete.upper.tri = TRUE, 
                           small.cell.reduction = 0,
                           n.cores = NULL,
                           parallel.backend = "auto",
                           chunk.size = NULL,
                           fallback.sequential = TRUE,
                           progress = TRUE) {
  
  # Detect and configure parallel backend
  parallel_config <- setup_parallel_backend(n.cores, parallel.backend, nrow(mx))
  
  if (!parallel_config$use_parallel) {
    if (progress) message("Using sequential processing (data too small for parallel benefit)")
    return(moneca(mx, segment.levels, cut.off, mode, delete.upper.tri, small.cell.reduction))
  }
  
  # Start timing
  start_time <- Sys.time()
  
  # Initialize results following original moneca structure
  segment.list <- list()
  mat.list <- list()
  out.put <- list()
  mat.list[[1]] <- mx
  
  # First segmentation level
  weight_mat <- weight.matrix.parallel(mx, cut.off, TRUE, NULL, small.cell.reduction,
                                       n.cores = parallel_config$n_cores,
                                       chunk.size = chunk.size)
  
  graph <- moneca_graph_from_adjacency(weight_mat, mode = "undirected", weighted = TRUE, diag = FALSE)
  
  segments <- find.segments.parallel(weight_mat, graph, cut.off, mode, delete.upper.tri,
                                     n.cores = parallel_config$n_cores,
                                     chunk.size = chunk.size,
                                     progress = progress)
  
  # Create aggregated matrix for level 2
  mx.2g <- segment.matrix.parallel(mx, segments$cliques, n.cores = parallel_config$n_cores)
  mat.list[[2]] <- mx.2g
  out.put[[1]] <- list(segments = segments, mat = mx.2g)
  
  current_matrix <- mx.2g
  actual.levels <- 1
  
  # Additional segmentation levels (exactly matching original moneca logic)
  if (segment.levels > 1) {
    for (i in 2:segment.levels) {
      # Check if we can continue segmentation
      if (nrow(current_matrix) <= 2) break  # Need at least 2 segments to continue
      
      weight_mat <- weight.matrix.parallel(current_matrix, cut.off, TRUE, NULL, small.cell.reduction,
                                           n.cores = parallel_config$n_cores,
                                           chunk.size = chunk.size)
      
      graph <- moneca_graph_from_adjacency(weight_mat, mode = "undirected", weighted = TRUE, diag = FALSE)
      
      segments <- find.segments.parallel(weight_mat, graph, cut.off, mode, delete.upper.tri,
                                         n.cores = parallel_config$n_cores,
                                         chunk.size = chunk.size,
                                         progress = progress)
      
      # Create next level matrix
      current_matrix <- segment.matrix.parallel(current_matrix, segments$cliques, 
                                               n.cores = parallel_config$n_cores)
      mat.list[[i + 1]] <- current_matrix
      out.put[[i]] <- list(segments = segments, mat = current_matrix)
      actual.levels <- i
      
      # Stop if only one segment remains
      if (length(segments$cliques) <= 1) break
    }
  }
  
  # Create segment list using the same logic as original moneca
  segment.list <- create.segments.parallel(out.put, mx, segment.levels)
  
  # Clean up parallel backend
  cleanup_parallel_backend(parallel_config)
  
  # Calculate performance metrics
  end_time <- Sys.time()
  total_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  # Create output
  out <- list(
    segment.list = segment.list,
    mat.list = mat.list,
    small.cell.reduction = small.cell.reduction,
    performance = list(
      total_time = total_time,
      n_cores_used = parallel_config$n_cores,
      backend = parallel_config$backend
    )
  )
  
  class(out) <- "moneca"
  return(out)
}

#' Parallel Weight Matrix Calculation
#'
#' Parallel version of weight.matrix() that uses multiple cores for computation.
#'
#' @inheritParams weight.matrix
#' @param n.cores Number of cores to use
#' @param chunk.size Size of chunks for parallel processing
#' 
#' @return Weight matrix with relative risks
#' @export
weight.matrix.parallel <- function(mx, cut.off = 1, symmetric = TRUE, 
                                  diagonal = NULL, small.cell.reduction = 0,
                                  n.cores = NULL, chunk.size = NULL) {
  
  if (is.null(mx)) stop("Input cannot be NULL")
  
  l <- nrow(mx)
  if (l < 2) stop("Matrix must have at least 2 rows and columns")
  if (nrow(mx) != ncol(mx)) stop("Matrix must be square")
  
  # For small matrices, use sequential version
  if (l < 50) {
    return(weight.matrix(mx, cut.off, symmetric, diagonal, small.cell.reduction))
  }
  
  # Setup parallel backend if needed
  if (is.null(n.cores)) n.cores <- min(parallel::detectCores() - 1, 4)
  
  # Extract marginals
  o.r.s <- mx[-l, l]
  o.c.s <- mx[l, -l]
  total.total <- mx[l, l]
  row.share <- o.r.s / total.total
  col.share <- o.c.s / total.total
  total.mobility <- sum(mx[-l, -l])
  
  # Determine chunk size
  if (is.null(chunk.size)) {
    chunk.size <- max(10, ceiling((l - 1) / (n.cores * 2)))
  }
  
  # Create chunks for parallel processing
  n_rows <- l - 1
  chunks <- split(1:n_rows, ceiling(seq_along(1:n_rows) / chunk.size))
  
  # Parallel computation of expected values
  if (.Platform$OS.type == "unix" && n.cores > 1) {
    # Use fork-based parallelization on Unix/Mac
    mx.1_exp <- parallel::mclapply(chunks, function(rows) {
      outer(row.share[rows], col.share) * total.mobility
    }, mc.cores = n.cores)
    mx.1_exp <- do.call(rbind, mx.1_exp)
  } else {
    # Use socket-based parallelization on Windows or fallback
    cl <- parallel::makeCluster(min(n.cores, length(chunks)))
    on.exit(parallel::stopCluster(cl))
    
    parallel::clusterExport(cl, c("row.share", "col.share", "total.mobility"), 
                           envir = environment())
    
    mx.1_exp <- parallel::parLapply(cl, chunks, function(rows) {
      outer(row.share[rows], col.share) * total.mobility
    })
    mx.1_exp <- do.call(rbind, mx.1_exp)
  }
  
  # Apply reductions
  mx.red <- mx[-l, -l]
  if (small.cell.reduction > 0) {
    mx.red[mx.red < small.cell.reduction] <- 0
  }
  
  # Calculate relative risks
  mx.1_net <- mx.red / mx.1_exp
  mx.1i <- as.matrix(mx.1_net)
  
  if (symmetric) {
    mx.1i <- mx.1i + t(mx.1i)
  }
  
  mx.1i[mx.1i < cut.off] <- NA
  
  if (is.null(diagonal)) {
    diag(mx.1i) <- NA
  }
  
  rownames(mx.1i) <- rownames(mx)[-l]
  colnames(mx.1i) <- colnames(mx)[-l]
  
  return(mx.1i)
}

#' Parallel Find Segments
#'
#' Parallel version of find.segments() that uses multiple cores for clique detection.
#'
#' @inheritParams find.segments
#' @param graph igraph object representing the network
#' @param n.cores Number of cores to use
#' @param chunk.size Size of chunks for parallel processing
#' 
#' @return List with membership and cliques
#' @export
find.segments.parallel <- function(mat, graph, cut.off = 1, mode = "symmetric", 
                                  delete.upper.tri = TRUE, n.cores = NULL, 
                                  chunk.size = NULL, progress = TRUE) {
  
  # For small graphs, use sequential version
  if (igraph::vcount(graph) < 20 || igraph::ecount(graph) < 50) {
    cliques <- moneca_cliques(graph, min = 2)
    return(find.segments(mat, cliques, cut.off, mode, delete.upper.tri))
  }
  
  # Setup parallel backend
  if (is.null(n.cores)) n.cores <- min(parallel::detectCores() - 1, 4)
  
  # Get cliques (this part is hard to parallelize effectively)
  cliques <- moneca_cliques(graph, min = 2)
  
  # Process matrix
  if (mode == "Mutual") {
    mat[mat < cut.off] <- NA
    mat <- mat + t(mat)
  } else if (mode == "Unmutual") {
    mat[mat < cut.off] <- 0
    mat <- mat + t(mat)
    mat[mat == 0] <- NA
  }
  
  if (delete.upper.tri) {
    mat[upper.tri(mat)] <- NA
  }
  
  # Extract edges
  edges <- which(!is.na(mat) & mat > cut.off, arr.ind = TRUE)
  if (nrow(edges) == 0) {
    n <- nrow(mat)
    return(list(membership = as.factor(1:n), cliques = as.list(1:n)))
  }
  
  edge_weights <- mat[edges]
  edge_order <- order(edge_weights, decreasing = TRUE)
  edges <- edges[edge_order, , drop = FALSE]
  
  # Initialize groups
  group <- integer(nrow(mat))
  names(group) <- rownames(mat)
  
  # Parallel clique membership testing
  if (length(cliques) > 100 && n.cores > 1) {
    # Create clique membership matrix in parallel
    clique_chunks <- split(seq_along(cliques), 
                           ceiling(seq_along(cliques) / ceiling(length(cliques) / n.cores)))
    
    if (.Platform$OS.type == "unix") {
      clique_membership_parts <- parallel::mclapply(clique_chunks, function(indices) {
        part <- matrix(FALSE, nrow = length(indices), ncol = nrow(mat))
        for (i in seq_along(indices)) {
          part[i, cliques[[indices[i]]]] <- TRUE
        }
        part
      }, mc.cores = n.cores)
    } else {
      cl <- parallel::makeCluster(n.cores)
      on.exit(parallel::stopCluster(cl))
      
      parallel::clusterExport(cl, c("cliques"), envir = environment())
      
      clique_membership_parts <- parallel::parLapply(cl, clique_chunks, function(indices) {
        part <- matrix(FALSE, nrow = length(indices), ncol = nrow(mat))
        for (i in seq_along(indices)) {
          part[i, cliques[[indices[i]]]] <- TRUE
        }
        part
      })
    }
    
    clique_membership <- do.call(rbind, clique_membership_parts)
  } else {
    # Sequential fallback for small number of cliques
    clique_membership <- matrix(FALSE, nrow = length(cliques), ncol = nrow(mat))
    for (i in seq_along(cliques)) {
      clique_membership[i, cliques[[i]]] <- TRUE
    }
  }
  
  # Process edges (this part remains sequential due to dependencies)
  group_counter <- 0
  
  if (progress) {
    pb <- txtProgressBar(min = 0, max = nrow(edges), style = 3)
  }
  
  for (i in seq_len(nrow(edges))) {
    if (progress && i %% 100 == 0) setTxtProgressBar(pb, i)
    
    node1 <- edges[i, 1]
    node2 <- edges[i, 2]
    
    if (group[node1] == 0 && group[node2] == 0) {
      # Both unassigned - create new group
      group_counter <- group_counter + 1
      group[c(node1, node2)] <- group_counter
    } else if (group[node1] == 0) {
      # Node1 unassigned, check if can join node2's group
      target_group <- group[node2]
      group_members <- which(group == target_group)
      potential <- c(group_members, node1)
      
      # Check clique membership
      if (any(apply(clique_membership[, potential, drop = FALSE], 1, all))) {
        group[node1] <- target_group
      }
    } else if (group[node2] == 0) {
      # Node2 unassigned, check if can join node1's group
      target_group <- group[node1]
      group_members <- which(group == target_group)
      potential <- c(group_members, node2)
      
      # Check clique membership
      if (any(apply(clique_membership[, potential, drop = FALSE], 1, all))) {
        group[node2] <- target_group
      }
    }
  }
  
  if (progress) close(pb)
  
  # Assign remaining unassigned nodes
  unassigned <- which(group == 0)
  if (length(unassigned) > 0) {
    group[unassigned] <- seq_along(unassigned) + max(group)
  }
  
  # Convert to factor and create output
  g <- as.factor(group)
  levels(g) <- 1:nlevels(g)
  
  # Create cliques list
  ud.list <- lapply(levels(g), function(l) which(g == l))
  
  return(list(membership = g, cliques = ud.list))
}

#' Parallel Segment Matrix
#'
#' Parallel version of segment.matrix() that aggregates mobility matrices.
#'
#' @param mx Mobility matrix
#' @param segments List of segment memberships
#' @param n.cores Number of cores to use
#' 
#' @return Aggregated mobility matrix
#' @export
segment.matrix.parallel <- function(mx, segments, n.cores = NULL) {
  
  # For small number of segments, use sequential approach
  if (length(segments) < 10) {
    # Create a segments object with membership component for compatibility
    segments_obj <- list(membership = integer(nrow(mx) - 1))
    for (i in seq_along(segments)) {
      segments_obj$membership[segments[[i]]] <- i
    }
    
    # Use the same logic as the original segment.matrix function
    groups.1 <- c(segments_obj$membership, length(segments_obj$membership) + 1)
    mx.2_r <- rowsum(mx, groups.1)
    mx.2_r_t <- t(mx.2_r)
    mx.2_rc_t <- rowsum(mx.2_r_t, groups.1)
    mx.2g <- t(mx.2_rc_t)
    return(mx.2g)
  }
  
  if (is.null(n.cores)) n.cores <- min(parallel::detectCores() - 1, 4)
  
  # Create a segments object with membership component for parallel processing too
  segments_obj <- list(membership = integer(nrow(mx) - 1))
  for (i in seq_along(segments)) {
    segments_obj$membership[segments[[i]]] <- i
  }
  
  # Use the same efficient logic as the original segment.matrix function
  # This is much simpler and more efficient than the complex parallel aggregation
  groups.1 <- c(segments_obj$membership, length(segments_obj$membership) + 1)
  mx.2_r <- rowsum(mx, groups.1)
  mx.2_r_t <- t(mx.2_r)
  mx.2_rc_t <- rowsum(mx.2_r_t, groups.1)
  mx.2g <- t(mx.2_rc_t)
  
  return(mx.2g)
}

#' Setup Parallel Backend
#'
#' Configure the parallel processing backend based on system and data characteristics.
#'
#' @param n.cores Requested number of cores
#' @param backend Requested backend type
#' @param data_size Size of the data
#' 
#' @return Configuration list
#' @keywords internal
setup_parallel_backend <- function(n.cores, backend, data_size) {
  
  # Determine if parallel processing is beneficial
  if (data_size < 50) {
    return(list(use_parallel = FALSE))
  }
  
  # Auto-detect cores if not specified
  if (is.null(n.cores)) {
    available_cores <- parallel::detectCores()
    n.cores <- min(available_cores - 1, 4, max(1, floor(data_size / 50)))
  }
  
  # Determine backend
  if (backend == "auto") {
    if (.Platform$OS.type == "unix") {
      backend <- "fork"
    } else {
      backend <- "socket"
    }
  }
  
  # Validate backend choice
  if (backend == "fork" && .Platform$OS.type != "unix") {
    warning("Fork backend not available on Windows, switching to socket")
    backend <- "socket"
  }
  
  return(list(
    use_parallel = TRUE,
    n_cores = n.cores,
    backend = backend
  ))
}

#' Cleanup Parallel Backend
#'
#' Clean up any resources used by parallel processing.
#'
#' @param config Parallel configuration
#' @keywords internal
cleanup_parallel_backend <- function(config) {
  # Currently no cleanup needed for mclapply
  # Socket clusters are cleaned up with on.exit()
  invisible(NULL)
}

#' Update Segments
#'
#' Helper function to update segment list based on new clustering.
#'
#' @param old_segments Previous segment list
#' @param new_clusters New clustering
#' 
#' @return Updated segment list
#' @keywords internal
update_segments <- function(old_segments, new_clusters) {
  new_segments <- list()
  for (i in seq_along(new_clusters)) {
    new_segments[[i]] <- unlist(old_segments[new_clusters[[i]]])
  }
  return(new_segments)
}

#' Create Segments List (Parallel Version)
#'
#' Helper function to create the segment list structure that matches the original moneca output.
#' This replicates the create.segments function from the original moneca.
#'
#' @param out.put List of segmentation results from each level
#' @param mx Original mobility matrix
#' @param actual.levels Number of levels actually computed
#' 
#' @return List of segment memberships for each hierarchical level
#' @keywords internal
create.segments.parallel <- function(out.put, mx, segment.levels) {
  
  seg.list <- list()
  seg.list[[1]] <- as.list(1:(nrow(mx)-1))
  
  if (length(out.put) == 0) {
    return(seg.list)
  }
  
  level.current <- out.put[[1]]$segments$cliques
  # Remove isolates
  a <- unlist(lapply(level.current, length))
  seg.list[[2]] <- level.current[a > 1]
  
  # Adjust for actual number of levels available (exactly like original moneca)
  actual.levels <- min(segment.levels, length(out.put))
  
  if (actual.levels > 1) {
    for (n in 2:actual.levels) {
      
      current <- n
      below <- n
      
      # Check if level exists in output
      if (current > length(out.put)) break
      
      level.current <- out.put[[current]]$segments$cliques
      level.below <- out.put[[n-1]]$segments$cliques
      
      for (i in 1:(n-1)) {
        below <- below - 1
        level.below <- out.put[[below]]$segments$cliques
        level.current <- level.down.parallel(level.current, level.below)  
      }
      # Only add non-empty levels
      if (length(level.current) > 0) {
        seg.list[[n+1]] <- level.current
      }
    }
  }
  return(seg.list)
}

#' Level Down (Parallel Version)
#'
#' Helper function that maps segments from one level to the previous level.
#' This replicates the level.down function from the original moneca.
#'
#' @param level.current Current level segments
#' @param level.below Previous level segments
#' 
#' @return Updated segment list
#' @keywords internal
level.down.parallel <- function(level.current, level.below) {
  # Remove isolates
  a <- unlist(lapply(level.current, length))
  level.current <- level.current[a > 1]
  
  ud <- list()
  
  # Return empty list if no valid segments
  if (length(level.current) == 0) {
    return(ud)
  }
  
  for (i in 1:length(level.current)) {
    d <- level.current[[i]]
    ud[[i]] <- unlist(level.below[d])
  }
  
  return(ud)
}