#' Fast moneca - Optimized Mobility Network Clustering Analysis
#'
#' An optimized version of the moneca algorithm with improved performance for large datasets.
#' Uses maximal cliques instead of all cliques and implements various performance optimizations.
#'
#' @param mx A mobility table (square matrix) with row and column totals in the last
#'   row/column. Row names should identify the categories/classes.
#' @param segment.levels Integer specifying the number of hierarchical segmentation 
#'   levels to compute. Default is 3.
#' @param cut.off Numeric threshold for the minimum relative risk to be considered
#'   a significant tie. Default is 1.
#' @param mode Character string specifying edge mode. Currently uses symmetric mode.
#' @param delete.upper.tri Logical indicating whether to use only lower triangle. Default is TRUE.
#' @param small.cell.reduction Numeric value to handle small cell counts. Default is 0.
#' @param use.sparse Logical indicating whether to use sparse matrices for large data. Default is FALSE.
#' @param min.density Minimum edge density to continue processing. Default is 0.01.
#' @param max.clique.size Maximum size of cliques to consider (NULL for no limit). Default is NULL.
#' @param progress Logical indicating whether to show progress bars. Default is TRUE.
#' 
#' @return An object of class "moneca" with the same structure as the original moneca() function.
#' 
#' @export
moneca_fast <- function(mx = mx, 
                       segment.levels = 3, 
                       cut.off = 1, 
                       mode = "symmetric", 
                       delete.upper.tri = TRUE, 
                       small.cell.reduction = 0,
                       use.sparse = FALSE,
                       min.density = 0.01,
                       max.clique.size = NULL,
                       progress = TRUE,
                       auto_tune = FALSE, 
                       tune_method = "stability", 
                       tune_verbose = FALSE) {
  
  # Convert to sparse matrix if requested and Matrix package is available
  if (use.sparse && requireNamespace("Matrix", quietly = TRUE)) {
    if (!inherits(mx, "sparseMatrix")) {
      mx <- Matrix::Matrix(mx, sparse = TRUE)
    }
  } else if (!is.matrix(mx)) {
    mx <- as.matrix(mx)
  }
  
  # Fast weight matrix calculation with caching
  weight.matrix.fast <- function(mx, cut.off = 1, symmetric = TRUE, diagonal = NULL, small.cell.reduction = 0) {
    # Input validation
    if (is.null(mx)) stop("Input cannot be NULL")
    
    l <- nrow(mx)
    if (l < 2) stop("Matrix must have at least 2 rows and columns")
    if (nrow(mx) != ncol(mx)) stop("Matrix must be square")
    
    # Use vectorized operations
    o.r.s <- mx[-l, l]
    o.c.s <- mx[l, -l]
    total.total <- mx[l, l]
    row.share <- o.r.s / total.total
    col.share <- o.c.s / total.total
    total.mobility <- sum(mx[-l, -l])
    
    # Vectorized expected calculation
    mx.1_exp <- outer(row.share, col.share) * total.mobility
    
    mx.red <- mx[-l, -l]
    if (small.cell.reduction > 0) {
      mx.red[mx.red < small.cell.reduction] <- 0
    }
    
    mx.1_net <- mx.red / mx.1_exp
    mx.1i <- as.matrix(mx.1_net)
    
    if (symmetric) {
      mx.1i <- mx.1i + t(mx.1i)
    }
    
    mx.1i[mx.1i < cut.off] <- NA
    
    if (is.null(diagonal)) {
      diag(mx.1i) <- NA
    }
    
    return(mx.1i)
  }
  
  # Optimized segment finding
  find.segments.fast <- function(mat, graph, cut.off = 1, mode = "symmetric", delete.upper.tri = TRUE, progress = TRUE) {
    
    # Early stopping if graph is too sparse
    density <- igraph::edge_density(graph)
    if (density < min.density) {
      n <- nrow(mat)
      out <- list(
        membership = as.factor(1:n),
        cliques = as.list(1:n)
      )
      return(out)
    }
    
    # Use maximal cliques instead of all cliques
    if (is.null(max.clique.size)) {
      cliques <- moneca_max_cliques(graph, min = 2)
    } else {
      cliques <- moneca_max_cliques(graph, min = 2, max = max.clique.size)
    }
    
    # If no cliques found, return trivial segmentation
    if (length(cliques) == 0) {
      n <- nrow(mat)
      out <- list(
        membership = as.factor(1:n),
        cliques = as.list(1:n)
      )
      return(out)
    }
    
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
    
    # Vectorized edge extraction and sorting
    edges <- which(!is.na(mat) & mat > cut.off, arr.ind = TRUE)
    if (nrow(edges) == 0) {
      n <- nrow(mat)
      out <- list(
        membership = as.factor(1:n),
        cliques = as.list(1:n)
      )
      return(out)
    }
    
    edge_weights <- mat[edges]
    edge_order <- order(edge_weights, decreasing = TRUE)
    edges <- edges[edge_order, , drop = FALSE]
    
    # Initialize groups
    group <- integer(nrow(mat))
    names(group) <- rownames(mat)
    
    # Vectorized clique membership testing
    clique_membership <- matrix(FALSE, nrow = length(cliques), ncol = nrow(mat))
    for (i in seq_along(cliques)) {
      clique_membership[i, cliques[[i]]] <- TRUE
    }
    
    # Process edges
    group_counter <- 0
    
    if (progress) {
      pb <- txtProgressBar(min = 0, max = nrow(edges), style = 3)
    }
    
    for (i in seq_len(nrow(edges))) {
      if (progress) setTxtProgressBar(pb, i)
      
      edge <- edges[i, ]
      current_groups <- group[edge]
      
      # Both nodes unassigned
      if (all(current_groups == 0)) {
        group_counter <- group_counter + 1
        group[edge] <- group_counter
      } else if (any(current_groups != 0)) {
        # At least one node is assigned
        assigned_groups <- unique(current_groups[current_groups != 0])
        
        # Find all nodes in these groups
        group_members <- which(group %in% assigned_groups)
        potential_clique <- unique(c(group_members, edge))
        
        # Fast clique test using pre-computed membership
        is_clique <- any(apply(clique_membership[, potential_clique, drop = FALSE], 1, all))
        
        if (is_clique) {
          # Merge groups - use the smallest group number
          target_group <- min(assigned_groups)
          group[potential_clique] <- target_group
        }
      }
    }
    
    if (progress) close(pb)
    
    # Assign remaining unassigned nodes
    unassigned <- which(group == 0)
    if (length(unassigned) > 0) {
      group[unassigned] <- seq_along(unassigned) + max(group)
    }
    
    # Renumber groups consecutively
    unique_groups <- sort(unique(group))
    group <- match(group, unique_groups)
    
    g <- as.factor(group)
    
    # Create clique list
    ud.list <- split(seq_along(g), g)
    
    out <- list(
      membership = g,
      cliques = ud.list
    )
    
    return(out)
  }
  
  # Fast segment matrix aggregation
  segment.matrix.fast <- function(mx, segments) {
    # Use standard aggregation
    groups.1 <- c(segments$membership, length(segments$membership) + 1)
    
    # Convert to regular matrix if sparse (rowsum doesn't work with sparse matrices)
    if (inherits(mx, "sparseMatrix")) {
      mx <- as.matrix(mx)
    }
    
    # Standard aggregation
    mx.2_r <- rowsum(mx, groups.1)
    mx.2_r_t <- t(mx.2_r)
    mx.2_rc_t <- rowsum(mx.2_r_t, groups.1)
    mx.2g <- t(mx.2_rc_t)
    
    return(mx.2g)
  }
  
  # Optimized level.down function
  level.down.fast <- function(level.current, level.below) {
    # Remove isolates
    lengths <- lengths(level.current)
    level.current <- level.current[lengths > 1]
    
    if (length(level.current) == 0) return(list())
    
    # Vectorized operation
    ud <- lapply(level.current, function(d) unlist(level.below[d]))
    return(ud)
  }
  
  # Main segmentation function
  make.segments.fast <- function(mx, cut.off = 1, mode = mode, delete.upper.tri = delete.upper.tri, 
                                small.cell.reduction = small.cell.reduction) {
    
    mx.1i <- weight.matrix(mx, cut.off, small.cell.reduction = small.cell.reduction,
                           auto_tune = auto_tune, tune_method = tune_method, tune_verbose = tune_verbose)
    
    # Create graph
    mx.1i.graph <- mx.1i
    mx.1i.graph[is.na(mx.1i.graph)] <- 0
    
    gra.1ii <- moneca_graph_from_adjacency(
      adjmatrix = mx.1i.graph, 
      mode = "undirected", 
      weighted = TRUE, 
      diag = FALSE
    )
    
    # Early stopping based on edge density
    if (igraph::edge_density(gra.1ii) < min.density) {
      n <- nrow(mx.1i)
      return(list(
        membership = as.factor(1:n),
        cliques = as.list(1:n)
      ))
    }
    
    clust.1 <- find.segments.fast(mx.1i, gra.1ii, cut.off = cut.off, progress = progress)
    
    return(clust.1)
  }
  
  # Create segments function (optimized)
  create.segments.fast <- function(out.put, mx) {
    seg.list <- list()
    seg.list[[1]] <- as.list(1:(nrow(mx) - 1))
    
    if (length(out.put) == 0) return(seg.list)
    
    level.current <- out.put[[1]]$segments$cliques
    lengths <- lengths(level.current)
    seg.list[[2]] <- level.current[lengths > 1]
    
    actual.levels <- min(segment.levels, length(out.put))
    
    if (actual.levels > 1) {
      for (n in 2:actual.levels) {
        if (n > length(out.put)) break
        
        level.current <- out.put[[n]]$segments$cliques
        
        # Optimized level down calculation
        for (i in seq_len(n - 1)) {
          level.below <- out.put[[n - i]]$segments$cliques
          level.current <- level.down.fast(level.current, level.below)
        }
        
        if (length(level.current) > 0) {
          seg.list[[n + 1]] <- level.current
        }
      }
    }
    
    return(seg.list)
  }
  
  # Main algorithm
  mat.list <- list()
  mat.list[[1]] <- mx
  
  segments <- make.segments.fast(
    mx, 
    cut.off = cut.off, 
    mode = mode, 
    delete.upper.tri = delete.upper.tri, 
    small.cell.reduction = small.cell.reduction
  )
  
  mx.2g <- segment.matrix.fast(mx, segments)
  mat.list[[2]] <- mx.2g
  
  out.put <- list()
  out.put[[1]] <- list(segments = segments, mat = mx.2g)
  
  # Early stopping if only one segment
  if (length(segments$cliques) <= 1) {
    segment.list <- create.segments.fast(out.put, mx)
    out <- list(
      segment.list = segment.list, 
      mat.list = mat.list, 
      small.cell.reduction = small.cell.reduction
    )
    class(out) <- "moneca"
    return(out)
  }
  
  # Continue segmentation
  if (segment.levels > 1) {
    for (i in 2:segment.levels) {
      segments <- make.segments.fast(
        mx.2g, 
        cut.off = cut.off, 
        mode = mode, 
        delete.upper.tri = delete.upper.tri, 
        small.cell.reduction = small.cell.reduction
      )
      
      mx.2g <- segment.matrix.fast(mx.2g, segments)
      mat.list[[i + 1]] <- mx.2g
      out.put[[i]] <- list(segments = segments, mat = mx.2g)
      
      # Stop if only one segment remains or density too low
      if (length(segments$cliques) <= 1) {
        break
      }
    }
  }
  
  # Create final segments
  segment.list <- create.segments.fast(out.put, mx)
  
  # Create output
  out <- list(
    segment.list = segment.list, 
    mat.list = mat.list, 
    small.cell.reduction = small.cell.reduction
  )
  
  class(out) <- "moneca"
  
  return(out)
}

#' Export fast moneca functions for use in package
#' @keywords internal
moneca_graph_from_adjacency <- function(...) {
  if (exists(".moneca_env") && exists("moneca_graph_from_adjacency", envir = .moneca_env)) {
    .moneca_env$moneca_graph_from_adjacency(...)
  } else {
    # Fallback to igraph function
    if (utils::packageVersion("igraph") >= "1.3.0") {
      igraph::graph_from_adjacency_matrix(...)
    } else {
      igraph::graph.adjacency(...)
    }
  }
}