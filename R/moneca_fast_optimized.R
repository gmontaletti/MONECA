#' Fast moneca - Optimized Mobility Network Clustering Analysis (FIXED)
#'
#' An optimized version of the moneca algorithm with improved performance for large datasets.
#' This version fixes the performance issue with high-density graphs by using a more efficient
#' clique-finding strategy.
#'
#' @inheritParams moneca_fast
#' @export
moneca_fast_fixed <- function(mx = mx, 
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
  
  # Convert to sparse matrix if requested
  if (use.sparse && requireNamespace("Matrix", quietly = TRUE)) {
    if (!inherits(mx, "sparseMatrix")) {
      mx <- Matrix::Matrix(mx, sparse = TRUE)
    }
  } else if (!is.matrix(mx)) {
    mx <- as.matrix(mx)
  }
  
  # Fast weight matrix calculation
  weight.matrix.fast <- function(mx, cut.off = 1, symmetric = TRUE, diagonal = NULL, small.cell.reduction = 0) {
    if (is.null(mx)) stop("Input cannot be NULL")
    
    l <- nrow(mx)
    if (l < 2) stop("Matrix must have at least 2 rows and columns")
    if (nrow(mx) != ncol(mx)) stop("Matrix must be square")
    
    o.r.s <- mx[-l, l]
    o.c.s <- mx[l, -l]
    total.total <- mx[l, l]
    row.share <- o.r.s / total.total
    col.share <- o.c.s / total.total
    total.mobility <- sum(mx[-l, -l])
    
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
  
  # OPTIMIZED segment finding - key fix is here
  find.segments.fast <- function(mat, graph, cut.off = 1, mode = "symmetric", delete.upper.tri = TRUE, progress = TRUE) {
    
    # Early stopping using strength-based density
    strengths <- igraph::strength(graph, mode = "all")
    if (length(strengths) > 0 && any(strengths > 0)) {
      # Use relative strength: mean relative to max observed
      strength_density <- mean(strengths) / max(strengths)
    } else {
      strength_density <- 0
    }
    
    if (strength_density < min.density) {
      n <- nrow(mat)
      return(list(membership = as.factor(1:n), cliques = as.list(1:n)))
    }
    
    # Keep edge density for clique strategy decision
    edge_density <- igraph::edge_density(graph)
    
    # CRITICAL FIX: For high-density graphs, use a different strategy
    # Instead of finding all maximal cliques (exponential), use a greedy approach
    if (edge_density > 0.8) {
      # Use the original moneca algorithm approach for dense graphs
      # This processes edges one by one without pre-computing all cliques
      return(find.segments.greedy(mat, cut.off, mode, delete.upper.tri, progress))
    }
    
    # For moderate density, use maximal cliques with size limit
    if (is.null(max.clique.size)) {
      # Limit clique size for dense graphs to prevent explosion
      if (edge_density > 0.5) {
        max.clique.size <- min(10, ceiling(sqrt(vcount(graph))))
      }
      cliques <- moneca_max_cliques(graph, min = 2, max = max.clique.size)
    } else {
      cliques <- moneca_max_cliques(graph, min = 2, max = max.clique.size)
    }
    
    if (length(cliques) == 0) {
      n <- nrow(mat)
      return(list(membership = as.factor(1:n), cliques = as.list(1:n)))
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
    
    # Extract and sort edges
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
    
    # OPTIMIZATION: Only create membership matrix for small number of cliques
    if (length(cliques) < 1000) {
      clique_membership <- matrix(FALSE, nrow = length(cliques), ncol = nrow(mat))
      for (i in seq_along(cliques)) {
        clique_membership[i, cliques[[i]]] <- TRUE
      }
      use_membership_matrix <- TRUE
    } else {
      use_membership_matrix <- FALSE
    }
    
    # Process edges
    group_counter <- 0
    
    if (progress) {
      pb <- txtProgressBar(min = 0, max = nrow(edges), style = 3)
    }
    
    for (i in seq_len(nrow(edges))) {
      if (progress && i %% 100 == 0) setTxtProgressBar(pb, i)
      
      edge <- edges[i, ]
      current_groups <- group[edge]
      
      if (all(current_groups == 0)) {
        group_counter <- group_counter + 1
        group[edge] <- group_counter
      } else if (any(current_groups != 0)) {
        assigned_groups <- unique(current_groups[current_groups != 0])
        group_members <- which(group %in% assigned_groups)
        potential_clique <- unique(c(group_members, edge))
        
        # OPTIMIZATION: Use different test methods based on problem size
        if (use_membership_matrix) {
          # Fast test for small number of cliques
          is_clique <- any(apply(clique_membership[, potential_clique, drop = FALSE], 1, all))
        } else {
          # Direct test for large number of cliques
          is_clique <- test_clique_direct(potential_clique, cliques)
        }
        
        if (is_clique) {
          target_group <- min(assigned_groups)
          group[potential_clique] <- target_group
        }
      }
    }
    
    if (progress) close(pb)
    
    # Assign remaining nodes
    unassigned <- which(group == 0)
    if (length(unassigned) > 0) {
      group[unassigned] <- seq_along(unassigned) + max(group)
    }
    
    # Renumber groups
    unique_groups <- sort(unique(group))
    group <- match(group, unique_groups)
    
    g <- as.factor(group)
    ud.list <- split(seq_along(g), g)
    
    return(list(membership = g, cliques = ud.list))
  }
  
  # NEW: Greedy segmentation for high-density graphs (similar to original moneca)
  find.segments.greedy <- function(mat, cut.off = 1, mode = "symmetric", delete.upper.tri = TRUE, progress = TRUE) {
    
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
    
    # Initialize
    group <- vector(mode = "numeric", length = nrow(mat))
    names(group) <- rownames(mat)
    max.mat <- mat
    
    # Helper function for testing cliques (simplified)
    test_simple_clique <- function(nodes, mat) {
      if (length(nodes) <= 2) return(TRUE)
      # Check if all pairs are connected
      for (i in 1:(length(nodes)-1)) {
        for (j in (i+1):length(nodes)) {
          val1 <- mat[nodes[i], nodes[j]]
          val2 <- mat[nodes[j], nodes[i]]
          if (is.na(val1) && is.na(val2)) return(FALSE)
        }
      }
      return(TRUE)
    }
    
    # Count non-NA edges
    loop.length <- sum(!is.na(max.mat))
    if (loop.length == 0) {
      g <- as.factor(1:nrow(mat))
      ud.list <- as.list(1:nrow(mat))
      return(list(membership = g, cliques = ud.list))
    }
    
    if (progress) {
      pb <- txtProgressBar(min = 0, max = loop.length, style = 3)
    }
    
    for (i in 1:loop.length) {
      if (progress && i %% 100 == 0) setTxtProgressBar(pb, i)
      
      # Find maximum value
      max.ind <- which(max.mat == max(max.mat, na.rm = TRUE), arr.ind = TRUE)[1,]
      max.mat[max.ind[1], max.ind[2]] <- NA
      
      group.candidates <- group[max.ind]
      candidate <- max.ind
      
      if (sum(group.candidates) == 0) {
        group[max.ind] <- i
      } else if (sum(group.candidates) != 0) {
        group.candidates <- group.candidates[group.candidates != 0]
        group.members <- which(group %in% group.candidates)
        group.size <- table(group[group.members])
        group.assigned <- as.numeric(names(group.size))[which.max(group.size)[1]]
        
        potential.clique <- unique(sort(c(group.members, candidate)))
        
        # Simple clique test
        if (test_simple_clique(potential.clique, mat)) {
          group[potential.clique] <- group.assigned
        }
      }
    }
    
    if (progress) close(pb)
    
    # Assign unassigned nodes
    sub <- group[group == 0]
    group[group == 0] <- 1:length(sub) + max(group)
    g <- as.factor(group)
    levels(g) <- 1:nlevels(g)
    
    # Create cliques list
    l <- levels(g)
    ud.list <- list()
    for (i in 1:length(l)) ud.list[[i]] <- which(g == l[i])
    
    return(list(membership = g, cliques = ud.list))
  }
  
  # Helper function for direct clique testing
  test_clique_direct <- function(nodes, cliques) {
    for (clique in cliques) {
      if (all(nodes %in% clique)) return(TRUE)
    }
    return(FALSE)
  }
  
  # Fast segment matrix aggregation
  segment.matrix.fast <- function(mx, segments) {
    groups.1 <- c(segments$membership, length(segments$membership) + 1)
    
    if (inherits(mx, "sparseMatrix")) {
      mx <- as.matrix(mx)
    }
    
    mx.2_r <- rowsum(mx, groups.1)
    mx.2_r_t <- t(mx.2_r)
    mx.2_rc_t <- rowsum(mx.2_r_t, groups.1)
    mx.2g <- t(mx.2_rc_t)
    
    return(mx.2g)
  }
  
  # Optimized level.down
  level.down.fast <- function(level.current, level.below) {
    lengths <- lengths(level.current)
    level.current <- level.current[lengths > 1]
    
    if (length(level.current) == 0) return(list())
    
    ud <- lapply(level.current, function(d) unlist(level.below[d]))
    return(ud)
  }
  
  # Main segmentation function
  make.segments.fast <- function(mx, cut.off = 1, mode = mode, delete.upper.tri = delete.upper.tri, 
                                small.cell.reduction = small.cell.reduction) {
    
    mx.1i <- weight.matrix(mx, cut.off, small.cell.reduction = small.cell.reduction,
                           auto_tune = auto_tune, tune_method = tune_method, tune_verbose = tune_verbose)
    
    mx.1i.graph <- mx.1i
    mx.1i.graph[is.na(mx.1i.graph)] <- 0
    
    gra.1ii <- moneca_graph_from_adjacency(
      adjmatrix = mx.1i.graph, 
      mode = "undirected", 
      weighted = TRUE, 
      diag = FALSE
    )
    
    # Check strength-based density
    strengths <- igraph::strength(gra.1ii, mode = "all")
    if (length(strengths) > 0 && any(strengths > 0)) {
      strength_density <- mean(strengths) / max(strengths)
    } else {
      strength_density <- 0
    }
    
    if (strength_density < min.density) {
      n <- nrow(mx.1i)
      return(list(membership = as.factor(1:n), cliques = as.list(1:n)))
    }
    
    clust.1 <- find.segments.fast(mx.1i, gra.1ii, cut.off = cut.off, progress = progress)
    
    return(clust.1)
  }
  
  # Create segments function
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
      
      if (length(segments$cliques) <= 1) {
        break
      }
    }
  }
  
  segment.list <- create.segments.fast(out.put, mx)
  
  out <- list(
    segment.list = segment.list, 
    mat.list = mat.list, 
    small.cell.reduction = small.cell.reduction
  )
  
  class(out) <- "moneca"
  
  return(out)
}

# Export wrapper functions
moneca_graph_from_adjacency <- function(...) {
  if (utils::packageVersion("igraph") >= "1.3.0") {
    igraph::graph_from_adjacency_matrix(...)
  } else {
    igraph::graph.adjacency(...)
  }
}

moneca_max_cliques <- function(...) {
  if (utils::packageVersion("igraph") >= "1.3.0") {
    igraph::max_cliques(...)
  } else {
    igraph::maximal.cliques(...)
  }
}