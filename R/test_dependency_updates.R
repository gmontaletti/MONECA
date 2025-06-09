#' Test framework for dependency updates
#' 
#' This script provides comprehensive testing for the igraph migration
#' and dependency updates to ensure compatibility and functionality
#' 
#' @keywords internal

#' Test igraph compatibility layer
#' 
#' @return List with test results
#' @export
test_igraph_compatibility <- function() {
  results <- list()
  
  # Test 1: Graph creation compatibility
  tryCatch({
    # Create a simple adjacency matrix
    adj_mat <- matrix(c(0, 1, 1, 0), nrow = 2)
    
    # Test old style (should work via compatibility)
    g1 <- moneca_graph_from_adjacency(adj_mat, mode = "undirected", weighted = NULL)
    
    # Verify graph properties
    results$graph_creation <- list(
      success = inherits(g1, "igraph"),
      vertices = igraph::vcount(g1),
      edges = igraph::ecount(g1)
    )
  }, error = function(e) {
    results$graph_creation <- list(success = FALSE, error = e$message)
  })
  
  # Test 2: Edge list compatibility
  tryCatch({
    edges <- moneca_get_edgelist(g1)
    results$edgelist <- list(
      success = is.matrix(edges),
      dimensions = dim(edges)
    )
  }, error = function(e) {
    results$edgelist <- list(success = FALSE, error = e$message)
  })
  
  # Test 3: Graph density compatibility
  tryCatch({
    density <- moneca_graph_density(g1)
    results$density <- list(
      success = is.numeric(density),
      value = density
    )
  }, error = function(e) {
    results$density <- list(success = FALSE, error = e$message)
  })
  
  # Test 4: Norm coords compatibility
  tryCatch({
    layout <- matrix(runif(4), ncol = 2)
    normalized <- moneca_norm_coords(layout)
    results$norm_coords <- list(
      success = is.matrix(normalized),
      in_range = all(normalized >= -1 & normalized <= 1)
    )
  }, error = function(e) {
    results$norm_coords <- list(success = FALSE, error = e$message)
  })
  
  # Summary
  results$summary <- list(
    total_tests = length(results) - 1,
    passed = sum(sapply(results[names(results) != "summary"], 
                       function(x) isTRUE(x$success)))
  )
  
  return(results)
}

#' Analyze package dependencies usage
#' 
#' @return Data frame with dependency analysis
#' @export
analyze_package_dependencies <- function() {
  r_files <- list.files("R", pattern = "\\.R$", full.names = TRUE)
  
  # Define patterns for each package
  dep_patterns <- list(
    igraph = c("graph", "vertex", "edge", "clique", "degree", "vcount", 
               "ecount", "layout", "E\\(", "V\\("),
    ggplot2 = c("ggplot", "geom_", "aes", "theme", "scale_", "coord_", 
                "facet_", "labs", "ggtitle"),
    RColorBrewer = c("brewer\\.pal", "display\\.brewer", "brewer\\.pal\\.info"),
    scales = c("alpha", "rescale", "percent", "comma", "dollar"),
    grid = c("grid\\.", "viewport", "pushViewport", "popViewport", "unit"),
    toOrdinal = c("toOrdinal")
  )
  
  # Initialize results
  dep_usage <- data.frame(
    package = character(),
    function_pattern = character(),
    file = character(),
    count = integer(),
    stringsAsFactors = FALSE
  )
  
  # Analyze each file
  for (file in r_files) {
    content <- readLines(file)
    
    for (pkg in names(dep_patterns)) {
      for (pattern in dep_patterns[[pkg]]) {
        matches <- grep(pattern, content, ignore.case = FALSE)
        if (length(matches) > 0) {
          dep_usage <- rbind(dep_usage, data.frame(
            package = pkg,
            function_pattern = pattern,
            file = basename(file),
            count = length(matches),
            stringsAsFactors = FALSE
          ))
        }
      }
    }
  }
  
  return(dep_usage)
}

#' Validate DESCRIPTION file dependencies
#' 
#' @return List with validation results
#' @export
validate_description_dependencies <- function() {
  desc_file <- "DESCRIPTION"
  if (!file.exists(desc_file)) {
    return(list(success = FALSE, error = "DESCRIPTION file not found"))
  }
  
  # Read DESCRIPTION
  desc_content <- readLines(desc_file)
  
  # Extract dependencies
  depends_line <- grep("^Depends:", desc_content)
  imports_line <- grep("^Imports:", desc_content)
  suggests_line <- grep("^Suggests:", desc_content)
  
  results <- list()
  
  # Check R version requirement
  r_version_match <- grep("R \\(>= [0-9.]+\\)", desc_content, value = TRUE)
  if (length(r_version_match) > 0) {
    r_version <- gsub(".*R \\(>= ([0-9.]+)\\).*", "\\1", r_version_match[1])
    results$r_version <- list(
      specified = r_version,
      current = paste(R.version$major, R.version$minor, sep = "."),
      compatible = utils::compareVersion(paste(R.version$major, R.version$minor, sep = "."), 
                                       r_version) >= 0
    )
  }
  
  # Check package versions
  import_packages <- c("ggplot2", "igraph", "RColorBrewer", "scales", "toOrdinal")
  for (pkg in import_packages) {
    if (requireNamespace(pkg, quietly = TRUE)) {
      results[[pkg]] <- list(
        installed = TRUE,
        version = as.character(utils::packageVersion(pkg))
      )
    } else {
      results[[pkg]] <- list(
        installed = FALSE,
        version = NA
      )
    }
  }
  
  return(results)
}

#' Generate comprehensive dependency report
#' 
#' @param output_file Optional file path to save the report
#' @return Invisible NULL
#' @export
generate_dependency_report <- function(output_file = NULL) {
  cat("MONECA Package Dependency Update Report\n")
  cat("=======================================\n\n")
  
  # 1. Test compatibility layer
  cat("1. igraph Compatibility Layer Tests\n")
  cat("-----------------------------------\n")
  compat_results <- test_igraph_compatibility()
  cat(sprintf("Total tests: %d, Passed: %d\n", 
              compat_results$summary$total_tests,
              compat_results$summary$passed))
  
  for (test_name in names(compat_results)) {
    if (test_name != "summary") {
      cat(sprintf("  - %s: %s\n", test_name, 
                  ifelse(compat_results[[test_name]]$success, "PASS", "FAIL")))
    }
  }
  
  # 2. Dependency usage analysis
  cat("\n2. Package Dependency Usage\n")
  cat("---------------------------\n")
  dep_usage <- analyze_package_dependencies()
  pkg_summary <- aggregate(count ~ package, data = dep_usage, sum)
  for (i in 1:nrow(pkg_summary)) {
    cat(sprintf("  - %s: %d uses across %d files\n", 
                pkg_summary$package[i], 
                pkg_summary$count[i],
                length(unique(dep_usage$file[dep_usage$package == pkg_summary$package[i]]))))
  }
  
  # 3. DESCRIPTION validation
  cat("\n3. DESCRIPTION File Validation\n")
  cat("------------------------------\n")
  desc_validation <- validate_description_dependencies()
  
  if (!is.null(desc_validation$r_version)) {
    cat(sprintf("  R Version: Required >= %s, Current = %s (%s)\n",
                desc_validation$r_version$specified,
                desc_validation$r_version$current,
                ifelse(desc_validation$r_version$compatible, "OK", "INCOMPATIBLE")))
  }
  
  cat("\n  Package Status:\n")
  for (pkg in c("ggplot2", "igraph", "RColorBrewer", "scales", "toOrdinal")) {
    if (!is.null(desc_validation[[pkg]])) {
      cat(sprintf("    - %s: %s (v%s)\n", 
                  pkg,
                  ifelse(desc_validation[[pkg]]$installed, "Installed", "Missing"),
                  ifelse(desc_validation[[pkg]]$installed, 
                         desc_validation[[pkg]]$version, "N/A")))
    }
  }
  
  # 4. Migration recommendations
  cat("\n4. Migration Recommendations\n")
  cat("---------------------------\n")
  igraph_usage <- analyze_igraph_usage()
  deprecated_count <- sum(igraph_usage$deprecated)
  
  if (deprecated_count > 0) {
    cat(sprintf("  - Found %d uses of deprecated igraph functions\n", deprecated_count))
    cat("  - Run migrate_all_igraph_functions() to update\n")
  } else {
    cat("  - No deprecated igraph functions found\n")
  }
  
  # Save to file if requested
  if (!is.null(output_file)) {
    sink(output_file)
    generate_dependency_report()
    sink()
    cat(sprintf("\nReport saved to: %s\n", output_file))
  }
  
  invisible(NULL)
}