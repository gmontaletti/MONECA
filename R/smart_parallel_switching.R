#' Smart Parallel Processing Decision Engine for MONECA
#'
#' This module provides intelligent switching between parallel and sequential
#' processing based on problem size, available resources, and efficiency 
#' considerations. It optimizes performance by making data-driven decisions
#' about when parallelization provides benefits vs. overhead costs.
#'
#' @name smart_parallel_switching
#' @keywords internal
NULL

#' Detect System Resources and Capabilities
#'
#' Analyzes the system's computing resources including CPU cores, memory,
#' and parallel package availability to inform parallelization decisions.
#'
#' @param verbose Logical indicating whether to display detailed resource information.
#'
#' @return List containing system resource information:
#'   \item{total_cores}{Total number of CPU cores available}
#'   \item{usable_cores}{Recommended number of cores for parallel processing}
#'   \item{memory_gb}{Available system memory in GB (approximate)}
#'   \item{has_parallel_packages}{Whether required parallel packages are available}
#'   \item{platform}{Operating system platform}
#'   \item{parallel_backend}{Recommended parallel backend for this system}
#'   \item{max_recommended_cores}{Maximum recommended cores based on system}
#'
#' @details
#' The function performs comprehensive system analysis:
#' - Detects CPU cores and recommends usage (typically total cores - 1)
#' - Estimates available memory for large dataset considerations  
#' - Checks for required packages (foreach, doParallel)
#' - Determines optimal parallel backend (fork vs socket)
#' - Sets platform-specific recommendations
#'
#' @examples
#' \dontrun{
#' # Get system resource information
#' resources <- detect_system_resources(verbose = TRUE)
#' print(resources)
#' 
#' # Use in parallel decision making
#' use_parallel <- resources$usable_cores > 1 && resources$has_parallel_packages
#' }
#'
#' @export
detect_system_resources <- function(verbose = FALSE) {
  
  # Detect CPU cores
  total_cores <- parallel::detectCores()
  if (is.na(total_cores)) {
    total_cores <- 1
    warning("Could not detect CPU cores, assuming 1 core available")
  }
  
  # Recommend usable cores (leave one for system)
  usable_cores <- max(1, total_cores - 1)
  
  # Cap at reasonable maximum to avoid overhead
  max_recommended_cores <- min(usable_cores, 8)  # Rarely beneficial beyond 8 cores for typical workloads
  
  # Estimate memory (rough approximation)
  memory_gb <- tryCatch({
    # Try to get memory info on different platforms
    if (.Platform$OS.type == "unix") {
      # Unix/Linux/Mac
      system_info <- try(system("free -m", intern = TRUE), silent = TRUE)
      if (inherits(system_info, "try-error")) {
        # Fallback for Mac
        system_info <- try(system("vm_stat", intern = TRUE), silent = TRUE)
        if (!inherits(system_info, "try-error")) {
          # Very rough estimate for Mac
          4.0  # Conservative default
        } else {
          4.0  # Conservative default
        }
      } else {
        # Parse Linux free output
        mem_line <- system_info[2]  # Second line has memory info
        mem_match <- regmatches(mem_line, regexec("\\s+([0-9]+)", mem_line))
        if (length(mem_match[[1]]) > 1) {
          as.numeric(mem_match[[1]][2]) / 1024  # Convert MB to GB
        } else {
          4.0  # Default
        }
      }
    } else {
      # Windows - use memory.size() if available
      if (exists("memory.size")) {
        memory.size() / 1024  # Convert MB to GB
      } else {
        4.0  # Conservative default for Windows
      }
    }
  }, error = function(e) {
    4.0  # Default fallback
  })
  
  # Check for parallel packages
  has_foreach <- requireNamespace("foreach", quietly = TRUE)
  has_doparallel <- requireNamespace("doParallel", quietly = TRUE)
  has_parallel_packages <- has_foreach && has_doparallel
  
  # Determine platform and recommended backend
  platform <- .Platform$OS.type
  parallel_backend <- if (platform == "unix") "fork" else "socket"
  
  resource_info <- list(
    total_cores = total_cores,
    usable_cores = usable_cores,
    memory_gb = memory_gb,
    has_parallel_packages = has_parallel_packages,
    platform = platform,
    parallel_backend = parallel_backend,
    max_recommended_cores = max_recommended_cores,
    package_availability = list(
      foreach = has_foreach,
      doParallel = has_doparallel
    )
  )
  
  if (verbose) {
    cat("System Resource Analysis\n")
    cat("========================\n")
    cat("CPU Cores (total):", resource_info$total_cores, "\n")
    cat("CPU Cores (usable):", resource_info$usable_cores, "\n")
    cat("Max recommended cores:", resource_info$max_recommended_cores, "\n")
    cat("Memory (approx):", round(resource_info$memory_gb, 1), "GB\n")
    cat("Platform:", resource_info$platform, "\n")
    cat("Recommended backend:", resource_info$parallel_backend, "\n")
    cat("Parallel packages available:", resource_info$has_parallel_packages, "\n")
    if (!resource_info$has_parallel_packages) {
      cat("  Missing packages:", 
          paste(names(resource_info$package_availability)[!unlist(resource_info$package_availability)], 
                collapse = ", "), "\n")
    }
    cat("\n")
  }
  
  class(resource_info) <- "moneca_system_resources"
  return(resource_info)
}

#' Estimate Memory Requirements for Parameter Combinations
#'
#' Estimates the memory requirements for evaluating a given number of
#' parameter combinations with specified matrix size and bootstrap samples.
#'
#' @param n_combinations Number of parameter combinations to evaluate.
#' @param matrix_size Size of the mobility matrix (n x n).
#' @param n_bootstrap Number of bootstrap samples for stability assessment.
#' @param cache_enabled Whether caching is enabled (increases memory usage).
#'
#' @return List containing memory estimates:
#'   \item{per_combination_mb}{Memory per combination in MB}
#'   \item{total_estimated_mb}{Total estimated memory usage in MB}
#'   \item{bootstrap_overhead_mb}{Additional memory for bootstrap samples}
#'   \item{cache_overhead_mb}{Additional memory for caching}
#'   \item{recommended_chunk_size}{Recommended processing chunk size}
#'
#' @keywords internal
estimate_memory_requirements <- function(n_combinations, matrix_size, 
                                       n_bootstrap = 50, cache_enabled = TRUE) {
  
  # Base memory per matrix (rough estimates in MB)
  matrix_mb <- (matrix_size^2 * 8) / (1024^2)  # 8 bytes per double
  
  # Memory per combination (includes weight matrix, segments, etc.)
  per_combination_mb <- matrix_mb * 3  # Original + weight + segments
  
  # Bootstrap memory (temporary storage)
  bootstrap_overhead_mb <- if (n_bootstrap > 0) {
    matrix_mb * min(n_bootstrap, 10)  # Cache up to 10 bootstrap matrices
  } else {
    0
  }
  
  # Cache memory (if enabled)
  cache_overhead_mb <- if (cache_enabled) {
    per_combination_mb * min(n_combinations, 50)  # Cache up to 50 combinations
  } else {
    0
  }
  
  # Total memory estimate
  total_estimated_mb <- (per_combination_mb * n_combinations) + 
                       bootstrap_overhead_mb + 
                       cache_overhead_mb
  
  # Recommended chunk size to stay within reasonable memory limits
  max_memory_mb <- 2048  # 2GB limit for processing chunks
  recommended_chunk_size <- max(1, floor(max_memory_mb / per_combination_mb))
  recommended_chunk_size <- min(recommended_chunk_size, n_combinations)
  
  return(list(
    per_combination_mb = per_combination_mb,
    total_estimated_mb = total_estimated_mb,
    bootstrap_overhead_mb = bootstrap_overhead_mb,
    cache_overhead_mb = cache_overhead_mb,
    recommended_chunk_size = recommended_chunk_size
  ))
}

#' Estimate Parallel vs Sequential Performance
#'
#' Uses heuristics to estimate the relative performance of parallel vs
#' sequential processing for given problem characteristics.
#'
#' @param n_combinations Number of parameter combinations to evaluate.
#' @param matrix_size Size of the mobility matrix.
#' @param n_bootstrap Number of bootstrap samples.
#' @param n_cores Number of available cores.
#' @param system_resources System resource information from detect_system_resources().
#'
#' @return List containing performance estimates:
#'   \item{sequential_time_est}{Estimated sequential processing time (seconds)}
#'   \item{parallel_time_est}{Estimated parallel processing time (seconds)}
#'   \item{speedup_est}{Estimated speedup factor}
#'   \item{parallel_overhead_est}{Estimated overhead cost (seconds)}
#'   \item{efficiency_est}{Estimated parallel efficiency (0-1)}
#'   \item{recommendation}{Recommended approach ("parallel" or "sequential")}
#'
#' @details
#' The function uses empirical models based on:
#' - Problem complexity (matrix size, combinations, bootstrap samples)
#' - System characteristics (cores, memory, platform)
#' - Parallel overhead estimates (setup, communication, synchronization)
#' - Memory bandwidth limitations
#'
#' @keywords internal
estimate_parallel_benefit <- function(n_combinations, matrix_size, n_bootstrap = 50,
                                    n_cores = NULL, system_resources = NULL) {
  
  if (is.null(system_resources)) {
    system_resources <- detect_system_resources(verbose = FALSE)
  }
  
  if (is.null(n_cores)) {
    n_cores <- system_resources$usable_cores
  }
  
  # Base computational complexity estimates (empirical models)
  # These are rough estimates based on typical MONECA performance
  
  # Time per combination in seconds (sequential baseline)
  base_time_per_combination <- (matrix_size^1.5 * 0.001) + (n_bootstrap * 0.1)
  
  # Sequential time estimate
  sequential_time_est <- n_combinations * base_time_per_combination
  
  # Parallel overhead estimates
  setup_overhead <- 2.0  # Cluster setup time
  communication_overhead_per_combination <- 0.01 * matrix_size / 100  # Communication cost
  sync_overhead <- max(0.5, n_combinations * 0.005)  # Synchronization
  
  total_parallel_overhead <- setup_overhead + 
                           (communication_overhead_per_combination * n_combinations) + 
                           sync_overhead
  
  # Theoretical speedup (Amdahl's law with overhead)
  parallelizable_fraction <- 0.85  # 85% of work can be parallelized
  theoretical_speedup <- 1 / ((1 - parallelizable_fraction) + (parallelizable_fraction / n_cores))
  
  # Memory bandwidth limitations (reduce efficiency if memory-bound)
  memory_requirements <- estimate_memory_requirements(n_combinations, matrix_size, n_bootstrap)
  memory_bound_factor <- if (memory_requirements$total_estimated_mb > system_resources$memory_gb * 512) {
    # If using more than half available memory, reduce efficiency
    0.6
  } else if (memory_requirements$total_estimated_mb > system_resources$memory_gb * 256) {
    # If using more than quarter available memory, slight reduction
    0.8
  } else {
    1.0
  }
  
  # Effective speedup considering real-world factors
  effective_speedup <- theoretical_speedup * memory_bound_factor * 0.8  # 80% efficiency factor
  
  # Parallel time estimate
  parallel_computation_time <- sequential_time_est / effective_speedup
  parallel_time_est <- parallel_computation_time + total_parallel_overhead
  
  # Calculate metrics
  speedup_est <- if (parallel_time_est > 0) sequential_time_est / parallel_time_est else 1.0
  efficiency_est <- speedup_est / n_cores
  
  # Make recommendation based on multiple criteria
  recommendation <- "sequential"  # Default
  
  # Criteria for parallel processing
  criteria_met <- 0
  total_criteria <- 5
  
  # 1. Minimum problem size
  if (n_combinations >= 10 && matrix_size >= 20) criteria_met <- criteria_met + 1
  
  # 2. Sufficient speedup
  if (speedup_est > 1.3) criteria_met <- criteria_met + 1
  
  # 3. Memory not constrained
  if (memory_bound_factor > 0.8) criteria_met <- criteria_met + 1
  
  # 4. Sufficient cores available
  if (n_cores >= 2 && system_resources$has_parallel_packages) criteria_met <- criteria_met + 1
  
  # 5. Efficiency threshold
  if (efficiency_est > 0.4) criteria_met <- criteria_met + 1
  
  # Recommend parallel if majority of criteria met
  if (criteria_met >= 3) {
    recommendation <- "parallel"
  }
  
  return(list(
    sequential_time_est = sequential_time_est,
    parallel_time_est = parallel_time_est,
    speedup_est = speedup_est,
    parallel_overhead_est = total_parallel_overhead,
    efficiency_est = efficiency_est,
    recommendation = recommendation,
    criteria_met = criteria_met,
    total_criteria = total_criteria,
    memory_bound_factor = memory_bound_factor,
    reasoning = list(
      n_combinations = n_combinations,
      matrix_size = matrix_size,
      n_bootstrap = n_bootstrap,
      n_cores = n_cores,
      theoretical_speedup = theoretical_speedup,
      effective_speedup = effective_speedup
    )
  ))
}

#' Get Optimal Number of Cores
#'
#' Determines the optimal number of cores to use for a given problem,
#' considering problem characteristics and system resources.
#'
#' @param n_combinations Number of parameter combinations.
#' @param matrix_size Size of mobility matrix.
#' @param system_resources System resource information.
#' @param max_cores Maximum number of cores to consider.
#'
#' @return Integer representing optimal number of cores to use.
#'
#' @details
#' The function optimizes core count by:
#' - Ensuring sufficient work per core (avoid over-parallelization)
#' - Considering memory bandwidth limitations
#' - Balancing parallel overhead vs benefit
#' - Respecting system resource constraints
#'
#' @examples
#' \dontrun{
#' resources <- detect_system_resources()
#' optimal_cores <- get_optimal_cores(
#'   n_combinations = 100,
#'   matrix_size = 50,
#'   system_resources = resources
#' )
#' }
#'
#' @export
get_optimal_cores <- function(n_combinations, matrix_size, 
                            system_resources = NULL, max_cores = NULL) {
  
  if (is.null(system_resources)) {
    system_resources <- detect_system_resources(verbose = FALSE)
  }
  
  if (is.null(max_cores)) {
    max_cores <- system_resources$max_recommended_cores
  }
  
  # Start with available cores
  available_cores <- min(system_resources$usable_cores, max_cores)
  
  # Ensure minimum work per core (avoid over-parallelization)
  min_combinations_per_core <- 3
  max_cores_by_work <- max(1, floor(n_combinations / min_combinations_per_core))
  
  # Consider memory limitations
  memory_requirements <- estimate_memory_requirements(n_combinations, matrix_size)
  memory_per_core <- memory_requirements$total_estimated_mb / available_cores
  max_memory_per_core_mb <- 1024  # 1GB per core
  max_cores_by_memory <- max(1, floor(memory_requirements$total_estimated_mb / max_memory_per_core_mb))
  
  # Matrix size considerations (small matrices don't benefit from many cores)
  max_cores_by_matrix_size <- if (matrix_size < 30) {
    2
  } else if (matrix_size < 100) {
    4
  } else {
    available_cores
  }
  
  # Take the minimum of all constraints
  optimal_cores <- min(
    available_cores,
    max_cores_by_work,
    max_cores_by_memory,
    max_cores_by_matrix_size
  )
  
  # Ensure at least 1 core
  optimal_cores <- max(1, optimal_cores)
  
  return(optimal_cores)
}

#' Main Decision Function for Parallel Processing
#'
#' Makes the primary decision about whether to use parallel or sequential
#' processing based on comprehensive analysis of problem characteristics,
#' system resources, and performance estimates.
#'
#' @param n_combinations Number of parameter combinations to evaluate.
#' @param matrix_size Size of the mobility matrix (n x n).
#' @param n_bootstrap Number of bootstrap samples for stability assessment.
#' @param user_preference User's explicit preference: "auto", "parallel", "sequential".
#' @param system_resources Pre-computed system resources (optional, will detect if NULL).
#' @param verbose Logical indicating whether to display decision reasoning.
#'
#' @return List containing the parallelization decision:
#'   \item{use_parallel}{Logical indicating whether to use parallel processing}
#'   \item{n_cores}{Recommended number of cores (if parallel)}
#'   \item{reasoning}{Text explanation of the decision}
#'   \item{performance_estimates}{Detailed performance predictions}
#'   \item{system_info}{System resource information used}
#'   \item{user_override}{Whether user preference overrode recommendation}
#'
#' @details
#' The decision process considers:
#' 1. User preferences (explicit overrides)
#' 2. Problem size thresholds (minimum viable parallel work)
#' 3. Resource availability (cores, memory, packages)
#' 4. Performance estimates (speedup, efficiency)
#' 5. Platform-specific considerations
#'
#' Decision thresholds:
#' - Minimum 8 parameter combinations for parallel consideration
#' - Matrix size >= 15 for meaningful parallel work
#' - At least 2 cores available with required packages
#' - Expected speedup > 1.3x
#' - Parallel efficiency > 40%
#'
#' @examples
#' \dontrun{
#' # Automatic decision making
#' decision <- should_use_parallel(
#'   n_combinations = 50,
#'   matrix_size = 100,
#'   n_bootstrap = 30,
#'   user_preference = "auto",
#'   verbose = TRUE
#' )
#' 
#' if (decision$use_parallel) {
#'   cat("Using", decision$n_cores, "cores for parallel processing\n")
#' } else {
#'   cat("Using sequential processing\n")
#' }
#' 
#' # Force parallel processing
#' decision_forced <- should_use_parallel(
#'   n_combinations = 10,
#'   matrix_size = 20,
#'   user_preference = "parallel"
#' )
#' }
#'
#' @seealso 
#' \code{\link{detect_system_resources}}, 
#' \code{\link{estimate_parallel_benefit}},
#' \code{\link{get_optimal_cores}}
#'
#' @export
should_use_parallel <- function(n_combinations, matrix_size, n_bootstrap = 50,
                               user_preference = "auto", system_resources = NULL,
                               verbose = FALSE) {
  
  # Input validation
  if (!user_preference %in% c("auto", "parallel", "sequential", TRUE, FALSE)) {
    stop("user_preference must be one of: 'auto', 'parallel', 'sequential', TRUE, or FALSE")
  }
  
  # Convert logical preferences to strings for consistency
  if (isTRUE(user_preference)) user_preference <- "parallel"
  if (isFALSE(user_preference)) user_preference <- "sequential"
  
  # Get system resources if not provided
  if (is.null(system_resources)) {
    system_resources <- detect_system_resources(verbose = FALSE)
  }
  
  # Initialize decision structure
  decision <- list(
    use_parallel = FALSE,
    n_cores = 1,
    reasoning = "",
    performance_estimates = NULL,
    system_info = system_resources,
    user_override = FALSE
  )
  
  # Handle explicit user preferences first
  if (user_preference == "sequential") {
    decision$reasoning <- "Sequential processing requested by user"
    decision$user_override <- TRUE
    
    if (verbose) {
      cat("Parallel Processing Decision: SEQUENTIAL (user requested)\n")
      cat("Reasoning:", decision$reasoning, "\n\n")
    }
    return(decision)
  }
  
  if (user_preference == "parallel") {
    # User wants parallel - check if it's feasible
    if (!system_resources$has_parallel_packages) {
      decision$reasoning <- "Parallel processing requested but required packages not available"
      if (verbose) {
        cat("Parallel Processing Decision: SEQUENTIAL (packages missing)\n")
        cat("Reasoning:", decision$reasoning, "\n")
        cat("Install required packages: install.packages(c('foreach', 'doParallel'))\n\n")
      }
      return(decision)
    }
    
    if (system_resources$usable_cores < 2) {
      decision$reasoning <- "Parallel processing requested but insufficient cores available"
      if (verbose) {
        cat("Parallel Processing Decision: SEQUENTIAL (insufficient cores)\n") 
        cat("Reasoning:", decision$reasoning, "\n\n")
      }
      return(decision)
    }
    
    # User override - use parallel even if not optimal
    optimal_cores <- get_optimal_cores(n_combinations, matrix_size, system_resources)
    decision$use_parallel <- TRUE
    decision$n_cores <- optimal_cores
    decision$reasoning <- "Parallel processing requested by user (override)"
    decision$user_override <- TRUE
    
    if (verbose) {
      cat("Parallel Processing Decision: PARALLEL (user requested)\n")
      cat("Cores to use:", decision$n_cores, "\n")
      cat("Reasoning:", decision$reasoning, "\n\n")
    }
    return(decision)
  }
  
  # Automatic decision making (user_preference == "auto")
  
  # Quick feasibility checks
  if (!system_resources$has_parallel_packages) {
    decision$reasoning <- "Required parallel packages not available"
    if (verbose) {
      cat("Parallel Processing Decision: SEQUENTIAL (no packages)\n")
      cat("Reasoning:", decision$reasoning, "\n")
      cat("Install required packages: install.packages(c('foreach', 'doParallel'))\n\n")
    }
    return(decision)
  }
  
  if (system_resources$usable_cores < 2) {
    decision$reasoning <- "Insufficient CPU cores for parallel processing"
    if (verbose) {
      cat("Parallel Processing Decision: SEQUENTIAL (single core)\n")
      cat("Reasoning:", decision$reasoning, "\n\n")
    }
    return(decision)
  }
  
  # Minimum problem size thresholds
  if (n_combinations < 8) {
    decision$reasoning <- paste("Too few parameter combinations for parallel benefit",
                               "(", n_combinations, "< 8 minimum)")
    if (verbose) {
      cat("Parallel Processing Decision: SEQUENTIAL (too few combinations)\n")
      cat("Reasoning:", decision$reasoning, "\n\n")
    }
    return(decision)
  }
  
  if (matrix_size < 15) {
    decision$reasoning <- paste("Matrix too small for parallel benefit",
                               "(", matrix_size, "x", matrix_size, "< 15x15 minimum)")
    if (verbose) {
      cat("Parallel Processing Decision: SEQUENTIAL (small matrix)\n")
      cat("Reasoning:", decision$reasoning, "\n\n")
    }
    return(decision)
  }
  
  # Detailed performance analysis
  performance_est <- estimate_parallel_benefit(
    n_combinations = n_combinations,
    matrix_size = matrix_size, 
    n_bootstrap = n_bootstrap,
    n_cores = system_resources$usable_cores,
    system_resources = system_resources
  )
  
  decision$performance_estimates <- performance_est
  
  # Make decision based on performance estimates
  if (performance_est$recommendation == "parallel") {
    optimal_cores <- get_optimal_cores(n_combinations, matrix_size, system_resources)
    decision$use_parallel <- TRUE
    decision$n_cores <- optimal_cores
    decision$reasoning <- sprintf(
      "Parallel processing beneficial (estimated %.1fx speedup, %.0f%% efficiency with %d cores)",
      performance_est$speedup_est, 
      performance_est$efficiency_est * 100,
      optimal_cores
    )
  } else {
    decision$reasoning <- sprintf(
      "Sequential processing optimal (estimated parallel speedup only %.1fx with %.0f%% efficiency)",
      performance_est$speedup_est,
      performance_est$efficiency_est * 100
    )
  }
  
  if (verbose) {
    cat("Parallel Processing Decision:", if (decision$use_parallel) "PARALLEL" else "SEQUENTIAL", "\n")
    if (decision$use_parallel) {
      cat("Cores to use:", decision$n_cores, "\n")
    }
    cat("Reasoning:", decision$reasoning, "\n")
    
    if (!is.null(performance_est)) {
      cat("\nPerformance Estimates:\n")
      cat("  Sequential time:", round(performance_est$sequential_time_est, 1), "seconds\n")
      cat("  Parallel time:", round(performance_est$parallel_time_est, 1), "seconds\n")
      cat("  Speedup factor:", round(performance_est$speedup_est, 1), "x\n")
      cat("  Parallel efficiency:", round(performance_est$efficiency_est * 100, 1), "%\n")
      cat("  Overhead cost:", round(performance_est$parallel_overhead_est, 1), "seconds\n")
      cat("  Decision criteria met:", performance_est$criteria_met, "/", performance_est$total_criteria, "\n")
    }
    cat("\n")
  }
  
  return(decision)
}

#' Create Smart Parallel Configuration
#'
#' Creates a standardized configuration object for parallel processing
#' that can be used throughout the MONECA package.
#'
#' @param decision Decision object from should_use_parallel().
#' @param backend_preference Preferred parallel backend ("auto", "fork", "socket").
#'
#' @return List containing parallel configuration:
#'   \item{use_parallel}{Whether to use parallel processing}
#'   \item{n_cores}{Number of cores to use}
#'   \item{backend}{Parallel backend to use}
#'   \item{cluster}{Cluster object (NULL until initialized)}
#'   \item{decision_info}{Original decision information}
#'
#' @keywords internal
create_parallel_config <- function(decision, backend_preference = "auto") {
  
  config <- list(
    use_parallel = decision$use_parallel,
    n_cores = if (decision$use_parallel) decision$n_cores else 1,
    backend = "sequential",
    cluster = NULL,
    decision_info = decision
  )
  
  if (decision$use_parallel) {
    # Determine backend
    if (backend_preference == "auto") {
      config$backend <- decision$system_info$parallel_backend
    } else if (backend_preference == "fork" && .Platform$OS.type != "unix") {
      warning("Fork backend not available on Windows, using socket")
      config$backend <- "socket"
    } else {
      config$backend <- backend_preference
    }
  }
  
  class(config) <- "moneca_parallel_config"
  return(config)
}

#' Print Method for System Resources
#'
#' @param x Object of class "moneca_system_resources".
#' @param ... Additional arguments (unused).
#'
#' @export
print.moneca_system_resources <- function(x, ...) {
  cat("MONECA System Resources\n")
  cat("======================\n")
  cat("CPU Cores (total):", x$total_cores, "\n")
  cat("CPU Cores (usable):", x$usable_cores, "\n")  
  cat("Max recommended:", x$max_recommended_cores, "\n")
  cat("Memory (approx):", round(x$memory_gb, 1), "GB\n")
  cat("Platform:", x$platform, "\n")
  cat("Parallel backend:", x$parallel_backend, "\n")
  cat("Parallel packages:", x$has_parallel_packages, "\n")
  
  if (!x$has_parallel_packages) {
    missing <- names(x$package_availability)[!unlist(x$package_availability)]
    cat("Missing packages:", paste(missing, collapse = ", "), "\n")
    cat("Install with: install.packages(c(", paste0("'", missing, "'", collapse = ", "), "))\n")
  }
}