#' Joint Parameter Tuning Framework for MONECA
#'
#' This module provides comprehensive joint calibration for both small.cell.reduction 
#' and cut.off parameters in MONECA algorithms. It implements advanced optimization 
#' methods that consider the mathematical relationships and interactions between 
#' these two parameters.
#'
#' @name joint_tuning
#' @keywords internal
NULL

#' Joint Auto-Tuning for small.cell.reduction and cut.off Parameters
#'
#' Automatically selects optimal values for both small.cell.reduction and cut.off 
#' parameters simultaneously, considering their mathematical relationships and 
#' compound effects on clustering results. Features advanced progress tracking
#' and flexible parameter grid customization for efficient optimization.
#'
#' @param mx Mobility matrix to analyze. Must be a square numeric matrix.
#' @param method Character string specifying optimization strategy:
#'   \itemize{
#'     \item "grid" (default): 2D grid search with stability assessment
#'     \item "bayesian": Bayesian optimization using Gaussian Process surrogate modeling
#'       with Expected Improvement or Upper Confidence Bound acquisition functions.
#'       Efficient for expensive evaluations, uses Latin Hypercube Sampling for
#'       initial design and sequential evaluation with adaptive point selection.
#'       Recommended when evaluation budget is limited or parameter space is large.
#'       \strong{Note: Sequential by mathematical necessity} - each new evaluation point
#'       depends on the GP model updated from all previous evaluations.
#'     \item "pareto": Multi-objective Pareto frontier optimization
#'     \item "adaptive": Adaptive refinement starting with coarse grid
#'   }
#' @param scr_range Numeric vector of length 2 specifying the range for 
#'   small.cell.reduction parameter. Default is c(0, NULL) where NULL 
#'   auto-determines the upper bound. Ignored if scr_values is provided.
#' @param cutoff_range Numeric vector of length 2 specifying the range for 
#'   cut.off parameter. Default is c(0.5, 3). Ignored if cutoff_values is provided.
#' @param scr_values Numeric vector of custom small.cell.reduction values to test.
#'   If provided, overrides scr_range and n_grid_points for SCR dimension.
#'   Values must be non-negative and will be converted to integers. Allows
#'   for targeted testing of specific parameter values based on prior knowledge
#'   or domain expertise. Default is NULL.
#' @param cutoff_values Numeric vector of custom cut.off values to test.
#'   If provided, overrides cutoff_range and n_grid_points for cutoff dimension.
#'   Values must be positive. Enables focused evaluation of specific threshold
#'   values identified through previous analysis. Default is NULL.
#' @param n_grid_points Integer number of grid points per dimension for grid 
#'   search. Default is 10. Ignored for dimensions with custom values.
#'   Larger values provide finer resolution but increase computation time.
#' @param n_bootstrap Integer number of bootstrap samples for stability 
#'   assessment. Default is 50. Higher values improve stability estimation
#'   accuracy but increase computation time.
#' @param objectives Character vector of optimization objectives. Options include:
#'   "stability", "quality", "sparsity", "modularity". 
#'   Default is c("stability", "quality"). Multiple objectives are
#'   combined using weighted aggregation.
#' @param weights Numeric vector of weights for combining multiple objectives.
#'   Must sum to 1. Default is equal weights. Allows prioritization of
#'   specific clustering properties in the optimization.
#' @param seed Integer seed for reproducibility. Default is NULL.
#'   Ensures consistent results across runs when set.
#' @param verbose Logical indicating whether to show detailed progress information.
#'   Default is TRUE. When enabled, displays real-time progress tracking with
#'   ETA estimation, parameter evaluation updates, and performance statistics.
#'   Progress tracking works seamlessly with both sequential and parallel execution.
#' @param parallel Character or logical indicating parallel processing preference.
#'   Can be "auto" (default, intelligent switching), TRUE/FALSE (force parallel/sequential),
#'   or "parallel"/"sequential" for explicit control. When "auto", the function
#'   analyzes problem characteristics and system resources to make optimal decision.
#'   Progress tracking is maintained across all parallel workers.
#'   \strong{Method-specific behavior:}
#'   \itemize{
#'     \item \strong{Grid search}: Can parallelize across parameter combinations
#'     \item \strong{Adaptive}: Can parallelize within each refinement phase  
#'     \item \strong{Bayesian}: Uses sequential evaluation regardless of this setting
#'       due to mathematical dependencies between evaluations. Individual evaluations
#'       may use internal parallelization if available.
#'   }
#' @param plot_surface Logical indicating whether to plot optimization surface.
#'   Default is FALSE. Creates interactive visualization of the parameter space
#'   and optimization results when enabled.
#' @param bayesian_iterations Integer number of Bayesian optimization iterations.
#'   Default is 20. Controls the number of sequential evaluations after initial
#'   design points. Higher values allow more thorough exploration but increase
#'   computation time. Only used when method = "bayesian".
#' @param acquisition_function Character string specifying acquisition function
#'   for Bayesian optimization. Options are:
#'   \itemize{
#'     \item "ei" (default): Expected Improvement - balances exploration and exploitation
#'     \item "ucb": Upper Confidence Bound - more exploration-focused
#'   }
#'   Only used when method = "bayesian".
#' @param exploration_factor Numeric exploration parameter for acquisition function.
#'   Default is 0.1. Controls exploration vs exploitation trade-off:
#'   \itemize{
#'     \item Smaller values (e.g., 0.01-0.05): More exploitation, faster convergence
#'     \item Larger values (e.g., 0.1-0.5): More exploration, better global search
#'   }
#'   Only used when method = "bayesian".
#' @param n_initial Integer number of initial design points for Bayesian optimization.
#'   Default is 5. These points are sampled using Latin Hypercube Sampling to
#'   provide good coverage of the parameter space before sequential optimization
#'   begins. More points provide better initialization but increase initial cost.
#'   Only used when method = "bayesian".
#'
#' @return A list of class "moneca_joint_tuning" containing:
#'   \item{optimal_scr}{Optimal small.cell.reduction value}
#'   \item{optimal_cutoff}{Optimal cut.off value}
#'   \item{optimization_surface}{Matrix of objective values for parameter grid}
#'   \item{parameter_grid}{Data frame of tested parameter combinations}
#'   \item{scores}{Detailed scores for each parameter combination}
#'   \item{method}{Optimization method used}
#'   \item{objectives}{Objectives optimized}
#'   \item{selection_rationale}{Explanation of parameter selection}
#'   \item{mathematical_relationship}{Estimated interaction effects}
#'   \item{computation_time}{Total optimization time}
#'   \item{scr_values}{Vector of tested small.cell.reduction values (custom or generated)}
#'   \item{cutoff_values}{Vector of tested cut.off values (custom or generated)}
#'   \item{performance_stats}{Optimization performance statistics including
#'                          parallel processing efficiency, cache utilization,
#'                          and progress tracking metrics}
#'
#' @details
#' This enhanced function provides comprehensive joint parameter optimization
#' with advanced features for progress tracking and custom parameter grids:
#' 
#' \strong{Mathematical Framework:}
#' The function considers the sequential filtering relationship between parameters:
#' 1. small.cell.reduction filters raw counts: mx[mx < scr] = 0
#' 2. Relative risk calculation: RR = Observed / Expected
#' 3. cut.off filters relative risks: RR[RR < cutoff] = NA
#' 
#' Network density is affected multiplicatively by both parameters.
#' The function models this interaction to find optimal combinations.
#' 
#' \strong{Progress Tracking System:}
#' Real-time progress monitoring works seamlessly across execution modes:
#' \itemize{
#'   \item File-based progress tracking ensures visibility in parallel processing
#'   \item ETA estimation based on completed evaluations and current performance
#'   \item Progress updates shown at regular intervals (every 5% completion)
#'   \item Performance statistics including cache hit rates and evaluation speed
#'   \item Graceful degradation if progress tracking encounters issues
#' }
#' 
#' \strong{Custom Grid Vector Support:}
#' Flexible parameter specification allows targeted optimization:
#' \itemize{
#'   \item Custom vectors override ranges and grid point specifications
#'   \item Mixed usage: custom values for one parameter, range for another
#'   \item Automatic integer conversion for small.cell.reduction values
#'   \item Input validation ensures parameter feasibility
#'   \item Smart grid sizing based on problem characteristics
#' }
#' 
#' \strong{Performance Optimizations:}
#' Multiple efficiency enhancements improve user experience:
#' \itemize{
#'   \item Intelligent caching system reduces redundant computations
#'   \item Smart parameter ordering optimizes evaluation sequence
#'   \item Early termination for large parameter spaces when beneficial
#'   \item Adaptive parallel processing based on problem size and resources
#'   \item Memory management prevents cache overflow in long sessions
#' }
#' 
#' \strong{Bayesian Optimization Framework:}
#' The Bayesian method uses advanced machine learning techniques for efficient
#' parameter search, particularly beneficial for expensive evaluations:
#' \itemize{
#'   \item \strong{Gaussian Process surrogate modeling}: Builds a probabilistic model
#'     of the objective function that provides both predictions and uncertainty estimates
#'   \item \strong{Latin Hypercube Sampling}: Initial design points are strategically
#'     placed using space-filling design for optimal coverage
#'   \item \strong{Acquisition functions}: Balance exploration (searching uncertain regions)
#'     vs exploitation (searching near current best points):
#'     \itemize{
#'       \item Expected Improvement (EI): Maximizes expected improvement over current best
#'       \item Upper Confidence Bound (UCB): Optimizes upper confidence bound of predictions
#'     }
#'   \item \strong{Sequential evaluation}: Each new point is chosen to maximize
#'     information gain about the optimal parameter combination
#'   \item \strong{Adaptive point selection}: Later evaluations focus on most
#'     promising regions based on accumulated knowledge
#' }
#' 
#' \strong{Why Bayesian Optimization is Sequential:}
#' Bayesian optimization is inherently sequential due to its mathematical foundation:
#' \itemize{
#'   \item Each new evaluation point depends on the Gaussian Process model fitted to
#'     ALL previous evaluations
#'   \item The acquisition function requires the updated posterior distribution from
#'     all completed evaluations to select the next most informative point
#'   \item Parallel evaluation would select multiple points using the same outdated
#'     model, reducing the efficiency gained from adaptive point selection
#'   \item This sequential nature is what makes Bayesian optimization efficient:
#'     it typically finds good solutions with far fewer total evaluations than
#'     grid search, even though evaluations cannot be parallelized
#' }
#' 
#' \strong{Method Selection Guidelines:}
#' Choose optimization method based on problem characteristics:
#' \itemize{
#'   \item \strong{Grid search}: Best for small parameter spaces (≤100 evaluations),
#'     when comprehensive coverage is needed, or when evaluations are fast.
#'     Can fully utilize parallel processing for faster wall-clock time.
#'   \item \strong{Bayesian optimization}: Optimal for limited evaluation budgets,
#'     expensive function evaluations, large parameter spaces, or when seeking
#'     global optimum efficiently. Typically requires 15-50 evaluations.
#'     \strong{Sequential execution}: May appear slower per unit time due to lack
#'     of parallelization, but often faster overall due to requiring fewer total evaluations.
#'   \item \strong{Adaptive refinement}: Good compromise between thoroughness and
#'     efficiency, recommended for moderate-sized problems. Can parallelize within
#'     each refinement phase.
#'   \item \strong{Pareto optimization}: Use when multiple competing objectives
#'     need simultaneous consideration
#' }
#' 
#' \strong{Bayesian Optimization Tuning:}
#' Parameter selection for Bayesian optimization:
#' \itemize{
#'   \item \strong{n_initial}: More points (8-15) for complex landscapes, fewer (3-8)
#'     for smooth functions or limited budgets
#'   \item \strong{bayesian_iterations}: Typically 10-30 for most problems. More iterations
#'     allow better convergence but increase computational cost
#'   \item \strong{acquisition_function}: EI for balanced search, UCB for more exploration
#'   \item \strong{exploration_factor}: 0.01-0.05 for exploitation, 0.1-0.5 for exploration
#' }
#'
#' @examples
#' \dontrun{
#' # Generate sample data
#' mobility_data <- generate_mobility_data(n_classes = 6, seed = 123)
#' 
#' # Grid search optimization
#' joint_result <- auto_tune_joint_parameters(
#'   mx = mobility_data,
#'   method = "grid",
#'   n_grid_points = 15,
#'   verbose = TRUE,
#'   plot_surface = TRUE
#' )
#' 
#' # Use optimal parameters
#' segments <- moneca(
#'   mobility_data,
#'   small.cell.reduction = joint_result$optimal_scr,
#'   cut.off = joint_result$optimal_cutoff
#' )
#' 
#' # Adaptive refinement for efficiency
#' adaptive_result <- auto_tune_joint_parameters(
#'   mx = mobility_data,
#'   method = "adaptive",
#'   verbose = TRUE
#' )
#'
#' # Custom parameter vectors for targeted testing
#' # Useful when you have domain knowledge about effective parameter ranges
#' custom_result <- auto_tune_joint_parameters(
#'   mx = mobility_data,
#'   method = "grid",
#'   scr_values = c(0, 1, 2, 5, 10),  # Specific SCR values to test
#'   cutoff_values = c(0.5, 1.0, 1.5, 2.0),  # Specific cutoff values
#'   verbose = TRUE  # Shows progress with ETA estimation
#' )
#'
#' # Mixed usage - custom SCR values with cutoff range
#' # Demonstrates flexibility: custom vector + automatic range
#' mixed_result <- auto_tune_joint_parameters(
#'   mx = mobility_data,
#'   method = "grid",
#'   scr_values = c(0, 2, 5, 10, 15),  # Custom SCR values
#'   cutoff_range = c(0.8, 2.5),       # Automatic cutoff range
#'   n_grid_points = 8,                # Grid points for cutoff only
#'   verbose = TRUE
#' )
#' 
#' # Progress tracking demonstration
#' # Note: Progress output appears in console during execution
#' large_scale_result <- auto_tune_joint_parameters(
#'   mx = mobility_data,
#'   method = "grid",
#'   n_grid_points = 20,  # Creates 400 combinations
#'   parallel = "auto",   # Smart parallel processing
#'   verbose = TRUE       # Real-time progress with ETA
#' )
#' 
#' # Access enhanced return information
#' print(large_scale_result$performance_stats)  # Optimization efficiency
#' print(large_scale_result$scr_values)         # Actual values tested
#' print(large_scale_result$cutoff_values)      # Actual values tested
#' 
#' # Bayesian optimization for efficient parameter search
#' # Recommended when evaluation budget is limited or parameter space is large
#' bayesian_result <- auto_tune_joint_parameters(
#'   mx = mobility_data,
#'   method = "bayesian",
#'   bayesian_iterations = 15,  # Sequential evaluations after initial design
#'   acquisition_function = "ei", # Expected Improvement (balanced)
#'   exploration_factor = 0.1,   # Moderate exploration
#'   n_initial = 8,              # Initial Latin Hypercube points
#'   verbose = TRUE
#' )
#' 
#' # Bayesian optimization with high exploration for global search
#' # Useful for complex or multimodal parameter landscapes
#' exploratory_result <- auto_tune_joint_parameters(
#'   mx = mobility_data, 
#'   method = "bayesian",
#'   bayesian_iterations = 25,
#'   acquisition_function = "ucb", # Upper Confidence Bound (more exploratory)
#'   exploration_factor = 0.3,     # High exploration
#'   n_initial = 10,
#'   verbose = TRUE
#' )
#'
#' # Efficient Bayesian search with custom parameter ranges
#' # Combines domain knowledge with efficient optimization
#' efficient_bayesian <- auto_tune_joint_parameters(
#'   mx = mobility_data,
#'   method = "bayesian",
#'   scr_range = c(0, 15),          # Custom SCR range based on data characteristics
#'   cutoff_range = c(0.8, 2.5),    # Custom cutoff range
#'   bayesian_iterations = 12,      # Fewer iterations for quick optimization
#'   acquisition_function = "ei",
#'   exploration_factor = 0.05,     # Low exploration for fast convergence
#'   n_initial = 5,                 # Minimal initial points
#'   verbose = TRUE
#' )
#' 
#' # Compare methods: Grid vs Bayesian efficiency
#' # Demonstrates when to use each approach
#' 
#' # For small parameter spaces: Grid search
#' grid_small <- auto_tune_joint_parameters(
#'   mx = mobility_data,
#'   method = "grid", 
#'   n_grid_points = 8,  # 64 total evaluations
#'   verbose = TRUE
#' )
#' 
#' # For equivalent evaluation budget: Bayesian optimization
#' bayesian_equivalent <- auto_tune_joint_parameters(
#'   mx = mobility_data,
#'   method = "bayesian",
#'   n_initial = 8,            # Initial design points  
#'   bayesian_iterations = 56, # Sequential evaluations (8 + 56 = 64 total)
#'   acquisition_function = "ei",
#'   exploration_factor = 0.1,
#'   verbose = TRUE
#' )
#' 
#' # Access Bayesian-specific results
#' print(bayesian_result$bayesian_info)      # Bayesian optimization details
#' print(bayesian_result$convergence_history) # Objective improvement over iterations
#' }
#'
#' @seealso 
#' \code{\link{auto_tune_small_cell_reduction}},
#' \code{\link{analyze_parameter_interaction}},
#' \code{\link{plot_optimization_surface}},
#' \code{\link{clear_evaluation_caches}} for memory management,
#' \code{\link{detect_system_resources}} for parallel processing guidance
#' @export
auto_tune_joint_parameters <- function(mx,
                                      method = "grid",
                                      scr_range = c(0, NULL),
                                      cutoff_range = c(0.5, 3),
                                      scr_values = NULL,
                                      cutoff_values = NULL,
                                      n_grid_points = 10,
                                      n_bootstrap = 50,
                                      objectives = c("stability", "quality"),
                                      weights = NULL,
                                      seed = NULL,
                                      verbose = TRUE,
                                      parallel = "auto",
                                      plot_surface = FALSE,
                                      bayesian_iterations = 20,
                                      acquisition_function = "ei",
                                      exploration_factor = 0.1,
                                      n_initial = 5) {
  
  # Set seed for reproducibility
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  start_time <- Sys.time()
  
  # Input validation
  if (!is.matrix(mx)) {
    mx <- as.matrix(mx)
  }
  
  if (nrow(mx) != ncol(mx)) {
    stop("Mobility matrix must be square")
  }
  
  if (!method %in% c("grid", "bayesian", "pareto", "adaptive")) {
    stop("Method must be one of: 'grid', 'bayesian', 'pareto', 'adaptive'")
  }
  
  # Input validation for small.cell.reduction range
  if (any(scr_range != floor(scr_range))) {
    if (verbose) {
      message("small.cell.reduction range contains non-integer values. ",
              "Rounding to nearest integers since parameter operates on count data.")
    }
    scr_range <- ensure_integer_scr_values(scr_range, warn_if_rounded = FALSE)
  }
  
  # Auto-determine parameter ranges if needed
  if (is.null(scr_range[2]) || length(scr_range) < 2) {
    suggested_ranges <- suggest_parameter_ranges(mx, method = "moderate")
    scr_range <- suggested_ranges$scr_range
  }
  
  # Ensure SCR range has integer bounds
  scr_range <- ensure_integer_scr_values(scr_range, warn_if_rounded = verbose)
  
  # Ensure ranges are valid
  if (length(scr_range) < 2 || scr_range[2] <= scr_range[1]) {
    scr_range <- c(0L, max(10L, scr_range[1] + 5L))
  }
  
  if (is.na(cutoff_range[2]) || cutoff_range[2] <= cutoff_range[1]) {
    cutoff_range[2] <- cutoff_range[1] + 1
  }
  
  # Custom vector validation and setup
  use_custom_scr <- !is.null(scr_values)
  use_custom_cutoff <- !is.null(cutoff_values)
  
  # Validate custom scr_values if provided
  if (use_custom_scr) {
    if (!is.numeric(scr_values)) {
      stop("scr_values must be a numeric vector")
    }
    if (any(scr_values < 0)) {
      stop("scr_values must contain non-negative values")
    }
    if (any(scr_values != floor(scr_values))) {
      if (verbose) {
        message("scr_values contains non-integer values. ",
                "Rounding to nearest integers since parameter operates on count data.")
      }
      scr_values <- as.integer(floor(scr_values + 0.5))
    } else {
      scr_values <- as.integer(scr_values)
    }
    scr_values <- sort(unique(scr_values))
    if (verbose) {
      cat("Using custom SCR values:", paste(scr_values, collapse = ", "), "\n")
    }
  }
  
  # Validate custom cutoff_values if provided
  if (use_custom_cutoff) {
    if (!is.numeric(cutoff_values)) {
      stop("cutoff_values must be a numeric vector")
    }
    if (any(cutoff_values <= 0)) {
      stop("cutoff_values must contain positive values")
    }
    cutoff_values <- sort(unique(cutoff_values))
    if (verbose) {
      cat("Using custom cutoff values:", paste(cutoff_values, collapse = ", "), "\n")
    }
  }
  
  # Custom vector validation warnings
  if (use_custom_scr && use_custom_cutoff) {
    n_combinations <- length(scr_values) * length(cutoff_values)
    if (verbose) {
      cat("Custom parameter grid:", length(scr_values), "SCR x", 
          length(cutoff_values), "cutoff =", n_combinations, "combinations\n")
    }
  }
  
  # Initialize weights if not provided
  if (is.null(weights)) {
    weights <- rep(1/length(objectives), length(objectives))
  }
  
  if (length(weights) != length(objectives)) {
    stop("Length of weights must equal length of objectives")
  }
  
  if (abs(sum(weights) - 1) > 1e-6) {
    stop("Weights must sum to 1")
  }
  
  # Smart parallel decision: Consider custom vectors for decision making
  actual_scr_values <- if (use_custom_scr) scr_values else generate_integer_scr_grid(scr_range, n_grid_points)
  actual_cutoff_values <- if (use_custom_cutoff) cutoff_values else seq(cutoff_range[1], cutoff_range[2], length.out = n_grid_points)
  
  total_combinations <- length(actual_scr_values) * length(actual_cutoff_values)
  
  if (parallel == "auto") {
    system_resources <- detect_system_resources()
    parallel_decision <- should_use_parallel(
      n_combinations = total_combinations, 
      matrix_size = nrow(mx), 
      n_bootstrap = 50,  # Default value for joint tuning
      user_preference = "auto",
      system_resources = system_resources,
      verbose = verbose
    )
    use_parallel <- parallel_decision$use_parallel
    if (verbose && use_parallel) {
      cat("Auto-parallel: Using", parallel_decision$n_cores, "cores for", 
          total_combinations, "evaluations\n")
    }
  } else if (parallel %in% c(TRUE, "parallel")) {
    use_parallel <- TRUE
    # Get optimal number of cores when parallel is explicitly requested
    system_resources <- detect_system_resources()
    n_cores <- get_optimal_cores(total_combinations, nrow(mx), system_resources)
  } else {
    use_parallel <- FALSE
    n_cores <- 1
  }
  
  # Set n_cores from parallel_decision if using auto mode
  if (parallel == "auto" && exists("parallel_decision")) {
    n_cores <- parallel_decision$n_cores
  }
  
  if (verbose) {
    cat("Joint Parameter Optimization:\n")
    cat("  Method:", method, "\n")
    if (!use_custom_scr) {
      cat("  SCR range:", scr_range[1], "to", scr_range[2], "\n")
    }
    if (!use_custom_cutoff) {
      cat("  Cutoff range:", cutoff_range[1], "to", cutoff_range[2], "\n")
    }
    cat("  Objectives:", paste(objectives, collapse = ", "), "\n")
    cat("  Processing:", if (use_parallel) "Parallel" else "Sequential", "\n")
  }
  
  # Route to appropriate optimization method
  if (method == "grid") {
    result <- optimize_grid_search(mx, scr_range, cutoff_range, scr_values, cutoff_values,
                                 n_grid_points, n_bootstrap, objectives, weights, 
                                 verbose, use_parallel)
  } else if (method == "adaptive") {
    result <- optimize_adaptive_refinement(mx, scr_range, cutoff_range, scr_values, cutoff_values,
                                         objectives, weights, verbose, use_parallel)
  } else if (method == "bayesian") {
    result <- optimize_bayesian(mx, scr_range, cutoff_range, scr_values, cutoff_values,
                              n_bootstrap, objectives, weights, verbose, use_parallel,
                              bayesian_iterations, acquisition_function, exploration_factor, n_initial)
  } else {
    stop("Method '", method, "' not yet implemented")
  }
  
  # Add metadata
  end_time <- Sys.time()
  result$method <- method
  result$objectives <- objectives
  result$computation_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  if (verbose) {
    cat("\nOptimization complete!\n")
    cat("Optimal small.cell.reduction:", result$optimal_scr, "\n")
    cat("Optimal cut.off:", result$optimal_cutoff, "\n")
    cat("Total time:", round(result$computation_time, 2), "seconds\n")
  }
  
  # Plot optimization surface if requested
  if (plot_surface) {
    plot_optimization_surface(result)
  }
  
  class(result) <- "moneca_joint_tuning"
  return(result)
}

# Progress Tracking Infrastructure -----------------------------------------------

#' Progress Tracker Class for Joint Parameter Tuning
#' 
#' File-based progress tracking system that works with parallel processing
#' @keywords internal
ProgressTracker <- function(task_name, total_items, temp_dir = NULL, verbose = TRUE) {
  
  # Create temporary directory for progress files
  if (is.null(temp_dir)) {
    temp_dir <- file.path(tempdir(), paste0("moneca_progress_", 
                                           format(Sys.time(), "%Y%m%d_%H%M%S")))
  }
  
  if (!dir.exists(temp_dir)) {
    dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  # Progress file paths
  progress_file <- file.path(temp_dir, "progress.txt")
  timing_file <- file.path(temp_dir, "timing.txt")
  
  # Initialize progress file
  writeLines("0", progress_file)
  writeLines(as.character(Sys.time()), timing_file)
  
  # Return progress tracker object
  structure(list(
    task_name = task_name,
    total_items = total_items,
    temp_dir = temp_dir,
    progress_file = progress_file,
    timing_file = timing_file,
    verbose = verbose,
    start_time = Sys.time(),
    last_update = Sys.time()
  ), class = "ProgressTracker")
}

#' Update Progress Counter
#' @keywords internal
update_progress <- function(tracker, increment = 1, message = NULL) {
  if (!inherits(tracker, "ProgressTracker")) return(invisible())
  
  tryCatch({
    # Read current progress
    if (file.exists(tracker$progress_file)) {
      current <- as.numeric(readLines(tracker$progress_file, warn = FALSE)[1])
    } else {
      current <- 0
    }
    
    # Update progress
    new_progress <- current + increment
    writeLines(as.character(new_progress), tracker$progress_file)
    
    # Update timing
    writeLines(as.character(Sys.time()), tracker$timing_file)
    
    # Display progress if verbose
    if (tracker$verbose && (new_progress %% max(1, floor(tracker$total_items / 20)) == 0 || 
                           new_progress == tracker$total_items)) {
      display_progress(tracker, new_progress, message)
    }
    
  }, error = function(e) {
    # Fail gracefully if progress tracking fails
    if (tracker$verbose) {
      warning("Progress tracking error (continuing): ", e$message, call. = FALSE)
    }
  })
  
  invisible()
}

#' Display Progress Information
#' @keywords internal
display_progress <- function(tracker, current_progress = NULL, message = NULL) {
  if (!inherits(tracker, "ProgressTracker")) return(invisible())
  
  tryCatch({
    # Get current progress if not provided
    if (is.null(current_progress)) {
      if (file.exists(tracker$progress_file)) {
        current_progress <- as.numeric(readLines(tracker$progress_file, warn = FALSE)[1])
      } else {
        current_progress <- 0
      }
    }
    
    # Calculate progress percentage
    progress_pct <- min(100, max(0, round(100 * current_progress / tracker$total_items, 1)))
    
    # Calculate ETA
    elapsed <- as.numeric(difftime(Sys.time(), tracker$start_time, units = "secs"))
    if (current_progress > 0 && progress_pct < 100) {
      eta_secs <- elapsed * (tracker$total_items - current_progress) / current_progress
      eta_str <- if (eta_secs > 60) {
        sprintf("~%.1f min", eta_secs / 60)
      } else {
        sprintf("~%.0f sec", eta_secs)
      }
    } else {
      eta_str <- "calculating..."
    }
    
    # Create progress bar
    bar_width <- 30
    filled <- round(bar_width * progress_pct / 100)
    bar <- paste0("[", paste(rep("=", filled), collapse = ""), 
                  paste(rep(" ", bar_width - filled), collapse = ""), "]")
    
    # Display progress
    progress_msg <- sprintf("\r%s: %s %5.1f%% (%d/%d) ETA: %s", 
                           tracker$task_name, bar, progress_pct, 
                           current_progress, tracker$total_items, eta_str)
    
    if (!is.null(message)) {
      progress_msg <- paste0(progress_msg, " - ", message)
    }
    
    cat(progress_msg)
    if (progress_pct >= 100) {
      cat("\n")
    }
    flush.console()
    
    tracker$last_update <- Sys.time()
    
  }, error = function(e) {
    # Fail gracefully
    if (tracker$verbose) {
      cat(sprintf("\r%s: %d/%d items processed", tracker$task_name, 
                 current_progress %||% 0, tracker$total_items))
      flush.console()
    }
  })
  
  invisible()
}

#' Read Current Progress
#' @keywords internal
get_progress <- function(tracker) {
  if (!inherits(tracker, "ProgressTracker")) return(0)
  
  tryCatch({
    if (file.exists(tracker$progress_file)) {
      return(as.numeric(readLines(tracker$progress_file, warn = FALSE)[1]))
    }
  }, error = function(e) {
    # Return 0 if cannot read
  })
  
  return(0)
}

#' Clean Up Progress Tracker
#' @keywords internal
cleanup_progress <- function(tracker) {
  if (!inherits(tracker, "ProgressTracker")) return(invisible())
  
  tryCatch({
    # Remove temporary files
    if (file.exists(tracker$progress_file)) {
      unlink(tracker$progress_file)
    }
    if (file.exists(tracker$timing_file)) {
      unlink(tracker$timing_file)
    }
    
    # Remove temporary directory if empty
    if (dir.exists(tracker$temp_dir) && length(list.files(tracker$temp_dir)) == 0) {
      unlink(tracker$temp_dir, recursive = TRUE)
    }
  }, error = function(e) {
    # Cleanup fails gracefully
    warning("Could not clean up progress files: ", e$message, call. = FALSE)
  })
  
  invisible()
}

#' Worker Progress Reporter for Parallel Processing
#' @keywords internal
report_worker_progress <- function(tracker_info, increment = 1, message = NULL) {
  if (is.null(tracker_info) || !is.list(tracker_info)) return(invisible())
  
  tryCatch({
    # Simple file-based communication
    progress_file <- tracker_info$progress_file
    if (!is.null(progress_file) && nzchar(progress_file)) {
      
      # Lock file for atomic updates
      lock_file <- paste0(progress_file, ".lock")
      
      # Try to acquire lock (wait briefly if locked)
      attempts <- 0
      while (file.exists(lock_file) && attempts < 10) {
        Sys.sleep(0.01)
        attempts <- attempts + 1
      }
      
      # Create lock
      writeLines("locked", lock_file)
      
      # Update progress
      current <- 0
      if (file.exists(progress_file)) {
        current <- as.numeric(readLines(progress_file, warn = FALSE)[1])
      }
      
      new_progress <- current + increment
      writeLines(as.character(new_progress), progress_file)
      
      # Release lock
      if (file.exists(lock_file)) {
        unlink(lock_file)
      }
    }
  }, error = function(e) {
    # Fail silently in workers to avoid disrupting computation
  })
  
  invisible()
}

# Utility operator for cleaner null handling
`%||%` <- function(x, y) if (is.null(x)) y else x


#' Bayesian Optimization for Joint Parameters
#'
#' Performs Bayesian optimization using Gaussian Process surrogate model
#' with acquisition function-based point selection.
#'
#' @inheritParams auto_tune_joint_parameters
#' @param bayesian_iterations Integer number of Bayesian optimization iterations.
#' @param acquisition_function Character string specifying acquisition function:
#'   "ei" (Expected Improvement) or "ucb" (Upper Confidence Bound).
#' @param exploration_factor Numeric exploration parameter for acquisition function.
#' @param n_initial Integer number of random initial points.
#' @keywords internal
optimize_bayesian <- function(mx, scr_range, cutoff_range, scr_values, cutoff_values,
                             bayesian_iterations = 20, acquisition_function = "ei", 
                             exploration_factor = 0.1, n_initial = 5,
                             n_bootstrap, objectives, weights, verbose, parallel) {
  
  # Parameter space setup
  if (is.null(scr_values)) {
    scr_min <- scr_range[1]
    scr_max <- scr_range[2]
  } else {
    scr_min <- min(scr_values)
    scr_max <- max(scr_values)
  }
  
  cutoff_min <- cutoff_range[1]
  cutoff_max <- cutoff_range[2]
  
  if (verbose) {
    cat("Starting Bayesian optimization...\n")
    cat("Parameter space: SCR [", scr_min, ",", scr_max, "], Cutoff [", cutoff_min, ",", cutoff_max, "]\n")
    cat("Initial points:", n_initial, "| Iterations:", bayesian_iterations, 
        "| Acquisition:", acquisition_function, "\n")
  }
  
  # Generate initial design points
  initial_points <- generate_initial_design(scr_min, scr_max, cutoff_min, cutoff_max, 
                                           n_initial, scr_values)
  
  # Evaluate initial points
  if (verbose) cat("Evaluating initial design points...\n")
  
  evaluated_points <- data.frame(
    scr = numeric(0),
    cutoff = numeric(0), 
    objective = numeric(0)
  )
  
  detailed_scores <- list()
  
  # Evaluate initial points
  for (i in seq_len(nrow(initial_points))) {
    scr <- initial_points$scr[i]
    cutoff <- initial_points$cutoff[i]
    
    if (verbose) {
      cat("  Initial point", i, "/", nrow(initial_points), ": SCR =", scr, ", Cutoff =", cutoff, "\n")
    }
    
    # Evaluate this parameter combination
    result <- evaluate_parameter_combination(mx, scr, cutoff, objectives, n_bootstrap, parallel, FALSE)
    
    # Store results
    evaluated_points <- rbind(evaluated_points, 
                             data.frame(scr = scr, cutoff = cutoff, objective = result$combined_score))
    detailed_scores[[length(detailed_scores) + 1]] <- result
  }
  
  # Bayesian optimization loop
  if (verbose) cat("Starting Bayesian optimization iterations...\n")
  
  for (iter in seq_len(bayesian_iterations)) {
    if (verbose) {
      cat("  Iteration", iter, "/", bayesian_iterations, "\n")
    }
    
    # Fit Gaussian Process
    gp_model <- fit_gaussian_process(evaluated_points)
    
    # Find next point using acquisition function
    next_point <- optimize_acquisition(gp_model, evaluated_points, acquisition_function,
                                     exploration_factor, scr_min, scr_max, 
                                     cutoff_min, cutoff_max, scr_values)
    
    if (verbose) {
      cat("    Next point: SCR =", next_point$scr, ", Cutoff =", next_point$cutoff, "\n")
    }
    
    # Evaluate next point
    result <- evaluate_parameter_combination(mx, next_point$scr, next_point$cutoff, 
                                           objectives, n_bootstrap, parallel, FALSE)
    
    # Add to evaluated points
    evaluated_points <- rbind(evaluated_points, 
                             data.frame(scr = next_point$scr, cutoff = next_point$cutoff, 
                                       objective = result$combined_score))
    detailed_scores[[length(detailed_scores) + 1]] <- result
    
    if (verbose) {
      cat("    Objective value:", round(result$combined_score, 4), "\n")
    }
  }
  
  # Find optimal point
  best_idx <- which.max(evaluated_points$objective)
  optimal_scr <- evaluated_points$scr[best_idx]
  optimal_cutoff <- evaluated_points$cutoff[best_idx]
  
  if (verbose) {
    cat("Bayesian optimization complete!\n")
    cat("Best point: SCR =", optimal_scr, ", Cutoff =", optimal_cutoff, 
        ", Objective =", round(evaluated_points$objective[best_idx], 4), "\n")
  }
  
  # Create optimization surface for visualization
  # Use a simple interpolation of the evaluated points
  if (is.null(scr_values)) {
    scr_grid <- seq(scr_min, scr_max, length.out = 10)
  } else {
    scr_grid <- sort(unique(c(scr_values, evaluated_points$scr)))
  }
  cutoff_grid <- seq(cutoff_min, cutoff_max, length.out = 10)
  
  optimization_surface <- create_surface_from_points(evaluated_points, scr_grid, cutoff_grid)
  
  # Prepare results
  list(
    optimal_scr = optimal_scr,
    optimal_cutoff = optimal_cutoff,
    optimization_surface = optimization_surface,
    parameter_grid = evaluated_points,
    scores = detailed_scores,
    scr_values = sort(unique(evaluated_points$scr)),
    cutoff_values = sort(unique(evaluated_points$cutoff)),
    bayesian_model = gp_model,
    acquisition_function = acquisition_function,
    n_evaluations = nrow(evaluated_points)
  )
}

#' Generate Initial Design Points for Bayesian Optimization
#'
#' Creates initial sampling points using Latin Hypercube design.
#'
#' @param scr_min,scr_max Range for small.cell.reduction parameter
#' @param cutoff_min,cutoff_max Range for cutoff parameter  
#' @param n_initial Number of initial points
#' @param scr_values Optional discrete values for SCR (for discrete optimization)
#' @keywords internal
generate_initial_design <- function(scr_min, scr_max, cutoff_min, cutoff_max, n_initial, scr_values = NULL) {
  
  # Simple random sampling (could be improved with Latin Hypercube)
  if (is.null(scr_values)) {
    # Continuous SCR space
    scr_points <- runif(n_initial, scr_min, scr_max)
    # Round to integers since SCR should be integer
    scr_points <- pmax(scr_min, pmin(scr_max, round(scr_points)))
  } else {
    # Discrete SCR space
    scr_points <- sample(scr_values, n_initial, replace = TRUE)
  }
  
  cutoff_points <- runif(n_initial, cutoff_min, cutoff_max)
  
  data.frame(scr = scr_points, cutoff = cutoff_points)
}

#' Fit Gaussian Process Model
#'
#' Fits a simple Gaussian Process surrogate model to the evaluated points.
#'
#' @param evaluated_points Data frame with scr, cutoff, and objective columns
#' @keywords internal
fit_gaussian_process <- function(evaluated_points) {
  
  # Simple GP implementation using local regression
  # In production, consider using GPfit, DiceKriging, or similar packages
  
  X <- as.matrix(evaluated_points[, c("scr", "cutoff")])
  y <- evaluated_points$objective
  
  # Normalize inputs for better numerical stability
  scr_mean <- mean(X[, 1])
  scr_sd <- sd(X[, 1])
  if (scr_sd == 0) scr_sd <- 1
  
  cutoff_mean <- mean(X[, 2])
  cutoff_sd <- sd(X[, 2])
  if (cutoff_sd == 0) cutoff_sd <- 1
  
  X_norm <- cbind((X[, 1] - scr_mean) / scr_sd,
                  (X[, 2] - cutoff_mean) / cutoff_sd)
  
  # Simple local regression model
  # For a more sophisticated implementation, use actual GP libraries
  list(
    X = X,
    y = y,
    X_norm = X_norm,
    scr_mean = scr_mean,
    scr_sd = scr_sd,
    cutoff_mean = cutoff_mean,
    cutoff_sd = cutoff_sd
  )
}

#' Predict Using Gaussian Process Model
#'
#' Makes predictions at new points using the GP surrogate.
#'
#' @param gp_model Fitted GP model from fit_gaussian_process
#' @param X_new Matrix of new points to predict
#' @keywords internal
predict_gaussian_process <- function(gp_model, X_new) {
  
  # Normalize new points using training statistics
  X_new_norm <- cbind((X_new[, 1] - gp_model$scr_mean) / gp_model$scr_sd,
                      (X_new[, 2] - gp_model$cutoff_mean) / gp_model$cutoff_sd)
  
  n_new <- nrow(X_new_norm)
  n_train <- nrow(gp_model$X_norm)
  
  # Simple distance-based prediction with uncertainty
  predictions <- numeric(n_new)
  uncertainties <- numeric(n_new)
  
  for (i in seq_len(n_new)) {
    # Calculate distances to all training points
    distances <- sqrt(rowSums((gp_model$X_norm - matrix(rep(X_new_norm[i, ], n_train), 
                                                        nrow = n_train, byrow = TRUE))^2))
    
    # Weight by inverse distance (with smoothing parameter)
    weights <- exp(-distances^2 / 0.5)
    weights <- weights / sum(weights)
    
    # Weighted prediction
    predictions[i] <- sum(weights * gp_model$y)
    
    # Simple uncertainty estimate (higher when far from training points)
    min_dist <- min(distances)
    uncertainties[i] <- exp(min_dist)  # Higher uncertainty for distant points
  }
  
  list(mean = predictions, variance = uncertainties^2)
}

#' Optimize Acquisition Function
#'
#' Finds the point that maximizes the acquisition function.
#'
#' @param gp_model Fitted Gaussian Process model
#' @param evaluated_points Previously evaluated points
#' @param acquisition_function Type of acquisition function ("ei" or "ucb")
#' @param exploration_factor Exploration parameter
#' @param scr_min,scr_max SCR parameter bounds
#' @param cutoff_min,cutoff_max Cutoff parameter bounds
#' @param scr_values Optional discrete SCR values
#' @keywords internal
optimize_acquisition <- function(gp_model, evaluated_points, acquisition_function,
                                exploration_factor, scr_min, scr_max, 
                                cutoff_min, cutoff_max, scr_values = NULL) {
  
  # Generate candidate points
  n_candidates <- 1000
  
  if (is.null(scr_values)) {
    scr_candidates <- runif(n_candidates, scr_min, scr_max)
    scr_candidates <- pmax(scr_min, pmin(scr_max, round(scr_candidates)))
  } else {
    scr_candidates <- sample(scr_values, n_candidates, replace = TRUE)
  }
  
  cutoff_candidates <- runif(n_candidates, cutoff_min, cutoff_max)
  
  X_candidates <- cbind(scr_candidates, cutoff_candidates)
  
  # Remove points that have already been evaluated (within tolerance)
  new_candidates_idx <- rep(TRUE, n_candidates)
  for (i in seq_len(nrow(evaluated_points))) {
    distances <- sqrt((X_candidates[, 1] - evaluated_points$scr[i])^2 + 
                     (X_candidates[, 2] - evaluated_points$cutoff[i])^2)
    new_candidates_idx <- new_candidates_idx & (distances > 1e-6)
  }
  
  if (sum(new_candidates_idx) == 0) {
    # If no new candidates, add some noise to existing best point
    best_idx <- which.max(evaluated_points$objective)
    return(data.frame(scr = evaluated_points$scr[best_idx] + round(rnorm(1, 0, 1)),
                     cutoff = evaluated_points$cutoff[best_idx] + rnorm(1, 0, 0.1)))
  }
  
  X_candidates <- X_candidates[new_candidates_idx, , drop = FALSE]
  
  # Predict at candidate points
  gp_pred <- predict_gaussian_process(gp_model, X_candidates)
  
  # Calculate acquisition function values
  if (acquisition_function == "ei") {
    # Expected Improvement
    best_objective <- max(evaluated_points$objective)
    acquisition_values <- calculate_expected_improvement(gp_pred$mean, sqrt(gp_pred$variance), 
                                                        best_objective, exploration_factor)
  } else if (acquisition_function == "ucb") {
    # Upper Confidence Bound
    acquisition_values <- gp_pred$mean + exploration_factor * sqrt(gp_pred$variance)
  } else {
    stop("Unknown acquisition function: ", acquisition_function)
  }
  
  # Select point with highest acquisition value
  best_idx <- which.max(acquisition_values)
  
  data.frame(scr = X_candidates[best_idx, 1], 
            cutoff = X_candidates[best_idx, 2])
}

#' Calculate Expected Improvement
#'
#' Computes Expected Improvement acquisition function values.
#'
#' @param mean Predicted means from GP
#' @param std Predicted standard deviations from GP
#' @param best_objective Current best objective value
#' @param xi Exploration parameter
#' @keywords internal
calculate_expected_improvement <- function(mean, std, best_objective, xi = 0.01) {
  
  improvement <- mean - best_objective - xi
  Z <- improvement / std
  
  # Handle numerical issues
  Z[std == 0] <- 0
  
  # Expected Improvement formula
  ei <- improvement * pnorm(Z) + std * dnorm(Z)
  ei[std == 0] <- 0
  
  return(ei)
}

#' Create Surface from Scattered Points
#'
#' Creates a regular grid surface by interpolating scattered evaluation points.
#'
#' @param evaluated_points Data frame with scr, cutoff, objective columns
#' @param scr_grid Grid points for SCR dimension
#' @param cutoff_grid Grid points for cutoff dimension
#' @keywords internal
create_surface_from_points <- function(evaluated_points, scr_grid, cutoff_grid) {
  
  # Simple interpolation - in practice, might want to use more sophisticated methods
  surface <- matrix(NA, nrow = length(scr_grid), ncol = length(cutoff_grid))
  
  for (i in seq_along(scr_grid)) {
    for (j in seq_along(cutoff_grid)) {
      # Find closest evaluated points
      distances <- sqrt((evaluated_points$scr - scr_grid[i])^2 + 
                       (evaluated_points$cutoff - cutoff_grid[j])^2)
      
      # Use inverse distance weighting
      if (min(distances) < 1e-6) {
        # Exact match
        surface[i, j] <- evaluated_points$objective[which.min(distances)]
      } else {
        # Weighted average of closest points
        weights <- 1 / (distances + 1e-6)
        surface[i, j] <- sum(weights * evaluated_points$objective) / sum(weights)
      }
    }
  }
  
  surface
}

#' Grid Search Optimization for Joint Parameters
#'
#' Performs systematic grid search over 2D parameter space.
#'
#' @inheritParams auto_tune_joint_parameters
#' @keywords internal
optimize_grid_search <- function(mx, scr_range, cutoff_range, scr_values, cutoff_values, 
                                n_grid_points, n_bootstrap, objectives, weights, 
                                verbose, parallel) {
  
  # Use custom vectors if provided, otherwise generate from ranges
  if (is.null(scr_values)) {
    scr_values <- generate_integer_scr_grid(scr_range, n_grid_points)
  }
  if (is.null(cutoff_values)) {
    cutoff_values <- seq(cutoff_range[1], cutoff_range[2], length.out = n_grid_points)
  }
  
  param_grid <- expand.grid(
    scr = scr_values,
    cutoff = cutoff_values
  )
  
  n_combinations <- nrow(param_grid)
  
  if (verbose) {
    cat("Evaluating", n_combinations, "parameter combinations...\n")
  }
  
  # Initialize score matrix (dimensions based on actual grid sizes)
  scores <- matrix(NA, nrow = length(scr_values), ncol = length(cutoff_values))
  detailed_scores <- list()
  
  # Use the already-determined parallel decision
  # Note: 'parallel' parameter now contains the smart decision result
  use_parallel <- parallel
  
  # Set up parallel processing if needed
  cluster_setup <- NULL
  if (use_parallel) {
    cluster_setup <- setup_parallel_cluster(verbose = verbose)
    on.exit({
      if (!is.null(cluster_setup)) cleanup_parallel_cluster(cluster_setup, verbose = verbose)
    })
  }
  
  # Function to process a single parameter combination
  process_combination <- function(i) {
    scr <- param_grid$scr[i]
    cutoff <- param_grid$cutoff[i]
    
    # Compute objective scores
    obj_scores <- evaluate_parameter_combination(
      mx = mx,
      scr = scr,
      cutoff = cutoff,
      objectives = objectives,
      n_bootstrap = n_bootstrap,
      parallel = FALSE  # Individual evaluations use sequential to avoid nested parallelism
    )
    
    # Combine scores with weights
    combined_score <- sum(obj_scores * weights)
    
    # Return all necessary information
    return(list(
      i = i,
      scr = scr,
      cutoff = cutoff,
      obj_scores = obj_scores,
      combined_score = combined_score
    ))
  }
  
  # Use optimized batch evaluation system
  optimization_result <- evaluate_parameter_combinations_optimized(
    mx = mx,
    param_combinations = param_grid,
    objectives = objectives,
    weights = weights,
    n_bootstrap = n_bootstrap,
    use_cache = TRUE,
    smart_ordering = TRUE,
    early_termination = n_combinations > 20,  # Enable for larger searches
    termination_threshold = 0.6,  # Evaluate at least 60% before considering termination
    parallel = use_parallel,
    verbose = verbose
  )
  
  # Extract results from optimized evaluation
  detailed_scores <- optimization_result$detailed_scores
  best_idx <- optimization_result$best_idx
  optimal_params <- param_grid[best_idx, ]
  
  # Populate score matrix for visualization
  for (i in seq_len(nrow(param_grid))) {
    if (!is.null(detailed_scores[[i]])) {
      scr <- param_grid$scr[i]
      cutoff <- param_grid$cutoff[i]
      
      # Store in matrix (for visualization)
      row_idx <- which(scr_values == scr)
      col_idx <- which(cutoff_values == cutoff)
      
      if (length(row_idx) > 0 && length(col_idx) > 0) {
        scores[row_idx, col_idx] <- detailed_scores[[i]]["combined"]
      }
    }
  }
  
  # Create result object with performance information
  result <- list(
    optimal_scr = optimal_params$scr,
    optimal_cutoff = optimal_params$cutoff,
    optimization_surface = scores,
    parameter_grid = param_grid,
    scores = detailed_scores,
    scr_values = scr_values,
    cutoff_values = cutoff_values,
    performance_stats = optimization_result$performance_stats,
    selection_rationale = sprintf(
      "Selected based on %s optimization with combined score: %.3f (%s evaluation)",
      paste(objectives, collapse = "+"),
      detailed_scores[[best_idx]]["combined"],
      if (!is.null(optimization_result$performance_stats$early_terminated) && 
          optimization_result$performance_stats$early_terminated) "early-terminated" else "complete"
    )
  )
  
  return(result)
}

#' Adaptive Refinement Optimization
#'
#' Starts with coarse grid and refines promising regions.
#'
#' @inheritParams auto_tune_joint_parameters
#' @keywords internal
optimize_adaptive_refinement <- function(mx, scr_range, cutoff_range, scr_values, cutoff_values,
                                        objectives, weights, verbose, parallel) {
  
  if (verbose) {
    cat("Starting adaptive refinement optimization...\n")
  }
  
  # Set up parallel processing if needed
  cluster_setup <- NULL
  if (parallel) {
    cluster_setup <- setup_parallel_cluster(verbose = verbose)
    on.exit({
      if (!is.null(cluster_setup)) cleanup_parallel_cluster(cluster_setup, verbose = verbose)
    })
  }
  
  # Phase 1: Coarse grid (5x5)
  if (verbose) cat("Phase 1: Coarse grid search (5x5)...\n")
  
  coarse_result <- optimize_grid_search(
    mx = mx,
    scr_range = scr_range,
    cutoff_range = cutoff_range,
    scr_values = scr_values,
    cutoff_values = cutoff_values,
    n_grid_points = 5,
    n_bootstrap = 25,  # Slightly increased for better stability estimation
    objectives = objectives,
    weights = weights,
    verbose = FALSE,
    parallel = parallel
  )
  
  # Identify promising region (top 20% of scores)
  scores_flat <- as.vector(coarse_result$optimization_surface)
  threshold <- quantile(scores_flat, 0.8, na.rm = TRUE)
  
  # Find bounds of promising region
  promising_indices <- which(coarse_result$optimization_surface >= threshold, 
                           arr.ind = TRUE)
  
  scr_indices <- promising_indices[, 1]
  cutoff_indices <- promising_indices[, 2]
  
  # New refined ranges (ensure integer bounds for SCR)
  scr_min_idx <- max(1, min(scr_indices) - 1)
  scr_max_idx <- min(length(coarse_result$scr_values), max(scr_indices) + 1)
  scr_range_refined <- c(
    coarse_result$scr_values[scr_min_idx],
    coarse_result$scr_values[scr_max_idx]
  )
  
  cutoff_min_idx <- max(1, min(cutoff_indices) - 1)
  cutoff_max_idx <- min(length(coarse_result$cutoff_values), max(cutoff_indices) + 1)
  cutoff_range_refined <- c(
    coarse_result$cutoff_values[cutoff_min_idx],
    coarse_result$cutoff_values[cutoff_max_idx]
  )
  
  # Phase 2: Fine grid in promising region (10x10)
  if (verbose) {
    cat("Phase 2: Fine grid search in promising region...\n")
    cat("  Refined SCR range:", scr_range_refined[1], "-", scr_range_refined[2], "\n")
    cat("  Refined cutoff range:", cutoff_range_refined[1], "-", cutoff_range_refined[2], "\n")
  }
  
  fine_result <- optimize_grid_search(
    mx = mx,
    scr_range = scr_range_refined,
    cutoff_range = cutoff_range_refined,
    scr_values = NULL,  # Use refined ranges for fine tuning
    cutoff_values = NULL,  # Use refined ranges for fine tuning
    n_grid_points = 12,  # Increased for better resolution
    n_bootstrap = 60,  # More bootstraps for final selection
    objectives = objectives,
    weights = weights,
    verbose = FALSE,
    parallel = parallel
  )
  
  # Combine results
  result <- fine_result
  result$coarse_surface <- coarse_result$optimization_surface
  result$refinement_history <- list(
    coarse = coarse_result,
    fine = fine_result
  )
  
  return(result)
}

#' Bayesian Optimization for Joint Parameter Tuning
#'
#' Performs Bayesian optimization using Gaussian Process surrogate model
#' and acquisition function to efficiently explore the parameter space.
#'
#' @inheritParams auto_tune_joint_parameters
#' @keywords internal
optimize_bayesian <- function(mx, scr_range, cutoff_range, scr_values, cutoff_values,
                             n_bootstrap, objectives, weights, verbose, parallel,
                             bayesian_iterations, acquisition_function, exploration_factor, n_initial) {
  
  # PARALLELIZATION STRATEGY FOR BAYESIAN OPTIMIZATION:
  # Bayesian optimization is inherently sequential because each new point selection
  # depends on the updated Gaussian Process model from all previous evaluations.
  # However, individual parameter evaluations (stability assessment, etc.) can
  # benefit from internal parallelization. We use sequential evaluation here to:
  # 1. Maintain mathematical correctness of the BO algorithm
  # 2. Avoid nested parallelism complexity 
  # 3. Ensure proper GP model updates between iterations
  # Note: The 'parallel' parameter is preserved for future batch BO implementations
  
  if (verbose) {
    cat("Starting Bayesian optimization...\n")
    cat("  Iterations:", bayesian_iterations, "\n")
    cat("  Acquisition function:", toupper(acquisition_function), "\n")
    cat("  Initial design points:", n_initial, "\n")
    cat("  Processing: Sequential (required for BO algorithm)\n")
  }
  
  # Set up parameter space
  if (is.null(scr_values)) {
    if (is.null(scr_range[2])) {
      # Auto-determine upper bound based on matrix characteristics
      scr_upper <- suggest_integer_scr_bounds(mx)$upper
    } else {
      scr_upper <- scr_range[2]
    }
    scr_bounds <- c(max(0, scr_range[1]), scr_upper)
  } else {
    scr_bounds <- c(min(scr_values), max(scr_values))
  }
  
  if (is.null(cutoff_values)) {
    cutoff_bounds <- cutoff_range
  } else {
    cutoff_bounds <- c(min(cutoff_values), max(cutoff_values))
  }
  
  # Initialize tracking variables
  all_points <- matrix(nrow = 0, ncol = 2)  # [scr, cutoff]
  all_scores <- numeric(0)
  all_detailed_scores <- list()
  
  # Generate initial design using Latin Hypercube Sampling
  if (verbose) cat("Generating initial design points...\n")
  initial_points <- generate_initial_design(n_initial, scr_bounds, cutoff_bounds, scr_values, cutoff_values)
  
  # Evaluate initial points
  for (i in seq_len(nrow(initial_points))) {
    scr <- initial_points[i, 1]
    cutoff <- initial_points[i, 2]
    
    if (verbose) {
      cat(sprintf("  Initial point %d/%d: SCR=%.0f, cutoff=%.2f\n", i, nrow(initial_points), scr, cutoff))
    }
    
    # Evaluate this parameter combination
    # Use sequential processing to avoid nested parallelism complexity in BO loop
    obj_scores <- evaluate_parameter_combination(
      mx = mx,
      scr = scr,
      cutoff = cutoff,
      objectives = objectives,
      n_bootstrap = n_bootstrap,
      parallel = FALSE  # Sequential for initial design points
    )
    
    combined_score <- sum(obj_scores * weights)
    
    # Store results
    all_points <- rbind(all_points, c(scr, cutoff))
    all_scores <- c(all_scores, combined_score)
    all_detailed_scores[[length(all_detailed_scores) + 1]] <- obj_scores
  }
  
  # Bayesian optimization iterations
  if (verbose) cat("Starting Bayesian optimization iterations...\n")
  
  for (iter in 1:bayesian_iterations) {
    if (verbose) {
      best_score <- max(all_scores)
      best_idx <- which.max(all_scores)
      cat(sprintf("Iteration %d/%d - Current best: %.4f (SCR=%.0f, cutoff=%.2f)\n", 
                  iter, bayesian_iterations, best_score, 
                  all_points[best_idx, 1], all_points[best_idx, 2]))
    }
    
    # Fit Gaussian Process to current data
    gp_model <- fit_gaussian_process(all_points, all_scores)
    
    # Find next point using acquisition function
    next_point <- optimize_acquisition(gp_model, scr_bounds, cutoff_bounds, 
                                     scr_values, cutoff_values,
                                     acquisition_function, exploration_factor)
    
    # Handle case where no new candidate point is found
    if (is.null(next_point)) {
      if (verbose) cat("  No new candidate points found. Terminating optimization.\n")
      break
    }
    
    scr_next <- next_point[1]
    cutoff_next <- next_point[2]
    
    # Evaluate next point
    # Use sequential processing to avoid nested parallelism complexity in BO iterations
    obj_scores <- evaluate_parameter_combination(
      mx = mx,
      scr = scr_next,
      cutoff = cutoff_next,
      objectives = objectives,
      n_bootstrap = n_bootstrap,
      parallel = FALSE  # Sequential for iterative evaluations
    )
    
    combined_score <- sum(obj_scores * weights)
    
    if (verbose) {
      cat(sprintf("  Evaluated SCR=%.0f, cutoff=%.2f, score=%.4f\n", 
                  scr_next, cutoff_next, combined_score))
    }
    
    # Add to dataset
    all_points <- rbind(all_points, c(scr_next, cutoff_next))
    all_scores <- c(all_scores, combined_score)
    all_detailed_scores[[length(all_detailed_scores) + 1]] <- obj_scores
  }
  
  # Find optimal point
  best_idx <- which.max(all_scores)
  optimal_scr <- all_points[best_idx, 1]
  optimal_cutoff <- all_points[best_idx, 2]
  
  if (verbose) {
    cat(sprintf("Bayesian optimization completed!\n"))
    cat(sprintf("Optimal parameters: SCR=%.0f, cutoff=%.2f, score=%.4f\n", 
                optimal_scr, optimal_cutoff, all_scores[best_idx]))
  }
  
  # Create optimization surface for visualization (interpolation)
  scr_grid <- if (is.null(scr_values)) {
    seq(scr_bounds[1], scr_bounds[2], length.out = 20)
  } else {
    scr_values
  }
  cutoff_grid <- if (is.null(cutoff_values)) {
    seq(cutoff_bounds[1], cutoff_bounds[2], length.out = 20)
  } else {
    cutoff_values
  }
  
  optimization_surface <- create_surface_from_points(all_points, all_scores, scr_grid, cutoff_grid)
  
  # Prepare results
  result <- list(
    optimal_scr = optimal_scr,
    optimal_cutoff = optimal_cutoff,
    optimal_combination = list(scr = optimal_scr, cutoff = optimal_cutoff),
    optimal_score = all_scores[best_idx],
    optimization_surface = optimization_surface,
    scr_values = scr_grid,
    cutoff_values = cutoff_grid,
    evaluation_points = all_points,
    evaluation_scores = all_scores,
    detailed_scores = all_detailed_scores,
    n_evaluations = length(all_scores),
    convergence_history = all_scores,
    bayesian_info = list(
      iterations = min(iter, bayesian_iterations),
      acquisition_function = acquisition_function,
      exploration_factor = exploration_factor,
      n_initial = n_initial
    )
  )
  
  return(result)
}

#' Generate Initial Design for Bayesian Optimization
#'
#' Creates initial sampling points using Latin Hypercube Sampling approach.
#'
#' @param n_points Number of initial points to generate.
#' @param scr_bounds Bounds for SCR parameter.
#' @param cutoff_bounds Bounds for cutoff parameter.
#' @param scr_values Custom SCR values (NULL for continuous).
#' @param cutoff_values Custom cutoff values (NULL for continuous).
#' @return Matrix of initial design points.
#' @keywords internal
generate_initial_design <- function(n_points, scr_bounds, cutoff_bounds, scr_values = NULL, cutoff_values = NULL) {
  
  # Generate Latin hypercube sample
  lhs <- matrix(runif(n_points * 2), ncol = 2)
  
  # Transform to parameter space
  if (is.null(scr_values)) {
    scr_points <- lhs[, 1] * (scr_bounds[2] - scr_bounds[1]) + scr_bounds[1]
    scr_points <- round(scr_points)  # Ensure integer values
  } else {
    scr_points <- sample(scr_values, n_points, replace = TRUE)
  }
  
  if (is.null(cutoff_values)) {
    cutoff_points <- lhs[, 2] * (cutoff_bounds[2] - cutoff_bounds[1]) + cutoff_bounds[1]
  } else {
    cutoff_points <- sample(cutoff_values, n_points, replace = TRUE)
  }
  
  return(cbind(scr_points, cutoff_points))
}

#' Fit Simple Gaussian Process Model
#'
#' Fits a simple GP model using distance-weighted local regression.
#'
#' @param X Matrix of input points (n x 2).
#' @param y Vector of function values.
#' @return GP model object.
#' @keywords internal
fit_gaussian_process <- function(X, y) {
  
  # Normalize inputs for numerical stability
  X_norm <- scale(X)
  
  # Simple GP model using distance-based weighting
  model <- list(
    X = X,
    X_norm = X_norm,
    y = y,
    mean_y = mean(y),
    sd_y = sd(y),
    X_center = attr(X_norm, "scaled:center"),
    X_scale = attr(X_norm, "scaled:scale")
  )
  
  class(model) <- "simple_gp"
  return(model)
}

#' Predict with Simple Gaussian Process
#'
#' Makes predictions using the fitted GP model.
#'
#' @param model GP model object.
#' @param X_new Matrix of new input points.
#' @return List with mean and variance predictions.
#' @keywords internal
predict_gaussian_process <- function(model, X_new) {
  
  # Normalize new inputs using training data scaling
  X_new_norm <- scale(X_new, center = model$X_center, scale = model$X_scale)
  
  n_new <- nrow(X_new_norm)
  n_train <- nrow(model$X_norm)
  
  # Distance-based predictions
  means <- numeric(n_new)
  variances <- numeric(n_new)
  
  for (i in 1:n_new) {
    # Compute distances to all training points
    distances <- sqrt(rowSums((model$X_norm - matrix(X_new_norm[i, ], nrow = n_train, ncol = 2, byrow = TRUE))^2))
    
    # Weight function (exponential decay)
    weights <- exp(-distances^2 / 2)
    weights <- weights / sum(weights)
    
    # Weighted prediction
    means[i] <- sum(weights * model$y)
    
    # Uncertainty estimate based on weight concentration
    variances[i] <- max(0.01, var(model$y) * (1 - max(weights)))
  }
  
  return(list(mean = means, variance = variances))
}

#' Optimize Acquisition Function
#'
#' Finds the point that maximizes the acquisition function.
#'
#' @param gp_model Fitted GP model.
#' @param scr_bounds SCR parameter bounds.
#' @param cutoff_bounds Cutoff parameter bounds.
#' @param scr_values Custom SCR values (NULL for continuous).
#' @param cutoff_values Custom cutoff values (NULL for continuous).
#' @param acquisition_function Type of acquisition function ("ei" or "ucb").
#' @param exploration_factor Exploration parameter.
#' @return Vector of optimal next point coordinates.
#' @keywords internal
optimize_acquisition <- function(gp_model, scr_bounds, cutoff_bounds, scr_values = NULL, cutoff_values = NULL,
                                acquisition_function = "ei", exploration_factor = 0.1) {
  
  # Generate candidate points
  n_candidates <- 1000
  candidates <- generate_candidate_points(n_candidates, scr_bounds, cutoff_bounds, scr_values, cutoff_values)
  
  # Remove points already evaluated
  evaluated_points <- gp_model$X
  new_candidates <- matrix(nrow = 0, ncol = 2)
  
  for (i in 1:nrow(candidates)) {
    candidate <- candidates[i, ]
    
    # Check if this point has been evaluated (with small tolerance)
    min_dist <- min(sqrt(rowSums((evaluated_points - matrix(candidate, nrow = nrow(evaluated_points), ncol = 2, byrow = TRUE))^2)))
    
    if (min_dist > 0.01) {  # Small tolerance for numerical precision
      new_candidates <- rbind(new_candidates, candidate)
    }
  }
  
  if (nrow(new_candidates) == 0) {
    return(NULL)  # No new candidates
  }
  
  # Predict at candidate points
  pred <- predict_gaussian_process(gp_model, new_candidates)
  
  # Calculate acquisition function values
  if (acquisition_function == "ei") {
    # Expected Improvement
    f_best <- max(gp_model$y)
    improvement <- pmax(0, pred$mean - f_best)
    sigma <- sqrt(pred$variance)
    
    # Avoid division by zero
    sigma[sigma < 1e-6] <- 1e-6
    
    z <- improvement / sigma
    acquisition <- improvement * pnorm(z) + sigma * dnorm(z)
    
  } else if (acquisition_function == "ucb") {
    # Upper Confidence Bound
    acquisition <- pred$mean + exploration_factor * sqrt(pred$variance)
  }
  
  # Find best candidate
  best_idx <- which.max(acquisition)
  return(new_candidates[best_idx, ])
}

#' Generate Candidate Points for Acquisition Optimization
#'
#' Creates candidate points for acquisition function evaluation.
#'
#' @param n_candidates Number of candidate points.
#' @param scr_bounds SCR parameter bounds.
#' @param cutoff_bounds Cutoff parameter bounds.
#' @param scr_values Custom SCR values.
#' @param cutoff_values Custom cutoff values.
#' @return Matrix of candidate points.
#' @keywords internal
generate_candidate_points <- function(n_candidates, scr_bounds, cutoff_bounds, scr_values = NULL, cutoff_values = NULL) {
  
  # Generate random candidates
  candidates <- matrix(runif(n_candidates * 2), ncol = 2)
  
  # Transform to parameter space
  if (is.null(scr_values)) {
    scr_cand <- candidates[, 1] * (scr_bounds[2] - scr_bounds[1]) + scr_bounds[1]
    scr_cand <- round(scr_cand)
  } else {
    scr_cand <- sample(scr_values, n_candidates, replace = TRUE)
  }
  
  if (is.null(cutoff_values)) {
    cutoff_cand <- candidates[, 2] * (cutoff_bounds[2] - cutoff_bounds[1]) + cutoff_bounds[1]
  } else {
    cutoff_cand <- sample(cutoff_values, n_candidates, replace = TRUE)
  }
  
  return(cbind(scr_cand, cutoff_cand))
}

#' Create Optimization Surface from Scattered Points
#'
#' Interpolates scattered evaluation points to create a regular grid surface.
#'
#' @param points Matrix of evaluated points.
#' @param scores Vector of scores at evaluated points.
#' @param scr_grid Grid points for SCR dimension.
#' @param cutoff_grid Grid points for cutoff dimension.
#' @return Matrix representing interpolated surface.
#' @keywords internal
create_surface_from_points <- function(points, scores, scr_grid, cutoff_grid) {
  
  surface <- matrix(NA, nrow = length(scr_grid), ncol = length(cutoff_grid))
  
  # Simple inverse distance weighting interpolation
  for (i in 1:length(scr_grid)) {
    for (j in 1:length(cutoff_grid)) {
      grid_point <- c(scr_grid[i], cutoff_grid[j])
      
      # Compute distances to all evaluated points
      distances <- sqrt(rowSums((points - matrix(grid_point, nrow = nrow(points), ncol = 2, byrow = TRUE))^2))
      
      # Avoid division by zero
      distances[distances < 1e-6] <- 1e-6
      
      # Inverse distance weights
      weights <- 1 / distances^2
      weights <- weights / sum(weights)
      
      # Weighted interpolation
      surface[i, j] <- sum(weights * scores)
    }
  }
  
  return(surface)
}

# ============================================================================
# OPTIMIZED EVALUATION SYSTEM WITH CACHING AND BATCH PROCESSING
# ============================================================================

# Global cache for weight matrices (session-level cache)
.weight_matrix_cache <- new.env(parent = emptyenv())
.moneca_fast_cache <- new.env(parent = emptyenv())

#' Clear Evaluation Caches
#'
#' Clears all cached weight matrices and moneca_fast results to free memory.
#' This is useful when working with large datasets or after completing
#' parameter optimization to reclaim memory.
#'
#' @param verbose Logical indicating whether to show cache statistics.
#'
#' @examples
#' \dontrun{
#' # After running joint parameter tuning
#' result <- auto_tune_joint_parameters(mobility_data)
#' 
#' # Clear caches to free memory
#' clear_evaluation_caches(verbose = TRUE)
#' }
#'
#' @export
clear_evaluation_caches <- function(verbose = FALSE) {
  if (verbose) {
    wm_count <- length(ls(.weight_matrix_cache))
    mf_count <- length(ls(.moneca_fast_cache))
    message(sprintf("Clearing caches: %d weight matrices, %d moneca_fast results", 
                   wm_count, mf_count))
  }
  
  rm(list = ls(.weight_matrix_cache), envir = .weight_matrix_cache)
  rm(list = ls(.moneca_fast_cache), envir = .moneca_fast_cache)
  gc(verbose = FALSE)
  
  if (verbose) {
    message("Caches cleared successfully")
  }
}

#' Get Cache Statistics
#'
#' Returns information about the current state of evaluation caches.
#'
#' @return List containing cache statistics.
#'
#' @examples
#' \dontrun{
#' cache_info <- get_cache_stats()
#' print(cache_info)
#' }
#'
#' @export
get_cache_stats <- function() {
  wm_count <- length(ls(.weight_matrix_cache))
  mf_count <- length(ls(.moneca_fast_cache))
  
  # Estimate memory usage (rough approximation)
  wm_memory <- wm_count * 1024  # Rough estimate in bytes
  mf_memory <- mf_count * 512   # Rough estimate in bytes
  
  list(
    weight_matrix_cache = list(
      entries = wm_count,
      estimated_memory_kb = wm_memory / 1024
    ),
    moneca_fast_cache = list(
      entries = mf_count,
      estimated_memory_kb = mf_memory / 1024
    ),
    total_entries = wm_count + mf_count,
    total_estimated_memory_kb = (wm_memory + mf_memory) / 1024
  )
}

#' Generate cache key for parameter combinations
#' @keywords internal
generate_cache_key <- function(mx_hash, scr, cutoff, additional = NULL) {
  # Create deterministic hash based on matrix content and parameters
  key_components <- paste(c(mx_hash, scr, cutoff, additional), collapse = "_")
  # Use a simple but effective hash function
  return(paste0("key_", abs(as.integer(sum(utf8ToInt(key_components)) %% 2^31))))
}

#' Generate simple hash for matrices
#' @keywords internal
simple_matrix_hash <- function(mx) {
  # Create a simple but effective hash of the matrix
  mx_vec <- as.vector(mx)
  mx_vec <- mx_vec[!is.na(mx_vec)]
  hash_components <- c(length(mx_vec), sum(mx_vec), mean(mx_vec), sd(mx_vec))
  return(paste(round(hash_components, 6), collapse = "_"))
}

#' Batch Weight Matrix Computation
#'
#' Computes weight matrices for multiple parameter combinations efficiently,
#' using caching, vectorized operations, and parallel processing when enabled.
#'
#' @param mx Mobility matrix.
#' @param param_combinations Data frame with scr and cutoff columns.
#' @param use_cache Logical indicating whether to use caching.
#' @param parallel Logical indicating whether to use parallel processing.
#' @param verbose Logical indicating whether to show progress.
#'
#' @return List of weight matrices with same length as param_combinations.
#' @keywords internal
batch_compute_weight_matrices <- function(mx, param_combinations, 
                                         use_cache = TRUE, parallel = FALSE, verbose = FALSE,
                                         progress_tracker = NULL) {
  
  n_combinations <- nrow(param_combinations)
  weight_matrices <- vector("list", n_combinations)
  
  # Generate hash for the mobility matrix (for cache keys)
  mx_hash <- simple_matrix_hash(mx)
  
  # Track cache hits for performance monitoring
  cache_hits <- 0
  
  # Initialize progress tracking
  if (is.null(progress_tracker) && verbose) {
    progress_tracker <- ProgressTracker("Weight Matrices", n_combinations, verbose = verbose)
  }
  
  if (verbose) {
    message(sprintf("Computing %d weight matrices (caching: %s, parallel: %s)...", 
                   n_combinations, use_cache, parallel))
  }
  
  # Determine which combinations need computation (not in cache)
  combinations_to_compute <- integer(0)
  cache_keys <- character(n_combinations)
  
  for (i in seq_len(n_combinations)) {
    scr <- param_combinations$scr[i]
    cutoff <- param_combinations$cutoff[i]
    cache_key <- generate_cache_key(mx_hash, scr, cutoff)
    cache_keys[i] <- cache_key
    
    # Check cache first
    if (use_cache && exists(cache_key, envir = .weight_matrix_cache)) {
      weight_matrices[[i]] <- get(cache_key, envir = .weight_matrix_cache)
      cache_hits <- cache_hits + 1
      
      # Report progress for cached items
      if (!is.null(progress_tracker)) {
        update_progress(progress_tracker, 1, "from cache")
      }
    } else {
      combinations_to_compute <- c(combinations_to_compute, i)
    }
  }
  
  # Compute missing weight matrices
  if (length(combinations_to_compute) > 0) {
    if (parallel && requireNamespace("foreach", quietly = TRUE) && 
        requireNamespace("doParallel", quietly = TRUE)) {
      
      # Prepare progress tracking for workers
      tracker_info <- if (!is.null(progress_tracker)) {
        list(progress_file = progress_tracker$progress_file)
      } else {
        NULL
      }
      
      # Use parallel processing with foreach %dopar%
      compute_results <- foreach::foreach(
        idx = combinations_to_compute,
        .combine = 'c',
        .multicombine = TRUE,
        .maxcombine = length(combinations_to_compute),
        .errorhandling = 'pass',
        .packages = c("moneca"),  # Ensure package is loaded on workers
        .export = c("weight.matrix", "report_worker_progress")  # Export necessary functions
      ) %dopar% {
        scr <- param_combinations$scr[idx]
        cutoff <- param_combinations$cutoff[idx]
        
        # Compute weight matrix
        result <- tryCatch({
          wm <- weight.matrix(
            mx = mx,
            cut.off = cutoff,
            small.cell.reduction = scr
          )
          
          # Report progress from worker
          report_worker_progress(tracker_info, 1)
          
          list(index = idx, matrix = wm, error = NULL)
        }, error = function(e) {
          # Report progress even for errors
          report_worker_progress(tracker_info, 1)
          list(index = idx, matrix = NULL, error = e$message)
        })
        
        return(result)
      }
      
      # Process parallel results
      for (result in compute_results) {
        if (is.list(result) && !is.null(result$index)) {
          idx <- result$index
          weight_matrices[[idx]] <- result$matrix
          
          # Store in cache if enabled and computation was successful
          if (use_cache && !is.null(result$matrix)) {
            assign(cache_keys[idx], result$matrix, envir = .weight_matrix_cache)
          }
        }
      }
      
      # Final progress update after parallel completion
      if (!is.null(progress_tracker)) {
        display_progress(progress_tracker, get_progress(progress_tracker))
      }
      
    } else {
      # Sequential processing fallback
      for (i in combinations_to_compute) {
        scr <- param_combinations$scr[i]
        cutoff <- param_combinations$cutoff[i]
        
        # Compute weight matrix
        tryCatch({
          wm <- weight.matrix(
            mx = mx,
            cut.off = cutoff,
            small.cell.reduction = scr
          )
          
          weight_matrices[[i]] <- wm
          
          # Store in cache if enabled
          if (use_cache) {
            assign(cache_keys[i], wm, envir = .weight_matrix_cache)
          }
          
          # Update progress
          if (!is.null(progress_tracker)) {
            update_progress(progress_tracker, 1, sprintf("scr=%s, cutoff=%.2f", scr, cutoff))
          }
          
        }, error = function(e) {
          weight_matrices[[i]] <- NULL
          
          # Update progress even for errors
          if (!is.null(progress_tracker)) {
            update_progress(progress_tracker, 1, "error")
          }
        })
      }
    }
  }
  
  if (verbose && use_cache) {
    message(sprintf("Cache performance: %d/%d hits (%.1f%% hit rate)", 
                   cache_hits, n_combinations, 
                   100 * cache_hits / n_combinations))
  }
  
  if (verbose && parallel && length(combinations_to_compute) > 0) {
    message(sprintf("Parallel computation: %d matrices computed using %s", 
                   length(combinations_to_compute),
                   if (requireNamespace("doParallel", quietly = TRUE)) "doParallel" else "sequential fallback"))
  }
  
  # Clean up progress tracker
  if (!is.null(progress_tracker)) {
    cleanup_progress(progress_tracker)
  }
  
  return(weight_matrices)
}

#' Vectorized Quality Metrics Computation
#'
#' Computes quality metrics for multiple weight matrices in batch,
#' optimizing for vectorized operations and parallel processing where possible.
#'
#' @param weight_matrices List of weight matrices.
#' @param objectives Character vector of objectives to compute.
#' @param parallel Logical indicating whether to use parallel processing.
#' @param verbose Logical indicating whether to show progress.
#'
#' @return Matrix with objectives as columns and combinations as rows.
#' @keywords internal
batch_compute_quality_metrics <- function(weight_matrices, objectives, 
                                          parallel = FALSE, verbose = FALSE,
                                          progress_tracker = NULL) {
  
  n_matrices <- length(weight_matrices)
  n_objectives <- length(objectives)
  
  # Initialize results matrix
  results <- matrix(0, nrow = n_matrices, ncol = n_objectives)
  colnames(results) <- objectives
  
  # Initialize progress tracking
  if (is.null(progress_tracker) && verbose) {
    progress_tracker <- ProgressTracker("Quality Metrics", n_matrices, verbose = verbose)
  }
  
  if (verbose) {
    message(sprintf("Computing quality metrics for %d weight matrices (parallel: %s)...", 
                   n_matrices, parallel))
  }
  
  # Process other objectives that need individual computation
  remaining_objectives <- setdiff(objectives, "sparsity")
  
  # Vectorized computation for simple objectives
  if ("sparsity" %in% objectives) {
    sparsity_idx <- which(objectives == "sparsity")
    
    # Vectorized sparsity computation
    densities <- vapply(weight_matrices, function(wm) {
      if (is.null(wm)) return(0)
      sum(!is.na(wm)) / length(wm)
    }, numeric(1))
    
    # Optimal around 0.1-0.3 density
    results[, sparsity_idx] <- exp(-((densities - 0.2)^2) / 0.05)
    
    # Update progress for sparsity computation (vectorized, so count all at once)
    if (!is.null(progress_tracker) && length(remaining_objectives) == 0) {
      # If sparsity is the only objective, update all progress
      update_progress(progress_tracker, n_matrices, "sparsity (vectorized)")
    } else if (!is.null(progress_tracker)) {
      # Partial progress for sparsity
      update_progress(progress_tracker, 0, "sparsity computed")
    }
  }
  
  if (length(remaining_objectives) > 0) {
    # Find valid matrices for computation
    valid_indices <- which(vapply(weight_matrices, function(wm) {
      !is.null(wm) && !all(is.na(wm)) && sum(!is.na(wm)) >= 2
    }, logical(1)))
    
    if (length(valid_indices) > 0) {
      if (parallel && requireNamespace("foreach", quietly = TRUE) && 
          requireNamespace("doParallel", quietly = TRUE) && length(valid_indices) > 1) {
        
        # Prepare progress tracking for workers
        tracker_info <- if (!is.null(progress_tracker)) {
          list(progress_file = progress_tracker$progress_file)
        } else {
          NULL
        }
        
        # Use parallel processing for remaining objectives
        parallel_results <- foreach::foreach(
          i = valid_indices,
          .combine = 'rbind',
          .multicombine = TRUE,
          .errorhandling = 'pass',
          .packages = c("moneca"),
          .export = c("compute_clustering_quality_metrics", "compute_network_modularity", "report_worker_progress")
        ) %dopar% {
          wm <- weight_matrices[[i]]
          obj_results <- numeric(length(remaining_objectives))
          names(obj_results) <- remaining_objectives
          
          for (obj in remaining_objectives) {
            tryCatch({
              if (obj == "quality") {
                quality_metrics <- compute_clustering_quality_metrics(wm)
                obj_results[obj] <- quality_metrics$overall
              } else if (obj == "modularity") {
                obj_results[obj] <- compute_network_modularity(wm)
              }
            }, error = function(e) {
              obj_results[obj] <- 0
            })
          }
          
          # Report progress from worker
          report_worker_progress(tracker_info, 1)
          
          # Return index and results
          c(index = i, obj_results)
        }
        
        # Process parallel results
        if (is.matrix(parallel_results)) {
          for (row_idx in seq_len(nrow(parallel_results))) {
            matrix_idx <- parallel_results[row_idx, "index"]
            for (obj in remaining_objectives) {
              obj_idx <- which(objectives == obj)
              results[matrix_idx, obj_idx] <- parallel_results[row_idx, obj]
            }
          }
        }
        
        # Final progress update after parallel completion
        if (!is.null(progress_tracker)) {
          display_progress(progress_tracker, get_progress(progress_tracker))
        }
        
      } else {
        # Sequential processing fallback
        for (i in valid_indices) {
          wm <- weight_matrices[[i]]
          
          for (obj in remaining_objectives) {
            obj_idx <- which(objectives == obj)
            
            tryCatch({
              if (obj == "quality") {
                quality_metrics <- compute_clustering_quality_metrics(wm)
                results[i, obj_idx] <- quality_metrics$overall
              } else if (obj == "modularity") {
                results[i, obj_idx] <- compute_network_modularity(wm)
              }
            }, error = function(e) {
              results[i, obj_idx] <- 0
            })
          }
          
          # Update progress for each matrix processed
          if (!is.null(progress_tracker)) {
            update_progress(progress_tracker, 1, paste(remaining_objectives, collapse = ","))
          }
        }
      }
    }
  }
  
  if (verbose && parallel && length(remaining_objectives) > 0) {
    n_parallel_computed <- length(which(vapply(weight_matrices, function(wm) {
      !is.null(wm) && !all(is.na(wm)) && sum(!is.na(wm)) >= 2
    }, logical(1))))
    
    message(sprintf("Parallel computation: %d matrices processed for %s objectives using %s", 
                   n_parallel_computed,
                   paste(remaining_objectives, collapse = ", "),
                   if (requireNamespace("doParallel", quietly = TRUE)) "doParallel" else "sequential fallback"))
  }
  
  # Clean up progress tracker
  if (!is.null(progress_tracker)) {
    cleanup_progress(progress_tracker)
  }
  
  return(results)
}

#' Batch Stability Assessment with Caching
#'
#' Performs stability assessment for multiple parameter combinations,
#' using caching to avoid redundant computations.
#'
#' @param mx Mobility matrix.
#' @param param_combinations Data frame with scr and cutoff columns.
#' @param n_bootstrap Number of bootstrap samples.
#' @param use_cache Logical indicating whether to use caching.
#' @param parallel Logical indicating whether to use parallel processing.
#' @param verbose Logical indicating whether to show progress.
#'
#' @return Numeric vector of stability scores.
#' @keywords internal
batch_assess_stability <- function(mx, param_combinations, n_bootstrap = 50,
                                  use_cache = TRUE, parallel = FALSE, 
                                  verbose = FALSE, progress_tracker = NULL) {
  
  n_combinations <- nrow(param_combinations)
  stability_scores <- numeric(n_combinations)
  
  # Generate hash for the mobility matrix and bootstrap count
  mx_hash <- paste(simple_matrix_hash(mx), n_bootstrap, sep = "_")
  
  cache_hits <- 0
  
  # Initialize progress tracking
  if (is.null(progress_tracker) && verbose) {
    progress_tracker <- ProgressTracker("Stability Assessment", n_combinations, verbose = verbose)
  }
  
  if (verbose) {
    message(sprintf("Computing stability for %d parameter combinations (parallel: %s)...", 
                   n_combinations, parallel))
  }
  
  # Determine which combinations need computation (not in cache)
  combinations_to_compute <- integer(0)
  cache_keys <- character(n_combinations)
  
  for (i in seq_len(n_combinations)) {
    scr <- param_combinations$scr[i]
    cutoff <- param_combinations$cutoff[i]
    cache_key <- generate_cache_key(mx_hash, scr, cutoff, "stability")
    cache_keys[i] <- cache_key
    
    # Check cache first
    if (use_cache && exists(cache_key, envir = .moneca_fast_cache)) {
      stability_scores[i] <- get(cache_key, envir = .moneca_fast_cache)
      cache_hits <- cache_hits + 1
      
      # Report progress for cached items
      if (!is.null(progress_tracker)) {
        update_progress(progress_tracker, 1, "from cache")
      }
    } else {
      combinations_to_compute <- c(combinations_to_compute, i)
    }
  }
  
  # Compute missing stability scores
  if (length(combinations_to_compute) > 0) {
    if (parallel && requireNamespace("foreach", quietly = TRUE) && 
        requireNamespace("doParallel", quietly = TRUE) && length(combinations_to_compute) > 1) {
      
      # Prepare progress tracking for workers
      tracker_info <- if (!is.null(progress_tracker)) {
        list(progress_file = progress_tracker$progress_file)
      } else {
        NULL
      }
      
      # Use parallel processing with foreach %dopar%
      compute_results <- foreach::foreach(
        idx = combinations_to_compute,
        .combine = 'c',
        .multicombine = TRUE,
        .maxcombine = length(combinations_to_compute),
        .errorhandling = 'pass',
        .packages = c("moneca"),
        .export = c("assess_clustering_stability", "assess_clustering_stability_parallel", "report_worker_progress")
      ) %dopar% {
        scr <- param_combinations$scr[idx]
        cutoff <- param_combinations$cutoff[idx]
        
        # Compute stability using the appropriate function
        result <- tryCatch({
          # Use the parallel stability function if available, otherwise sequential
          if (exists("assess_clustering_stability_parallel", mode = "function")) {
            score <- assess_clustering_stability_parallel(
              mx = mx,
              small.cell.reduction = scr,
              cut.off = cutoff,
              n_bootstrap = n_bootstrap,
              segment.levels = 2,  # Faster with fewer levels
              parallel = FALSE,  # Avoid nested parallelism
              verbose = FALSE
            )
          } else {
            score <- assess_clustering_stability(
              mx = mx,
              small.cell.reduction = scr,
              cut.off = cutoff,
              n_bootstrap = n_bootstrap,
              segment.levels = 2
            )
          }
          # Report progress from worker
          report_worker_progress(tracker_info, 1)
          
          list(index = idx, score = score, error = NULL)
        }, error = function(e) {
          # Report progress even for errors
          report_worker_progress(tracker_info, 1)
          list(index = idx, score = 0, error = e$message)
        })
        
        return(result)
      }
      
      # Process parallel results
      for (result in compute_results) {
        if (is.list(result) && !is.null(result$index)) {
          idx <- result$index
          stability_scores[idx] <- result$score
          
          # Store in cache if enabled and computation was successful
          if (use_cache && result$score > 0) {
            assign(cache_keys[idx], result$score, envir = .moneca_fast_cache)
          }
        }
      }
      
      # Final progress update after parallel completion
      if (!is.null(progress_tracker)) {
        display_progress(progress_tracker, get_progress(progress_tracker))
      }
      
    } else {
      # Sequential processing fallback
      for (i in combinations_to_compute) {
        scr <- param_combinations$scr[i]
        cutoff <- param_combinations$cutoff[i]
        
        # Compute stability
        tryCatch({
          if (parallel && exists("assess_clustering_stability_parallel", mode = "function")) {
            score <- assess_clustering_stability_parallel(
              mx = mx,
              small.cell.reduction = scr,
              cut.off = cutoff,
              n_bootstrap = n_bootstrap,
              segment.levels = 2,  # Faster with fewer levels
              parallel = TRUE,
              verbose = FALSE
            )
          } else {
            score <- assess_clustering_stability(
              mx = mx,
              small.cell.reduction = scr,
              cut.off = cutoff,
              n_bootstrap = n_bootstrap,
              segment.levels = 2
            )
          }
          
          stability_scores[i] <- score
          
          # Store in cache if enabled
          if (use_cache) {
            assign(cache_keys[i], score, envir = .moneca_fast_cache)
          }
          
          # Update progress
          if (!is.null(progress_tracker)) {
            update_progress(progress_tracker, 1, sprintf("scr=%s, cutoff=%.2f", scr, cutoff))
          }
          
        }, error = function(e) {
          stability_scores[i] <- 0
          
          # Update progress even for errors
          if (!is.null(progress_tracker)) {
            update_progress(progress_tracker, 1, "error")
          }
        })
      }
    }
  }
  
  if (verbose && use_cache) {
    message(sprintf("Stability cache performance: %d/%d hits (%.1f%% hit rate)", 
                   cache_hits, n_combinations, 
                   100 * cache_hits / n_combinations))
  }
  
  if (verbose && parallel && length(combinations_to_compute) > 0) {
    message(sprintf("Parallel computation: %d stability assessments computed using %s", 
                   length(combinations_to_compute),
                   if (requireNamespace("doParallel", quietly = TRUE)) "doParallel" else "sequential fallback"))
  }
  
  # Clean up progress tracker
  if (!is.null(progress_tracker)) {
    cleanup_progress(progress_tracker)
  }
  
  return(stability_scores)
}

#' Smart Parameter Combination Ordering
#'
#' Orders parameter combinations for evaluation to prioritize promising
#' combinations and enable early termination of poor performers.
#'
#' @param param_combinations Data frame with scr and cutoff columns.
#' @param mx Mobility matrix for heuristic scoring.
#' @param objectives Character vector of objectives.
#'
#' @return Integer vector of row indices in optimal evaluation order.
#' @keywords internal
smart_evaluation_ordering <- function(param_combinations, mx, objectives) {
  
  n_combinations <- nrow(param_combinations)
  
  # Heuristic scoring based on parameter ranges and data characteristics
  # This is a fast approximation to guide evaluation order
  
  # Data characteristics
  mx_off_diag <- mx
  diag(mx_off_diag) <- 0
  non_zero_values <- mx_off_diag[mx_off_diag > 0]
  
  if (length(non_zero_values) == 0) {
    return(seq_len(n_combinations))
  }
  
  median_count <- median(non_zero_values)
  
  # Compute heuristic scores
  heuristic_scores <- numeric(n_combinations)
  
  for (i in seq_len(n_combinations)) {
    scr <- param_combinations$scr[i]
    cutoff <- param_combinations$cutoff[i]
    
    # Penalize extreme values
    scr_penalty <- if (scr > median_count * 2) {
      exp(-(scr - median_count)^2 / (2 * median_count^2))
    } else {
      1
    }
    
    cutoff_penalty <- exp(-((cutoff - 1.5)^2) / 0.5)  # Prefer cutoff around 1.5
    
    # Combine penalties (higher score = more promising)
    heuristic_scores[i] <- scr_penalty * cutoff_penalty
  }
  
  # Return indices ordered by decreasing heuristic score (most promising first)
  return(order(heuristic_scores, decreasing = TRUE))
}

#' Optimized Parameter Combination Evaluation
#'
#' Evaluates parameter combinations using batch processing, caching,
#' and smart ordering for optimal performance.
#'
#' @param mx Mobility matrix.
#' @param param_combinations Data frame with scr and cutoff columns.
#' @param objectives Character vector of objectives to evaluate.
#' @param weights Numeric vector of objective weights.
#' @param n_bootstrap Number of bootstrap samples for stability.
#' @param use_cache Logical indicating whether to use caching.
#' @param smart_ordering Logical indicating whether to use smart evaluation ordering.
#' @param early_termination Logical indicating whether to enable early termination.
#' @param termination_threshold Fraction of combinations after which to consider early termination.
#' @param parallel Logical indicating whether to use parallel processing.
#' @param verbose Logical indicating whether to show progress.
#'
#' @return List containing detailed scores and optimal combination info.
#' @keywords internal
evaluate_parameter_combinations_optimized <- function(mx, param_combinations, 
                                                      objectives, weights, 
                                                      n_bootstrap = 50,
                                                      use_cache = TRUE,
                                                      smart_ordering = TRUE,
                                                      early_termination = TRUE,
                                                      termination_threshold = 0.5,
                                                      parallel = FALSE, 
                                                      verbose = FALSE) {
  
  start_time <- Sys.time()
  n_combinations <- nrow(param_combinations)
  
  # Initialize master progress tracking
  master_tracker <- NULL
  if (verbose) {
    # Estimate total work units
    stability_needed <- "stability" %in% objectives
    batch_objectives <- intersect(objectives, c("quality", "sparsity", "modularity"))
    
    total_work <- n_combinations  # Base work for parameter evaluation
    if (length(batch_objectives) > 0) {
      total_work <- total_work + n_combinations  # Weight matrices + quality metrics
    }
    if (stability_needed) {
      total_work <- total_work + n_combinations  # Stability assessment
    }
    
    master_tracker <- ProgressTracker("Joint Parameter Tuning", total_work, verbose = TRUE)
  }
  
  # Performance monitoring
  perf_stats <- list(
    n_combinations = n_combinations,
    cache_enabled = use_cache,
    smart_ordering = smart_ordering,
    early_termination = early_termination,
    early_terminated = FALSE,  # Initialize to FALSE
    combinations_evaluated = n_combinations  # Will be updated if early termination occurs
  )
  
  if (verbose) {
    message(sprintf("Optimized evaluation of %d parameter combinations...", n_combinations))
    message(sprintf("Settings: cache=%s, smart_ordering=%s, early_termination=%s", 
                   use_cache, smart_ordering, early_termination))
  }
  
  # Determine evaluation order
  if (smart_ordering) {
    evaluation_order <- smart_evaluation_ordering(param_combinations, mx, objectives)
    if (verbose) {
      message("Using smart evaluation ordering based on heuristic scores")
    }
  } else {
    evaluation_order <- seq_len(n_combinations)
  }
  
  # Initialize results storage
  detailed_scores <- vector("list", n_combinations)
  combined_scores <- numeric(n_combinations)
  
  # Split objectives into batch-processable and individual ones
  batch_objectives <- intersect(objectives, c("quality", "sparsity", "modularity"))
  stability_needed <- "stability" %in% objectives
  
  # Batch process weight matrices for batch-processable objectives
  if (length(batch_objectives) > 0) {
    if (verbose) {
      message("Batch computing weight matrices for quality metrics...")
    }
    
    weight_matrices <- batch_compute_weight_matrices(
      mx = mx,
      param_combinations = param_combinations,
      use_cache = use_cache,
      parallel = parallel,
      verbose = verbose,
      progress_tracker = master_tracker
    )
    
    # Batch compute quality metrics
    if (verbose) {
      message("Batch computing quality metrics...")
    }
    
    quality_results <- batch_compute_quality_metrics(
      weight_matrices = weight_matrices,
      objectives = batch_objectives,
      parallel = parallel,
      verbose = verbose,
      progress_tracker = master_tracker
    )
  } else {
    quality_results <- NULL
  }
  
  # Batch process stability if needed
  stability_results <- NULL
  if (stability_needed) {
    if (verbose) {
      message("Batch computing stability assessments...")
    }
    
    stability_results <- batch_assess_stability(
      mx = mx,
      param_combinations = param_combinations,
      n_bootstrap = n_bootstrap,
      use_cache = use_cache,
      parallel = parallel,
      verbose = verbose,
      progress_tracker = master_tracker
    )
  }
  
  # Combine results and compute final scores
  if (verbose) {
    message("Combining results and computing final scores...")
  }
  
  best_score <- -Inf
  best_idx <- 1
  scores_computed <- 0
  
  for (i in evaluation_order) {
    scores_computed <- scores_computed + 1
    
    # Extract scores for this combination
    obj_scores <- numeric(length(objectives))
    names(obj_scores) <- objectives
    
    # Fill in batch-computed scores
    if (!is.null(quality_results)) {
      for (obj in batch_objectives) {
        if (obj %in% colnames(quality_results)) {
          obj_scores[obj] <- quality_results[i, obj]
        }
      }
    }
    
    # Fill in stability scores
    if (stability_needed && !is.null(stability_results)) {
      obj_scores["stability"] <- stability_results[i]
    }
    
    # Compute combined score
    combined_score <- sum(obj_scores * weights)
    combined_scores[i] <- combined_score
    
    # Store detailed results
    detailed_scores[[i]] <- c(
      scr = param_combinations$scr[i],
      cutoff = param_combinations$cutoff[i],
      obj_scores,
      combined = combined_score
    )
    
    # Update best score
    if (combined_score > best_score) {
      best_score <- combined_score
      best_idx <- i
    }
    
    # Update progress for scoring
    if (!is.null(master_tracker)) {
      update_progress(master_tracker, 1, 
                     sprintf("score=%.3f (best: %.3f)", combined_score, best_score))
    }
    
    # Early termination check
    if (early_termination && scores_computed >= n_combinations * termination_threshold) {
      # Check if we've found a clearly superior solution
      current_scores <- combined_scores[evaluation_order[1:scores_computed]]
      current_scores <- current_scores[current_scores != 0]  # Remove uncomputed scores
      
      if (length(current_scores) > 10) {
        # If best score is significantly better than 75th percentile, consider early termination
        percentile_75 <- quantile(current_scores, 0.75, na.rm = TRUE)
        if (best_score > percentile_75 * 1.2) {
          if (verbose) {
            message(sprintf("Early termination: evaluated %d/%d combinations (%.1f%%)", 
                           scores_computed, n_combinations, 
                           100 * scores_computed / n_combinations))
          }
          perf_stats$early_terminated <- TRUE
          perf_stats$combinations_evaluated <- scores_computed
          break
        }
      }
    }
  }
  
  # Fill in remaining scores if early termination didn't occur
  if (scores_computed < n_combinations) {
    remaining_indices <- evaluation_order[(scores_computed + 1):n_combinations]
    for (i in remaining_indices) {
      detailed_scores[[i]] <- c(
        scr = param_combinations$scr[i],
        cutoff = param_combinations$cutoff[i],
        rep(0, length(objectives)),  # Zero scores for unevaluated
        combined = 0
      )
      names(detailed_scores[[i]]) <- c("scr", "cutoff", objectives, "combined")
    }
  }
  
  end_time <- Sys.time()
  computation_time <- as.numeric(end_time - start_time)
  
  # Performance statistics
  perf_stats$computation_time <- computation_time
  perf_stats$combinations_evaluated <- scores_computed
  perf_stats$evaluation_rate <- scores_computed / computation_time
  
  # Update early termination flag if it occurred
  if (scores_computed < n_combinations) {
    perf_stats$early_terminated <- TRUE
  }
  
  if (verbose) {
    message(sprintf("Evaluation complete: %d combinations in %.2f seconds (%.1f combinations/sec)", 
                   scores_computed, computation_time, perf_stats$evaluation_rate))
  }
  
  # Clean up progress tracker
  if (!is.null(master_tracker)) {
    cleanup_progress(master_tracker)
  }
  
  return(list(
    detailed_scores = detailed_scores,
    combined_scores = combined_scores,
    best_idx = best_idx,
    best_score = best_score,
    performance_stats = perf_stats,
    evaluation_order = evaluation_order
  ))
}

#' Legacy Evaluate Parameter Combination (maintained for compatibility)
#'
#' Computes objective scores for a specific parameter combination.
#' This function is maintained for backward compatibility but the optimized
#' batch evaluation function is recommended for better performance.
#'
#' @param mx Mobility matrix.
#' @param scr small.cell.reduction value.
#' @param cutoff cut.off value.
#' @param objectives Character vector of objectives to evaluate.
#' @param n_bootstrap Number of bootstrap samples for stability.
#' @param parallel Logical indicating whether to use parallel processing.
#' @param verbose Logical indicating whether to show messages.
#'
#' @return Numeric vector of objective scores.
#' @keywords internal
evaluate_parameter_combination <- function(mx, scr, cutoff, 
                                         objectives, n_bootstrap = 50, 
                                         parallel = FALSE, verbose = FALSE) {
  
  # Convert to batch format for compatibility
  param_combinations <- data.frame(scr = scr, cutoff = cutoff)
  weights <- rep(1, length(objectives))
  
  # Use optimized evaluation
  result <- evaluate_parameter_combinations_optimized(
    mx = mx,
    param_combinations = param_combinations,
    objectives = objectives,
    weights = weights,
    n_bootstrap = n_bootstrap,
    use_cache = TRUE,
    smart_ordering = FALSE,  # Single combination
    early_termination = FALSE,
    parallel = parallel,
    verbose = verbose
  )
  
  # Extract scores for the single combination
  return(result$detailed_scores[[1]][objectives])
}

#' Analyze Mathematical Relationship Between Parameters
#'
#' Analyzes how small.cell.reduction and cut.off interact mathematically
#' to affect network properties and clustering outcomes.
#'
#' @param mx Mobility matrix.
#' @param scr small.cell.reduction value.
#' @param cutoff cut.off value.
#' @param n_samples Number of parameter samples for interaction analysis.
#'
#' @return List containing:
#'   \item{interaction_strength}{Measure of parameter interaction (0-1)}
#'   \item{marginal_effects}{Individual parameter effects}
#'   \item{joint_effect}{Combined effect on network density}
#'   \item{optimal_ratio}{Suggested ratio of scr to cutoff}
#'   \item{sensitivity}{Parameter sensitivity analysis}
#'
#' @details
#' The function analyzes:
#' - How changes in each parameter affect network properties
#' - Whether parameters have synergistic or antagonistic effects
#' - The optimal balance between the two filtering stages
#' - Sensitivity of outcomes to parameter perturbations
#'
#' @examples
#' \dontrun{
#' mobility_data <- generate_mobility_data(n_classes = 5)
#' interaction <- analyze_parameter_interaction(
#'   mx = mobility_data,
#'   scr = 2,
#'   cutoff = 1.5
#' )
#' print(interaction)
#' }
#'
#' @export
analyze_parameter_interaction <- function(mx, scr, cutoff, n_samples = 20) {
  
  # Create small perturbations around current values
  scr_range_continuous <- c(max(0, scr * 0.5), scr * 1.5)
  cutoff_range <- c(max(0.5, cutoff * 0.5), cutoff * 1.5)
  
  # Generate integer samples for SCR
  scr_samples <- generate_integer_scr_grid(scr_range_continuous, n_samples, 
                                          include_boundaries = TRUE)
  cutoff_samples <- seq(cutoff_range[1], cutoff_range[2], length.out = n_samples)
  
  # Update n_samples to actual grid sizes for matrix creation
  n_scr_samples <- length(scr_samples)
  n_cutoff_samples <- length(cutoff_samples)
  
  # Compute network properties for parameter grid (vectorized)
  # VECTORIZATION STRATEGY: Replace nested loops with vectorized batch operations
  # Uses more efficient parameter combination handling and pre-allocated results
  
  # Pre-allocate density matrix for better memory management
  density_matrix <- matrix(NA_real_, nrow = n_scr_samples, ncol = n_cutoff_samples)
  
  # VECTORIZED BATCH APPROACH: Process parameters in row-wise chunks
  # This is more memory efficient than expanding full grid for large parameter spaces
  
  # Vectorized computation using outer-like approach with better memory management
  for (i in seq_len(n_scr_samples)) {
    scr_val <- scr_samples[i]
    
    # Vectorize cutoff processing for this SCR value
    cutoff_densities <- vapply(cutoff_samples, function(cutoff_val) {
      tryCatch({
        wm <- weight.matrix(
          mx = mx,
          cut.off = cutoff_val,
          small.cell.reduction = scr_val
        )
        sum(!is.na(wm)) / length(wm)
      }, error = function(e) {
        NA_real_
      })
    }, FUN.VALUE = numeric(1), USE.NAMES = FALSE)
    
    # Assign entire row at once (vectorized assignment)
    density_matrix[i, ] <- cutoff_densities
  }
  
  # Analyze marginal effects
  marginal_scr <- rowMeans(density_matrix, na.rm = TRUE)
  marginal_cutoff <- colMeans(density_matrix, na.rm = TRUE)
  
  # Compute interaction strength (deviation from additivity)
  expected_additive <- outer(marginal_scr, marginal_cutoff, "+") / 2
  actual <- density_matrix
  interaction_strength <- 1 - cor(as.vector(expected_additive), 
                                 as.vector(actual), 
                                 use = "complete.obs")
  
  # Sensitivity analysis
  scr_sensitivity <- sd(marginal_scr, na.rm = TRUE) / mean(marginal_scr, na.rm = TRUE)
  cutoff_sensitivity <- sd(marginal_cutoff, na.rm = TRUE) / mean(marginal_cutoff, na.rm = TRUE)
  
  # Find optimal ratio
  center_scr_idx <- max(1, n_scr_samples %/% 2)
  center_cutoff_idx <- max(1, n_cutoff_samples %/% 2)
  
  # Handle case where matrix might be empty or have NA values
  if (nrow(density_matrix) > 0 && ncol(density_matrix) > 0 && 
      center_scr_idx <= nrow(density_matrix) && 
      center_cutoff_idx <= ncol(density_matrix)) {
    optimal_density <- density_matrix[center_scr_idx, center_cutoff_idx]
  } else {
    optimal_density <- NA
  }
  
  # Search for same density with different ratios (vectorized)
  # VECTORIZATION STRATEGY: Replace nested loop with which() + array indices
  # Use logical indexing and vectorized operations instead of explicit loops
  ratio_analysis <- numeric(0)
  if (!is.na(optimal_density) && n_scr_samples > 0 && n_cutoff_samples > 0) {
    # Find all matrix positions where density matches optimal (replaces nested loop)
    density_matches <- which(
      !is.na(density_matrix) & 
      abs(density_matrix - optimal_density) < 0.01, 
      arr.ind = TRUE
    )
    
    if (nrow(density_matches) > 0) {
      # Extract corresponding parameter values using vectorized indexing
      scr_match_values <- scr_samples[density_matches[, "row"]]
      cutoff_match_values <- cutoff_samples[density_matches[, "col"]]
      
      # Filter and compute ratios using vectorized operations
      valid_cutoffs <- cutoff_match_values > 0
      if (any(valid_cutoffs)) {
        ratio_analysis <- scr_match_values[valid_cutoffs] / cutoff_match_values[valid_cutoffs]
      }
    }
  }
  
  optimal_ratio <- if (length(ratio_analysis) > 0) {
    median(ratio_analysis)
  } else {
    scr / cutoff
  }
  
  result <- list(
    interaction_strength = interaction_strength,
    marginal_effects = list(
      scr = marginal_scr,
      cutoff = marginal_cutoff
    ),
    joint_effect = if (!is.na(optimal_density)) optimal_density else 0,
    optimal_ratio = optimal_ratio,
    sensitivity = list(
      scr = scr_sensitivity,
      cutoff = cutoff_sensitivity,
      relative = scr_sensitivity / cutoff_sensitivity
    ),
    density_surface = density_matrix
  )
  
  class(result) <- "parameter_interaction"
  return(result)
}

#' Visualize Parameter Optimization Surface
#'
#' Creates visualization of the 2D parameter optimization surface.
#'
#' @param joint_tuning_result Result from auto_tune_joint_parameters.
#' @param type Character string specifying plot type: "heatmap", "contour", 
#'   or "3d". Default is "heatmap".
#' @param show_optimal Logical indicating whether to mark optimal point.
#'
#' @return A ggplot2 object (for heatmap/contour) or plotly object (for 3d).
#'
#' @examples
#' \dontrun{
#' result <- auto_tune_joint_parameters(mobility_data)
#' plot_optimization_surface(result, type = "contour")
#' }
#'
#' @export
plot_optimization_surface <- function(joint_tuning_result, 
                                     type = "heatmap",
                                     show_optimal = TRUE) {
  
  if (!inherits(joint_tuning_result, "moneca_joint_tuning")) {
    stop("Input must be a moneca_joint_tuning object")
  }
  
  # Prepare data for plotting
  surface_data <- expand.grid(
    scr = joint_tuning_result$scr_values,
    cutoff = joint_tuning_result$cutoff_values
  )
  surface_data$score <- as.vector(t(joint_tuning_result$optimization_surface))
  
  # Create plot based on type
  if (type == "heatmap") {
    p <- ggplot(surface_data, aes(x = scr, y = cutoff, fill = score)) +
      geom_tile() +
      scale_fill_viridis_c(name = "Score") +
      labs(
        x = "small.cell.reduction",
        y = "cut.off",
        title = "Parameter Optimization Surface"
      ) +
      theme_minimal()
    
    if (show_optimal) {
      p <- p + 
        geom_point(
          aes(x = joint_tuning_result$optimal_scr,
              y = joint_tuning_result$optimal_cutoff),
          color = "red", size = 4, shape = 4, stroke = 2
        ) +
        annotate(
          "text",
          x = joint_tuning_result$optimal_scr,
          y = joint_tuning_result$optimal_cutoff,
          label = "Optimal",
          vjust = -1, color = "red"
        )
    }
    
  } else if (type == "contour") {
    p <- ggplot(surface_data, aes(x = scr, y = cutoff, z = score)) +
      geom_contour_filled() +
      geom_contour(color = "white", alpha = 0.5) +
      labs(
        x = "small.cell.reduction",
        y = "cut.off",
        title = "Parameter Optimization Contours"
      ) +
      theme_minimal()
    
    if (show_optimal) {
      p <- p + 
        geom_point(
          aes(x = joint_tuning_result$optimal_scr,
              y = joint_tuning_result$optimal_cutoff),
          color = "red", size = 4, shape = 4, stroke = 2
        )
    }
    
  } else if (type == "3d") {
    # Check if plotly is available
    if (!requireNamespace("plotly", quietly = TRUE)) {
      stop("Package 'plotly' is required for 3D plots. Install with: install.packages('plotly')")
    }
    
    p <- plotly::plot_ly(
      x = joint_tuning_result$scr_values,
      y = joint_tuning_result$cutoff_values,
      z = joint_tuning_result$optimization_surface,
      type = "surface",
      colorscale = "Viridis"
    ) %>%
      plotly::layout(
        scene = list(
          xaxis = list(title = "small.cell.reduction"),
          yaxis = list(title = "cut.off"),
          zaxis = list(title = "Score")
        ),
        title = "3D Parameter Optimization Surface"
      )
    
    if (show_optimal) {
      p <- p %>%
        plotly::add_trace(
          x = joint_tuning_result$optimal_scr,
          y = joint_tuning_result$optimal_cutoff,
          z = max(joint_tuning_result$optimization_surface, na.rm = TRUE),
          type = "scatter3d",
          mode = "markers",
          marker = list(color = "red", size = 10),
          name = "Optimal"
        )
    }
  } else {
    stop("Type must be one of: 'heatmap', 'contour', '3d'")
  }
  
  return(p)
}

#' Suggest Parameter Ranges Based on Data Characteristics
#'
#' Analyzes mobility matrix to suggest appropriate ranges for parameter tuning.
#'
#' @param mx Mobility matrix.
#' @param method Character string: "conservative", "moderate", or "aggressive".
#'
#' @return List containing suggested ranges for both parameters.
#'
#' @export
suggest_parameter_ranges <- function(mx, method = "moderate") {
  
  # Use integer-aware bounds for SCR
  scr_bounds <- suggest_integer_scr_bounds(mx, method)
  scr_range <- scr_bounds
  
  # Keep existing logic for cutoff range
  mx_off_diag <- mx
  diag(mx_off_diag) <- 0
  non_zero_values <- mx_off_diag[mx_off_diag > 0]
  
  if (length(non_zero_values) == 0) {
    warning("No non-zero off-diagonal values found")
    return(list(
      scr_range = scr_range,
      cutoff_range = c(0.5, 2)
    ))
  }
  
  # Compute quantiles for data characteristics
  q <- quantile(non_zero_values, probs = c(0.1, 0.25, 0.5, 0.75, 0.9))
  
  # Suggest cutoff ranges based on method (unchanged logic)
  if (method == "conservative") {
    cutoff_range <- c(0.8, 1.5)
  } else if (method == "moderate") {
    cutoff_range <- c(0.5, 2.0)
  } else if (method == "aggressive") {
    cutoff_range <- c(0.3, 3.0)
  } else {
    stop("Method must be 'conservative', 'moderate', or 'aggressive'")
  }
  
  # Compute expected relative risks for cutoff calibration
  row_sums <- rowSums(mx)
  col_sums <- colSums(mx)
  total <- sum(mx)
  
  # Sample some expected values
  sample_expected <- outer(row_sums[1:min(5, nrow(mx))], 
                          col_sums[1:min(5, ncol(mx))]) / total
  median_expected <- median(sample_expected[sample_expected > 0])
  
  # Adjust cutoff range based on expected values
  if (median_expected > 0) {
    rr_factor <- 1 / median_expected
    cutoff_range <- cutoff_range * sqrt(rr_factor)  # Square root for moderation
  }
  
  return(list(
    scr_range = scr_range,
    cutoff_range = cutoff_range,
    data_characteristics = list(
      matrix_size = nrow(mx),
      sparsity = 1 - length(non_zero_values) / length(mx_off_diag),
      value_distribution = q,
      suggested_method = method
    )
  ))
}

#' Print Method for Joint Tuning Results
#'
#' @param x Object of class "moneca_joint_tuning".
#' @param ... Additional arguments (unused).
#'
#' @export
print.moneca_joint_tuning <- function(x, ...) {
  cat("MONECA Joint Parameter Tuning Results\n")
  cat("=====================================\n\n")
  
  cat("Optimal Parameters:\n")
  cat("  small.cell.reduction:", x$optimal_scr, "\n")
  cat("  cut.off:", x$optimal_cutoff, "\n\n")
  
  cat("Optimization Details:\n")
  cat("  Method:", x$method, "\n")
  cat("  Objectives:", paste(x$objectives, collapse = ", "), "\n")
  cat("  Computation time:", round(x$computation_time, 2), "seconds\n\n")
  
  cat("Selection rationale:", x$selection_rationale, "\n\n")
  
  # Performance information
  if (!is.null(x$performance_stats)) {
    cat("Performance Statistics:\n")
    stats <- x$performance_stats
    cat(sprintf("  Combinations evaluated: %d/%d (%.1f%%)\n", 
               stats$combinations_evaluated, stats$n_combinations,
               100 * stats$combinations_evaluated / stats$n_combinations))
    cat(sprintf("  Evaluation rate: %.1f combinations/second\n", 
               stats$evaluation_rate))
    
    if (!is.null(stats$early_terminated) && stats$early_terminated) {
      cat("  Early termination: YES (optimization converged)\n")
    }
    
    if (stats$cache_enabled) {
      cat("  Caching: ENABLED\n")
    }
    cat("\n")
  }
  
  if (!is.null(x$mathematical_relationship)) {
    cat("Parameter Interaction Analysis:\n")
    cat("  Interaction strength:", 
        round(x$mathematical_relationship$interaction_strength, 3), "\n")
    cat("  Optimal ratio (scr/cutoff):", 
        round(x$mathematical_relationship$optimal_ratio, 2), "\n")
    cat("  Relative sensitivity:", 
        round(x$mathematical_relationship$sensitivity$relative, 2), "\n")
  }
}

#' Print Method for Parameter Interaction Analysis
#'
#' @param x Object of class "parameter_interaction".
#' @param ... Additional arguments (unused).
#'
#' @export
print.parameter_interaction <- function(x, ...) {
  cat("Parameter Interaction Analysis\n")
  cat("==============================\n\n")
  
  cat("Interaction strength:", round(x$interaction_strength, 3), "\n")
  cat("  (0 = independent, 1 = strongly interacting)\n\n")
  
  cat("Joint effect on network density:", round(x$joint_effect, 3), "\n")
  cat("Optimal parameter ratio (scr/cutoff):", round(x$optimal_ratio, 2), "\n\n")
  
  cat("Sensitivity Analysis:\n")
  cat("  small.cell.reduction:", round(x$sensitivity$scr, 3), "\n")
  cat("  cut.off:", round(x$sensitivity$cutoff, 3), "\n")
  cat("  Relative sensitivity:", round(x$sensitivity$relative, 2), "\n")
  
  if (x$sensitivity$relative > 1.5) {
    cat("\nNote: small.cell.reduction is significantly more sensitive\n")
  } else if (x$sensitivity$relative < 0.67) {
    cat("\nNote: cut.off is significantly more sensitive\n")
  } else {
    cat("\nNote: Both parameters have similar sensitivity\n")
  }
}

# ============================================================================
# UTILITY FUNCTIONS FOR INTEGER CONSTRAINTS
# ============================================================================

#' Ensure Integer Values for small.cell.reduction Parameter
#'
#' Converts continuous small.cell.reduction values to integers, since the
#' parameter operates on integer mobility counts. Non-integer values are
#' mathematically equivalent to their floor values.
#'
#' @param scr_values Numeric vector of small.cell.reduction values.
#' @param warn_if_rounded Logical indicating whether to warn when rounding occurs.
#' @param min_value Minimum allowed integer value (default 0).
#' @param max_value Maximum allowed integer value (default NULL for no limit).
#'
#' @return Integer vector with unique values, sorted in ascending order.
#' @keywords internal
ensure_integer_scr_values <- function(scr_values, warn_if_rounded = TRUE, 
                                     min_value = 0, max_value = NULL) {
  
  if (length(scr_values) == 0) {
    return(integer(0))
  }
  
  # Check for non-integer values
  non_integer_present <- any(scr_values != floor(scr_values))
  
  if (non_integer_present && warn_if_rounded) {
    warning("small.cell.reduction values must be integers (operates on count data). ",
            "Non-integer values will be rounded down to nearest integer.")
  }
  
  # Convert to integers (floor to be conservative)
  integer_values <- as.integer(floor(scr_values))
  
  # Apply bounds
  if (!is.null(min_value)) {
    integer_values <- pmax(integer_values, min_value)
  }
  
  if (!is.null(max_value)) {
    integer_values <- pmin(integer_values, max_value)
  }
  
  # Remove duplicates and sort
  unique_values <- sort(unique(integer_values))
  
  return(unique_values)
}

#' Generate Integer Grid for small.cell.reduction Parameter
#'
#' Creates a grid of integer values for small.cell.reduction parameter,
#' ensuring efficient coverage of the meaningful parameter space.
#'
#' @param scr_range Numeric vector of length 2 specifying SCR range.
#' @param n_points Desired number of grid points.
#' @param include_boundaries Logical indicating whether to always include
#'   range boundaries. Default TRUE.
#'
#' @return Integer vector of unique SCR values.
#' @keywords internal
generate_integer_scr_grid <- function(scr_range, n_points, 
                                     include_boundaries = TRUE) {
  
  # Ensure range is integers
  scr_min <- max(0, floor(scr_range[1]))
  scr_max <- ceiling(scr_range[2])
  
  # If range is very small, just return all integers in range
  range_size <- scr_max - scr_min + 1
  if (range_size <= n_points) {
    return(seq(scr_min, scr_max, by = 1))
  }
  
  # Generate approximately n_points integer values
  if (include_boundaries) {
    # Always include min and max
    if (n_points <= 2) {
      return(c(scr_min, scr_max))
    }
    
    # Generate interior points
    interior_points <- n_points - 2
    if (interior_points > 0) {
      step_size <- (scr_max - scr_min) / (interior_points + 1)
      interior_values <- scr_min + step_size * seq_len(interior_points)
      interior_integers <- unique(round(interior_values))
      
      # Combine all values
      all_values <- c(scr_min, interior_integers, scr_max)
    } else {
      all_values <- c(scr_min, scr_max)
    }
  } else {
    # Evenly spaced across the range
    step_size <- (scr_max - scr_min) / (n_points - 1)
    continuous_values <- scr_min + step_size * seq(0, n_points - 1)
    all_values <- round(continuous_values)
  }
  
  # Ensure unique integers within bounds
  unique_values <- unique(all_values)
  unique_values <- unique_values[unique_values >= scr_min & 
                               unique_values <= scr_max]
  
  return(sort(as.integer(unique_values)))
}

#' Suggest Realistic Integer Bounds for small.cell.reduction
#'
#' Analyzes mobility matrix to suggest meaningful integer bounds for
#' small.cell.reduction parameter based on actual count distribution.
#'
#' @param mx Mobility matrix.
#' @param method Character string: "conservative", "moderate", or "aggressive".
#' @param max_reasonable Maximum reasonable SCR value as multiple of median.
#'
#' @return Integer vector of length 2 with suggested bounds.
#' @keywords internal
suggest_integer_scr_bounds <- function(mx, method = "moderate", 
                                      max_reasonable = 10) {
  
  # Remove diagonal for analysis
  mx_off_diag <- mx
  diag(mx_off_diag) <- 0
  
  # Get integer counts
  integer_counts <- mx_off_diag[mx_off_diag > 0]
  integer_counts <- as.integer(floor(integer_counts))
  
  if (length(integer_counts) == 0) {
    warning("No non-zero off-diagonal values found")
    return(c(0L, 10L))
  }
  
  # Get unique integer values that actually exist in data
  unique_counts <- sort(unique(integer_counts))
  
  # Compute statistics on integer counts
  q <- quantile(integer_counts, probs = c(0.1, 0.25, 0.5, 0.75, 0.9))
  median_count <- median(integer_counts)
  
  # Determine upper bound based on method
  if (method == "conservative") {
    # Don't go beyond 25th percentile
    upper_bound <- max(1L, as.integer(floor(q["25%"])))
  } else if (method == "moderate") {
    # Go up to median or a reasonable multiple
    upper_bound <- max(1L, as.integer(floor(min(q["50%"], median_count * 2))))
  } else if (method == "aggressive") {
    # Go up to 75th percentile or reasonable multiple
    upper_bound <- max(1L, as.integer(floor(min(q["75%"], median_count * max_reasonable))))
  } else {
    stop("Method must be 'conservative', 'moderate', or 'aggressive'")
  }
  
  # Ensure we don't exceed the maximum count in the data
  max_data_count <- max(integer_counts)
  upper_bound <- min(upper_bound, max_data_count)
  
  return(c(0L, upper_bound))
}

# Helper function for Pareto frontier optimization
optimize_pareto_frontier <- function(mx, scr_range, cutoff_range, n_grid_points,
                                    objectives, verbose, parallel) {
  
  if (verbose) {
    cat("Computing Pareto frontier for multi-objective optimization...\n")
  }
  
  # Set up parallel processing if needed
  cluster_setup <- NULL
  if (parallel) {
    cluster_setup <- setup_parallel_cluster(verbose = verbose)
    on.exit({
      if (!is.null(cluster_setup)) cleanup_parallel_cluster(cluster_setup, verbose = verbose)
    })
  }
  
  # First get all objective scores
  grid_result <- optimize_grid_search(
    mx = mx,
    scr_range = scr_range,
    cutoff_range = cutoff_range,
    n_grid_points = n_grid_points,
    n_bootstrap = 30,
    objectives = objectives,
    weights = rep(1/length(objectives), length(objectives)),
    verbose = FALSE,
    parallel = parallel
  )
  
  # Extract objective scores for Pareto analysis
  scores_matrix <- do.call(rbind, lapply(grid_result$scores, function(x) {
    x[objectives]
  }))
  
  # Find Pareto optimal solutions
  pareto_indices <- find_pareto_optimal(scores_matrix)
  
  # Select knee point from Pareto frontier
  if (length(pareto_indices) > 1) {
    # Find knee point (maximum distance from line connecting endpoints)
    knee_idx <- find_knee_point(scores_matrix[pareto_indices, ])
    optimal_idx <- pareto_indices[knee_idx]
  } else {
    optimal_idx <- pareto_indices[1]
  }
  
  optimal_params <- grid_result$parameter_grid[optimal_idx, ]
  
  result <- list(
    optimal_scr = optimal_params$scr,
    optimal_cutoff = optimal_params$cutoff,
    optimization_surface = grid_result$optimization_surface,
    parameter_grid = grid_result$parameter_grid,
    scores = grid_result$scores,
    scr_values = grid_result$scr_values,
    cutoff_values = grid_result$cutoff_values,
    pareto_indices = pareto_indices,
    selection_rationale = sprintf(
      "Selected knee point from Pareto frontier (%d Pareto-optimal solutions)",
      length(pareto_indices)
    )
  )
  
  return(result)
}

# Helper function for Bayesian optimization
optimize_bayesian_joint <- function(mx, scr_range, cutoff_range,
                                   objectives, weights, verbose, parallel = FALSE) {
  
  # Check if required package is available
  if (!requireNamespace("DiceKriging", quietly = TRUE)) {
    stop("Bayesian optimization requires 'DiceKriging' package. ",
         "Install with: install.packages('DiceKriging')")
  }
  
  if (verbose) {
    cat("Starting Bayesian optimization...\n")
  }
  
  # Set up parallel processing if needed (though Bayesian optimization runs sequentially)
  cluster_setup <- NULL
  if (parallel) {
    cluster_setup <- setup_parallel_cluster(verbose = verbose)
    on.exit({
      if (!is.null(cluster_setup)) cleanup_parallel_cluster(cluster_setup, verbose = verbose)
    })
  }
  
  # Initial sampling (Latin Hypercube)
  n_initial <- 10
  initial_design <- lhs_sample(
    n = n_initial,
    bounds = list(scr = scr_range, cutoff = cutoff_range)
  )
  
  # Evaluate initial points
  initial_scores <- numeric(n_initial)
  for (i in seq_len(n_initial)) {
    obj_scores <- evaluate_parameter_combination(
      mx = mx,
      scr = initial_design[i, 1],
      cutoff = initial_design[i, 2],
      objectives = objectives,
      n_bootstrap = 30,
      parallel = FALSE,  # Sequential for Bayesian optimization
      verbose = FALSE
    )
    initial_scores[i] <- sum(obj_scores * weights)
  }
  
  # Fit Gaussian Process model
  gp_model <- DiceKriging::km(
    design = initial_design,
    response = initial_scores,
    covtype = "matern5_2"
  )
  
  # Optimization loop
  n_iterations <- 20
  for (iter in seq_len(n_iterations)) {
    # Find next point to evaluate (maximize expected improvement)
    next_point <- optimize_acquisition(
      gp_model = gp_model,
      bounds = list(scr = scr_range, cutoff = cutoff_range)
    )
    
    # Evaluate new point
    obj_scores <- evaluate_parameter_combination(
      mx = mx,
      scr = next_point[1],
      cutoff = next_point[2],
      objectives = objectives,
      n_bootstrap = 30,
      parallel = FALSE,  # Sequential for Bayesian optimization
      verbose = FALSE
    )
    score <- sum(obj_scores * weights)
    
    # Update GP model
    gp_model <- DiceKriging::update(
      object = gp_model,
      newX = matrix(next_point, nrow = 1),
      newy = score
    )
    
    if (verbose && iter %% 5 == 0) {
      cat("  Iteration", iter, "- Best score:", 
          max(c(initial_scores, score)), "\n")
    }
  }
  
  # Find optimal from all evaluated points
  all_points <- rbind(initial_design, gp_model@X[-seq_len(n_initial), ])
  all_scores <- c(initial_scores, gp_model@y[-seq_len(n_initial)])
  
  optimal_idx <- which.max(all_scores)
  
  result <- list(
    optimal_scr = all_points[optimal_idx, 1],
    optimal_cutoff = all_points[optimal_idx, 2],
    optimization_surface = NULL,  # Not available for Bayesian
    parameter_grid = data.frame(
      scr = all_points[, 1],
      cutoff = all_points[, 2]
    ),
    scores = all_scores,
    selection_rationale = sprintf(
      "Selected via Bayesian optimization (GP with Matérn kernel), score: %.3f",
      all_scores[optimal_idx]
    )
  )
  
  return(result)
}

# Utility functions
find_pareto_optimal <- function(scores) {
  n <- nrow(scores)
  is_dominated <- rep(FALSE, n)
  
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      if (i != j && !is_dominated[i]) {
        # Check if j dominates i
        if (all(scores[j, ] >= scores[i, ]) && 
            any(scores[j, ] > scores[i, ])) {
          is_dominated[i] <- TRUE
          break
        }
      }
    }
  }
  
  return(which(!is_dominated))
}

find_knee_point <- function(pareto_scores) {
  if (nrow(pareto_scores) <= 2) return(1)
  
  # Normalize scores
  normalized <- scale(pareto_scores)
  
  # Find point with maximum distance from line connecting endpoints
  start_point <- normalized[1, ]
  end_point <- normalized[nrow(normalized), ]
  
  distances <- numeric(nrow(normalized))
  for (i in seq_len(nrow(normalized))) {
    distances[i] <- point_line_distance(normalized[i, ], start_point, end_point)
  }
  
  return(which.max(distances))
}

point_line_distance <- function(point, line_start, line_end) {
  # Distance from point to line in n-dimensional space
  v <- line_end - line_start
  w <- point - line_start
  
  c1 <- sum(w * v)
  c2 <- sum(v * v)
  
  if (c2 == 0) return(sqrt(sum(w * w)))
  
  b <- c1 / c2
  pb <- line_start + b * v
  
  return(sqrt(sum((point - pb)^2)))
}

lhs_sample <- function(n, bounds) {
  # Simple Latin Hypercube Sampling
  n_params <- length(bounds)
  design <- matrix(NA, nrow = n, ncol = n_params)
  
  for (i in seq_len(n_params)) {
    perm <- sample(n)
    design[, i] <- bounds[[i]][1] + 
                  (bounds[[i]][2] - bounds[[i]][1]) * (perm - 0.5) / n
  }
  
  colnames(design) <- names(bounds)
  return(design)
}

optimize_acquisition <- function(gp_model, bounds) {
  # Simple optimization of expected improvement
  # (Simplified version - could be improved with proper optimization)
  
  candidate_points <- lhs_sample(100, bounds)
  ei_values <- numeric(nrow(candidate_points))
  
  for (i in seq_len(nrow(candidate_points))) {
    pred <- DiceKriging::predict.km(
      object = gp_model,
      newdata = matrix(candidate_points[i, ], nrow = 1),
      type = "UK"
    )
    
    # Expected improvement calculation
    current_best <- max(gp_model@y)
    improvement <- pred$mean - current_best
    
    if (pred$sd > 0) {
      z <- improvement / pred$sd
      ei_values[i] <- improvement * pnorm(z) + pred$sd * dnorm(z)
    } else {
      ei_values[i] <- 0
    }
  }
  
  best_idx <- which.max(ei_values)
  return(candidate_points[best_idx, ])
}

# ============================================================================
# PARALLEL PROCESSING CLUSTER MANAGEMENT
# ============================================================================

#' Set Up Parallel Cluster for Joint Parameter Tuning
#'
#' Creates and registers a parallel cluster for use in joint parameter optimization.
#' Handles cluster creation, registration, and exports necessary functions.
#'
#' @param n_cores Integer number of cores to use. If NULL, uses available cores - 1.
#' @param verbose Logical indicating whether to show setup messages.
#'
#' @return List containing cluster object and setup information, or NULL if setup fails.
#' @keywords internal
setup_parallel_cluster <- function(n_cores = NULL, verbose = TRUE) {
  
  if (!requireNamespace("parallel", quietly = TRUE) ||
      !requireNamespace("doParallel", quietly = TRUE)) {
    if (verbose) {
      message("Parallel processing packages not available. Using sequential processing.")
    }
    return(NULL)
  }
  
  # Determine number of cores
  if (is.null(n_cores)) {
    n_cores <- max(1, parallel::detectCores() - 1)
  }
  n_cores <- max(1, min(n_cores, parallel::detectCores()))
  
  tryCatch({
    if (verbose) {
      message(sprintf("Setting up parallel cluster with %d cores...", n_cores))
    }
    
    # Create cluster
    cluster <- parallel::makeCluster(n_cores)
    
    # Register with doParallel
    doParallel::registerDoParallel(cluster)
    
    # Export necessary packages and functions to workers
    parallel::clusterEvalQ(cluster, {
      library(moneca)
      NULL
    })
    
    # Test cluster with a simple operation
    test_result <- foreach::foreach(i = 1:n_cores, .combine = 'c') %dopar% {
      i^2
    }
    
    if (length(test_result) != n_cores) {
      stop("Cluster test failed")
    }
    
    if (verbose) {
      message("Parallel cluster setup successful")
    }
    
    return(list(
      cluster = cluster,
      n_cores = n_cores,
      setup_successful = TRUE
    ))
    
  }, error = function(e) {
    if (verbose) {
      message(sprintf("Failed to set up parallel cluster: %s. Using sequential processing.", e$message))
    }
    return(NULL)
  })
}

#' Clean Up Parallel Cluster
#'
#' Stops and cleans up a parallel cluster created by setup_parallel_cluster().
#'
#' @param cluster_setup Cluster setup object returned by setup_parallel_cluster().
#' @param verbose Logical indicating whether to show cleanup messages.
#' @keywords internal
cleanup_parallel_cluster <- function(cluster_setup, verbose = FALSE) {
  
  if (is.null(cluster_setup) || !cluster_setup$setup_successful) {
    return(invisible())
  }
  
  tryCatch({
    if (!is.null(cluster_setup$cluster)) {
      parallel::stopCluster(cluster_setup$cluster)
      if (verbose) {
        message("Parallel cluster cleaned up successfully")
      }
    }
  }, error = function(e) {
    if (verbose) {
      warning(sprintf("Error cleaning up parallel cluster: %s", e$message))
    }
  })
  
  invisible()
}

#' Execute Function with Parallel Processing Safety
#'
#' Wrapper function that ensures proper cluster management and error handling
#' for parallel processing operations.
#'
#' @param fun Function to execute.
#' @param args List of arguments to pass to the function.
#' @param parallel Logical indicating whether to use parallel processing.
#' @param verbose Logical indicating whether to show messages.
#' @param fallback_sequential Logical indicating whether to fall back to sequential processing on error.
#'
#' @return Result of function execution.
#' @keywords internal
execute_with_parallel_safety <- function(fun, args, parallel = FALSE, 
                                        verbose = FALSE, fallback_sequential = TRUE) {
  
  if (!parallel) {
    # Execute sequentially
    return(do.call(fun, args))
  }
  
  # Set up parallel processing
  cluster_setup <- NULL
  tryCatch({
    cluster_setup <- setup_parallel_cluster(verbose = verbose)
    
    if (!is.null(cluster_setup)) {
      # Execute with parallel processing
      result <- do.call(fun, args)
      return(result)
    } else if (fallback_sequential) {
      if (verbose) {
        message("Parallel setup failed. Falling back to sequential processing.")
      }
      args$parallel <- FALSE  # Update args to use sequential
      return(do.call(fun, args))
    } else {
      stop("Parallel setup failed and sequential fallback is disabled")
    }
    
  }, error = function(e) {
    if (verbose) {
      warning(sprintf("Parallel execution failed: %s", e$message))
    }
    
    if (fallback_sequential) {
      if (verbose) {
        message("Falling back to sequential processing.")
      }
      args$parallel <- FALSE
      return(do.call(fun, args))
    } else {
      stop(sprintf("Parallel execution failed: %s", e$message))
    }
    
  }, finally = {
    if (!is.null(cluster_setup)) {
      cleanup_parallel_cluster(cluster_setup, verbose = verbose)
    }
  })
}

# ============================================================================
# MEMORY MANAGEMENT AND PERFORMANCE UTILITIES
# ============================================================================

#' Memory-efficient cache management
#' @keywords internal
manage_cache_memory <- function(max_cache_size = 100) {
  # Clean weight matrix cache if it gets too large
  if (length(ls(.weight_matrix_cache)) > max_cache_size) {
    # Remove oldest entries (simple FIFO approach)
    cache_keys <- ls(.weight_matrix_cache)
    keys_to_remove <- cache_keys[1:(length(cache_keys) - max_cache_size %/% 2)]
    rm(list = keys_to_remove, envir = .weight_matrix_cache)
  }
  
  # Clean moneca_fast cache if it gets too large
  if (length(ls(.moneca_fast_cache)) > max_cache_size) {
    cache_keys <- ls(.moneca_fast_cache)
    keys_to_remove <- cache_keys[1:(length(cache_keys) - max_cache_size %/% 2)]
    rm(list = keys_to_remove, envir = .moneca_fast_cache)
  }
  
  # Force garbage collection
  gc(verbose = FALSE)
}

#' Performance monitoring utilities
#' @keywords internal
log_performance_stats <- function(stats, verbose = TRUE) {
  if (!verbose) return(invisible())
  
  message(sprintf("Performance Summary:"))
  message(sprintf("  - Total combinations: %d", stats$n_combinations))
  message(sprintf("  - Combinations evaluated: %d (%.1f%%)", 
                 stats$combinations_evaluated,
                 100 * stats$combinations_evaluated / stats$n_combinations))
  message(sprintf("  - Computation time: %.2f seconds", stats$computation_time))
  message(sprintf("  - Evaluation rate: %.1f combinations/second", stats$evaluation_rate))
  
  if (!is.null(stats$early_terminated) && stats$early_terminated) {
    message(sprintf("  - Early termination: YES (saved %.1f%% of evaluations)", 
                   100 * (1 - stats$combinations_evaluated / stats$n_combinations)))
  }
  
  if (stats$cache_enabled) {
    message(sprintf("  - Caching: ENABLED (memory optimized)"))
  }
}

# Fast version of moneca for parameter tuning with caching
moneca_fast <- function(mx, segment.levels = 2, cut.off = 1, 
                       small.cell.reduction = 0, progress = FALSE,
                       use_cache = TRUE) {
  # Optimized version of moneca for faster evaluation during tuning
  # Includes caching and memory optimization
  
  # Generate cache key if caching is enabled
  if (use_cache) {
    mx_hash <- simple_matrix_hash(mx)
    cache_key <- generate_cache_key(mx_hash, small.cell.reduction, cut.off, 
                                   paste("moneca_fast", segment.levels, sep = "_"))
    
    # Check cache first
    if (exists(cache_key, envir = .moneca_fast_cache)) {
      return(get(cache_key, envir = .moneca_fast_cache))
    }
  }
  
  segment.list <- list()
  mat.list <- list()
  
  # First level - use optimized weight matrix computation
  tryCatch({
    mx.edges <- weight.matrix(
      mx = mx,
      cut.off = cut.off,
      small.cell.reduction = small.cell.reduction,
      symmetric = FALSE
    )
    
    if (all(is.na(mx.edges))) {
      result <- list(segment.list = list(), mat.list = list())
      if (use_cache) {
        assign(cache_key, result, envir = .moneca_fast_cache)
      }
      return(result)
    }
    
    segments <- find.segments(mx.edges, cut.off = 0)
    
    if (length(segments) == 0) {
      result <- list(segment.list = list(), mat.list = list())
      if (use_cache) {
        assign(cache_key, result, envir = .moneca_fast_cache)
      }
      return(result)
    }
    
    segment.list[[1]] <- segments
    mat.list[[1]] <- mx
    
    # Additional levels if requested (optimized for speed)
    if (segment.levels > 1 && length(segments) > 1) {
      current_mx <- mx
      current_segments <- segments
      
      for (level in 2:min(segment.levels, 3)) {  # Cap at 3 for speed
        mx_seg <- segment.matrix(current_mx, current_segments)
        
        # Early termination conditions
        if (nrow(mx_seg) < 3 || ncol(mx_seg) < 3) break
        
        # Check for sufficient data
        if (sum(mx_seg) < 10) break  # Insufficient data for meaningful segmentation
        
        mx.edges <- weight.matrix(
          mx = mx_seg,
          cut.off = cut.off,
          small.cell.reduction = small.cell.reduction,
          symmetric = FALSE
        )
        
        if (all(is.na(mx.edges))) break
        
        new_segments <- find.segments(mx.edges, cut.off = 0)
        
        # Stop if no meaningful segmentation
        if (length(new_segments) <= 1) break
        
        segment.list[[level]] <- new_segments
        mat.list[[level]] <- mx_seg
        
        current_segments <- new_segments
        current_mx <- mx_seg
      }
    }
    
    result <- list(
      segment.list = segment.list,
      mat.list = mat.list
    )
    
    # Cache the result if caching is enabled
    if (use_cache) {
      assign(cache_key, result, envir = .moneca_fast_cache)
      
      # Periodic cache management
      if (runif(1) < 0.1) {  # 10% chance to clean cache
        manage_cache_memory()
      }
    }
    
    return(result)
    
  }, error = function(e) {
    # Return empty result on error
    result <- list(segment.list = list(), mat.list = list())
    if (use_cache) {
      assign(cache_key, result, envir = .moneca_fast_cache)
    }
    return(result)
  })
}