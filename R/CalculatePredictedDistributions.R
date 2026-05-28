#' Calculate Predicted Distributions
#'
#' This function calculates predicted distributions based on fixed effects, random parameters,
#' and covariates. It computes means, medians, quantiles, and variances for each group in the data.
#'
#' @param data A data frame or data.table containing the data
#' @param fixed_effects Character vector of column names for fixed effects
#' @param random_params Character vector of column names for random parameters
#' @param by Character, name of the column to group by. If "Single Chain Dataset", a SingleChain column will be created
#' @param tolerance_level Numeric, tolerance level percentage (between 0 and 100)
#' @param multiplication_factor Numeric, factor to multiply random parameters variance by (default = 1)
#' @param covariate_cols Character vector of column names for covariates (default = NULL)
#' @param covariate_values Numeric vector of values to multiply with covariates (default = NULL)
#' @param verbose Logical, if TRUE, print detailed information during execution (default = FALSE)
#'
#' @return A data frame with the following columns:
#' \item{By}{The grouping variable}
#' \item{mean}{Mean of fixed effects plus covariates}
#' \item{median}{Median value}
#' \item{lower_quantile}{Lower quantile based on tolerance level}
#' \item{upper_quantile}{Upper quantile based on tolerance level}
#' \item{variance}{Variance of random parameters}
#'
#' @examples
#' \dontrun{
#' data <- data.frame(
#'   Group = rep(c("A", "B"), each = 5),
#'   Fixed1 = c(1:5, 6:10),
#'   Random1 = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0),
#'   Covariate1 = c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11)
#' )
#'
#' # Without covariates
#' result1 <- calculate_predicted_distributions(
#'   data,
#'   fixed_effects = "Fixed1",
#'   random_params = "Random1",
#'   by = "Group",
#'   tolerance_level = 95
#' )
#'
#' # With covariates
#' result2 <- calculate_predicted_distributions(
#'   data,
#'   fixed_effects = "Fixed1",
#'   random_params = "Random1",
#'   by = "Group",
#'   tolerance_level = 95,
#'   covariate_cols = "Covariate1",
#'   covariate_values = 2.5
#' )
#' }
#'
#' @importFrom data.table as.data.table copy setorder
#' @importFrom stats qnorm
#' @export
calculate_predicted_distributions <- function(data, fixed_effects, random_params, by,
                                              tolerance_level, multiplication_factor = 1,
                                              covariate_cols = NULL, covariate_values = NULL,
                                              verbose = FALSE) {

  # Input validation
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame or data.table")
  }

  if (!is.character(fixed_effects) || length(fixed_effects) == 0) {
    stop("'fixed_effects' must be a character vector of column names")
  }

  if (!is.character(random_params) || length(random_params) == 0) {
    stop("'random_params' must be a character vector of column names")
  }

  if (!is.character(by) || length(by) != 1) {
    stop("'by' must be a single character string")
  }

  if (!is.numeric(tolerance_level) || tolerance_level <= 0 || tolerance_level >= 100) {
    stop("'tolerance_level' must be a number between 0 and 100")
  }

  if (!is.numeric(multiplication_factor)) {
    stop("'multiplication_factor' must be a numeric value")
  }

  # Validate covariate inputs
  if (!is.null(covariate_cols)) {
    if (!is.character(covariate_cols)) {
      stop("'covariate_cols' must be a character vector of column names")
    }

    # Check if covariate columns exist
    missing_covariate_cols <- setdiff(covariate_cols, names(data))
    if (length(missing_covariate_cols) > 0) {
      stop(paste("Covariate columns not found in data:", paste(missing_covariate_cols, collapse = ", ")))
    }

    # If covariate_values is not provided, default to 1 for each covariate
    if (is.null(covariate_values)) {
      covariate_values <- rep(1, length(covariate_cols))
      if (verbose) cat("No covariate values provided, defaulting to 1 for all covariates\n")
    }

    # Check if lengths match
    if (length(covariate_cols) != length(covariate_values)) {
      stop("Length of 'covariate_cols' must match length of 'covariate_values'")
    }

    # Validate that covariate values are numeric
    if (!is.numeric(covariate_values)) {
      stop("'covariate_values' must be numeric")
    }
  }

  # Print input parameters
  if (verbose) {
    cat("Fixed effects columns:", paste(fixed_effects, collapse=", "), "\n")
    cat("Random params columns:", paste(random_params, collapse=", "), "\n")
    cat("By column:", by, "\n")
    cat("Tolerance level:", tolerance_level, "\n")
    cat("Multiplication factor:", multiplication_factor, "\n")
    if (!is.null(covariate_cols)) {
      cat("Covariate columns:", paste(covariate_cols, collapse=", "), "\n")
      cat("Covariate values:", paste(covariate_values, collapse=", "), "\n")
    }
  }

  # Verbose printing function
  vprint <- function(...) {
    if (verbose) cat(...)
  }

  # Check if columns exist in the data
  missing_fixed <- setdiff(fixed_effects, names(data))
  if (length(missing_fixed) > 0) {
    stop(paste("Fixed effect columns not found in data:", paste(missing_fixed, collapse = ", ")))
  }

  missing_random <- setdiff(random_params, names(data))
  if (length(missing_random) > 0) {
    stop(paste("Random parameter columns not found in data:", paste(missing_random, collapse = ", ")))
  }

  # Handle "Single Chain Dataset" case
  if (by == "Single Chain Dataset") {
    by <- "SingleChain"
    if (!by %in% names(data)) {
      data$SingleChain <- 1
    }
  } else if (!by %in% names(data)) {
    stop(paste("By column", by, "not found in data"))
  }

  # Create a copy of the data to avoid modifying the original
  dt <- data.table::copy(data.table::as.data.table(data))

  vprint("\n=== Data Summary ===\n")
  vprint("Number of rows:", nrow(dt), "\n")
  vprint("First few rows of input data:\n")
  if (verbose) print(head(dt))

  # Initialize mean vector with zeros
  mean_vector <- rep(0, nrow(dt))

  # STEP 1: Add covariate effects FIRST (NEW FUNCTIONALITY)
  if (!is.null(covariate_cols)) {
    vprint("\n=== Processing Covariates ===\n")

    for (i in seq_along(covariate_cols)) {
      covariate_col <- covariate_cols[i]
      covariate_val <- covariate_values[i]

      # Get the column values
      col_vector <- as.numeric(dt[[covariate_col]])

      # Check for NA values
      if (any(is.na(col_vector))) {
        warning(paste("Covariate column", covariate_col, "contains NA values"))
      }

      # Add covariate effect to mean
      covariate_effect <- col_vector * covariate_val
      mean_vector <- mean_vector + covariate_effect

      vprint(paste("Added Covariate", i, "effect:", covariate_col, "*", covariate_val, "\n"))
      vprint(paste("Sample covariate effect values:", paste(head(covariate_effect, 3), collapse=", "), "\n"))
      vprint(paste("Sample cumulative mean after covariate", i, ":", paste(head(mean_vector, 3), collapse=", "), "\n"))
    }
  }

  # STEP 2: Add fixed effects to the mean (existing functionality)
  if (length(fixed_effects) > 0) {
    vprint("\n=== Processing Fixed Effects ===\n")

    fixed_matrix <- tryCatch({
      matrix(
        as.numeric(unlist(dt[, fixed_effects, with = FALSE])),
        nrow = nrow(dt),
        ncol = length(fixed_effects)
      )
    }, error = function(e) {
      stop(paste("Error creating fixed effects matrix:", e$message))
    })

    # Add fixed effects to mean
    fixed_effects_sum <- rowSums(fixed_matrix, na.rm = TRUE)
    mean_vector <- mean_vector + fixed_effects_sum

    vprint("Fixed effects matrix (first few rows):\n")
    if (verbose) print(head(fixed_matrix))
    vprint("Sample fixed effects sum:", paste(head(fixed_effects_sum, 3), collapse=", "), "\n")
    vprint("Sample total mean after fixed effects:", paste(head(mean_vector, 3), collapse=", "), "\n")
  }

  # STEP 3: Calculate variance from random parameters (existing functionality)
  if (length(random_params) > 0) {
    vprint("\n=== Processing Random Parameters ===\n")

    random_matrix <- tryCatch({
      matrix(
        as.numeric(unlist(dt[, random_params, with = FALSE])),
        nrow = nrow(dt),
        ncol = length(random_params)
      )
    }, error = function(e) {
      stop(paste("Error creating random parameters matrix:", e$message))
    })

    # Calculate total variance
    variance_vector <- rowSums(random_matrix, na.rm = TRUE) * multiplication_factor

    vprint("Random parameters matrix (first few rows):\n")
    if (verbose) print(head(random_matrix))
    vprint("Sample variance values:", paste(head(variance_vector, 3), collapse=", "), "\n")
  } else {
    variance_vector <- rep(0, nrow(dt))
  }

  # Print final vectors for debugging
  vprint("\n=== Final Calculations ===\n")
  vprint("Number of rows:", length(mean_vector), "\n")
  vprint("Final mean vector sample:", paste(head(mean_vector, 3), collapse=", "), "\n")
  vprint("Final variance vector sample:", paste(head(variance_vector, 3), collapse=", "), "\n")

  # Calculate tolerance parameters
  tolerance_alpha <- tolerance_level / 100
  tolerance_slice <- (1 - tolerance_alpha) / 2

  vprint("\n=== Tolerance Parameters ===\n")
  vprint("Tolerance alpha:", tolerance_alpha, "\n")
  vprint("Tolerance slice:", tolerance_slice, "\n")

  # Calculate quantiles using the mean and variance
  lower_quantiles <- stats::qnorm(tolerance_slice,
                                  mean = mean_vector,
                                  sd = sqrt(pmax(variance_vector, 1e-10)))

  upper_quantiles <- stats::qnorm(1 - tolerance_slice,
                                  mean = mean_vector,
                                  sd = sqrt(pmax(variance_vector, 1e-10)))

  medians <- stats::qnorm(0.5,
                          mean = mean_vector,
                          sd = sqrt(pmax(variance_vector, 1e-10)))

  vprint("\n=== Quantiles ===\n")
  vprint("First few lower quantiles:", head(lower_quantiles), "\n")
  vprint("First few upper quantiles:", head(upper_quantiles), "\n")
  vprint("First few medians:", head(medians), "\n")

  # Create result data.table
  PredictedDistributions_dt <- data.table::data.table(
    By = dt[[by]],
    mean = mean_vector,
    median = medians,
    lower_quantile = lower_quantiles,
    upper_quantile = upper_quantiles,
    variance = variance_vector
  )

  # Handle infinite values and round
  cols_to_process <- c("mean", "median", "lower_quantile", "upper_quantile", "variance")
  for(col in cols_to_process) {
    PredictedDistributions_dt[[col]] <- sapply(PredictedDistributions_dt[[col]], function(x) {
      if(is.infinite(x)) return(ifelse(x > 0, "+Inf", "-Inf"))
      return(round(x, 3))
    })
  }

  # Sort by the "By" column
  data.table::setorder(PredictedDistributions_dt, By)

  vprint("\n=== Final Result ===\n")
  vprint("First few rows of result:\n")
  if (verbose) print(head(PredictedDistributions_dt))

  return(as.data.frame(PredictedDistributions_dt))
}
