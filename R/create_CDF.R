#' Create CDF Data for Posterior Predictive Distribution
#'
#' Creates a dataframe or list of dataframes containing cumulative distribution function (CDF) data
#' for posterior predictive distribution analysis. If by_value is specified, returns a single dataframe
#' for that by-value. If by_value is NULL, returns a list of dataframes for all by-values.
#'
#' @param data A data frame containing the input data. If provided, predicted distributions will be calculated internally.
#' @param fixed_effects Character vector of column names for fixed effects. Required if data is provided.
#' @param random_params Character vector of column names for random parameters. Required if data is provided.
#' @param by Character, name of the column to group by. If not provided and pred_dist is NULL, defaults to "Single Chain Dataset".
#' @param by_value Character or numeric, the specific by-value to process. Use "Single Chain" for single chain dataset.
#'                If NULL, CDF data will be created for all by-values.
#' @param tolerance_level Numeric, tolerance level percentage (between 0 and 100). Required if data is provided.
#' @param n_points Integer, number of points for CDF evaluation (between 3 and 300, inclusive)
#' @param ci_level Numeric, confidence interval level as percentage (default = 90)
#' @param multiplication_factor Numeric, factor to multiply random parameters variance by (default = 1)
#' @param log_normal Logical, whether to use log-normal transformation (default = FALSE)
#' @param covariate_cols Character vector of column names for covariates (default = NULL)
#' @param covariate_values Numeric vector of values to multiply with covariates (default = NULL)
#' @param pred_dist Optional. A dataframe containing predicted distributions. If provided, data parameters are ignored.
#'
#' @return If by_value is specified: A dataframe containing CDF data for that by-value.
#'         If by_value is NULL: A list of dataframes, each containing CDF data for a different by-value.
#'
#' @importFrom stats qnorm pnorm quantile approx
#' @export
create_CDF <- function(data = NULL, fixed_effects = NULL, random_params = NULL,
                       by = NULL, by_value = NULL, tolerance_level = NULL, n_points = 20,
                       ci_level = 90, multiplication_factor = 1,
                       log_normal = FALSE, covariate_cols = NULL, covariate_values = NULL,
                       pred_dist = NULL) {

  # Check if n_points is within the allowed bounds
  if (n_points < 3 || n_points > 300) {
    stop("Number of evaluation points 'n_points' out of allowed bounds 3-300")
  }

  # Set default by value if not provided and pred_dist is NULL
  if (is.null(by) && is.null(pred_dist)) {
    by <- "Single Chain Dataset"
    warning("Assuming Single Chain Dataset as by value not provided!")
  }

  # Check if we need to calculate predicted distributions
  if (is.null(pred_dist)) {
    # Validate required parameters
    if (is.null(data) || is.null(fixed_effects) || is.null(random_params) ||
        is.null(by) || is.null(tolerance_level)) {
      stop("When pred_dist is not provided, data, fixed_effects, random_params, by, and tolerance_level are required")
    }

    # Calculate predicted distributions WITH COVARIATE SUPPORT
    pred_dist <- calculate_predicted_distributions(
      data = data,
      fixed_effects = fixed_effects,
      random_params = random_params,
      by = by,
      tolerance_level = tolerance_level,
      multiplication_factor = multiplication_factor,
      covariate_cols = covariate_cols,
      covariate_values = covariate_values
    )
  }

  # If by_value is NULL, create CDF data for all by-values
  if (is.null(by_value)) {
    # Get unique by-values
    by_values <- unique(pred_dist$By)

    # Check if it's a single chain dataset
    is_single_chain <- length(by_values) == 1 && (by_values == "SingleChain" || by_values == "1")

    # Process each by-value
    result_list <- lapply(by_values, function(bv) {
      if (is_single_chain) {
        bv_to_use <- "Single Chain"
      } else {
        bv_to_use <- bv
      }

      tryCatch({
        # Call this function recursively for each by-value
        create_CDF(
          pred_dist = pred_dist,
          by_value = bv_to_use,
          n_points = n_points,
          ci_level = ci_level,
          log_normal = log_normal
        )
      }, error = function(e) {
        warning(paste("Error processing by-value", bv, ":", e$message))
        NULL
      })
    })

    # Remove any NULL results (from errors)
    result_list <- result_list[!sapply(result_list, is.null)]

    # Name the list elements
    names(result_list) <- sapply(result_list, function(df) df$by_value[1])

    return(result_list)
  }

  # Handle single chain case
  is_single_chain <- by_value == "Single Chain"
  if (is_single_chain) {
    if (!"SingleChain" %in% pred_dist$By && !"1" %in% pred_dist$By) {
      stop("SingleChain or 1 not found in pred_dist for single chain analysis")
    }
    data <- pred_dist[pred_dist$By == "SingleChain" | pred_dist$By == "1", ]
  } else {
    data <- pred_dist[pred_dist$By == by_value, ]
  }

  if (nrow(data) == 0) {
    stop(paste("No data found for by_value:", by_value))
  }

  # Calculate parameters - using MEAN not MEDIAN (for location parameter)
  overall_median <- mean(data$mean)  # Use mean of means for centering
  overall_sd <- sqrt(mean(data$variance))

  # Generate x values in log space
  xmin_log <- qnorm(0.0001, mean = overall_median, sd = overall_sd)
  xmax_log <- qnorm(0.9999, mean = overall_median, sd = overall_sd)
  x_values_log <- seq(xmin_log, xmax_log, length.out = n_points)

  # Transform x values if needed
  x_values <- if(log_normal) exp(x_values_log) else x_values_log

  # Calculate CDF values for each observation
  y_values <- do.call(rbind, lapply(1:nrow(data), function(i) {
    pnorm(x_values_log, mean = data$mean[i], sd = sqrt(data$variance[i]))
  }))

  # KEY CHANGE: Use MEDIAN instead of MEAN for central tendency
  y_mean <- apply(y_values, 2, median)

  # Calculate confidence intervals
  alpha <- 1 - ci_level/100
  if (nrow(y_values) > 1) {
    ci_bounds <- apply(y_values, 2, function(col) {
      quantile(col, probs = c(alpha/2, 1-alpha/2), na.rm = TRUE)
    })
    ci_lower <- ci_bounds[1,]
    ci_upper <- ci_bounds[2,]
  } else {
    ci_lower <- ci_upper <- y_mean
  }

  # Create result dataframe
  result <- data.frame(
    x = x_values,
    x_log = x_values_log,
    y_mean = y_mean,  # Note: variable name kept for compatibility, but contains median
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    by_value = if(is_single_chain) "Single Chain" else as.character(by_value)
  )

  # Store source data as attribute for potential later use
  attr(result, "source_data") <- pred_dist
  attr(result, "log_normal") <- log_normal
  attr(result, "ci_level") <- ci_level

  return(result)
}
