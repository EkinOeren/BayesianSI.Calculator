#' Perform Bayesian Statistical Interval Calculations
#'
#' This function coordinates the calculation of various statistical intervals including
#' predicted distributions, credible intervals, tolerance intervals, and prediction intervals.
#'
#' @param data A data frame containing the input data
#' @param fixed_effects Character vector of column names for fixed effects
#' @param random_params Character vector of column names for random parameters
#' @param by Character, name of the column to group by (default = "Single Chain Dataset")
#' @param tolerance_interval_level Numeric, tolerance interval level percentage (between 0 and 100)
#' @param multiplication_factor Numeric, factor to multiply random parameters variance by (default = 1)
#' @param credible_interval_level Numeric, percentage for credible intervals (default = 95)
#' @param prediction_interval_level Numeric, percentage for prediction intervals (default = 90)
#' @param credibility_of_tolerance_interval Numeric, credibility level for tolerance intervals (default = 95)
#' @param log_normal Logical, whether to transform results from log space (default = FALSE)
#' @param covariate_cols Character vector of column names for covariates (default = NULL)
#' @param covariate_values Numeric vector of values to multiply with covariates (default = NULL)
#' @param use_krishnamoorthy Logical, whether to calculate two-sided tolerance intervals using
#'        the Krishnamoorthy method (default = FALSE)
#' @param krishnamoorthy_tolerance Numeric, convergence tolerance for Krishnamoorthy method (default = 0.01)
#'
#' @return A list containing:
#' \itemize{
#' \item data: A data frame with calculated intervals and reducible uncertainty metrics
#' \item summary: A string summarizing the calculation parameters
#' \item krishnamoorthy_results: If use_krishnamoorthy=TRUE, a list of Krishnamoorthy method results
#' }
#'
#' @details
#' When \code{use_krishnamoorthy=TRUE}, the function adds two additional columns to the output data:
#' \itemize{
#' \item K_TI_Lower: Lower tolerance interval bound calculated using Krishnamoorthy method
#' \item K_TI_Upper: Upper tolerance interval bound calculated using Krishnamoorthy method
#' }
#'
#' The Krishnamoorthy method provides an alternative approach to calculating two-sided tolerance
#' intervals that may be more appropriate in some situations, particularly when the distribution
#' of lower and upper quantiles is not symmetric. The method finds optimal points (g1, g2) that
#' satisfy the constraint g2 = 2*gP_mean - g1, where gP_mean is the average midpoint of the
#' lower and upper quantiles.
#'
#' @examples
#' \dontrun{
#' data <- data.frame(
#'   t1 = c(-0.82, -0.75, -0.79),
#'   v1 = c(0.91, 0.88, 0.90),
#'   cov1 = c(1.2, 1.5, 1.3),
#'   By = c(1, 1, 2)
#' )
#'
#' # Without covariates
#' result1 <- bayesian_statistical_intervals(
#'   data = data,
#'   fixed_effects = "t1",
#'   random_params = "v1",
#'   by = "By",
#'   tolerance_interval_level = 90
#' )
#'
#' # With covariates
#' result2 <- bayesian_statistical_intervals(
#'   data = data,
#'   fixed_effects = "t1",
#'   random_params = "v1",
#'   by = "By",
#'   tolerance_interval_level = 90,
#'   covariate_cols = "cov1",
#'   covariate_values = 2.0
#' )
#'
#' # With Krishnamoorthy method
#' result3 <- bayesian_statistical_intervals(
#'   data = data,
#'   fixed_effects = "t1",
#'   random_params = "v1",
#'   by = "By",
#'   tolerance_interval_level = 90,
#'   credibility_of_tolerance_interval = 80,
#'   use_krishnamoorthy = TRUE
#' )
#' }
#'
#' @export
bayesian_statistical_intervals <- function(data,
                                           fixed_effects,
                                           random_params,
                                           by = "Single Chain Dataset",
                                           tolerance_interval_level,
                                           multiplication_factor = 1,
                                           credible_interval_level = 95,
                                           prediction_interval_level = 90,
                                           credibility_of_tolerance_interval = 95,
                                           log_normal = FALSE,
                                           covariate_cols = NULL,
                                           covariate_values = NULL,
                                           use_krishnamoorthy = FALSE,
                                           krishnamoorthy_tolerance = 0.01) {

  # Input validation
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame")
  }

  # Check if 'by' is not provided and print warning
  if (missing(by)) {
    warning("As by value is not specified, default value \"Single Chain Dataset\" is used")
  }

  # Calculate predicted distributions
  pred_dist <- calculate_predicted_distributions(
    data = data,
    fixed_effects = fixed_effects,
    random_params = random_params,
    by = by,
    tolerance_level = tolerance_interval_level,
    multiplication_factor = multiplication_factor,
    covariate_cols = covariate_cols,
    covariate_values = covariate_values
  )

  # Calculate CI
  ci_result <- calculate_ci(
    data = pred_dist,
    ColToAggregate = "median",
    PercentForCI = credible_interval_level,
    ByColumn = "By"
  )

  # Calculate TI
  ti_result <- calculate_tolerance_interval(
    data = pred_dist,
    LowerColToAggregate = "lower_quantile",
    UpperColToAggregate = "upper_quantile",
    ConfidenceLevel = credibility_of_tolerance_interval,
    ByColumn = "By"
  )

  # Calculate PI
  pi_result <- calculate_PI(
    data = pred_dist,
    percent_for_pi = prediction_interval_level,
    Eta = 0.001
  )

  # Merge results
  combined_result <- merge(merge(ci_result, ti_result, by = "By"), pi_result, by = "By")

  # Ensure only one row per unique "By" value if there are duplicates
  if (any(table(combined_result$By) > 1)) {
    combined_result <- combined_result %>%
      dplyr::group_by(By) %>%
      dplyr::summarise(dplyr::across(dplyr::everything(), dplyr::first)) %>%
      dplyr::ungroup()
  }

  # Add reducible uncertainty calculations
  combined_result$ReducibleUpper <- ((combined_result$TI_Upper - combined_result$PI_Upper) /
                                       (combined_result$TI_Upper - combined_result$Median))
  combined_result$ReducibleLower <- ((combined_result$TI_Lower - combined_result$PI_Lower) /
                                       (combined_result$TI_Lower - combined_result$Median))

  # Round to 5 decimal places
  combined_result$ReducibleUpper <- round(combined_result$ReducibleUpper, 5)
  combined_result$ReducibleLower <- round(combined_result$ReducibleLower, 5)

  # Initialize Krishnamoorthy results
  krishnamoorthy_results <- NULL

  # Apply Krishnamoorthy method if requested
  if (use_krishnamoorthy) {
    # Calculate gP_mean
    gP_mean_vector <- calculate_gP_mean(pred_dist, tolerance_interval_level)

    # Find optimal points
    krishnamoorthy_results <- find_optimal_point(
      scatter_data = pred_dist,
      gP_mean_vector = gP_mean_vector,
      gamma_actual = credibility_of_tolerance_interval,
      tolerance = krishnamoorthy_tolerance
    )

    # Add Krishnamoorthy TI values as new columns
    combined_result <- add_krishnamoorthy_ti(combined_result, krishnamoorthy_results)

    # Store original Krishnamoorthy results for log-normal transformation if needed
    original_krishnamoorthy <- krishnamoorthy_results
  }

  # Apply log-normal transformation if requested
  if (log_normal) {
    log_cols <- c("Median", "CI_Lower", "CI_Upper",
                  "TI_Lower", "TI_Upper",
                  "PI_Lower", "PI_Upper")
    combined_result[log_cols] <- lapply(combined_result[log_cols], exp)

    # Also transform Krishnamoorthy TI values if they exist
    if (use_krishnamoorthy && "K_TI_Lower" %in% names(combined_result)) {
      combined_result$K_TI_Lower <- exp(combined_result$K_TI_Lower)
      combined_result$K_TI_Upper <- exp(combined_result$K_TI_Upper)

      # Transform the stored Krishnamoorthy results
      krishnamoorthy_results <- lapply(original_krishnamoorthy, function(result) {
        if (!is.null(result$g1) && !is.null(result$g2)) {
          result$g1 <- exp(result$g1)
          result$g2 <- exp(result$g2)
        }
        return(result)
      })
    }

    # Recalculate reducible uncertainty after transformation
    combined_result$ReducibleUpper <- ((combined_result$TI_Upper - combined_result$PI_Upper) /
                                         (combined_result$TI_Upper - combined_result$Median))
    combined_result$ReducibleLower <- ((combined_result$TI_Lower - combined_result$PI_Lower) /
                                         (combined_result$TI_Lower - combined_result$Median))
    combined_result$ReducibleUpper <- round(combined_result$ReducibleUpper, 5)
    combined_result$ReducibleLower <- round(combined_result$ReducibleLower, 5)
  }

  # Create formula string
  fixed_effects_str <- paste(fixed_effects, collapse = " + ")
  random_effects_str <- paste(random_params, collapse = " + ")
  mult_factor_str <- ifelse(multiplication_factor != 1,
                            paste0(multiplication_factor, " * "),
                            "")

  # Build covariate string
  covariate_str <- ""
  if (!is.null(covariate_cols)) {
    covariate_terms <- sapply(seq_along(covariate_cols), function(i) {
      if (covariate_values[i] == 1) {
        covariate_cols[i]
      } else {
        paste0(covariate_values[i], " * ", covariate_cols[i])
      }
    })
    covariate_str <- paste(covariate_terms, collapse = " + ")
  }

  # Combine mean components
  mean_components <- c()
  if (covariate_str != "") mean_components <- c(mean_components, covariate_str)
  if (fixed_effects_str != "") mean_components <- c(mean_components, fixed_effects_str)
  mean_part <- paste(mean_components, collapse = " + ")

  formula_str <- paste0(
    "y ~ N(",
    mean_part,
    " , ",
    mult_factor_str,
    "(",
    random_effects_str,
    "))"
  )

  # Create summary string
  summary_str <- paste0(
    "For parameters ", formula_str, " and a ",
    credible_interval_level, "% credible interval, ",
    prediction_interval_level, "% prediction interval, ",
    tolerance_interval_level, "% tolerance interval with ",
    credibility_of_tolerance_interval, "% credibility of tolerance",
    ifelse(log_normal, " (log-normal transformation applied)", ""),
    ifelse(use_krishnamoorthy, " (Krishnamoorthy method applied for two-sided TI)", ""),
    ", the following results have been reached."
  )

  # Create result list
  result <- list(
    data = combined_result,
    summary = summary_str,
    pred_dist = pred_dist
  )

  # Add Krishnamoorthy results if available
  if (use_krishnamoorthy) {
    result$krishnamoorthy_results <- krishnamoorthy_results
  }

  return(result)
}
