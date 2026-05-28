#' Perform Statistical Interval Calculations
#'
#' This function coordinates the calculation of various statistical intervals including
#' predicted distributions, confidence intervals, tolerance intervals, and prediction intervals.
#'
#' @param data A data frame containing the input data
#' @param fixed_effects Character vector of column names for fixed effects
#' @param random_params Character vector of column names for random parameters
#' @param by Character, name of the column to group by
#' @param tolerance_level Numeric, tolerance level percentage (between 0 and 100)
#' @param multiplication_factor Numeric, factor to multiply random parameters variance by (default = 1)
#' @param percent_for_ci Numeric, percentage for confidence intervals (default = 95)
#' @param percent_for_pi Numeric, percentage for prediction intervals (default = 95)
#' @param confidence_of_tolerance Numeric, confidence level for tolerance intervals (default = 95)
#' @param log_normal Logical, whether to transform results from log space (default = FALSE)
#'
#' @return A list containing:
#' \itemize{
#'   \item data: A data frame with calculated intervals and reducible uncertainty metrics
#'   \item summary: A string summarizing the calculation parameters
#' }
#'
#' @examples
#' \dontrun{
#' data <- data.frame(
#'   t1 = c(-0.82, -0.75, -0.79),
#'   v1 = c(0.91, 0.88, 0.90),
#'   By = c(1, 1, 2)
#' )
#' result <- perform_calculations(
#'   data = data,
#'   fixed_effects = "t1",
#'   random_params = "v1",
#'   by = "By",
#'   tolerance_level = 90
#' )
#' }
#'
#' @export
perform_calculations <- function(data,
                                 fixed_effects,
                                 random_params,
                                 by,
                                 tolerance_level,
                                 multiplication_factor = 1,
                                 percent_for_ci = 95,
                                 percent_for_pi = 95,
                                 confidence_of_tolerance = 95,
                                 log_normal = FALSE) {

  # Input validation
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame")
  }

  # Calculate predicted distributions
  pred_dist <- calculate_predicted_distributions(
    data = data,
    fixed_effects = fixed_effects,
    random_params = random_params,
    by = by,
    tolerance_level = tolerance_level,
    multiplication_factor = multiplication_factor
  )

  # Calculate CI
  ci_result <- calculate_ci(
    data = pred_dist,
    ColToAggregate = "median",
    PercentForCI = percent_for_ci,
    ByColumn = "By"
  )

  # Calculate TI
  ti_result <- calculate_tolerance_interval(
    data = pred_dist,
    LowerColToAggregate = "lower_quantile",
    UpperColToAggregate = "upper_quantile",
    ConfidenceLevel = confidence_of_tolerance,
    ByColumn = "By"
  )

  # Calculate PI
  pi_result <- calculate_PI(
    data = pred_dist,
    percent_for_pi = percent_for_pi,
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

  # Convert to percentages
  combined_result$ReducibleUpper <- round(combined_result$ReducibleUpper * 100, 3)
  combined_result$ReducibleLower <- round(combined_result$ReducibleLower * 100, 3)

  # Apply log-normal transformation if requested
  if (log_normal) {
    log_cols <- c("Median", "CI_Lower", "CI_Upper",
                  "TI_Lower", "TI_Upper",
                  "PI_Lower", "PI_Upper")
    combined_result[log_cols] <- lapply(combined_result[log_cols], exp)

    # Recalculate reducible uncertainty after transformation
    combined_result$ReducibleUpper <- ((combined_result$TI_Upper - combined_result$PI_Upper) /
                                         (combined_result$TI_Upper - combined_result$Median))
    combined_result$ReducibleLower <- ((combined_result$TI_Lower - combined_result$PI_Lower) /
                                         (combined_result$TI_Lower - combined_result$Median))
    combined_result$ReducibleUpper <- round(combined_result$ReducibleUpper * 100, 3)
    combined_result$ReducibleLower <- round(combined_result$ReducibleLower * 100, 3)
  }

  # Create formula string
  fixed_effects_str <- paste(fixed_effects, collapse = " + ")
  random_effects_str <- paste(random_params, collapse = " + ")
  mult_factor_str <- ifelse(multiplication_factor != 1,
                            paste0(multiplication_factor, " * "),
                            "")

  formula_str <- paste0(
    "y ~ N(",
    fixed_effects_str,
    " , ",
    mult_factor_str,
    "(",
    random_effects_str,
    "))"
  )

  # Create summary string
  summary_str <- paste0(
    "For parameters ", formula_str, " and a ",
    percent_for_ci, "% credibility interval, ",
    percent_for_pi, "% prediction interval, ",
    tolerance_level, "% tolerance interval with ",
    confidence_of_tolerance, "% confidence of tolerance",
    ifelse(log_normal, " (log-normal transformation applied)", ""),
    ", the following results have been reached."
  )

  # Return both the data and the summary string
  return(list(
    data = combined_result,
    summary = summary_str
  ))
}
