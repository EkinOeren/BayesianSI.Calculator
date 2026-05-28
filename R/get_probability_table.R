#' Create Posterior Predictive Probability Table
#'
#' Creates a table showing probability values and confidence intervals for a given x value or range
#' using exactly the same calculations as the Shiny app.
#'
#' @param data A data frame containing the input data. If provided, predicted distributions will be calculated internally.
#' @param fixed_effects Character vector of column names for fixed effects. Required if data is provided.
#' @param random_params Character vector of column names for random parameters. Required if data is provided.
#' @param by Character, name of the column to group by. Required if data is provided. Default is "Single Chain Dataset".
#' @param by_value Character or numeric, the specific by-value to process. Default is "1".
#' @param x_value Character or numeric, single value or range in format "[min,max]"
#' @param tolerance_level Numeric, tolerance level percentage (between 0 and 100). Required if data is provided.
#' @param ci_percent Numeric, confidence interval percentage (default = 90)
#' @param multiplication_factor Numeric, factor to multiply random parameters variance by (default = 1)
#' @param log_normal Logical, whether to use log-normal transformation (default = FALSE)
#' @param n_points Integer, number of points for CDF evaluation (default = 100)
#' @param pred_dist Optional. A dataframe containing predicted distributions. If provided, data parameters are ignored.
#'
#' @return A data frame formatted as a table for display
#'
#' @export
get_probability_table <- function(data = NULL, fixed_effects = NULL, random_params = NULL,
                                  by = "Single Chain Dataset", by_value = "1", x_value, tolerance_level = NULL,
                                  ci_percent = 90, multiplication_factor = 1,
                                  log_normal = FALSE, n_points = 100, pred_dist = NULL) {

  # Check if we need to calculate predicted distributions
  if (is.null(pred_dist)) {
    # Validate required parameters
    if (is.null(data) || is.null(fixed_effects) || is.null(random_params) ||
        is.null(by) || is.null(tolerance_level)) {
      stop("When pred_dist is not provided, data, fixed_effects, random_params, by, and tolerance_level are required")
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
  }

  # Filter data for the specific by_value
  data <- pred_dist[pred_dist$By == by_value, ]

  if (nrow(data) == 0) {
    stop(paste("No data found for by_value:", by_value))
  }

  # Calculate parameters
  overall_median <- data$median[1]
  overall_sd <- sqrt(data$variance[1])

  # Generate x values in log space
  xmin_log <- qnorm(0.0001, mean = overall_median, sd = overall_sd)
  xmax_log <- qnorm(0.9999, mean = overall_median, sd = overall_sd)
  x_values_log <- seq(xmin_log, xmax_log, length.out = n_points)

  # Transform x values for display if log_normal is TRUE
  x_values <- if(log_normal) exp(x_values_log) else x_values_log

  # Calculate CDF values for each observation
  y_values <- do.call(rbind, lapply(1:nrow(data), function(i) {
    pnorm(x_values_log, mean = data$median[i], sd = sqrt(data$variance[i]))
  }))

  # Calculate mean CDF
  y_mean <- colMeans(y_values)

  # Calculate confidence intervals
  alpha <- 1 - ci_percent/100

  if (nrow(y_values) > 1) {
    # Calculate empirical confidence intervals
    empirical_quantiles <- apply(y_values, 2, function(col) {
      quantile(col, probs = c(alpha/2, 1-alpha/2), na.rm = TRUE)
    })

    ci_lower <- empirical_quantiles[1,]
    ci_upper <- empirical_quantiles[2,]
  } else {
    # Only one observation, so no empirical confidence intervals possible
    ci_lower <- y_mean
    ci_upper <- y_mean
  }

  # Parse the x_value input
  parsed_input <- parse_x_value_input(x_value)

  if (is.null(parsed_input)) {
    # Create empty data frame with appropriate headers for single value case
    table_data <- data.frame(
      X_Value = NA,
      Probability = NA
    )
    table_data[[paste0("CI_", ci_percent)]] <- NA

    # Set proper column names with corrected capitalization and format
    colnames(table_data) <- c(
      "x value",
      "Probability of X ≤ x",
      paste0(ci_percent, "% CI")
    )

    return(table_data)
  }

  if (parsed_input$type == "single") {
    # Handle single value case
    x_val <- parsed_input$value

    # Convert input x value to log space if log_normal is TRUE
    x_val_log <- if(log_normal) log(x_val) else x_val

    # Check if the log value is in range
    if (x_val_log >= xmin_log && x_val_log <= xmax_log) {
      # Always use interpolation for precise values - exactly as in the app
      y_val <- approx(x_values_log, y_mean, xout = x_val_log)$y
      ci_lower_val <- approx(x_values_log, ci_lower, xout = x_val_log)$y
      ci_upper_val <- approx(x_values_log, ci_upper, xout = x_val_log)$y

      # Create data frame with temporary column names
      table_data <- data.frame(
        X_Value = round(x_val, 4),
        Probability = round(y_val, 4),
        CI = sprintf("[%.4f, %.4f]", ci_lower_val, ci_upper_val)
      )

      # Set proper column names with corrected capitalization and format
      colnames(table_data) <- c(
        "x value",
        "Probability of X ≤ x",
        paste0(ci_percent, "% CI")
      )
    } else {
      # Create data frame with temporary column names
      table_data <- data.frame(
        X_Value = x_val,
        Probability = "Out of range",
        CI = "Out of range"
      )

      # Set proper column names with corrected capitalization and format
      colnames(table_data) <- c(
        "x value",
        "Probability of X ≤ x",
        paste0(ci_percent, "% CI")
      )
    }
  } else if (parsed_input$type == "range") {
    # Handle range case
    xmin <- parsed_input$xmin
    xmax <- parsed_input$xmax

    # Convert input x values to log space if log_normal is TRUE
    xmin_log <- if(log_normal) log(xmin) else xmin
    xmax_log <- if(log_normal) log(xmax) else xmax

    # Check if both values are in range
    if (xmin_log >= xmin_log && xmin_log <= xmax_log &&
        xmax_log >= xmin_log && xmax_log <= xmax_log) {

      # Calculate probabilities using interpolation of mean CDF - exactly as in the app
      y_min <- approx(x_values_log, y_mean, xout = xmin_log)$y
      y_max <- approx(x_values_log, y_mean, xout = xmax_log)$y

      # Calculate range probability: P(xmin ≤ X ≤ xmax) = P(X ≤ xmax) - P(X ≤ xmin)
      range_prob <- y_max - y_min

      # Calculate the CDF value at xmin and xmax for each observation
      # This is for the confidence interval calculation
      cdf_diffs <- sapply(1:nrow(data), function(i) {
        # Get parameters for this observation
        mu <- data$median[i]
        sigma <- sqrt(data$variance[i])

        # Calculate CDF values
        p_xmin <- pnorm(xmin_log, mean = mu, sd = sigma)
        p_xmax <- pnorm(xmax_log, mean = mu, sd = sigma)

        # Return the difference
        return(p_xmax - p_xmin)
      })

      # Calculate empirical confidence interval for the differences
      alpha <- 1 - ci_percent/100
      ci_diff_lower <- quantile(cdf_diffs, alpha/2)
      ci_diff_upper <- quantile(cdf_diffs, 1 - alpha/2)

      # Create data frame with range information
      table_data <- data.frame(
        X_Values = sprintf("[%.4f, %.4f]", xmin, xmax),
        Range_Probability = round(range_prob, 4),
        Diff_CI = sprintf("[%.4f, %.4f]", ci_diff_lower, ci_diff_upper)
      )

      # Set proper column names for range case
      colnames(table_data) <- c(
        "x Interval",
        "P(xmin ≤ X ≤ xmax)",
        paste0(ci_percent, "% CI for P(xmin ≤ X ≤ xmax)")
      )
    } else {
      # Create data frame with range information but out of range
      table_data <- data.frame(
        X_Values = sprintf("[%.4f, %.4f]", xmin, xmax),
        Range_Probability = "Out of range",
        Diff_CI = "Out of range"
      )

      # Set proper column names for range case
      colnames(table_data) <- c(
        "x Interval",
        "P(xmin ≤ X ≤ xmax)",
        paste0(ci_percent, "% CI for P(xmin ≤ X ≤ xmax)")
      )
    }
  }

  return(table_data)
}
