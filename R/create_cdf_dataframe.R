#' Create CDF Data for Posterior Predictive Distribution
#'
#' Creates a dataframe containing cumulative distribution function (CDF) data
#' for posterior predictive distribution analysis.
#'
#' @param pred_dist A dataframe containing predicted distributions (output from calculate_predicted_distributions)
#' @param by_value Character or numeric, the specific by-value to process. Use "Single Chain" for single chain dataset
#' @param n_points Integer, number of points for CDF evaluation (default = 20)
#' @param ci_level Numeric, confidence interval level as percentage (default = 90)
#' @param log_normal Logical, whether to use log-normal transformation (default = FALSE)
#'
#' @return A dataframe containing:
#' \itemize{
#'   \item{x}{Numeric vector of x values (transformed if log_normal = TRUE)}
#'   \item{x_log}{Numeric vector of x values in log space}
#'   \item{y_mean}{Numeric vector of mean CDF values}
#'   \item{ci_lower}{Numeric vector of lower confidence interval bounds}
#'   \item{ci_upper}{Numeric vector of upper confidence interval bounds}
#'   \item{by_value}{Character or numeric, the by-value being processed}
#'   \item{range_info}{List containing xmin, xmax in both original and log space}
#' }
#'
#' @examples
#' \dontrun{
#' # Single chain example
#' pred_dist <- calculate_predicted_distributions(data, "fixed1", "random1",
#'                                              "Single Chain Dataset", 95)
#' cdf_data <- create_cdf_dataframe(pred_dist, by_value = "Single Chain")
#'
#' # Multiple chain example
#' cdf_data_A <- create_cdf_dataframe(pred_dist, by_value = "A")
#' }
#'
#' @importFrom stats qnorm pnorm quantile
#' @export
create_cdf_dataframe <- function(pred_dist, by_value, n_points = 20,
                                 ci_level = 90, log_normal = FALSE) {
  # Input validation
  if (!is.data.frame(pred_dist)) {
    stop("pred_dist must be a data frame")
  }

  if (!all(c("By", "median", "variance") %in% names(pred_dist))) {
    stop("pred_dist must contain 'By', 'median', and 'variance' columns")
  }

  if (n_points < 3 || n_points > 300) {
    stop("n_points must be between 3 and 300")
  }

  if (ci_level <= 0 || ci_level >= 100) {
    stop("ci_level must be between 0 and 100")
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
    stop("No data found for specified by_value")
  }

  # Calculate parameters
  overall_median <- data$median[1]
  overall_sd <- sqrt(data$variance[1])

  # Generate x values in log space
  xmin_log <- qnorm(0.0001, mean = overall_median, sd = overall_sd)
  xmax_log <- qnorm(0.9999, mean = overall_median, sd = overall_sd)
  x_values_log <- seq(xmin_log, xmax_log, length.out = n_points)

  # Transform x values if needed
  x_values <- if(log_normal) exp(x_values_log) else x_values_log
  xmin <- if(log_normal) exp(xmin_log) else xmin_log
  xmax <- if(log_normal) exp(xmax_log) else xmax_log

  # Calculate CDF values for each observation
  y_values <- do.call(rbind, lapply(1:nrow(data), function(i) {
    pnorm(x_values_log, mean = data$median[i], sd = sqrt(data$variance[i]))
  }))

  # Calculate mean CDF
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
    y_mean = y_mean,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    by_value = if(is_single_chain) "Single Chain" else by_value,
    range_info = list(
      xmin = xmin,
      xmax = xmax,
      xmin_log = xmin_log,
      xmax_log = xmax_log
    )
  )

  return(result)
}
