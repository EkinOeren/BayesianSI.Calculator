#' Calculate Prediction Intervals
#'
#' This function calculates prediction intervals using a vectorized bisection method
#' for finding quantiles of normal distributions. It works with both single and
#' multiple groups of data.
#'
#' @param data A data frame or data.table containing at least the following columns:
#'   \itemize{
#'     \item By: A grouping variable
#'     \item mean: The mean of each group
#'     \item variance: The variance of each group
#'   }
#' @param percent_for_pi The desired prediction interval percentage (between 0 and 100)
#' @param Eta Convergence criterion for the bisection method (default = 0.001)
#'
#' @return A data frame with the following columns:
#'   \itemize{
#'     \item By: The grouping variable
#'     \item PI_Lower: Lower bound of the prediction interval
#'     \item PI_Upper: Upper bound of the prediction interval
#'   }
#'
#' @examples
#' \dontrun{
#' data <- data.frame(
#'   By = c("A", "A", "B", "B"),
#'   mean = c(10, 11, 20, 21),
#'   variance = c(2, 2.1, 3, 3.1)
#' )
#' result <- calculate_PI(data, percent_for_pi = 95)
#' }
#'
#' @importFrom data.table as.data.table :=
#' @export
calculate_PI <- function(data, percent_for_pi, Eta = 0.001) {
  # Check inputs
  if (!is.data.frame(data) || !is.numeric(percent_for_pi) ||
      percent_for_pi <= 0 || percent_for_pi >= 100) {
    stop("Invalid input parameters")
  }

  # Convert to data.table for faster processing
  dt <- data.table::as.data.table(data)

  # Calculate PI_request1 and PI_request2
  alpha <- 1 - percent_for_pi / 100
  slice <- alpha / 2

  PI_request1 <- slice
  PI_request2 <- 1 - slice

  # Vectorized bisection method
  find_PI_vectorized <- function(PI_requests, means, sds) {
    # Initialize result vector
    results <- numeric(length(means))

    # Set initial bounds to 99.99% and 0.001% of the distribution
    ax_vector <- stats::qnorm(0.0001, mean = means, sd = sds)
    bx_vector <- stats::qnorm(0.9999, mean = means, sd = sds)

    # Check if PI_request is within the initial bounds
    if (any(PI_requests < 0.0001) || any(PI_requests > 0.9999)) {
      stop("PI request is outside the 99.99% - 0.001% range of the distribution")
    }

    # Pre-allocate vectors for efficiency
    cx_vector <- numeric(length(means))
    cy_vector <- numeric(length(means))
    ay_vector <- numeric(length(means))
    by_vector <- numeric(length(means))

    # Perform bisection for each element
    for (iteration in 1:500) {
      # Calculate function values at bounds
      ay_vector <- stats::pnorm(ax_vector, mean = means, sd = sds) - PI_requests
      by_vector <- stats::pnorm(bx_vector, mean = means, sd = sds) - PI_requests

      # Check convergence
      if (all(abs(by_vector - ay_vector) < Eta)) {
        break
      }

      # Calculate midpoints
      cx_vector <- (ax_vector + bx_vector) / 2
      cy_vector <- stats::pnorm(cx_vector, mean = means, sd = sds) - PI_requests

      # Update bounds
      update_ax <- sign(cy_vector) == sign(ay_vector)
      ax_vector[update_ax] <- cx_vector[update_ax]
      bx_vector[!update_ax] <- cx_vector[!update_ax]
    }

    # Return midpoint as result
    return((ax_vector + bx_vector) / 2)
  }

  if (length(unique(dt$By)) == 1) {
    # Single chain case - Using mean like the Shiny app
    selected_mean <- mean(dt$mean)
    selected_sd <- sqrt(mean(dt$variance))

    # Create vectors of the same length for vectorized calculation
    means_vector <- rep(selected_mean, 2)
    sds_vector <- rep(selected_sd, 2)
    requests_vector <- c(PI_request1, PI_request2)

    pi_values <- find_PI_vectorized(requests_vector, means_vector, sds_vector)

    result_dt <- data.table::data.table(
      By = unique(dt$By),
      PI_Lower = pi_values[1],
      PI_Upper = pi_values[2]
    )
  } else {
    # Multiple groups case
    # Group by "By" column and take mean for each group
    by_groups <- dt[, .(mean = mean(mean), variance = mean(variance)), by = By]

    # Calculate PI for each group
    pi_results <- by_groups[, {
      selected_mean <- mean
      selected_sd <- sqrt(variance)

      # Create vectors for vectorized calculation
      means_vector <- rep(selected_mean, 2)
      sds_vector <- rep(selected_sd, 2)
      requests_vector <- c(PI_request1, PI_request2)

      pi_values <- find_PI_vectorized(requests_vector, means_vector, sds_vector)

      .(PI_Lower = pi_values[1], PI_Upper = pi_values[2])
    }, by = By]

    result_dt <- pi_results
  }

  return(as.data.frame(result_dt))
}
