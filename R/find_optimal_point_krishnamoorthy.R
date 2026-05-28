
#' Find Optimal Point for Krishnamoorthy Method
#'
#' This function finds the optimal point (g1, g2) for the Krishnamoorthy method
#' using a bisection algorithm. The optimal point satisfies the constraint
#' g2 = 2*gP_mean - g1 and achieves the target gamma (confidence level).
#'
#' @param scatter_data A data frame containing at least the following columns:
#' \itemize{
#'   \item By: A grouping variable
#'   \item lower_quantile: Lower quantile values
#'   \item upper_quantile: Upper quantile values
#' }
#' @param gP_mean_vector Named vector of gP_mean values, with names corresponding to By values
#' @param gamma_actual Numeric, the target gamma (confidence level) percentage
#' @param max_iterations Integer, maximum number of iterations for bisection (default = 300)
#' @param tolerance Numeric, convergence criterion for bisection (default = 0.01)
#'
#' @return A list of results, one element per By value, each containing:
#' \itemize{
#'   \item By: The By value
#'   \item g1: The optimal x-coordinate
#'   \item g2: The optimal y-coordinate
#'   \item gamma_difference: Difference between target and achieved gamma
#'   \item gamma_achieved: The achieved gamma percentage
#'   \item points_satisfying: Number of points satisfying the condition
#'   \item total_points: Total number of points evaluated
#'   \item iterations: Number of iterations used
#' }
#'
#' @examples
#' \dontrun{
#' data <- data.frame(
#'   By = c("A", "A", "A", "A", "B", "B", "B", "B"),
#'   lower_quantile = c(-2, -1, -3, -2, -4, -3, -5, -4),
#'   upper_quantile = c(2, 3, 1, 2, 4, 5, 3, 4)
#' )
#' gP_mean_vector <- c(A = 0.5, B = 0)
#' results <- find_optimal_point(data, gP_mean_vector, 95)
#' }
#'
#' @export
find_optimal_point <- function(scatter_data, gP_mean_vector, gamma_actual, max_iterations = 300, tolerance = 0.01) {
  # Initialize results list
  results <- list()

  # Process each By value separately
  by_values <- unique(scatter_data$By)
  for(by_val in by_values) {
    if (getOption("verbose", FALSE)) {
      cat("\n\n=== Processing By Value:", by_val, "===\n")
    }

    # Get data for this By value
    by_data <- scatter_data[scatter_data$By == by_val, ]

    # Get gP_mean for this By value
    by_gP_mean <- gP_mean_vector[as.character(by_val)]

    if (getOption("verbose", FALSE)) {
      cat("gP_mean for this group:", by_gP_mean, "\n")
    }

    # Get min and max x values for this By value
    x_min <- min(by_data$lower_quantile)
    x_max <- max(by_data$lower_quantile)

    if (getOption("verbose", FALSE)) {
      cat("Search range: [", x_min, ",", x_max, "]\n")
    }

    # Initialize bisection variables
    a <- x_min
    b <- x_max
    iteration <- 0

    # Bisection method
    while(iteration < max_iterations) {
      # Calculate midpoint
      c <- (a + b) / 2

      # Calculate gamma differences at a and c
      diff_a <- calculate_gamma_difference(by_data, by_gP_mean, a, gamma_actual)
      diff_c <- calculate_gamma_difference(by_data, by_gP_mean, c, gamma_actual)

      if(getOption("verbose", FALSE) && iteration %% 10 == 0) { # Print every 10 iterations
        cat("\nIteration", iteration, ":\n")
        cat("Current interval: [", a, ",", b, "]\n")
        cat("Midpoint:", c, "\n")
        cat("Gamma at midpoint:", sprintf("%.2f", 100 - diff_c$gamma_difference), "% (Target:", gamma_actual, "%)\n")
        cat("Difference from target:", sprintf("%.2f", diff_c$gamma_difference), "\n")
      }

      # Check if we're close enough
      if(abs(diff_c$gamma_difference) < tolerance) {
        if (getOption("verbose", FALSE)) {
          cat("\nConverged! Difference from target (", sprintf("%.2f", diff_c$gamma_difference),
              ") is within tolerance (", tolerance, ")\n")
        }
        break
      }

      # Update interval
      if(sign(diff_c$gamma_difference) == sign(diff_a$gamma_difference)) {
        a <- c
      } else {
        b <- c
      }

      iteration <- iteration + 1
      if(iteration == max_iterations) {
        warning(paste("Maximum iterations (", max_iterations,
                      ") reached without achieving desired tolerance for By =", by_val))
      }
    }

    # Calculate final point
    optimal_x <- (a + b) / 2
    optimal_y <- 2 * by_gP_mean - optimal_x
    gamma_result <- calculate_gamma_difference(by_data, by_gP_mean, optimal_x, gamma_actual)

    # Print final results for this By value
    if (getOption("verbose", FALSE)) {
      cat("\nFinal Results for By =", by_val, ":\n")
      cat("Optimal point: (", sprintf("%.6f", optimal_x), ",", sprintf("%.6f", optimal_y), ")\n")
      cat("Final gamma achieved:", sprintf("%.2f", 100 - gamma_result$gamma_difference), "%\n")
      cat("Points satisfying:", gamma_result$points_satisfying, "out of",
          gamma_result$total_points, "\n")
      cat("Number of iterations:", iteration, "\n")
    }

    # Store results for this By value
    results[[as.character(by_val)]] <- list(
      By = by_val,
      g1 = optimal_x,
      g2 = optimal_y,
      gamma_difference = gamma_result$gamma_difference,
      gamma_achieved = 100 - gamma_result$gamma_difference,
      points_satisfying = gamma_result$points_satisfying,
      total_points = gamma_result$total_points,
      iterations = iteration
    )
  }

  return(results)
}
