find_optimal_point <- function(scatter_data, gP_mean_vector, gamma_actual, max_iterations = 300, tolerance = 0.01) {
  # Initialize results list
  results <- list()

  # Process each By value separately
  by_values <- unique(scatter_data$By)

  for(by_val in by_values) {

    # Get data for this By value
    by_data <- scatter_data[scatter_data$By == by_val, ]

    # Get gP_mean for this By value
    by_gP_mean <- gP_mean_vector[as.character(by_val)]

    # Get min and max x values for this By value
    x_min <- min(by_data$lower_quantile)
    x_max <- max(by_data$lower_quantile)

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

      if(iteration %% 10 == 0) { # Print every 10 iterations
      }

      # Check if we're close enough
      if(abs(diff_c$gamma_difference) < tolerance) {
        cat("\nConverged! Difference from target (", sprintf("%.2f", diff_c$gamma_difference),
            ") is within tolerance (", tolerance, ")\n")
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
        cat("\nWarning: Maximum iterations (", max_iterations,
            ") reached without achieving desired tolerance.\n")
      }
    }

    # Calculate final point
    optimal_x <- (a + b) / 2
    optimal_y <- 2 * by_gP_mean - optimal_x

    gamma_result <- calculate_gamma_difference(by_data, by_gP_mean, optimal_x, gamma_actual)

    # Print final results for this By value
    cat("Points satisfying:", gamma_result$points_satisfying, "out of",
        gamma_result$total_points, "\n")

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
