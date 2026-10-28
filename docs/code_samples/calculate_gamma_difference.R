calculate_gamma_difference <- function(scatter_data, gP_mean, point_x_vector, gamma_actual) {
  # Use the current confidence level from input
  gamma_actual <- isolate(input$confidence_of_tolerance)

  # Vectorized calculations
  point_y_vector <- 2 * gP_mean - point_x_vector
  total_points <- nrow(scatter_data)

  # Create a matrix of conditions
  conditions <- outer(scatter_data$upper_quantile, point_y_vector, ">=") &
    outer(scatter_data$lower_quantile, point_x_vector, "<=")

  # Sum the conditions for each point_x
  points_satisfying <- colSums(conditions)

  gamma_for_points <- (points_satisfying / total_points) * 100
  gamma_differences <- gamma_actual - gamma_for_points

  return(list(
    gamma_differences = gamma_differences,
    points_satisfying = points_satisfying,
    total_points = total_points
  ))
}
