
#' Calculate Gamma Difference for Krishnamoorthy Method
#'
#' This function calculates the difference between the target gamma (confidence level)
#' and the actual gamma achieved for a given set of points.
#'
#' @param scatter_data A data frame containing at least the following columns:
#' \itemize{
#'   \item By: A grouping variable
#'   \item lower_quantile: Lower quantile values
#'   \item upper_quantile: Upper quantile values
#' }
#' @param gP_mean Numeric, the gP_mean value for the By group
#' @param point_x_vector Numeric, the x-coordinate(s) to evaluate
#' @param gamma_actual Numeric, the target gamma (confidence level) percentage
#'
#' @return A list containing:
#' \itemize{
#'   \item gamma_differences: Differences between target and achieved gamma
#'   \item points_satisfying: Number of points satisfying the condition
#'   \item total_points: Total number of points evaluated
#' }
#'
#' @examples
#' \dontrun{
#' data <- data.frame(
#'   By = c("A", "A", "A", "A"),
#'   lower_quantile = c(-2, -1, -3, -2),
#'   upper_quantile = c(2, 3, 1, 2)
#' )
#' gP_mean_A <- 0.5
#' result <- calculate_gamma_difference(
#'   data, gP_mean_A, c(-2, -1.5, -1), 95
#' )
#' }
#'
#' @export
calculate_gamma_difference <- function(scatter_data, gP_mean, point_x_vector, gamma_actual) {
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
