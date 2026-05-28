#' Calculate gP Mean for Krishnamoorthy Method
#'
#' This function calculates the gP_mean value for each By group in the data,
#' which is the average of the midpoints between lower and upper quantiles.
#'
#' @param scatter_data A data frame containing at least the following columns:
#' \itemize{
#'   \item By: A grouping variable
#'   \item lower_quantile: Lower quantile values
#'   \item upper_quantile: Upper quantile values
#' }
#' @param P Numeric, the probability level (not used directly but included for API consistency)
#'
#' @return A named vector of gP_mean values, with names corresponding to By values
#'
#' @examples
#' \dontrun{
#' data <- data.frame(
#'   By = c("A", "A", "B", "B"),
#'   lower_quantile = c(-2, -1, -3, -2),
#'   upper_quantile = c(2, 3, 1, 2)
#' )
#' gP_mean <- calculate_gP_mean(data, 90)
#' # Returns: c(A = 0.5, B = -0.5)
#' }
#'
#' @export
calculate_gP_mean <- function(scatter_data, P) {
  # Ensure scatter_data is a data frame
  scatter_data <- as.data.frame(scatter_data)

  # Check if required columns exist
  required_cols <- c("By", "lower_quantile", "upper_quantile")
  if (!all(required_cols %in% names(scatter_data))) {
    stop("Required columns (By, lower_quantile, upper_quantile) not found in the data")
  }

  # Calculate gP_mean for each By value
  gP_mean <- scatter_data %>%
    dplyr::group_by(By) %>%
    dplyr::summarise(
      M = dplyr::n(), # number of samples
      gP_mean = (1/M) * sum((lower_quantile + upper_quantile)/2),
      .groups = 'drop'
    )

  # Convert to a named vector for easier access
  gP_mean_vector <- stats::setNames(gP_mean$gP_mean, gP_mean$By)

  return(gP_mean_vector)
}
