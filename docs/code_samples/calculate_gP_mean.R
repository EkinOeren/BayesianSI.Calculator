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
    group_by(By) %>%
    summarise(
      M = n(),  # number of samples
      gP_mean = (1/M) * sum((lower_quantile + upper_quantile)/2),
      .groups = 'drop'
    )

  # Convert to a named vector for easier access
  gP_mean_vector <- setNames(gP_mean$gP_mean, gP_mean$By)

  return(gP_mean_vector)
}
