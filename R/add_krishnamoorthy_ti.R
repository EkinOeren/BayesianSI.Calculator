#' Add Krishnamoorthy Tolerance Interval Values
#'
#' This function adds Krishnamoorthy TI values as new columns (K_TI_Lower and K_TI_Upper)
#' to the data frame, preserving the original TI values.
#'
#' @param data A data frame containing at least the following columns:
#' \itemize{
#'   \item By: A grouping variable
#'   \item TI_Lower: Original lower tolerance interval bound
#'   \item TI_Upper: Original upper tolerance interval bound
#' }
#' @param optimal_results A list of optimal results from find_optimal_point()
#'
#' @return A data frame with additional columns:
#' \itemize{
#'   \item K_TI_Lower: Krishnamoorthy lower tolerance interval bound
#'   \item K_TI_Upper: Krishnamoorthy upper tolerance interval bound
#' }
#'
#' @examples
#' \dontrun{
#' data <- data.frame(
#'   By = c("A", "B"),
#'   TI_Lower = c(-3, -4),
#'   TI_Upper = c(3, 4)
#' )
#' optimal_results <- list(
#'   A = list(By = "A", g1 = -2.5, g2 = 2.5),
#'   B = list(By = "B", g1 = -3.5, g2 = 3.5)
#' )
#' updated_data <- add_krishnamoorthy_ti(data, optimal_results)
#' }
#'
#' @export
add_krishnamoorthy_ti <- function(data, optimal_results) {
  if (is.null(optimal_results)) return(data)

  # Add new columns for Krishnamoorthy TI values
  data$K_TI_Lower <- NA
  data$K_TI_Upper <- NA

  # Loop through each optimal result
  for (by_val in names(optimal_results)) {
    result <- optimal_results[[by_val]]

    # Find the row(s) in data that match this By value
    row_idx <- which(data$By == result$By)

    if (length(row_idx) > 0) {
      # Add Krishnamoorthy TI values in new columns
      data$K_TI_Lower[row_idx] <- result$g1
      data$K_TI_Upper[row_idx] <- result$g2
    }
  }

  return(data)
}
