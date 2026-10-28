update_ti_values <- function(data, optimal_results) {
  if (is.null(optimal_results)) return(data)

  for (by_val in names(optimal_results)) {
    result <- optimal_results[[by_val]]
    row_idx <- which(data$By == result$By)
    if (length(row_idx) > 0) {
      data$TI_Lower[row_idx] <- result$g1
      data$TI_Upper[row_idx] <- result$g2
    }
  }
  return(data)
}
