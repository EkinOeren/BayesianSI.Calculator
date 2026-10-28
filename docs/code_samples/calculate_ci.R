calculate_ci <- function(data, ColToAggregate, PercentForCI, ByColumn) {
  # Convert to data.table if not already
  if (!is.data.table(data)) {
    dt <- as.data.table(data)
  } else {
    dt <- data
  }

  if (!ColToAggregate %in% names(dt) || !ByColumn %in% names(dt)) {
    stop(paste("Column", ColToAggregate, "or", ByColumn, "not found in the dataframe"))
  }

  # Calculate alpha and slice once
  alpha <- 1 - PercentForCI/100
  slice <- alpha / 2

  # Use data.table syntax for faster grouping and calculation
  results <- dt[, .(
    N = .N,
    CI_Lower = quantile(get(ColToAggregate), slice),
    CI_Upper = quantile(get(ColToAggregate), 1 - slice),
    Median = median(get(ColToAggregate))
  ), by = ByColumn]

  return(as.data.frame(results))
}
