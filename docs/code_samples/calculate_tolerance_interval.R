calculate_tolerance_interval <- function(data, LowerColToAggregate, UpperColToAggregate, ConfidenceLevel, ByColumn) {
  # Convert to data.table if not already
  if (!is.data.table(data)) {
    dt <- as.data.table(data)
  } else {
    dt <- data
  }

  if (!LowerColToAggregate %in% names(dt) || !UpperColToAggregate %in% names(dt) || !ByColumn %in% names(dt)) {
    stop(paste("One or more required columns not found in the dataframe"))
  }

  # Calculate confidence parameters once
  confidence_alpha <- 1 - ConfidenceLevel/100
  confidence_slice <- confidence_alpha / 2

  # Use data.table syntax for faster grouping and calculation
  results <- dt[, .(
    N = .N,
    Lower_Lower = quantile(get(LowerColToAggregate), confidence_slice),
    Lower_Upper = quantile(get(LowerColToAggregate), 1 - confidence_slice),
    Upper_Lower = quantile(get(UpperColToAggregate), confidence_slice),
    Upper_Upper = quantile(get(UpperColToAggregate), 1 - confidence_slice)
  ), by = ByColumn]

  # Add TI columns
  results[, `:=`(
    TI_Lower = Lower_Lower,
    TI_Upper = Upper_Upper
  )]

  # Select only needed columns
  results <- results[, .(By = get(ByColumn), TI_Lower, TI_Upper)]

  return(as.data.frame(results))
}
