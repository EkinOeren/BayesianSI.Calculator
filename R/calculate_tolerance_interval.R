
#' Calculate Tolerance Intervals
#'
#' This function calculates tolerance intervals for specified lower and upper columns,
#' grouped by another column. Tolerance intervals contain a specified proportion of
#' the population with a given confidence level.
#'
#' @param data A data frame or data.table containing the data
#' @param LowerColToAggregate Column name for the lower bound values
#' @param UpperColToAggregate Column name for the upper bound values
#' @param ConfidenceLevel Confidence level for the tolerance interval (between 0 and 100)
#' @param ByColumn Column name to group by
#'
#' @return A data frame with the following columns:
#'   \item{By}{The grouping variable}
#'   \item{TI_Lower}{Lower bound of the tolerance interval}
#'   \item{TI_Upper}{Upper bound of the tolerance interval}
#'
#' @examples
#' \dontrun{
#' data <- data.frame(
#'   Group = rep(c("A", "B"), each = 50),
#'   lower_quantile = c(rnorm(50, 5, 1), rnorm(50, 10, 2)),
#'   upper_quantile = c(rnorm(50, 15, 1), rnorm(50, 20, 2))
#' )
#' calculate_tolerance_interval(data, "lower_quantile", "upper_quantile", 95, "Group")
#' }
#'
#' @importFrom data.table as.data.table :=
#' @export
calculate_tolerance_interval <- function(data, LowerColToAggregate, UpperColToAggregate, ConfidenceLevel, ByColumn) {
  # Convert to data.table if not already
  if (!data.table::is.data.table(data)) {
    dt <- data.table::as.data.table(data)
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
    Lower_Lower = stats::quantile(get(LowerColToAggregate), confidence_slice),
    Lower_Upper = stats::quantile(get(LowerColToAggregate), 1 - confidence_slice),
    Upper_Lower = stats::quantile(get(UpperColToAggregate), confidence_slice),
    Upper_Upper = stats::quantile(get(UpperColToAggregate), 1 - confidence_slice)
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
