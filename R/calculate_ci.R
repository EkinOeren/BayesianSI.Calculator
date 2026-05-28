#' Calculate Credibility Intervals
#'
#' This function calculates credibility intervals for a specified column,
#' grouped by another column.
#'
#' @param data A data frame or data.table containing the data
#' @param ColToAggregate Column name to calculate credibility intervals for
#' @param PercentForCI Credibility level (between 0 and 100)
#' @param ByColumn Column name to group by
#'
#' @return A data frame with the following columns:
#' \item{By}{The grouping variable}
#' \item{N}{Number of observations in each group}
#' \item{CI_Lower}{Lower bound of the credibility interval}
#' \item{CI_Upper}{Upper bound of the credibility interval}
#' \item{Median}{Median value for each group}
#'
#' @examples
#' \dontrun{
#' data <- data.frame(
#'   Group = rep(c("A", "B"), each = 50),
#'   Value = c(rnorm(50, 10, 2), rnorm(50, 15, 3))
#' )
#' calculate_ci(data, "Value", 95, "Group")
#' }
#'
#' @importFrom data.table as.data.table
#' @export
calculate_ci <- function(data, ColToAggregate, PercentForCI, ByColumn) {
  # Input validation
  if (!ColToAggregate %in% names(data) || !ByColumn %in% names(data)) {
    stop(paste("Column", ColToAggregate, "or", ByColumn, "not found in the dataframe"))
  }

  if (!is.numeric(PercentForCI) || PercentForCI <= 0 || PercentForCI >= 100) {
    stop("PercentForCI must be a number between 0 and 100")
  }

  # Convert to data.table if not already
  if (!data.table::is.data.table(data)) {
    dt <- data.table::as.data.table(data)
  } else {
    dt <- data
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

  # Standardize the grouping column name to "By"
  data.table::setnames(results, ByColumn, "By")

  return(as.data.frame(results))
}
