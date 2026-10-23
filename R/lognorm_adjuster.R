#' Adjust Log-Normal Data and Calculate Reducible Imprecision
#'
#' This function takes interval data and performs two main operations:
#' 1. If log_normal is TRUE, exponentiates the values to transform from log space
#' 2. Calculates reducible imprecision percentages for upper and lower bounds
#'
#' @param IntervalsOutputInitial A data frame containing interval data with columns:
#'   Median, CI_Lower, CI_Upper, TI_Lower, TI_Upper, PI_Lower, PI_Upper
#' @param log_normal Logical indicating whether to transform data from log space
#'
#' @return A data frame with the original columns plus:
#'   \item{ReducibleUpper}{Upper reducible imprecision percentage}
#'   \item{ReducibleLower}{Lower reducible imprecision percentage}
#'
#' @export
#'
#' @examples
#' data <- data.frame(
#'   Median = 1, CI_Lower = 0.5, CI_Upper = 1.5,
#'   TI_Lower = 0.3, TI_Upper = 1.7, PI_Lower = 0.4, PI_Upper = 1.6
#' )
#' lognorm_adjuster(data, log_normal = TRUE)
#'
lognorm_adjuster <- function(IntervalsOutputInitial, log_normal) {
  if (missing(IntervalsOutputInitial)) {
    stop("Input data is required")
  }

  data <- IntervalsOutputInitial

  if (log_normal) {
    data <- data |>
      dplyr::mutate(dplyr::across(c(Median, CI_Lower, CI_Upper,
                             TI_Lower, TI_Upper, PI_Lower, PI_Upper), exp))
  }

  # Add the calculation for Reducible Imprecision
  data$ReducibleUpper <- ((data$TI_Upper - data$PI_Upper)/(data$TI_Upper - data$Median))
  data$ReducibleLower <- ((data$TI_Lower - data$PI_Lower)/(data$TI_Lower - data$Median))

  # Convert to percentages
  data$ReducibleUpper <- round(data$ReducibleUpper * 100, 3)
  data$ReducibleLower <- round(data$ReducibleLower * 100, 3)

  return(data)
}
