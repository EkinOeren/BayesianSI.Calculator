#' Adjust Log-Normal Data and Calculate Reducible Uncertainty
#'
#' This function takes interval data and performs two main operations:
#' 1. If log_normal is TRUE, exponentiates the values to transform from log space
#' 2. Calculates reducible uncertainty percentages for upper and lower bounds
#'
#' @param IntervalsOutputInitial A data frame containing interval data with columns:
#'   Median, CI_Lower, CI_Upper, TI_Lower, TI_Upper, PI_Lower, PI_Upper
#' @param log_normal Logical indicating whether to transform data from log space
#'
#' @return A data frame with the original columns plus:
#'   \item{ReducibleUpper}{Upper reducible uncertainty percentage}
#'   \item{ReducibleLower}{Lower reducible uncertainty percentage}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data <- data.frame(
#'   Median = c(1),
#'   CI_Lower = c(0.5),
#'   CI_Upper = c(1.5),
#'   TI_Lower = c(0.3),
#'   TI_Upper = c(1.7),
#'   PI_Lower = c(0.4),
#'   PI_Upper = c(1.6)
#' )
#' result <- lognorm_adjuster(data, TRUE)
#' }
lognorm_adjuster <- function(IntervalsOutputInitial, log_normal) {
  if (missing(IntervalsOutputInitial)) {
    stop("Input data is required")
  }

  data <- IntervalsOutputInitial

  if (log_normal) {
    data <- data %>%
      dplyr::mutate(across(c(Median, CI_Lower, CI_Upper,
                             TI_Lower, TI_Upper, PI_Lower, PI_Upper), exp))
  }

  # Add the calculation for Reducible Uncertainty
  data$ReducibleUpper <- ((data$TI_Upper - data$PI_Upper)/(data$TI_Upper - data$Median))
  data$ReducibleLower <- ((data$TI_Lower - data$PI_Lower)/(data$TI_Lower - data$Median))

  # Convert to percentages
  data$ReducibleUpper <- round(data$ReducibleUpper * 100, 3)
  data$ReducibleLower <- round(data$ReducibleLower * 100, 3)

  return(data)
}
