#' @importFrom stats median
#' @importFrom utils head
#' @importFrom grDevices colorRampPalette
#' @importFrom shiny tagList
#' @importFrom data.table .N
#' @noRd
NULL

# Column names used via non-standard evaluation (data.table / dplyr)
utils::globalVariables(c(
  ".", "By", "M", "variance", "lower_quantile", "upper_quantile",
  "Lower_Lower", "Upper_Upper", "TI_Lower", "TI_Upper",
  "Median", "CI_Lower", "CI_Upper", "PI_Lower", "PI_Upper"
))
