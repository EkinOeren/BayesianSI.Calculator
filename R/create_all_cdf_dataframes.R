#' Create CDF Data for All By-Values
#'
#' This function creates CDF data for all by-values in the predicted distributions output.
#'
#' @param pred_dist A dataframe containing predicted distributions (output from calculate_predicted_distributions)
#' @param n_points Integer, number of points for CDF evaluation (default = 20)
#' @param ci_level Numeric, confidence interval level as percentage (default = 90)
#' @param log_normal Logical, whether to use log-normal transformation (default = FALSE)
#'
#' @return A list of dataframes, each containing CDF data for a specific by-value
#'
#' @examples
#' \dontrun{
#' pred_dist <- calculate_predicted_distributions(data, "fixed1", "random1",
#'                                              "Group", 95)
#' all_cdf_data <- create_all_cdf_dataframes(pred_dist)
#' }
#'
#' @export
create_all_cdf_dataframes <- function(pred_dist, n_points = 20, ci_level = 90, log_normal = FALSE) {
  # Input validation
  if (!is.data.frame(pred_dist)) {
    stop("pred_dist must be a data frame")
  }

  if (!"By" %in% names(pred_dist)) {
    stop("pred_dist must contain a 'By' column")
  }

  # Get unique by-values
  by_values <- unique(pred_dist$By)

  # Check if it's a single chain dataset
  is_single_chain <- length(by_values) == 1 && (by_values == "SingleChain" || by_values == "1")

  # Process each by-value
  result_list <- lapply(by_values, function(bv) {
    if (is_single_chain) {
      bv <- "Single Chain"
    }
    tryCatch({
      create_cdf_dataframe(pred_dist, by_value = bv, n_points = n_points,
                           ci_level = ci_level, log_normal = log_normal)
    }, error = function(e) {
      warning(paste("Error processing by-value", bv, ":", e$message))
      NULL
    })
  })

  # Remove any NULL results (from errors)
  result_list <- result_list[!sapply(result_list, is.null)]

  # Name the list elements
  names(result_list) <- sapply(result_list, function(df) df$by_value[1])

  return(result_list)
}
