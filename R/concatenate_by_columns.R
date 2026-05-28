#' Concatenate Multiple By Columns
#'
#' Creates a new column by concatenating values from multiple by columns with an underscore separator.
#'
#' @param data A data frame containing the input data
#' @param by_columns Character vector of column names to concatenate. If NULL, returns the original dataset.
#' @param new_column_name Character, name for the concatenated column (default = "concatenated_by_column")
#'
#' @return A data frame with the original data and a new column containing concatenated by values
#'
#' @examples
#' \dontrun{
#' data <- data.frame(
#'   by1 = c(1, 1, 1, 2, 3, 3, 2),
#'   by2 = c("orange", "orange", "apple", "apple", "apple", "apple", "apple"),
#'   value = rnorm(7)
#' )
#'
#' # Basic usage
#' result <- concatenate_by_columns(data, by_columns = c("by1", "by2"))
#'
#' # With custom column name
#' result <- concatenate_by_columns(data,
#'                                 by_columns = c("by1", "by2"),
#'                                 new_column_name = "my_by_column")
#' }
#'
#' @export
concatenate_by_columns <- function(data, by_columns = NULL, new_column_name = "concatenated_by_column") {
  # Input validation
  if (!is.data.frame(data)) {
    stop("Input 'data' must be a data frame")
  }

  # If no by_columns specified, return original dataset
  if (is.null(by_columns)) {
    return(data)
  }

  # Check if specified columns exist
  missing_cols <- setdiff(by_columns, names(data))
  if (length(missing_cols) > 0) {
    stop(sprintf("Column(s) not found in dataset: %s",
                 paste(missing_cols, collapse = ", ")))
  }

  # Create concatenated column
  data[[new_column_name]] <- do.call(paste, c(Map(function(col) as.character(data[[col]]),
                                                  by_columns),
                                              list(sep = "_")))

  return(data)
}
