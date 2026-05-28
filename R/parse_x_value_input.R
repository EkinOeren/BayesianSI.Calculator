#' Parse X Value Input
#'
#' Parses a string input representing either a single numeric value or a range [min,max].
#'
#' @param input_text Character string to parse, either a single number or a range in the format "[min,max]"
#'
#' @return A list with components:
#' \itemize{
#'   \item{type}{Either "single" for a single value or "range" for a range}
#'   \item{value}{The numeric value (if type is "single")}
#'   \item{xmin}{The minimum value (if type is "range")}
#'   \item{xmax}{The maximum value (if type is "range")}
#' }
#' @return NULL if the input cannot be parsed
#'
#' @examples
#' parse_x_value_input("5.2")
#' parse_x_value_input("[2.1, 7.8]")
#'
#' @export
parse_x_value_input <- function(input_text) {
  # Check if input is null, NA, or empty
  if (is.null(input_text) || is.na(input_text) || input_text == "") {
    message("Input is empty, NULL, or NA")
    return(NULL)
  }

  # Check if input matches range pattern [value1,value2]
  range_pattern <- "^\\s*\\[\\s*(-?\\d*\\.?\\d+)\\s*,\\s*(-?\\d*\\.?\\d+)\\s*\\]\\s*$"
  if (grepl(range_pattern, input_text)) {
    matches <- regmatches(input_text, regexec(range_pattern, input_text))[[1]]
    # Validate that the extracted values are numeric
    if (!grepl("^-?\\d*\\.?\\d+$", matches[2]) || !grepl("^-?\\d*\\.?\\d+$", matches[3])) {
      message("Range values must be numeric")
      return(NULL)
    }

    xmin <- as.numeric(matches[2])
    xmax <- as.numeric(matches[3])

    # Ensure xmin is less than xmax
    if (xmin > xmax) {
      temp <- xmin
      xmin <- xmax
      xmax <- temp
    }

    return(list(type = "range", xmin = xmin, xmax = xmax))
  } else {
    # Check if input matches single numeric value pattern
    if (!grepl("^\\s*-?\\d*\\.?\\d+\\s*$", input_text)) {
      message("Input must be either a single numeric value or a range in format [min,max]")
      return(NULL)
    }

    value <- as.numeric(trimws(input_text))
    return(list(type = "single", value = value))
  }
}
