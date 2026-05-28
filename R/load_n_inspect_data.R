#' Load and Inspect Data
#'
#' This function attempts to load data using various methods and performs data quality checks.
#' It tries different reading methods in order of efficiency and flexibility.
#'
#' @param file_path Character string specifying the path to the data file.
#' @param data Optional data frame. If provided, only inspection is performed.
#' @param silent Logical, if TRUE suppresses messages (but not warnings). Default is FALSE.
#'
#' @return A data frame containing the loaded data.
#'
#' @importFrom data.table fread
#' @importFrom readr read_csv
#' @importFrom utils read.csv read.table
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # From a file
#' data <- load_n_inspect_data("path/to/your/data.csv")
#'
#' # From existing data frame
#' df <- data.frame(x = 1:10, y = letters[1:10])
#' data <- load_n_inspect_data(data = df)
#' }
load_n_inspect_data <- function(file_path = NULL, data = NULL, silent = FALSE) {
  # Input validation
  if (is.null(file_path) && is.null(data)) {
    stop("Either file_path or data must be provided")
  }

  if (!is.null(file_path) && !is.null(data)) {
    warning("Both file_path and data provided. Using data and ignoring file_path.")
    return(inspect_loaded_data(data))
  }

  # If data is provided directly, only perform inspection
  if (!is.null(data)) {
    return(inspect_loaded_data(data))
  }

  # Check if file exists
  if (!file.exists(file_path)) {
    stop("File does not exist: ", file_path)
  }

  # Try different loading methods
  loaded_data <- try_loading_methods(file_path, silent)

  # Inspect the loaded data
  inspect_loaded_data(loaded_data)

  return(loaded_data)
}

#' Try Different Loading Methods
#'
#' @param file_path Path to the data file
#' @param silent Logical, whether to suppress messages
#'
#' @return A data frame containing the loaded data
#'
#' @keywords internal
try_loading_methods <- function(file_path, silent) {
  # Initialize result
  loaded_data <- NULL
  success <- FALSE

  # 1. Try fread first (fastest)
  if (!success) {
    tryCatch({
      if (!silent) message("Attempting to load with data.table::fread...")
      loaded_data <- data.table::fread(file_path, data.table = FALSE)
      success <- TRUE
      if (!silent) message("Successfully loaded with fread")
    }, error = function(e) {
      if (!silent) message("fread failed: ", e$message)
    })
  }

  # 2. Try readr::read_csv
  if (!success) {
    tryCatch({
      if (!silent) message("Attempting to load with readr::read_csv...")
      loaded_data <- readr::read_csv(file_path, show_col_types = FALSE)
      loaded_data <- as.data.frame(loaded_data)  # Convert to standard data.frame
      success <- TRUE
      if (!silent) message("Successfully loaded with read_csv")
    }, error = function(e) {
      if (!silent) message("read_csv failed: ", e$message)
    })
  }

  # 3. Try base R read.csv
  if (!success) {
    tryCatch({
      if (!silent) message("Attempting to load with base::read.csv...")
      loaded_data <- read.csv(file_path, stringsAsFactors = FALSE)
      success <- TRUE
      if (!silent) message("Successfully loaded with read.csv")
    }, error = function(e) {
      if (!silent) message("read.csv failed: ", e$message)
    })
  }

  # 4. Try read.table with tab separator
  if (!success) {
    tryCatch({
      if (!silent) message("Attempting to load with read.table (tab separator)...")
      loaded_data <- read.table(file_path, header = TRUE, sep = "\t",
                                stringsAsFactors = FALSE)
      success <- TRUE
      if (!silent) message("Successfully loaded with read.table (tab separator)")
    }, error = function(e) {
      if (!silent) message("read.table with tab separator failed: ", e$message)
    })
  }

  # 5. Try read.table with various separators
  if (!success) {
    separators <- c(",", ";", "|", "/")
    for (sep in separators) {
      tryCatch({
        if (!silent) message("Attempting to load with read.table (separator: ", sep, ")...")
        loaded_data <- read.table(file_path, header = TRUE, sep = sep,
                                  stringsAsFactors = FALSE)
        success <- TRUE
        if (!silent) message("Successfully loaded with read.table (separator: ", sep, ")")
        break
      }, error = function(e) {
        if (!silent) message("read.table with separator ", sep, " failed: ", e$message)
      })
    }
  }

  # If all methods failed, stop with error
  if (!success) {
    stop("All loading methods failed. Please check the file format and try again.")
  }

  return(loaded_data)
}

#' Inspect Loaded Data
#'
#' @param data A data frame to inspect
#'
#' @return The input data frame, invisibly
#'
#' @keywords internal
inspect_loaded_data <- function(data) {
  # Check for empty data
  if (nrow(data) == 0) {
    warning("The data has 0 rows")
  }
  if (ncol(data) == 0) {
    warning("The data has 0 columns")
  }

  # Check column types
  col_types <- sapply(data, class)
  list_cols <- names(which(sapply(col_types, function(x) "list" %in% x)))
  if (length(list_cols) > 0) {
    warning("The following columns contain list data which may not be suitable for analysis: ",
            paste(list_cols, collapse = ", "))
  }

  # Check for NA and NaN values
  na_counts <- sapply(data, function(col) {
    if (is.list(col)) return(0)
    sum(is.na(col) | is.nan(col))
  })
  cols_with_na <- names(na_counts[na_counts > 0])
  if (length(cols_with_na) > 0) {
    warning("The following columns contain NA/NaN values: \n",
            paste(" - ", cols_with_na, ": ", na_counts[cols_with_na], " missing values",
                  collapse = "\n"))
  }

  # Check for mixed types (numeric and string)
  mixed_type_cols <- sapply(data, function(col) {
    if (is.list(col)) return(FALSE)
    if (is.character(col)) {
      return(any(grepl("^\\s*-?\\d*\\.?\\d+\\s*$", col)) &&
               !all(grepl("^\\s*-?\\d*\\.?\\d+\\s*$", col)))
    }
    return(FALSE)
  })
  if (any(mixed_type_cols)) {
    warning("The following columns contain both numeric and string data: ",
            paste(names(which(mixed_type_cols)), collapse = ", "))
  }

  # Check for columns with only integers
  integer_cols <- sapply(data, function(col) {
    if (is.list(col) || !is.numeric(col)) return(FALSE)
    all(col == floor(col), na.rm = TRUE)
  })
  if (any(integer_cols)) {
    message("The following columns contain only integer values: ",
            paste(names(which(integer_cols)), collapse = ", "))
  }

  # Check for columns with only zeros
  zero_cols <- sapply(data, function(col) {
    if (is.list(col) || !is.numeric(col)) return(FALSE)
    all(col == 0, na.rm = TRUE)
  })
  if (any(zero_cols)) {
    warning("The following columns contain only zero values: ",
            paste(names(which(zero_cols)), collapse = ", "))
  }

  # Return the data invisibly
  invisible(data)
}
