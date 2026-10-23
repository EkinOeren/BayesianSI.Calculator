# Internal input checks shared by the interval functions.
# The Shiny application keeps its own copy of the same checks (inst/shiny/app.R)
# so that it can run without this package; keep the two in sync.

#' Check that a value is a single number strictly between 0 and 100
#' @param x value to check
#' @param name argument name used in the error message
#' @noRd
check_level <- function(x, name) {
  if (missing(x) || !is.numeric(x) || length(x) != 1 || !is.finite(x) || x <= 0 || x >= 100) {
    stop(sprintf("'%s' must be a single number strictly between 0 and 100", name), call. = FALSE)
  }
  invisible(TRUE)
}

#' Check that a value is a single TRUE or FALSE
#' @noRd
check_flag <- function(x, name) {
  if (!is.logical(x) || length(x) != 1 || is.na(x)) {
    stop(sprintf("'%s' must be TRUE or FALSE", name), call. = FALSE)
  }
  invisible(TRUE)
}

#' Check that a value is a single positive, finite number
#' @noRd
check_positive <- function(x, name) {
  if (!is.numeric(x) || length(x) != 1 || !is.finite(x) || x <= 0) {
    stop(sprintf("'%s' must be a single positive number", name), call. = FALSE)
  }
  invisible(TRUE)
}

#' Check that MCMC draw columns are numeric with no missing or infinite values
#'
#' @param dt data frame holding the draws
#' @param cols names of the columns to check
#' @param label description used in the error message, e.g. "Fixed effect"
#' @noRd
check_draw_columns <- function(dt, cols, label) {
  if (nrow(dt) == 0) {
    stop("'data' has no rows", call. = FALSE)
  }
  for (col in cols) {
    x <- dt[[col]]
    if (!is.numeric(x)) {
      converted <- suppressWarnings(as.numeric(as.character(x)))
      if (any(is.na(converted) & !is.na(x))) {
        stop(sprintf("%s column '%s' must be numeric but contains non-numeric values", label, col),
             call. = FALSE)
      }
      x <- converted
    }
    bad <- which(!is.finite(x))
    if (length(bad) > 0) {
      stop(sprintf("%s column '%s' contains %d missing or non-finite value(s) (first at row %d)",
                   label, col, length(bad), bad[1]), call. = FALSE)
    }
  }
  invisible(TRUE)
}

#' Check that the total variance of every draw is non-negative
#' @noRd
check_variance <- function(variance_vector) {
  neg <- which(variance_vector < 0)
  if (length(neg) > 0) {
    stop(sprintf("The variance components sum to a negative value in %d draw(s) (first at row %d); variances must be non-negative",
                 length(neg), neg[1]), call. = FALSE)
  }
  invisible(TRUE)
}
