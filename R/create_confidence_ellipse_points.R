#' Confidence ellipse coordinates
#'
#' Computes points on a confidence ellipse from a 2x2 covariance matrix and centre.
#'
#' @param cov_matrix 2x2 positive definite covariance matrix
#' @param centre Numeric vector of length 2
#' @param level Confidence level between 0 and 1
#' @param npoints Number of points on the ellipse
#' @return A matrix with columns "x" and "y"
#' @noRd
create_confidence_ellipse_points <- function(cov_matrix,
                                             centre,
                                             level = 0.95,
                                             npoints = 100) {
  if (!is.matrix(cov_matrix) || any(dim(cov_matrix) != c(2, 2))) {
    stop("'cov_matrix' must be a 2x2 matrix")
  }
  if (!is.numeric(centre) || length(centre) != 2) {
    stop("'centre' must be a numeric vector of length 2")
  }
  if (!is.numeric(level) || length(level) != 1 || level <= 0 || level >= 1) {
    stop("'level' must be a number between 0 and 1")
  }
  if (!is.numeric(npoints) || length(npoints) != 1 || npoints < 4) {
    stop("'npoints' must be a number >= 4")
  }

  eig <- eigen(cov_matrix, symmetric = TRUE)
  if (any(!is.finite(eig$values)) || any(eig$values <= .Machine$double.eps)) {
    stop("Covariance matrix must be positive definite")
  }

  radius <- sqrt(stats::qchisq(level, df = 2))
  angles <- seq(0, 2 * pi, length.out = npoints)
  unit_circle <- rbind(cos(angles), sin(angles))

  scale_matrix <- eig$vectors %*% diag(sqrt(eig$values), nrow = 2)
  transformed <- radius * (scale_matrix %*% unit_circle)

  points <- cbind(centre[1] + transformed[1, ], centre[2] + transformed[2, ])
  colnames(points) <- c("x", "y")
  points
}
