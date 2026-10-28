#' Calculate Prediction Intervals
#'
#' Calculates Bayesian prediction intervals from MCMC draws. Every draw \eqn{m}
#' defines a normal distribution with mean \eqn{\mu_m} and variance
#' \eqn{\sigma^2_m}. The posterior predictive distribution of a future
#' observation is approximated by the average of these distribution functions,
#' \deqn{\tilde p(x) = \frac{1}{M} \sum_{m=1}^{M} \Phi\left(\frac{x - \mu_m}{\sigma_m}\right),}
#' and the limits of a two-sided \eqn{P}\% prediction interval are the
#' \eqn{(1-P)/2} and \eqn{(1+P)/2} quantiles of \eqn{\tilde p}. The quantiles
#' are found numerically: a bisection that is accelerated by Newton steps
#' whenever they stay inside the bracket. The bracket \[smallest, largest\] of the per-draw
#' quantiles always contains the quantile of the average, so the sign of
#' \eqn{\tilde p(x) - p} is guaranteed to differ at the two starting points.
#'
#' @param data A data frame or data.table containing at least the following columns:
#'   \describe{
#'     \item{By}{A grouping variable}
#'     \item{mean}{The mean of the normal distribution of each draw}
#'     \item{variance}{The variance of the normal distribution of each draw}
#'   }
#' @param percent_for_pi The desired prediction interval percentage (between 0 and 100)
#' @param Eta Relative convergence tolerance: the search stops when the bracket,
#'   or a Newton step, is smaller than \code{Eta} times the standard deviation
#'   of the predictive distribution (default = 1e-8).
#'
#' @return A data frame with the following columns:
#'   \describe{
#'     \item{By}{The grouping variable}
#'     \item{PI_Lower}{Lower bound of the prediction interval}
#'     \item{PI_Upper}{Upper bound of the prediction interval}
#'   }
#'
#' @examples
#' data <- data.frame(
#'   By = c("A", "A", "B", "B"),
#'   mean = c(10, 11, 20, 21),
#'   variance = c(2, 2.1, 3, 3.1)
#' )
#' calculate_PI(data, percent_for_pi = 95)
#'
#' @export
calculate_PI <- function(data, percent_for_pi, Eta = 1e-8) {
  # Check inputs
  if (!is.data.frame(data) || !is.numeric(percent_for_pi) || length(percent_for_pi) != 1 ||
      !is.finite(percent_for_pi) || percent_for_pi <= 0 || percent_for_pi >= 100) {
    stop("Invalid input parameters")
  }
  if (!all(c("By", "mean", "variance") %in% names(data))) {
    stop("'data' must contain the columns By, mean and variance")
  }
  if (!is.numeric(Eta) || length(Eta) != 1 || !is.finite(Eta) || Eta <= 0) {
    stop("'Eta' must be a single positive number")
  }

  # The two probabilities at which the limits are required
  slice <- (1 - percent_for_pi / 100) / 2
  probabilities <- c(slice, 1 - slice)

  # Bisection on  mean_m(Phi((x - mu_m) / sigma_m)) - p
  find_PI_limits <- function(means, variances) {
    sds <- sqrt(pmax(variances, 1e-12))
    vapply(probabilities, function(p) {
      per_draw <- stats::qnorm(p, mean = means, sd = sds)
      lower <- min(per_draw)
      upper <- max(per_draw)
      initial_width <- upper - lower
      # Precision is relative to the spread of the predictive distribution itself
      # (average variance plus variance of the means), not to the size of the bracket
      total_sd <- sqrt(mean(sds^2) + if (length(means) > 1) stats::var(means) else 0)
      tolerance <- Eta * total_sd
      if (initial_width == 0) {
        return(lower)
      }
      # Start from the quantile of the single averaged normal, which is close
      x <- stats::qnorm(p, mean = mean(means), sd = sqrt(mean(sds^2)))
      if (!is.finite(x) || x <= lower || x >= upper) {
        x <- (lower + upper) / 2
      }
      for (iteration in seq_len(200)) {
        # value of (average of the per-draw CDFs) - p, and its bracket update
        value <- mean(stats::pnorm(x, mean = means, sd = sds)) - p
        if (value < 0) {
          lower <- x
        } else {
          upper <- x
        }
        if (upper - lower <= tolerance) {
          break
        }
        # Newton step (the derivative is the average of the per-draw densities);
        # whenever it leaves the bracket, bisect instead, so convergence is guaranteed
        slope <- mean(stats::dnorm(x, mean = means, sd = sds))
        step <- x - value / slope
        if (!is.finite(step) || step <= lower || step >= upper) {
          step <- (lower + upper) / 2
        }
        if (abs(step - x) <= tolerance) {
          x <- step
          break
        }
        x <- step
      }
      x
    }, numeric(1))
  }

  groups <- unique(data$By)
  limits <- lapply(groups, function(group) {
    rows <- which(data$By == group)
    means <- as.numeric(data$mean[rows])
    variances <- as.numeric(data$variance[rows])
    if (anyNA(means) || anyNA(variances)) {
      stop("'mean' and 'variance' must not contain missing values")
    }
    find_PI_limits(means, variances)
  })

  data.frame(
    By = groups,
    PI_Lower = vapply(limits, function(x) x[1], numeric(1)),
    PI_Upper = vapply(limits, function(x) x[2], numeric(1))
  )
}
