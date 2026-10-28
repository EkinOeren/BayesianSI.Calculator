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
