# calculate_PI() inverts the posterior predictive distribution
#   p(x) = (1/M) * sum_m Phi((x - mu_m) / sigma_m)   (manuscript Eq. 6-7)
# Synthetic data only.

# Independent reference: root finding on the same function with a very tight tolerance
mixture_quantile <- function(p, means, variances) {
  f <- function(x) mean(pnorm(x, means, sqrt(variances))) - p
  uniroot(f, interval = c(min(means) - 10 * sqrt(max(variances)),
                          max(means) + 10 * sqrt(max(variances))), tol = 1e-12)$root
}

make_pi_data <- function() {
  set.seed(101)
  data.frame(
    By = rep(c("A", "B"), each = 200),
    mean = c(rnorm(200, 10, 1.0), rnorm(200, 20, 0.3)),
    variance = c(rgamma(200, 20, 10), rgamma(200, 40, 10))
  )
}

test_that("calculate_PI returns the quantiles of the average of the per-draw CDFs", {
  d <- make_pi_data()
  res <- calculate_PI(d, 90)

  expect_s3_class(res, "data.frame")
  expect_named(res, c("By", "PI_Lower", "PI_Upper"))
  expect_equal(res$By, c("A", "B"))

  for (g in c("A", "B")) {
    x <- d[d$By == g, ]
    expect_equal(res$PI_Lower[res$By == g], mixture_quantile(0.05, x$mean, x$variance), tolerance = 1e-6)
    expect_equal(res$PI_Upper[res$By == g], mixture_quantile(0.95, x$mean, x$variance), tolerance = 1e-6)
  }
})

test_that("identical draws give the closed-form normal quantiles", {
  d <- data.frame(By = "g", mean = rep(5, 10), variance = rep(4, 10))
  res <- calculate_PI(d, 95)
  expect_equal(res$PI_Lower, 5 - qnorm(0.975) * 2, tolerance = 1e-7)
  expect_equal(res$PI_Upper, 5 + qnorm(0.975) * 2, tolerance = 1e-7)

  single <- data.frame(By = "g", mean = 0, variance = 1)
  res1 <- calculate_PI(single, 90)
  expect_equal(c(res1$PI_Lower, res1$PI_Upper), qnorm(c(0.05, 0.95)), tolerance = 1e-7)
})

test_that("disagreement between the draws widens the interval", {
  # same average mean and average variance, but the means differ between draws:
  # the old single-normal approximation would give the same interval for both
  same <- data.frame(By = "g", mean = rep(0, 100), variance = rep(1, 100))
  spread <- data.frame(By = "g", mean = rep(c(-1, 1), 50), variance = rep(1, 100))
  w_same <- unname(diff(unlist(calculate_PI(same, 90)[c("PI_Lower", "PI_Upper")])))
  w_spread <- unname(diff(unlist(calculate_PI(spread, 90)[c("PI_Lower", "PI_Upper")])))
  expect_gt(w_spread, w_same)
  # variance of the mixture is mean(sigma^2) + var(mu) = 2, so the width is close to
  # that of a normal with sd sqrt(2) (the mixture is slightly flatter-topped)
  expect_equal(w_spread, 2 * qnorm(0.95) * sqrt(2), tolerance = 0.03)
})

test_that("the result matches the exact Student-t interval of the one-mean model", {
  # y ~ N(mu, sigma^2) with a flat prior: the posterior predictive is Student-t
  set.seed(2026)
  n <- 10; M <- 20000
  y <- rnorm(n, 10, 2)
  sigma2 <- (n - 1) * var(y) / rchisq(M, n - 1)
  mu <- rnorm(M, mean(y), sqrt(sigma2 / n))
  exact <- mean(y) + qt(c(0.05, 0.95), n - 1) * sqrt(var(y) * (1 + 1 / n))
  res <- calculate_PI(data.frame(By = "g", mean = mu, variance = sigma2), 90)
  expect_equal(c(res$PI_Lower, res$PI_Upper), exact, tolerance = 0.01)
})

test_that("the limits lie inside the bracket of the per-draw quantiles", {
  d <- make_pi_data()
  res <- calculate_PI(d, 80)
  for (g in c("A", "B")) {
    x <- d[d$By == g, ]
    q <- qnorm(0.1, x$mean, sqrt(x$variance))
    expect_gte(res$PI_Lower[res$By == g], min(q))
    expect_lte(res$PI_Lower[res$By == g], max(q))
  }
})

test_that("intervals are ordered and widen with the level", {
  d <- make_pi_data()
  narrow <- calculate_PI(d, 80)
  wide <- calculate_PI(d, 99)
  expect_true(all(narrow$PI_Lower < narrow$PI_Upper))
  expect_true(all(wide$PI_Lower < narrow$PI_Lower))
  expect_true(all(wide$PI_Upper > narrow$PI_Upper))
})

test_that("group order and type are preserved, and data.table input works", {
  d <- make_pi_data()
  d$By <- ifelse(d$By == "A", 2, 1)            # numeric groups, "2" appears first
  res <- calculate_PI(data.table::as.data.table(d), 90)
  expect_equal(res$By, c(2, 1))
  expect_type(res$By, "double")
})

test_that("Eta controls the precision of the bisection", {
  d <- make_pi_data()[1:200, ]
  exact <- mixture_quantile(0.95, d$mean, d$variance)
  loose <- calculate_PI(d, 90, Eta = 1e-2)
  tight <- calculate_PI(d, 90, Eta = 1e-10)
  expect_lt(abs(tight$PI_Upper - exact), 1e-7)
  expect_gte(abs(loose$PI_Upper - exact), abs(tight$PI_Upper - exact))
})

test_that("calculate_PI rejects invalid input", {
  d <- make_pi_data()
  expect_error(calculate_PI("not a data frame", 95), "Invalid input parameters")
  expect_error(calculate_PI(d, 0), "Invalid input parameters")
  expect_error(calculate_PI(d, 100), "Invalid input parameters")
  expect_error(calculate_PI(d, "95"), "Invalid input parameters")
  expect_error(calculate_PI(d, c(80, 90)), "Invalid input parameters")
  expect_error(calculate_PI(d[, c("By", "mean")], 95), "columns By, mean and variance")
  expect_error(calculate_PI(d, 95, Eta = 0), "'Eta'")
  d$mean[3] <- NA
  expect_error(calculate_PI(d, 95), "missing values")
})
