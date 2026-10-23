# Synthetic data only.
make_pd_data <- function() {
  data.frame(
    grp = c("a", "a", "b", "b"),
    b1 = c(1, 2, 3, 4),
    cov = c(10, 20, 30, 40),
    s2 = c(4, 4, 9, 9)
  )
}

pd <- function(...) {
  calculate_predicted_distributions(
    data = make_pd_data(), fixed_effects = "b1", random_params = "s2",
    by = "grp", tolerance_level = 90, ...
  )
}

test_that("calculate_predicted_distributions returns the expected structure", {
  res <- pd()

  expect_s3_class(res, "data.frame")
  expect_named(res, c("By", "mean", "median", "lower_quantile", "upper_quantile", "variance"))
  expect_equal(nrow(res), 4)
  expect_equal(res$By, c("a", "a", "b", "b"))
})

test_that("mean, variance and quantiles follow the normal model", {
  res <- pd(multiplication_factor = 2)

  expect_equal(res$mean, c(1, 2, 3, 4))
  expect_equal(res$median, c(1, 2, 3, 4))
  expect_equal(res$variance, c(8, 8, 18, 18))
  expect_equal(res$lower_quantile, round(res$mean - qnorm(0.95) * sqrt(res$variance), 3))
  expect_equal(res$upper_quantile, round(res$mean + qnorm(0.95) * sqrt(res$variance), 3))
})

test_that("covariates are scaled and added to the mean", {
  res <- pd(covariate_cols = "cov", covariate_values = 0.5)
  expect_equal(res$mean, c(1, 2, 3, 4) + c(10, 20, 30, 40) * 0.5)

  # covariate_values defaults to 1 when omitted
  res_default <- pd(covariate_cols = "cov")
  expect_equal(res_default$mean, c(1, 2, 3, 4) + c(10, 20, 30, 40))
})

test_that("multiple fixed effects and variance components are summed", {
  dat <- data.frame(g = c("x", "x"), a = c(1, 2), b = c(10, 20), v1 = c(1, 2), v2 = c(3, 4))
  res <- calculate_predicted_distributions(dat, c("a", "b"), c("v1", "v2"), "g", 90)
  expect_equal(res$mean, c(11, 22))
  expect_equal(res$variance, c(4, 6))
})

test_that("'Single Chain Dataset' groups all rows together without altering the input", {
  dat <- make_pd_data()
  res <- calculate_predicted_distributions(dat, "b1", "s2", "Single Chain Dataset", 90)

  expect_equal(length(unique(res$By)), 1)
  expect_false("SingleChain" %in% names(dat))
})

test_that("calculate_predicted_distributions validates its inputs", {
  dat <- make_pd_data()
  expect_error(calculate_predicted_distributions("x", "b1", "s2", "grp", 90), "data frame")
  expect_error(calculate_predicted_distributions(dat, "nope", "s2", "grp", 90), "not found")
  expect_error(calculate_predicted_distributions(dat, "b1", "nope", "grp", 90), "not found")
  expect_error(calculate_predicted_distributions(dat, "b1", "s2", "nope", 90), "not found")
  expect_error(calculate_predicted_distributions(dat, "b1", "s2", "grp", 100), "between 0 and 100")
  expect_error(calculate_predicted_distributions(dat, "b1", "s2", "grp", 0), "between 0 and 100")
  expect_error(pd(multiplication_factor = "a"), "positive number")
  expect_error(pd(covariate_cols = "nope"), "not found")
  expect_error(pd(covariate_cols = "cov", covariate_values = c(1, 2)), "must match")
})
