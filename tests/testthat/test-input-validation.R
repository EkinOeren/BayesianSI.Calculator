# Synthetic data only.
make_draws <- function(n = 20) {
  set.seed(42)
  data.frame(
    G = rep(c("A", "B"), each = n / 2),
    b = rnorm(n, 10, 0.5),
    s2 = rgamma(n, shape = 20, rate = 10),
    cov1 = rnorm(n)
  )
}

bsi <- function(data = make_draws(), ...) {
  args <- list(data = data, fixed_effects = "b", random_params = "s2", by = "G",
               tolerance_interval_level = 90)
  extra <- list(...)
  args[names(extra)] <- extra
  do.call(bayesian_statistical_intervals, args)
}

test_that("a valid call still works", {
  res <- bsi()
  expect_equal(nrow(res$data), 2)
})

test_that("required arguments are reported clearly", {
  d <- make_draws()
  expect_error(bayesian_statistical_intervals(d, "b", "s2", by = "G"), "tolerance_interval_level")
  expect_error(bayesian_statistical_intervals(d, random_params = "s2", by = "G",
                                              tolerance_interval_level = 90), "fixed_effects")
  expect_error(bayesian_statistical_intervals(d, fixed_effects = "b", by = "G",
                                              tolerance_interval_level = 90), "random_params")
  expect_error(bsi(by = c("G", "b")), "'by'")
})

test_that("interval levels must be single numbers between 0 and 100", {
  for (arg in c("tolerance_interval_level", "credible_interval_level",
                "prediction_interval_level", "credibility_of_tolerance_interval")) {
    expect_error(bsi(), NA)
    expect_error(do.call(bsi, setNames(list(150), arg)), arg, fixed = TRUE)
    expect_error(do.call(bsi, setNames(list(0), arg)), arg, fixed = TRUE)
    expect_error(do.call(bsi, setNames(list(NA_real_), arg)), arg, fixed = TRUE)
    expect_error(do.call(bsi, setNames(list(c(80, 90)), arg)), arg, fixed = TRUE)
    expect_error(do.call(bsi, setNames(list("90"), arg)), arg, fixed = TRUE)
  }
})

test_that("other scalar arguments are checked", {
  expect_error(bsi(multiplication_factor = 0), "multiplication_factor")
  expect_error(bsi(multiplication_factor = -2), "multiplication_factor")
  expect_error(bsi(multiplication_factor = "a"), "multiplication_factor")
  expect_error(bsi(log_normal = "yes"), "log_normal")
  expect_error(bsi(use_krishnamoorthy = NA), "use_krishnamoorthy")
  expect_error(bsi(krishnamoorthy_tolerance = 0), "krishnamoorthy_tolerance")
})

test_that("covariates are validated", {
  expect_error(bsi(covariate_cols = "cov1", covariate_values = c(1, 2)), "must match length")
  expect_error(bsi(covariate_cols = "cov1", covariate_values = "a"), "numeric and finite")
  expect_error(bsi(covariate_cols = "cov1", covariate_values = NA_real_), "numeric and finite")
  expect_error(bsi(covariate_cols = "missing_col", covariate_values = 1), "not found")
  expect_error(bsi(covariate_cols = 3, covariate_values = 1), "character vector")
  # covariate_values defaults to 1 when omitted
  res <- bsi(covariate_cols = "cov1")
  expect_match(res$summary, "cov1")
})

test_that("missing or infinite draws are rejected instead of treated as zero", {
  d <- make_draws()
  d$b[3] <- NA
  expect_error(bsi(d), "Fixed effect column 'b' contains 1 missing or non-finite value\\(s\\) \\(first at row 3\\)")

  d <- make_draws()
  d$s2[c(2, 5)] <- Inf
  expect_error(bsi(d), "Variance component column 's2' contains 2")

  d <- make_draws()
  d$cov1[1] <- NaN
  expect_error(bsi(d, covariate_cols = "cov1", covariate_values = 1), "Covariate column 'cov1'")

  d <- make_draws()
  d$G[4] <- NA
  expect_error(bsi(d), "By column 'G' contains missing values")
})

test_that("non-numeric draw columns are rejected, numeric text is accepted", {
  d <- make_draws()
  d$b <- as.character(d$b)
  expect_error(bsi(d), NA)           # numbers stored as text are converted

  d$b[2] <- "abc"
  expect_error(bsi(d), "must be numeric")
})

test_that("negative variances are rejected with a clear message", {
  d <- make_draws()
  d$s2[7] <- -1
  expect_error(bsi(d), "negative value in 1 draw\\(s\\) \\(first at row 7\\)")
  # zero variance is allowed (degenerate but not invalid)
  d <- make_draws()
  d$s2 <- 0
  expect_error(bsi(d), NA)
})

test_that("an empty data frame is rejected", {
  expect_error(bsi(make_draws()[0, ]), "no rows")
})

test_that("the lower-level function applies the same draw checks", {
  d <- make_draws()
  d$b[1] <- NA
  expect_error(calculate_predicted_distributions(d, "b", "s2", "G", 90), "missing or non-finite")
  expect_error(calculate_predicted_distributions(make_draws(), "b", "s2", "G", 90,
                                                 multiplication_factor = -1), "multiplication_factor")
})
