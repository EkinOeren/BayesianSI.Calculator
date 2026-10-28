# Division guard for reducible imprecision, duplicate group handling, the message
# for the default `by`, and numeric output of the predicted distributions.
# Synthetic data only.

draws <- function() {
  set.seed(8)
  data.frame(G = rep(c("A", "B"), each = 60), b = rnorm(120, 10, 0.4), s2 = rgamma(120, 30, 15))
}

test_that("reducible_ratio gives the usual ratio where it is defined", {
  r <- reducible_ratio(bound = c(5, 6), pi = c(4, 4), median_value = c(2, 2), side = "upper")
  expect_equal(r, c((5 - 4) / (5 - 2), (6 - 4) / (6 - 2)))
})

test_that("reducible_ratio returns NA with a warning when the bound equals the median", {
  expect_warning(
    r <- reducible_ratio(bound = c(5, 2), pi = c(4, 1), median_value = c(2, 2), side = "upper"),
    "not defined for 1 group\\(s\\)")
  expect_equal(r[1], 1 / 3)
  expect_true(is.na(r[2]))
  expect_false(is.nan(r[2]))
  # no warning when asked not to warn
  expect_silent(reducible_ratio(2, 1, 2, "lower", warn = FALSE))
})

test_that("missing inputs stay missing without a division warning", {
  expect_silent(r <- reducible_ratio(NA_real_, 1, 2, "upper"))
  expect_true(is.na(r))
})

test_that("add_reducible_imprecision uses fractions (5 dp) or percentages (3 dp)", {
  df <- data.frame(Median = 2, TI_Upper = 5, PI_Upper = 4, TI_Lower = -1, PI_Lower = 0)
  frac <- add_reducible_imprecision(df)
  expect_equal(frac$ReducibleUpper, round(1 / 3, 5))
  expect_equal(frac$ReducibleLower, round((-1 - 0) / (-1 - 2), 5))
  pct <- add_reducible_imprecision(df, percent = TRUE)
  expect_equal(pct$ReducibleUpper, round(100 / 3, 3))
})

test_that("a degenerate group gives NA and a warning in the legacy percentage output", {
  df <- data.frame(Median = c(2, 2), TI_Upper = c(5, 2), PI_Upper = c(4, 1),
                   TI_Lower = c(-1, -1), PI_Lower = c(0, 0))
  expect_warning(res <- add_reducible_imprecision(df, percent = TRUE), "upper")
  expect_true(is.na(res$ReducibleUpper[2]))
  expect_equal(res$ReducibleUpper[1], round(100 / 3, 3))
})

test_that("duplicate By rows are reported instead of silently dropped", {
  res <- data.frame(By = c("A", "B", "A"), x = c(1, 2, 3))
  expect_warning(out <- keep_first_by_row(res), "Duplicate 'By' values .*A")
  expect_equal(out$By, c("A", "B"))
  expect_equal(out$x, c(1, 2))
  # no duplicates: unchanged and silent
  ok <- data.frame(By = c("A", "B"), x = 1:2)
  expect_silent(same <- keep_first_by_row(ok))
  expect_identical(same, ok)
})

test_that("the main function does not warn for ordinary data", {
  expect_no_warning(res <- bayesian_statistical_intervals(draws(), "b", "s2", by = "G",
                                                          tolerance_interval_level = 90))
  expect_false(anyNA(res$data$ReducibleUpper))
})

test_that("omitting `by` gives a message, not a warning", {
  expect_message(
    expect_no_warning(bayesian_statistical_intervals(draws(), "b", "s2", tolerance_interval_level = 90)),
    "As by value is not specified")
})

test_that("predicted distributions are numeric and rounded to 3 decimals", {
  pd <- calculate_predicted_distributions(draws(), "b", "s2", "G", 90)
  for (col in c("mean", "median", "lower_quantile", "upper_quantile", "variance")) {
    expect_type(pd[[col]], "double")
    expect_equal(pd[[col]], round(pd[[col]], 3))
  }
})

test_that("infinite covariate values are rejected instead of producing text columns", {
  expect_error(calculate_predicted_distributions(draws(), "b", "s2", "G", 90,
                                                 covariate_cols = "b", covariate_values = Inf),
               "numeric and finite")
})
