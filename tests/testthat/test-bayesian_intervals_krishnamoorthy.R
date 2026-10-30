# test-bayesian_intervals_krishnamoorthy.R

library(testthat)
library(dplyr)

test_that("bayesian_statistical_intervals works with and without Krishnamoorthy method", {

  # ============================================================================
  # SETUP: Create Toy Dataset
  # ============================================================================

  toy_data <- data.frame(
    EGTPTNUM = c(60, 60, 60, 60, 60, 60, 60, 60, 60, 60),
    Iteration = c(1001, 1301, 1601, 1901, 2201, 2501, 2801, 3101, 3401, 3701),
    s2 = c(74.1817, 58.917, 75.0402, 64.747, 67.8113, 64.5019, 69.6509, 70.4482, 55.461, 70.8997),
    b0 = c(45.8727, 46.3931, 63.4306, 37.1383, 37.4012, 14.781, 31.5076, 39.186, 42.1068, 36.5392),
    b1 = c(-4.5644, -1.8261, -1.9153, -4.8739, -4.1826, -3.6536, -4.6351, -3.1368, -3.3774, -5.8277),
    b2 = c(-5.2276, -3.9667, -2.9237, -7.6383, -2.5675, -5.1732, -1.451, -3.6605, -4.1559, -5.7852),
    b3 = c(6.8522, 10.6505, 10.7839, 6.4292, 8.9568, 8.8313, 4.3675, 7.8938, 9.3794, 8.1801),
    b4 = c(1.3195, 3.6841, 3.3926, 3.126, 3.4957, 2.9057, 3.2898, 2.0038, 3.9755, 1.4375),
    b5 = c(-0.1019, -0.112, -0.1536, -0.0809, -0.0868, -0.0265, -0.07, -0.0899, -0.0994, -0.0786)
  )

  # Test parameters
  params <- list(
    fixed_effects = "b2",
    covariate_cols = "b4",
    covariate_values = 1.5,
    random_params = "s2",
    multiplication_factor = 1.8,
    by = "EGTPTNUM",
    tolerance_interval_level = 90,
    credible_interval_level = 90,
    prediction_interval_level = 90,
    credibility_of_tolerance_interval = 80
  )

  # ============================================================================
  # TEST 1: Without Krishnamoorthy Method
  # ============================================================================

  cat("\n=== Testing Without Krishnamoorthy Method ===\n")

  result_standard <- do.call(
    bayesian_statistical_intervals,
    c(list(data = toy_data, use_krishnamoorthy = FALSE), params)
  )

  # Check structure
  expect_true(is.list(result_standard))
  expect_true(all(c("data", "summary") %in% names(result_standard)))
  expect_false("krishnamoorthy_results" %in% names(result_standard))
  expect_false(any(c("K_TI_Lower", "K_TI_Upper") %in% names(result_standard$data)))

  cat("\nStandard Results:\n")
  print(result_standard$data)

  # ============================================================================
  # TEST 2: With Krishnamoorthy Method
  # ============================================================================

  cat("\n=== Testing With Krishnamoorthy Method ===\n")

  result_krishnamoorthy <- do.call(
    bayesian_statistical_intervals,
    c(list(data = toy_data, use_krishnamoorthy = TRUE), params)
  )

  # Check structure
  expect_true(is.list(result_krishnamoorthy))
  expect_true(all(c("data", "summary", "krishnamoorthy_results") %in% names(result_krishnamoorthy)))
  expect_true(all(c("K_TI_Lower", "K_TI_Upper") %in% names(result_krishnamoorthy$data)))

  cat("\nKrishnamoorthy Results:\n")
  print(result_krishnamoorthy$data)

  # ============================================================================
  # TEST 3: Compare Results
  # ============================================================================

  cat("\n=== Comparing Results ===\n")

  # Check that standard TI columns are identical between methods
  expect_equal(result_standard$data$TI_Lower, result_krishnamoorthy$data$TI_Lower)
  expect_equal(result_standard$data$TI_Upper, result_krishnamoorthy$data$TI_Upper)

  # Verify Krishnamoorthy TI values: the interval is centred on the mean midpoint, and
  # the share of draws whose own interval lies inside it equals the credibility (80%).
  pd <- calculate_predicted_distributions(
    toy_data, params$fixed_effects, params$random_params, params$by,
    params$tolerance_interval_level, params$multiplication_factor,
    params$covariate_cols, params$covariate_values)
  k_lower <- result_krishnamoorthy$data$K_TI_Lower
  k_upper <- result_krishnamoorthy$data$K_TI_Upper
  expect_equal((k_lower + k_upper) / 2, mean((pd$lower_quantile + pd$upper_quantile) / 2), tolerance = 1e-6)
  expect_equal(mean(pd$lower_quantile >= k_lower & pd$upper_quantile <= k_upper),
               params$credibility_of_tolerance_interval / 100)

  # Regression values
  expect_equal(k_lower, -22.0166, tolerance = 1e-3)
  expect_equal(k_upper, 22.0961, tolerance = 1e-3)


  cat("\n=== Test Complete ===\n")
})
