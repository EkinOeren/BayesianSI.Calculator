# Test script for Krishnamoorthy method functions
# Save as: tests/testthat/test-krishnamoorthy.R

library(testthat)
library(dplyr)

# Create toy dataset
test_that("Krishnamoorthy method functions work correctly", {

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
    b5 = c(-0.1019, -0.112, -0.1536, -0.0809, -0.0868, -0.0265, -0.07, -0.0899, -0.0994, -0.0786),
    LogPrior = c(-42.25, -42.0222, -42.3597, -42.0782, -42.125, -42.016, -42.1286, -42.169, -41.9415, -42.1678),
    LogLike = c(-863.2, -862.6, -864.8, -862.3, -860, -863.1, -869.4, -860.9, -860.9, -862.4),
    LogPost = c(-905.5, -904.6, -907.1, -904.3, -902.1, -905.1, -911.5, -903.1, -902.9, -904.5)
  )

  # ============================================================================
  # STEP 1: Calculate Predicted Distributions
  # ============================================================================

  # Parameters
  fixed_effects <- "b2"
  covariate_cols <- "b4"
  covariate_values <- 1.5
  random_params <- "s2"
  multiplication_factor <- 1.8
  by <- "EGTPTNUM"
  tolerance_level <- 90
  confidence_level <- 80

  # Calculate predicted distributions
  # First, initialize mean vector with zeros
  mean_vector <- rep(0, nrow(toy_data))

  # Add covariate effects
  mean_vector <- mean_vector + (toy_data[[covariate_cols]] * covariate_values)

  # Add fixed effects
  mean_vector <- mean_vector + toy_data[[fixed_effects]]

  # Calculate variance
  variance_vector <- toy_data[[random_params]] * multiplication_factor

  # Calculate tolerance parameters
  tolerance_alpha <- tolerance_level / 100
  tolerance_slice <- (1 - tolerance_alpha) / 2

  # Calculate quantiles
  lower_quantiles <- qnorm(tolerance_slice,
                           mean = mean_vector,
                           sd = sqrt(pmax(variance_vector, 1e-10)))

  upper_quantiles <- qnorm(1 - tolerance_slice,
                           mean = mean_vector,
                           sd = sqrt(pmax(variance_vector, 1e-10)))

  medians <- qnorm(0.5,
                   mean = mean_vector,
                   sd = sqrt(pmax(variance_vector, 1e-10)))

  # Create predicted distributions data frame
  pred_dist <- data.frame(
    By = toy_data[[by]],
    mean = mean_vector,
    median = medians,
    lower_quantile = lower_quantiles,
    upper_quantile = upper_quantiles,
    variance = variance_vector
  )

  # Print the predicted distributions
  cat("\nPredicted Distributions:\n")
  print(head(pred_dist, 3))

  # ============================================================================
  # STEP 2: Test calculate_gP_mean function
  # ============================================================================

  cat("\n=== Testing calculate_gP_mean ===\n")

  # Call the function
  gP_mean_vector <- calculate_gP_mean(pred_dist, tolerance_level)

  # Print result
  cat("gP_mean_vector:", gP_mean_vector, "\n")

  # Test that it returns a named vector
  expect_true(is.numeric(gP_mean_vector))
  expect_true(length(gP_mean_vector) == 1)  # Only one By value
  expect_true(names(gP_mean_vector)[1] == "60")

  # ============================================================================
  # STEP 3: Test calculate_gamma_difference function
  # ============================================================================

  cat("\n=== Testing calculate_gamma_difference ===\n")

  # Get the gP_mean for By = 60
  gP_mean_60 <- gP_mean_vector["60"]

  # Test with a few x values
  test_x_values <- c(min(pred_dist$lower_quantile),
                     mean(pred_dist$lower_quantile),
                     max(pred_dist$lower_quantile))

  # Call the function
  gamma_diff_result <- calculate_gamma_difference(
    pred_dist,
    gP_mean_60,
    test_x_values,
    confidence_level
  )

  # Print result
  cat("Gamma differences:", gamma_diff_result$gamma_differences, "\n")
  cat("Points satisfying:", gamma_diff_result$points_satisfying, "\n")
  cat("Total points:", gamma_diff_result$total_points, "\n")

  # Test structure
  expect_true(is.list(gamma_diff_result))
  expect_true(all(c("gamma_differences", "points_satisfying", "total_points") %in% names(gamma_diff_result)))
  expect_equal(length(gamma_diff_result$gamma_differences), length(test_x_values))
  expect_equal(gamma_diff_result$total_points, nrow(pred_dist))

    #============================================================================
    # STEP 4: Test find_optimal_point function
    # ============================================================================

  cat("\n=== Testing find_optimal_point ===\n")

  # Call the function without the verbose parameter
  optimal_results <- find_optimal_point(
    pred_dist,
    gP_mean_vector,
    confidence_level
  )

  # Print result
  cat("\nOptimal Results:\n")
  print(optimal_results)

  # Test structure
  expect_true(is.list(optimal_results))
  expect_true("60" %in% names(optimal_results))
  expect_true(all(c("By", "g1", "g2", "gamma_achieved") %in% names(optimal_results[["60"]])))

  # ============================================================================
  # STEP 5: Test add_krishnamoorthy_ti function
  # ============================================================================

  cat("\n=== Testing add_krishnamoorthy_ti ===\n")

  # Create a simple intervals data frame
  intervals_data <- data.frame(
    By = 60,
    TI_Lower = -15.0,
    TI_Upper = 15.0
  )

  # Call the function
  updated_data <- add_krishnamoorthy_ti(intervals_data, optimal_results)

  # Print result
  cat("\nUpdated Data with Krishnamoorthy TI:\n")
  print(updated_data)

  # Test structure
  expect_true(all(c("K_TI_Lower", "K_TI_Upper") %in% names(updated_data)))
  expect_equal(nrow(updated_data), nrow(intervals_data))

  # ============================================================================
  # STEP 6: Verify expected Krishnamoorthy TI values
  # ============================================================================

  cat("\n=== Verifying Expected Krishnamoorthy TI Values ===\n")

  # Expected values with allowance
  expected_lower_ti <- -14.8654
  expected_upper_ti <- 14.9449
  allowance <- 0.0009

  # Get actual values
  actual_lower_ti <- updated_data$K_TI_Lower
  actual_upper_ti <- updated_data$K_TI_Upper

  # Print comparison
  cat("Expected Lower TI:", expected_lower_ti, "\n")
  cat("Actual Lower TI:", actual_lower_ti, "\n")
  cat("Difference:", abs(expected_lower_ti - actual_lower_ti), "\n\n")

  cat("Expected Upper TI:", expected_upper_ti, "\n")
  cat("Actual Upper TI:", actual_upper_ti, "\n")
  cat("Difference:", abs(expected_upper_ti - actual_upper_ti), "\n")

  # Test with allowance
  expect_true(abs(actual_lower_ti - expected_lower_ti) <= allowance,
              info = paste("Lower TI difference:", abs(actual_lower_ti - expected_lower_ti)))

  expect_true(abs(actual_upper_ti - expected_upper_ti) <= allowance,
              info = paste("Upper TI difference:", abs(actual_upper_ti - expected_upper_ti)))

  cat("\n=== All Krishnamoorthy Tests Passed! ===\n")
})
