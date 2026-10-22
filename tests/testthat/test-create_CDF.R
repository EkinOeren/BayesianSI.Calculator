library(testthat)
library(BayesianStatisticalIntervalsCalculator)  # Replace with the actual name of your package

# Load the test data
data(data_InputFileTester2)  # Assuming this dataset is included in your package

# Test function to compare dataframes
compare_dataframes <- function(df1, df2) {
  expect_equal(dim(df1), dim(df2))
  expect_equal(colnames(df1), colnames(df2))
  expect_equal(df1, df2, tolerance = 1e-7)
}

# Test function to compare lists of dataframes
compare_dataframe_lists <- function(list1, list2) {
  expect_equal(length(list1), length(list2))
  expect_equal(names(list1), names(list2))

  for (name in names(list1)) {
    compare_dataframes(list1[[name]], list2[[name]])
  }
}

test_that("create_CDF produces consistent results across different methods", {

  # Parameters for all tests
  fixed_effects <- c("t1", "t2")
  random_params <- "v1"
  by <- "By"
  tolerance_level <- 90
  n_points <- 100
  ci_level <- 90
  multiplication_factor <- 1.7
  log_normal <- FALSE

  # Pre-calculate predicted distributions
  pred_dist <- calculate_predicted_distributions(
    data = data_InputFileTester2,
    fixed_effects = fixed_effects,
    random_params = random_params,
    by = by,
    tolerance_level = tolerance_level,
    multiplication_factor = multiplication_factor
  )

  # Test 1: Single by-value using raw data
  cdf_data_single <- create_CDF(
    data = data_InputFileTester2,
    fixed_effects = fixed_effects,
    random_params = random_params,
    by = by,
    by_value = "2",
    tolerance_level = tolerance_level,
    n_points = n_points,
    ci_level = ci_level,
    multiplication_factor = multiplication_factor,
    log_normal = log_normal
  )

  # Test 2: Single by-value using pre-calculated predicted distributions
  cdf_data_single_from_pred <- create_CDF(
    pred_dist = pred_dist,
    by_value = "2",
    n_points = n_points,
    ci_level = ci_level,
    log_normal = log_normal
  )

  # Compare results from Test 1 and Test 2
  compare_dataframes(cdf_data_single, cdf_data_single_from_pred)

  # Test 3: All by-values using raw data
  cdf_data_all <- create_CDF(
    data = data_InputFileTester2,
    fixed_effects = fixed_effects,
    random_params = random_params,
    by = by,
    tolerance_level = tolerance_level,
    n_points = n_points,
    ci_level = ci_level,
    multiplication_factor = multiplication_factor,
    log_normal = log_normal
  )

  # Test 4: All by-values using pre-calculated predicted distributions
  cdf_data_all_from_pred <- create_CDF(
    pred_dist = pred_dist,
    n_points = n_points,
    ci_level = ci_level,
    log_normal = log_normal
  )

  # Compare results from Test 3 and Test 4
  compare_dataframe_lists(cdf_data_all, cdf_data_all_from_pred)

  # Additional test: Ensure single by-value result matches corresponding result in all by-values
  compare_dataframes(cdf_data_single, cdf_data_all[["2"]])

  # Test 5: Check if single chain dataset is handled correctly
  # Assuming "1" is the single chain identifier in your dataset
  cdf_data_single_chain <- create_CDF(
    data = data_InputFileTester2,
    fixed_effects = fixed_effects,
    random_params = random_params,
    by = by,
    by_value = "Single Chain",
    tolerance_level = tolerance_level,
    n_points = n_points,
    ci_level = ci_level,
    multiplication_factor = multiplication_factor,
    log_normal = log_normal
  )

  expect_equal(cdf_data_single_chain$by_value[1], "Single Chain")

  # Test 6: Check if log_normal transformation is applied correctly
  cdf_data_log_normal <- create_CDF(
    data = data_InputFileTester2,
    fixed_effects = fixed_effects,
    random_params = random_params,
    by = by,
    by_value = "2",
    tolerance_level = tolerance_level,
    n_points = n_points,
    ci_level = ci_level,
    multiplication_factor = multiplication_factor,
    log_normal = TRUE
  )

  expect_true(all(cdf_data_log_normal$x > 0))  # Log-normal values should be positive
  expect_true(!identical(cdf_data_log_normal$x, cdf_data_single$x))  # Should be different from non-log-normal
})
