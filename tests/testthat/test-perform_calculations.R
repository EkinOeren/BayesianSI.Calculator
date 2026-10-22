# tests/testthat/test-perform_calculations.R

test_that("perform_calculations produces expected results with specific data", {
  # Create sample data based on provided input
  data <- data.frame(
    iteration = 1:19,
    mu = c(2.34, 2.41, 2.37, 2.39, 2.36, 2.42, 2.35, 2.40, 2.38, 2.43,
           2.8, 2.9, 2.7, 2.6, 2.8, 3.3, 2.3, 2.1, 2),
    t1 = c(-0.78, -0.82, -0.75, -0.79, -0.77, -0.81, -0.76, -0.80, -0.78, -0.83,
           -0.6, -0.8, -0.8, -0.9, -0.77, -0.79, -0.87, -0.67, -0.88),
    t2 = c(1.56, 1.49, 1.52, 1.54, 1.51, 1.55, 1.53, 1.50, 1.57, 1.52,
           1.5, 1.55, 1.85, 1.45, 1.3, 1.51, 1.4, 1.59, 1.2),
    v1 = c(0.89, 0.91, 0.88, 0.90, 0.87, 0.92, 0.89, 0.91, 0.88, 0.90,
           0.8, 0.7, 0.87, 0.98, 0.78, 0.88, 0.67, 0.59, 0.87),
    v2 = c(1.12, 1.09, 1.14, 1.11, 1.13, 1.10, 1.15, 1.12, 1.11, 1.13,
           1.11, 1.1, 1.14, 1.09, 1.21, 1.16, 1.12, 1.56, 1.7),
    By = c(3, 1, 1, 1, 1, 1, 1, 1, 1, 2,
           2, 2, 2, 2, 2, 2, 2, 2, 2),
    Bytwo = c("apples", rep("oranges", 9), rep("oranges", 3), rep("tangerines", 6))
  )

  # Run the function with specified parameters
  result <- perform_calculations(
    data = data,
    fixed_effects = "t1",
    random_params = "v1",
    by = "By",
    tolerance_level = 90,
    multiplication_factor = 1.7,
    percent_for_ci = 90,
    percent_for_pi = 90,
    confidence_of_tolerance = 80,
    log_normal = FALSE
  )

  # Check structure
  expect_type(result, "list")
  expect_true(all(c("data", "summary") %in% names(result)))
  expect_s3_class(result$data, "data.frame")
  expect_type(result$summary, "character")

  # Sort result by By column to ensure consistent order
  result_data <- result$data[order(result$data$By), ]

  # Expected values based on provided output
  expected_values <- data.frame(
    By = c(1, 2, 3),
    Median = c(-0.7850, -0.8000, -0.7800),
    CI_Lower = c(-0.8165, -0.8910, -0.7800),
    CI_Upper = c(-0.7535, -0.6315, -0.7800),
    PI_Lower = c(-2.8144, -2.7144, -2.8037),
    PI_Upper = c(1.2444, 1.1324, 1.2437),
    TI_Lower = c(-2.8663, -2.8943, -2.8030),
    TI_Upper = c(1.2623, 1.2325, 1.2430)
  )

  # Check data structure
  expect_equal(nrow(result_data), 3)  # Three unique By values
  expect_equal(result_data$By, c(1, 2, 3))

  # Check key values with tolerance for floating point differences
  for (i in 1:nrow(expected_values)) {
    expect_equal(result_data$Median[i], expected_values$Median[i], tolerance = 0.001)
    expect_equal(result_data$CI_Lower[i], expected_values$CI_Lower[i], tolerance = 0.001)
    expect_equal(result_data$CI_Upper[i], expected_values$CI_Upper[i], tolerance = 0.001)
    expect_equal(result_data$PI_Lower[i], expected_values$PI_Lower[i], tolerance = 0.001)
    expect_equal(result_data$PI_Upper[i], expected_values$PI_Upper[i], tolerance = 0.001)
    expect_equal(result_data$TI_Lower[i], expected_values$TI_Lower[i], tolerance = 0.001)
    expect_equal(result_data$TI_Upper[i], expected_values$TI_Upper[i], tolerance = 0.001)
  }

  # Check reducible uncertainty calculations
  for (i in 1:nrow(result_data)) {
    expected_reducible_upper <- ((result_data$TI_Upper[i] - result_data$PI_Upper[i]) /
                                   (result_data$TI_Upper[i] - result_data$Median[i])) * 100
    expected_reducible_lower <- ((result_data$TI_Lower[i] - result_data$PI_Lower[i]) /
                                   (result_data$TI_Lower[i] - result_data$Median[i])) * 100

    expect_equal(result_data$ReducibleUpper[i], round(expected_reducible_upper, 3))
    expect_equal(result_data$ReducibleLower[i], round(expected_reducible_lower, 3))
  }

  # Check summary string
  expected_summary_pattern <- paste0(
    "For parameters y ~ N\\(t1 , 1\\.7 \\* \\(v1\\)\\) and a ",
    "90% credibility interval, ",
    "90% prediction interval, ",
    "90% tolerance interval with ",
    "80% confidence of tolerance",
    ", the following results have been reached."
  )

  expect_match(result$summary, expected_summary_pattern)
})

test_that("perform_calculations handles Single Chain Dataset correctly", {
  # Create sample data
  data <- data.frame(
    iteration = 1:5,
    t1 = c(-0.78, -0.82, -0.75, -0.79, -0.77),
    v1 = c(0.89, 0.91, 0.88, 0.90, 0.87)
  )

  # Test with Single Chain Dataset
  result <- perform_calculations(
    data = data,
    fixed_effects = "t1",
    random_params = "v1",
    by = "Single Chain Dataset",
    tolerance_level = 90,
    multiplication_factor = 1.7,
    percent_for_ci = 90,
    percent_for_pi = 90,
    confidence_of_tolerance = 80
  )

  # Should have one row
  expect_equal(nrow(result$data), 1)

  # Check summary string includes Single Chain Dataset info
  expect_match(result$summary, "For parameters y ~ N\\(t1 , 1\\.7 \\* \\(v1\\)\\)")
})

test_that("perform_calculations handles errors correctly", {
  # Create sample data
  data <- data.frame(
    t1 = c(-0.82, -0.75),
    v1 = c(0.91, 0.88),
    By = c(1, 1)
  )

  # Test with invalid fixed_effects
  expect_error(
    perform_calculations(
      data = data,
      fixed_effects = "nonexistent",
      random_params = "v1",
      by = "By",
      tolerance_level = 90
    ),
    "Fixed effect columns not found in data"
  )

  # Test with invalid tolerance_level
  expect_error(
    perform_calculations(
      data = data,
      fixed_effects = "t1",
      random_params = "v1",
      by = "By",
      tolerance_level = 101
    ),
    "tolerance_level' must be a number between 0 and 100"
  )
})
