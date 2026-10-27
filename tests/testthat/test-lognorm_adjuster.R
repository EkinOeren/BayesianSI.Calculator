test_that("lognorm_adjuster works correctly with log_normal = TRUE", {
  # Create test data
  test_data <- data.frame(
    Median = c(0, 1, 2),
    CI_Lower = c(-0.5, 0.5, 1.5),
    CI_Upper = c(0.5, 1.5, 2.5),
    TI_Lower = c(-1, 0, 1),
    TI_Upper = c(1, 2, 3),
    PI_Lower = c(-0.8, 0.2, 1.2),
    PI_Upper = c(0.8, 1.8, 2.8)
  )

  # Run function with log_normal = TRUE
  result <- lognorm_adjuster(test_data, log_normal = TRUE)

  # Check that exponentiation happened
  expect_equal(result$Median[2], exp(1))
  expect_equal(result$CI_Lower[2], exp(0.5))
  expect_equal(result$TI_Upper[2], exp(2))

  # Check reducible uncertainty calculations
  # For row 2:
  # ReducibleUpper = ((exp(2) - exp(1.8))/(exp(2) - exp(1))) * 100
  expected_upper <- ((exp(2) - exp(1.8))/(exp(2) - exp(1))) * 100
  expect_equal(result$ReducibleUpper[2], round(expected_upper, 3))

  # ReducibleLower = ((exp(0) - exp(0.2))/(exp(0) - exp(1))) * 100
  expected_lower <- ((exp(0) - exp(0.2))/(exp(0) - exp(1))) * 100
  expect_equal(result$ReducibleLower[2], round(expected_lower, 3))
})

test_that("lognorm_adjuster works correctly with log_normal = FALSE", {
  # Create test data
  test_data <- data.frame(
    Median = c(1, 2, 3),
    CI_Lower = c(0.5, 1.5, 2.5),
    CI_Upper = c(1.5, 2.5, 3.5),
    TI_Lower = c(0.3, 1.3, 2.3),
    TI_Upper = c(1.7, 2.7, 3.7),
    PI_Lower = c(0.4, 1.4, 2.4),
    PI_Upper = c(1.6, 2.6, 3.6)
  )

  # Run function with log_normal = FALSE
  result <- lognorm_adjuster(test_data, log_normal = FALSE)

  # Check that values remain unchanged (no exponentiation)
  expect_equal(result$Median, test_data$Median)
  expect_equal(result$CI_Lower, test_data$CI_Lower)
  expect_equal(result$TI_Upper, test_data$TI_Upper)

  # Check reducible uncertainty calculations for row 1
  # ReducibleUpper = ((1.7 - 1.6)/(1.7 - 1)) * 100 = 14.286%
  expect_equal(result$ReducibleUpper[1], round(((1.7 - 1.6)/(1.7 - 1)) * 100, 3))

  # ReducibleLower = ((0.3 - 0.4)/(0.3 - 1)) * 100 = 14.286%
  expect_equal(result$ReducibleLower[1], round(((0.3 - 0.4)/(0.3 - 1)) * 100, 3))
})

test_that("lognorm_adjuster handles missing input correctly", {
  expect_error(lognorm_adjuster(), "Input data is required")
})

test_that("lognorm_adjuster handles edge cases", {
  # Test with a single row
  single_row <- data.frame(
    Median = 1,
    CI_Lower = 0.5,
    CI_Upper = 1.5,
    TI_Lower = 0.3,
    TI_Upper = 1.7,
    PI_Lower = 0.4,
    PI_Upper = 1.6
  )

  result <- lognorm_adjuster(single_row, log_normal = FALSE)
  expect_equal(nrow(result), 1)
  expect_equal(result$ReducibleUpper, round(((1.7 - 1.6)/(1.7 - 1)) * 100, 3))

  # Test with zero in denominator case
  zero_denom <- data.frame(
    Median = 1,
    CI_Lower = 0.5,
    CI_Upper = 1.5,
    TI_Lower = 0.3,
    TI_Upper = 1, # This will cause division by zero in ReducibleUpper
    PI_Lower = 0.4,
    PI_Upper = 0.9
  )

  # The ratio is not defined here (the tolerance bound equals the median): the
  # function returns NA and says so, instead of silently returning Inf or NaN
  expect_warning(result <- lognorm_adjuster(zero_denom, log_normal = FALSE),
                 "not defined for 1 group")
  expect_true(is.na(result$ReducibleUpper))
  expect_false(is.nan(result$ReducibleUpper))
  expect_false(is.infinite(result$ReducibleUpper))
})
