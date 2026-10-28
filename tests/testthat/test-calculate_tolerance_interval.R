test_that("calculate_tolerance_interval produces correct tolerance intervals", {
  # Create test data
  cat("\nTESTING BASIC FUNCTIONALITY\n")
  set.seed(123)  # For reproducibility
  test_data <- data.frame(
    Group = rep(c("A", "B"), each = 100),
    lower_quantile = c(rnorm(100, 5, 1), rnorm(100, 10, 2)),
    upper_quantile = c(rnorm(100, 15, 1), rnorm(100, 20, 2))
  )

  cat("\nTest data summary:\n")
  print(summary(test_data))
  cat("\nFirst few rows of test data:\n")
  print(head(test_data))

  # Run the function
  cat("\nCalculating tolerance intervals with 95% confidence level...\n")
  result <- calculate_tolerance_interval(test_data, "lower_quantile", "upper_quantile", 95, "Group")

  # Print full result
  cat("\nFull result from calculate_tolerance_interval:\n")
  print(result)

  # Basic structure tests
  cat("\nChecking result structure...\n")
  cat("Class:", class(result), "\n")
  cat("Dimensions:", dim(result), "\n")
  cat("Column names:", names(result), "\n")

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)  # Should have 2 groups
  expect_equal(ncol(result), 3)  # Should have 3 columns: By, TI_Lower, TI_Upper

  # Check column names
  expected_cols <- c("By", "TI_Lower", "TI_Upper")
  expect_true(all(expected_cols %in% names(result)))

  # Check values make sense
  cat("\nChecking interval values...\n")

  # For Group A
  group_a <- result[result$By == "A", ]
  cat("\nGroup A results:\n")
  print(group_a)

  # Calculate expected values for comparison
  group_a_data <- test_data[test_data$Group == "A", ]
  expected_lower_a <- as.numeric(quantile(group_a_data$lower_quantile, 0.025))  # Remove names
  expected_upper_a <- as.numeric(quantile(group_a_data$upper_quantile, 0.975))  # Remove names

  cat("Expected TI_Lower ~", expected_lower_a, ", got:", group_a$TI_Lower, "\n")
  cat("Expected TI_Upper ~", expected_upper_a, ", got:", group_a$TI_Upper, "\n")

  expect_equal(group_a$TI_Lower, expected_lower_a)
  expect_equal(group_a$TI_Upper, expected_upper_a)

  # For Group B
  group_b <- result[result$By == "B", ]
  cat("\nGroup B results:\n")
  print(group_b)

  # Calculate expected values for comparison
  group_b_data <- test_data[test_data$Group == "B", ]
  expected_lower_b <- as.numeric(quantile(group_b_data$lower_quantile, 0.025))  # Remove names
  expected_upper_b <- as.numeric(quantile(group_b_data$upper_quantile, 0.975))  # Remove names

  cat("Expected TI_Lower ~", expected_lower_b, ", got:", group_b$TI_Lower, "\n")
  cat("Expected TI_Upper ~", expected_upper_b, ", got:", group_b$TI_Upper, "\n")

  expect_equal(group_b$TI_Lower, expected_lower_b)
  expect_equal(group_b$TI_Upper, expected_upper_b)
})

test_that("calculate_tolerance_interval handles different confidence levels", {
  cat("\n\nTESTING DIFFERENT CONFIDENCE LEVELS\n")
  set.seed(123)
  test_data <- data.frame(
    Group = rep("A", 100),
    lower_quantile = rnorm(100, 5, 1),
    upper_quantile = rnorm(100, 15, 1)
  )

  # Calculate with different confidence levels
  result_95 <- calculate_tolerance_interval(test_data, "lower_quantile", "upper_quantile", 95, "Group")
  result_80 <- calculate_tolerance_interval(test_data, "lower_quantile", "upper_quantile", 80, "Group")

  cat("\n95% confidence tolerance intervals:\n")
  print(result_95)
  cat("\n80% confidence tolerance intervals:\n")
  print(result_80)

  # 95% confidence should give wider intervals than 80% confidence
  cat("\nInterval widths:\n")
  width_95 <- result_95$TI_Upper - result_95$TI_Lower
  width_80 <- result_80$TI_Upper - result_80$TI_Lower
  cat("95% confidence width:", width_95, "\n")
  cat("80% confidence width:", width_80, "\n")

  expect_true(width_95 > width_80)

  # Calculate expected values for comparison
  expected_lower_95 <- as.numeric(quantile(test_data$lower_quantile, 0.025))  # Remove names
  expected_upper_95 <- as.numeric(quantile(test_data$upper_quantile, 0.975))  # Remove names
  expected_lower_80 <- as.numeric(quantile(test_data$lower_quantile, 0.1))    # Remove names
  expected_upper_80 <- as.numeric(quantile(test_data$upper_quantile, 0.9))    # Remove names

  cat("\nExpected values:\n")
  cat("95% confidence: [", expected_lower_95, ",", expected_upper_95, "]\n")
  cat("80% confidence: [", expected_lower_80, ",", expected_upper_80, "]\n")

  expect_equal(result_95$TI_Lower, expected_lower_95)
  expect_equal(result_95$TI_Upper, expected_upper_95)
  expect_equal(result_80$TI_Lower, expected_lower_80)
  expect_equal(result_80$TI_Upper, expected_upper_80)
})

test_that("calculate_tolerance_interval handles data.table input", {
  cat("\n\nTESTING DATA.TABLE INPUT\n")
  set.seed(123)
  test_data <- data.table::data.table(
    Group = rep(c("A", "B"), each = 50),
    lower_quantile = c(rnorm(50, 5, 1), rnorm(50, 10, 2)),
    upper_quantile = c(rnorm(50, 15, 1), rnorm(50, 20, 2))
  )

  cat("\nTest data (data.table):\n")
  print(head(test_data))
  cat("Class:", class(test_data), "\n")

  # Run the function
  result <- calculate_tolerance_interval(test_data, "lower_quantile", "upper_quantile", 90, "Group")

  cat("\nResult with data.table input:\n")
  print(result)

  # Check structure
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  expect_equal(ncol(result), 3)

  # Check values for one group
  group_a <- result[result$By == "A", ]
  group_a_data <- test_data[Group == "A"]
  expected_lower_a <- as.numeric(quantile(group_a_data$lower_quantile, 0.05))  # Remove names
  expected_upper_a <- as.numeric(quantile(group_a_data$upper_quantile, 0.95))  # Remove names

  cat("\nGroup A expected vs. actual:\n")
  cat("Expected TI_Lower:", expected_lower_a, ", got:", group_a$TI_Lower, "\n")
  cat("Expected TI_Upper:", expected_upper_a, ", got:", group_a$TI_Upper, "\n")

  expect_equal(group_a$TI_Lower, expected_lower_a)
  expect_equal(group_a$TI_Upper, expected_upper_a)
})

test_that("calculate_tolerance_interval handles errors appropriately", {
  cat("\n\nTESTING ERROR HANDLING\n")
  test_data <- data.frame(
    Group = rep(c("A", "B"), each = 5),
    lower_quantile = 1:10,
    upper_quantile = 11:20
  )

  cat("\nTest data for error handling:\n")
  print(test_data)

  # Missing column
  cat("\nTesting with non-existent column...\n")
  tryCatch({
    calculate_tolerance_interval(test_data, "nonexistent_column", "upper_quantile", 95, "Group")
    cat("ERROR: Function did not throw an error as expected\n")
  }, error = function(e) {
    cat("Expected error caught:", e$message, "\n")
  })

  expect_error(
    calculate_tolerance_interval(test_data, "nonexistent_column", "upper_quantile", 95, "Group"),
    "One or more required columns not found in the dataframe"
  )

  # Missing grouping column
  cat("\nTesting with non-existent grouping column...\n")
  tryCatch({
    calculate_tolerance_interval(test_data, "lower_quantile", "upper_quantile", 95, "nonexistent_group")
    cat("ERROR: Function did not throw an error as expected\n")
  }, error = function(e) {
    cat("Expected error caught:", e$message, "\n")
  })

  expect_error(
    calculate_tolerance_interval(test_data, "lower_quantile", "upper_quantile", 95, "nonexistent_group"),
    "One or more required columns not found in the dataframe"
  )
})

test_that("calculate_tolerance_interval handles single group correctly", {
  cat("\n\nTESTING SINGLE GROUP CASE\n")
  set.seed(123)
  test_data <- data.frame(
    Group = rep("A", 100),
    lower_quantile = rnorm(100, 5, 1),
    upper_quantile = rnorm(100, 15, 1)
  )

  cat("\nTest data (single group):\n")
  print(head(test_data))

  # Run the function
  result <- calculate_tolerance_interval(test_data, "lower_quantile", "upper_quantile", 95, "Group")

  cat("\nResult with single group:\n")
  print(result)

  # Check structure
  expect_equal(nrow(result), 1)
  expect_equal(ncol(result), 3)

  # Check values
  expected_lower <- as.numeric(quantile(test_data$lower_quantile, 0.025))  # Remove names
  expected_upper <- as.numeric(quantile(test_data$upper_quantile, 0.975))  # Remove names

  cat("\nExpected vs. actual:\n")
  cat("Expected TI_Lower:", expected_lower, ", got:", result$TI_Lower, "\n")
  cat("Expected TI_Upper:", expected_upper, ", got:", result$TI_Upper, "\n")

  expect_equal(result$TI_Lower, expected_lower)
  expect_equal(result$TI_Upper, expected_upper)
})
