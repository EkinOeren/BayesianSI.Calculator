test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that("calculate_ci produces correct credibility intervals", {
  # Create test data
  set.seed(123)  # For reproducibility
  test_data <- data.frame(
    Group = rep(c("A", "B"), each = 100),
    Value = c(rnorm(100, 10, 2), rnorm(100, 15, 3))
  )

  # Print test data summary
  cat("\nTest data summary:\n")
  print(summary(test_data))
  cat("\nFirst few rows of test data:\n")
  print(head(test_data))

  # Run the function
  cat("\nCalculating credibility intervals with 95% level...\n")
  result <- calculate_ci(test_data, "Value", 95, "Group")

  # Print full result
  cat("\nFull result from calculate_ci:\n")
  print(result)

  # Basic structure tests
  cat("\nChecking result structure...\n")
  cat("Class:", class(result), "\n")
  cat("Dimensions:", dim(result), "\n")
  cat("Column names:", names(result), "\n")

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)  # Should have 2 groups
  expect_equal(ncol(result), 5)  # Should have 5 columns

  # Check column names - UPDATED to use "By" instead of "Group"
  expected_cols <- c("By", "N", "CI_Lower", "CI_Upper", "Median")
  expect_true(all(expected_cols %in% names(result)))

  # Check N values
  cat("\nChecking N values...\n")
  print(result$N)
  expect_equal(result$N, c(100, 100))

  # Check that CI_Lower < Median < CI_Upper for each group
  cat("\nChecking interval relationships...\n")
  cat("CI_Lower < Median:", all(result$CI_Lower < result$Median), "\n")
  cat("Median < CI_Upper:", all(result$Median < result$CI_Upper), "\n")

  expect_true(all(result$CI_Lower < result$Median))
  expect_true(all(result$Median < result$CI_Upper))

  # Check approximate values (with tolerance)
  cat("\nChecking approximate values...\n")

  # For Group A (mean=10, sd=2) - UPDATED to use "By" instead of "Group"
  group_a <- result[result$By == "A", ]
  cat("\nGroup A results:\n")
  print(group_a)
  cat("Expected median ~10, got:", group_a$Median, "\n")
  cat("Expected CI_Lower ~", 10 - 1.96*2/sqrt(100), ", got:", group_a$CI_Lower, "\n")
  cat("Expected CI_Upper ~", 10 + 1.96*2/sqrt(100), ", got:", group_a$CI_Upper, "\n")

  expect_equal(group_a$Median, 10, tolerance = 0.5)
  expect_equal(group_a$CI_Lower, 10 - 1.96*2/sqrt(100), tolerance = 0.5)
  expect_equal(group_a$CI_Upper, 10 + 1.96*2/sqrt(100), tolerance = 0.5)

  # For Group B (mean=15, sd=3) - UPDATED to use "By" instead of "Group"
  group_b <- result[result$By == "B", ]
  cat("\nGroup B results:\n")
  print(group_b)
  cat("Expected median ~15, got:", group_b$Median, "\n")
  cat("Expected CI_Lower ~", 15 - 1.96*3/sqrt(100), ", got:", group_b$CI_Lower, "\n")
  cat("Expected CI_Upper ~", 15 + 1.96*3/sqrt(100), ", got:", group_b$CI_Upper, "\n")

  expect_equal(group_b$Median, 15, tolerance = 0.5)
  expect_equal(group_b$CI_Lower, 15 - 1.96*3/sqrt(100), tolerance = 0.5)
  expect_equal(group_b$CI_Upper, 15 + 1.96*3/sqrt(100), tolerance = 0.5)
})

test_that("calculate_ci handles edge cases", {
  # Single group
  cat("\n\nTESTING EDGE CASES\n")
  cat("\nTesting with single group...\n")
  test_data <- data.frame(
    Group = rep("A", 10),
    Value = 1:10
  )
  print(head(test_data))

  result <- calculate_ci(test_data, "Value", 90, "Group")
  cat("Result with single group:\n")
  print(result)
  expect_equal(nrow(result), 1)

  # Already a data.table
  cat("\nTesting with data.table input...\n")
  test_data <- data.table::data.table(
    Group = rep(c("A", "B"), each = 5),
    Value = 1:10
  )
  print(head(test_data))
  cat("Class of input:", class(test_data), "\n")

  result <- calculate_ci(test_data, "Value", 80, "Group")
  cat("Result with data.table input:\n")
  print(result)
  expect_equal(nrow(result), 2)

  # Different percentage
  cat("\nComparing different credibility levels...\n")
  result_95 <- calculate_ci(test_data, "Value", 95, "Group")
  result_80 <- calculate_ci(test_data, "Value", 80, "Group")

  cat("95% credibility intervals:\n")
  print(result_95)
  cat("80% credibility intervals:\n")
  print(result_80)

  cat("Width of 95% intervals:", result_95$CI_Upper - result_95$CI_Lower, "\n")
  cat("Width of 80% intervals:", result_80$CI_Upper - result_80$CI_Lower, "\n")

  # 95% CI should be wider than 80% CI
  expect_true(all(result_95$CI_Upper - result_95$CI_Lower >
                    result_80$CI_Upper - result_80$CI_Lower))
})

test_that("calculate_ci throws appropriate errors", {
  cat("\n\nTESTING ERROR HANDLING\n")
  test_data <- data.frame(
    Group = rep(c("A", "B"), each = 5),
    Value = 1:10
  )
  cat("Test data for error handling:\n")
  print(test_data)

  # Column not found
  cat("\nTesting with non-existent column...\n")
  tryCatch({
    calculate_ci(test_data, "NonExistentColumn", 95, "Group")
    cat("ERROR: Function did not throw an error as expected\n")
  }, error = function(e) {
    cat("Expected error caught:", e$message, "\n")
  })

  expect_error(calculate_ci(test_data, "NonExistentColumn", 95, "Group"),
               "Column NonExistentColumn or Group not found in the dataframe")

  cat("\nTesting with non-existent grouping column...\n")
  tryCatch({
    calculate_ci(test_data, "Value", 95, "NonExistentGroup")
    cat("ERROR: Function did not throw an error as expected\n")
  }, error = function(e) {
    cat("Expected error caught:", e$message, "\n")
  })

  expect_error(calculate_ci(test_data, "Value", 95, "NonExistentGroup"),
               "Column Value or NonExistentGroup not found in the dataframe")
})
