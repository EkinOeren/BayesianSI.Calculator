  test_that("load_n_inspect_data handles dataframes correctly", {
    # Create the test dataframe
    test_df <- data.frame(
      iteration = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
      mu = c(2.34, 2.41, 2.37, 2.39, 2.36, 2.42, 2.35, 2.40, 2.38, 2.43),
      t1 = c(-0.78, -0.82, -0.75, -0.79, -0.77, -0.81, -0.76, -0.80, -0.78, -0.83),
      t2 = c(1.56, 1.49, 1.52, 1.54, 1.51, 1.55, 1.53, 1.50, 1.57, 1.52),
      v1 = c(0.89, 0.91, 0.88, 0.90, 0.87, 0.92, 0.89, 0.91, 0.88, 0.90),
      v2 = c(1.12, 1.09, 1.14, 1.11, 1.13, 1.10, 1.15, 1.12, 1.11, 1.13),
      By = c(3, 1, 1, 1, 1, 1, 1, 1, 1, 2),
      Bytwo = c("apples", "oranges", "oranges", "oranges", "oranges",
                "oranges", "oranges", "oranges", "oranges", "oranges")
    )

    # Test direct data inspection
    result <- load_n_inspect_data(data = test_df)
    expect_equal(dim(result), dim(test_df))
    expect_equal(names(result), names(test_df))
  })

  test_that("load_n_inspect_data detects data issues", {
    # Create dataframe with issues
    problem_df <- data.frame(
      numeric_col = c(1, 2, 3, 4, 5),
      na_col = c(1, NA, 3, NA, 5),
      zero_col = c(0, 0, 0, 0, 0),
      stringsAsFactors = FALSE
    )

    # Add a mixed type column
    problem_df$mixed_col <- c("1", "2", "text", "4", "5")

    # Test NA detection
    expect_warning(load_n_inspect_data(data = problem_df),
                   "contain NA/NaN values")

    # Test zero column detection
    expect_warning(load_n_inspect_data(data = problem_df),
                   "contain only zero values")

    # Test mixed type detection
    expect_warning(load_n_inspect_data(data = problem_df),
                   "contain both numeric and string data")
  })

  test_that("load_n_inspect_data can load from file", {
    # Create a test dataframe
    test_df <- data.frame(
      x = 1:5,
      y = letters[1:5],
      stringsAsFactors = FALSE
    )

    # Save to temporary file
    temp_file <- tempfile(fileext = ".csv")
    write.csv(test_df, temp_file, row.names = FALSE)

    # Test loading
    result <- load_n_inspect_data(file_path = temp_file)
    expect_equal(nrow(result), nrow(test_df))
    expect_equal(ncol(result), ncol(test_df))

    # Clean up
    unlink(temp_file)
  })

  test_that("load_n_inspect_data handles missing file gracefully", {
    # Test with non-existent file
    expect_error(load_n_inspect_data(file_path = "non_existent_file.csv"),
                 "File does not exist")
  })


test_that("load_n_inspect_data reads a tab-separated file without an extension", {
  # self-contained: the file is written to the temporary directory
  path <- tempfile(pattern = "InputFileTester2")
  utils::write.table(data_InputFileTester2, path, sep = "\t", row.names = FALSE, quote = FALSE)
  on.exit(unlink(path), add = TRUE)

  result <- load_n_inspect_data(file_path = path)
  expect_true(is.data.frame(result))
  expect_equal(dim(result), dim(data_InputFileTester2))
  expect_equal(names(result), names(data_InputFileTester2))
})
