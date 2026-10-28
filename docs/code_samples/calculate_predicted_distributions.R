calculate_predicted_distributions <- function() {
  req(InputData(), input$fixed_var, input$random_var, input$by, input$tolerance_level)

  FixedEffects <- input$fixed_var
  RandomParams <- input$random_var

  # Check if "Single Chain Dataset" is selected
  is_single_chain <- identical(input$by, "Single Chain Dataset")
  By <- if(is_single_chain) "SingleChain" else input$by
  ToleranceLevel <- input$tolerance_level

  # If "Single Chain Dataset" is selected, create a SingleChain column if it doesn't exist
  if(By == "SingleChain" && !"SingleChain" %in% names(InputData())) {
    temp_data <- InputData()
    temp_data$SingleChain <- 1
    InputData(temp_data)
  }

  # Create a copy of the data to avoid modifying the original
  dt <- copy(as.data.table(InputData()))

  # Draws must be numeric and complete; NA or infinite draws would otherwise be
  # silently treated as zero. Errors are shown to the user through ErrorMessage.
  check_positive(MultiplicationFactorVariances(), "Multiplication factor")
  check_draw_columns(dt, FixedEffects, "Fixed effect")
  check_draw_columns(dt, RandomParams, "Variance component")
  for (k in 1:3) {
    cov_col_k <- input[[paste0("covariate_", k)]]
    if (!is.null(cov_col_k) && cov_col_k != "None" && cov_col_k %in% names(dt)) {
      check_draw_columns(dt, cov_col_k, "Covariate")
    }
  }
  if (anyNA(dt[[By]])) stop(sprintf("By column '%s' contains missing values", By), call. = FALSE)

  # Print column types for debugging
  for(col in c(FixedEffects, RandomParams)) {
  }

  # Initialize mean vector with zeros
  mean_vector <- rep(0, nrow(dt))

  # First, add covariate effects to mean
  for(i in 1:3) {
    covariate_col <- input[[paste0("covariate_", i)]]
    covariate_val <- input[[paste0("covariate_value_", i)]]

    # Only process if a covariate column is selected
    if(!is.null(covariate_col) && covariate_col != "None" && covariate_col %in% names(dt)) {
      # Get the column values
      col_vector <- as.numeric(dt[[covariate_col]])

      # Use covariate value (defaults to 1 if NULL or NA)
      multiplier <- if(is.null(covariate_val) || is.na(covariate_val)) 1 else covariate_val

      # Add covariate effect to mean
      mean_vector <- mean_vector + (col_vector * multiplier)

      print(paste("Sample values after adding covariate", i, ":",
                  paste(head(mean_vector, 3), collapse=", ")))
    }
  }

  # Then add fixed effects
  if(length(FixedEffects) > 0) {
    fixed_matrix <- matrix(
      as.numeric(unlist(dt[, ..FixedEffects])),
      nrow = nrow(dt),
      ncol = length(FixedEffects)
    )

    # Add fixed effects to mean
    mean_vector <- mean_vector + rowSums(fixed_matrix, na.rm = TRUE)

    print(paste("Sample values after adding fixed effects:",
                paste(head(mean_vector, 3), collapse=", ")))
  }

  # Calculate variance
  if(length(RandomParams) > 0) {
    random_matrix <- matrix(
      as.numeric(unlist(dt[, ..RandomParams])),
      nrow = nrow(dt),
      ncol = length(RandomParams)
    )

    # Calculate total variance
    variance_vector <- rowSums(random_matrix, na.rm = TRUE) * MultiplicationFactorVariances()
    check_variance(variance_vector)
  } else {
    variance_vector <- rep(0, nrow(dt))
  }

  # Print matrix types for debugging

  # Calculate tolerance parameters
  toleranceaplha <- ToleranceLevel / 100
  toleranceslice <- (1 - toleranceaplha) / 2

  # Calculate quantiles using the mean and variance
  lower_quantiles <- qnorm(toleranceslice, mean = mean_vector, sd = sqrt(pmax(variance_vector, 1e-10)))
  upper_quantiles <- qnorm(toleranceaplha + toleranceslice, mean = mean_vector, sd = sqrt(pmax(variance_vector, 1e-10)))
  medians <- qnorm(0.5, mean = mean_vector, sd = sqrt(pmax(variance_vector, 1e-10)))

  # Create result data.table
  PredictedDistributions_dt <- data.table(
    By = dt[[By]],
    mean = mean_vector,
    median = medians,
    lower_quantile = lower_quantiles,
    upper_quantile = upper_quantiles,
    variance = variance_vector
  )

  # Round to 3 decimals. The inputs are validated to be finite, so every value is
  # a finite number and the columns stay numeric.
  for (col in c("mean", "median", "lower_quantile", "upper_quantile", "variance")) {
    if (any(!is.finite(PredictedDistributions_dt[[col]]))) {
      stop(sprintf("Non-finite values occurred in '%s'; check the scale of the inputs", col),
           call. = FALSE)
    }
    PredictedDistributions_dt[[col]] <- round(PredictedDistributions_dt[[col]], 3)
  }

  # Sort by the "By" column
  setorder(PredictedDistributions_dt, By)

  return(as.data.frame(PredictedDistributions_dt))
}
