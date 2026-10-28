ForCDF1Dataframe <- reactive({
  req(predictedDistributions(), input$plot_distribution)
  req(!reactivity_paused())

  by_values <- unique(predictedDistributions()$By)

  result <- lapply(by_values, function(by_value) {
    if (isTRUE(input[[paste0("show_", by_value)]])) {
      data <- predictedDistributions()[predictedDistributions()$By == by_value, ]
      n_points <- input[[paste0("points_", by_value)]]

      # Validate number of points
      if (is.na(n_points) || !is.numeric(n_points) || n_points < 3 || n_points > 300) {
        return(NULL)
      }

      ci <- input[[paste0("ci_", by_value)]] / 100

      # Use mean of means for centering
      overall_mean <- mean(data$mean)
      overall_sd <- sqrt(mean(data$variance))

      # Generate x values
      xmin_log <- qnorm(0.0001, mean = overall_mean, sd = overall_sd)
      xmax_log <- qnorm(0.9999, mean = overall_mean, sd = overall_sd)
      x_values_log <- seq(xmin_log, xmax_log, length.out = n_points)

      # Transform x values for display if log_normal is TRUE
      x_values_display <- if(input$log_normal) exp(x_values_log) else x_values_log
      xmin_display <- if(input$log_normal) exp(xmin_log) else xmin_log
      xmax_display <- if(input$log_normal) exp(xmax_log) else xmax_log

      # Calculate CDF for each posterior sample at each x-value
      y_values <- do.call(rbind, lapply(1:nrow(data), function(i) {
        pnorm(x_values_log, mean = data$mean[i], sd = sqrt(data$variance[i]))
      }))

      # KEY CHANGE: Use median instead of mean for central tendency
      y_median <- apply(y_values, 2, median)

      # Calculate credibility intervals
      if (nrow(y_values) > 1) {
        alpha <- 1 - ci
        # Use standard quantile method (with interpolation)
        empirical_quantiles <- apply(y_values, 2, function(col) {
          quantile(col, probs = c(alpha/2, 1-alpha/2), na.rm = TRUE)
        })
        ci_lower <- empirical_quantiles[1,]
        ci_upper <- empirical_quantiles[2,]
      } else {
        # Only one sample - no uncertainty
        ci_lower <- y_median
        ci_upper <- y_median
      }

      # Create result data frame
      result <- data.frame(
        By = by_value,
        x = x_values_display,
        x_log = x_values_log,
        y_mean = y_median,  # Note: Keep column name for compatibility
        ci_lower = ci_lower,
        ci_upper = ci_upper,
        xmin = xmin_display,
        xmax = xmax_display,
        xmin_log = xmin_log,
        xmax_log = xmax_log
      )

      result
    }
  })

  result <- result[!sapply(result, is.null)]
  if (length(result) == 0) {
    return(NULL)
  }

  result
})
