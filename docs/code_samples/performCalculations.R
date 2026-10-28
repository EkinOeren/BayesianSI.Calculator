performCalculations <- function(switch_tab = FALSE) {
  tryCatch({
    # Calculate predictedDistributions
    incProgress(0.2)
    pred_dist <- calculate_predicted_distributions()
    predictedDistributions(pred_dist)

    # Calculate CI
    incProgress(0.2)
    ci_result <- calculate_ci(pred_dist, "median", input$percent_for_ci, "By")

    # Calculate TI
    incProgress(0.2)
    ti_result <- calculate_tolerance_interval(
      pred_dist,
      "lower_quantile",
      "upper_quantile",
      input$confidence_of_tolerance,
      "By"
    )

    # Calculate PI
    incProgress(0.2)
    pi_result <- calculate_PI(
      data = pred_dist,
      percent_for_pi = input$percent_for_pi
    )

    # check column names in PI result for debugging

    # Merge results
    incProgress(0.2)
    combined_result <- merge(merge(ci_result, ti_result, by = "By"), pi_result, by = "By")

    # Failsafe check
    if (any(table(combined_result$By) > 1)) {
      check_consistency <- combined_result %>%
        group_by(By) %>%
        summarise(across(everything(), function(x) length(unique(head(x, 5))) == 1)) %>%
        ungroup()

      if (!all(sapply(check_consistency[,-1], all))) {
        warning("Inconsistent values detected for some 'By' groups. Using first row for each group.")
      }

      # Ensure only one row per unique "By" value
      combined_result <- combined_result %>%
        group_by(By) %>%
        summarise(across(everything(), first)) %>%
        ungroup()
    }

    # Add reducible imprecision as percentages (with a notification if it is not
    # defined for a group). With log-normal data it is recalculated afterwards.
    combined_result <- withCallingHandlers(
      add_reducible_imprecision(combined_result, warn = !isTRUE(input$log_normal), percent = TRUE),
      warning = function(w) {
        showNotification(conditionMessage(w), type = "warning")
        invokeRestart("muffleWarning")
      })

    # Store the combined result
    IntervalsOutputInitial(combined_result)
    CICalculated(TRUE)

    # Trigger lognorm_adjuster
    lognorm_adjuster()

    # Only switch to the Intervals tab if switch_tab is TRUE
    if (switch_tab) {
      updateTabsetPanel(session, "main_tabs", selected = "One-Sided Intervals")
    }

  }, error = function(e) {
    ErrorMessage(paste("Error in calculating intervals:", e$message))
  })

  if (!is.null(IntervalsOutputActual())) {
    # Calculate plots once and cache them
    interval_plots_cache(render_intervals_plot_one_sided(IntervalsOutputActual(), input))
  }
}
