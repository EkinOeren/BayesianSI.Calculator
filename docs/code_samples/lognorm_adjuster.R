lognorm_adjuster <- function() {
  req(IntervalsOutputInitial())
  data <- IntervalsOutputInitial()

  # Handle the Krishnamoorthy TI values if they exist
  if (!is.null(originalOptimalPointResults())) {
    tryCatch({
      # Get the original optimal points
      original_points <- originalOptimalPointResults()

      # Create a modified copy based on log_normal status
      modified_points <- lapply(original_points, function(point) {
        if (!is.null(point$g1) && !is.null(point$g2)) {
          result <- point
          if (input$log_normal) {
            # Apply exp transformation
            result$g1 <- exp(point$g1)
            result$g2 <- exp(point$g2)
          } else {
            # Use original values
            result$g1 <- point$g1
            result$g2 <- point$g2
          }
          return(result)
        } else {
          return(point)
        }
      })

      # Update the reactive value with the modified copy
      optimalPointResults(modified_points)

    }, error = function(e) {
      # Log the error but continue
    })
  }

  # Transform the main data
  if (input$log_normal) {
    # If log_normal is checked, transform the values using exp()
    data <- data %>%
      mutate(across(c(Median, CI_Lower, CI_Upper, TI_Lower, TI_Upper, PI_Lower, PI_Upper), exp))
  }

  # Add reducible imprecision as percentages (notification if not defined)
  data <- withCallingHandlers(
    add_reducible_imprecision(data, percent = TRUE),
    warning = function(w) {
      showNotification(conditionMessage(w), type = "warning")
      invokeRestart("muffleWarning")
    })

  # Update the reactive values
  IntervalsOutputActual(data)
  plot_data(data)

  # Force plot update - this is the key addition
  interval_plots_cache(render_intervals_plot_one_sided(IntervalsOutputActual(), input))

}
