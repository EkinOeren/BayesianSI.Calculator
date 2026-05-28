#' @importFrom plotly plot_ly add_ribbons add_segments add_markers layout
#'
#' @title Create Interactive Statistical Intervals Plot
#'
#' @description Creates an interactive plot showing Confidence Intervals (CI), Prediction Intervals (PI),
#' and Tolerance Intervals (TI) using plotly. The plot matches the style and functionality
#' of the original Shiny app and includes parameter information.
#'
#' @param result A list output from perform_calculations() containing data and summary
#' @param show_reducible Logical, whether to show reducible uncertainty ribbons (default = FALSE)
#' @param x_axis_title Character, custom title for x-axis (optional)
#' @param show_summary Logical, whether to print the summary in the console (default = FALSE)
#'
#' @return A plotly object displaying the statistical intervals
#'
#' @examples
#' \dontrun{
#' data <- data.frame(
#'   t1 = c(-0.82, -0.75, -0.79),
#'   v1 = c(0.91, 0.88, 0.90),
#'   By = c(1, 1, 2)
#' )
#' calc_result <- perform_calculations(
#'   data = data,
#'   fixed_effects = "t1",
#'   random_params = "v1",
#'   by = "By",
#'   tolerance_level = 90
#' )
#' plot_intervals(calc_result)
#' plot_intervals(calc_result, show_summary = TRUE)
#' }
#'
#' @export
plot_intervals <- function(result, show_reducible = FALSE, x_axis_title = NULL,
                           show_summary = FALSE) {
  # Extract data and summary from result
  if (is.list(result) && "data" %in% names(result) && "summary" %in% names(result)) {
    data <- result$data
    summary_text <- result$summary
  } else {
    # For backward compatibility
    data <- result
    summary_text <- NULL
  }

  # Check if PI and TI percentages are the same when show_reducible is TRUE
  if (show_reducible && !is.null(summary_text)) {
    # Extract PI and TI percentages from summary text
    pi_match <- regexpr("([0-9]+)% prediction interval", summary_text)
    ti_match <- regexpr("([0-9]+)% tolerance interval", summary_text)

    if (pi_match > 0 && ti_match > 0) {
      pi_percent <- as.numeric(regmatches(summary_text,
                                          regexec("([0-9]+)% prediction interval", summary_text))[[1]][2])
      ti_percent <- as.numeric(regmatches(summary_text,
                                          regexec("([0-9]+)% tolerance interval", summary_text))[[1]][2])

      if (pi_percent != ti_percent) {
        stop("Reducible Uncertainty cannot be calculated between prediction and tolerance intervals of different quantiles. Please make sure the %PI and %TI you have requested in the previous function \"bayesian_statistical_intervals()\" match")
      }
    }
  }

  # Print summary in console if requested
  if (show_summary && !is.null(summary_text)) {
    cat("Summary:\n", summary_text, "\n")
  }

  # Input validation
  required_cols <- c("By", "CI_Lower", "CI_Upper", "TI_Lower", "TI_Upper",
                     "PI_Lower", "PI_Upper", "Median")
  missing_cols <- setdiff(required_cols, names(data))
  if(length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse=", ")))
  }

  # Convert By to factor and get number of unique values
  data$By <- factor(data$By)
  n_by <- length(unique(data$By))

  # Determine if this is a single chain situation
  is_single_chain <- n_by == 1 && (as.character(data$By[1]) == "1" ||
                                     as.character(data$By[1]) == "SingleChain")

  # Set x-axis title
  if (is.null(x_axis_title)) {
    x_axis_title <- if (is_single_chain) {
      "By Identifier"
    } else {
      "By"
    }
  }

  # Calculate adaptive scaling for visual elements
  base_cap_width <- 0.05
  base_offset <- 0.18

  cap_scaling <- 5 / (n_by + 4)
  cap_width <- max(0.02, min(0.1, cap_width <- base_cap_width * cap_scaling))

  offset_scaling <- 4 / (n_by + 3)
  offset_scale <- max(0.08, min(0.2, offset_scale <- base_offset * offset_scaling))

  # Initialize plot
  p <- plot_ly()

  # Add reducible uncertainty ribbons if enabled
  if (show_reducible) {
    for (i in 1:nrow(data)) {
      by_val <- as.numeric(data$By[i])
      by_label <- as.character(data$By[i])
      ribbon_width <- 0.15

      # Convert decimal values to percentages for display
      reducible_upper_percent <- data$ReducibleUpper[i] * 100
      reducible_lower_percent <- data$ReducibleLower[i] * 100

      # Add upper reducible uncertainty ribbon
      p <- add_ribbons(p,
                       x = c(by_val - ribbon_width, by_val + ribbon_width),
                       ymin = rep(data$PI_Upper[i], 2),
                       ymax = rep(data$TI_Upper[i], 2),
                       fillcolor = 'rgba(255, 165, 0, 0.6)',
                       line = list(color = 'rgba(255, 165, 0, 1)', width = 1),
                       name = "Reducible Imprecision",
                       showlegend = (i == 1),
                       hoverinfo = "text",
                       text = sprintf("Reducible Imprecision<br>By: %s<br>Value: %.2f%%<br>Range: %.4f to %.4f",
                                      by_label, reducible_upper_percent, data$PI_Upper[i], data$TI_Upper[i])
      )

      # Add lower reducible uncertainty ribbon
      p <- add_ribbons(p,
                       x = c(by_val - ribbon_width, by_val + ribbon_width),
                       ymin = rep(data$TI_Lower[i], 2),
                       ymax = rep(data$PI_Lower[i], 2),
                       fillcolor = 'rgba(255, 165, 0, 0.6)',
                       line = list(color = 'rgba(255, 165, 0, 1)', width = 1),
                       name = "Reducible Imprecision",
                       showlegend = FALSE,
                       hoverinfo = "text",
                       text = sprintf("Reducible Imprecision<br>By: %s<br>Value: %.2f%%<br>Range: %.4f to %.4f",
                                      by_label, reducible_lower_percent, data$TI_Lower[i], data$PI_Lower[i])
      )
    }
  }

  # Add intervals for each By value
  for (i in 1:nrow(data)) {
    by_val <- as.numeric(data$By[i])
    by_label <- as.character(data$By[i])

    # Add CI intervals (red)
    p <- add_segments(p,
                      x = by_val - offset_scale, y = data$CI_Lower[i],
                      xend = by_val - offset_scale, yend = data$CI_Upper[i],
                      line = list(color = '#de2f2f', width = 1),
                      showlegend = i == 1,
                      name = "CI",
                      hoverinfo = "text",
                      text = sprintf("CI<br>By: %s<br>Lower: %.4f<br>Upper: %.4f<br>Median: %.4f",
                                     by_label, data$CI_Lower[i], data$CI_Upper[i], data$Median[i])
    ) %>%
      # Add CI caps
      add_segments(
        x = by_val - offset_scale - cap_width, y = data$CI_Lower[i],
        xend = by_val - offset_scale + cap_width, yend = data$CI_Lower[i],
        line = list(color = '#de2f2f', width = 1.5),
        showlegend = FALSE
      ) %>%
      add_segments(
        x = by_val - offset_scale - cap_width, y = data$CI_Upper[i],
        xend = by_val - offset_scale + cap_width, yend = data$CI_Upper[i],
        line = list(color = '#de2f2f', width = 1.5),
        showlegend = FALSE
      ) %>%
      # Add CI median point
      add_markers(
        x = by_val - offset_scale, y = data$Median[i],
        marker = list(color = '#de2f2f', size = 3.5),
        showlegend = FALSE
      )

    # Add PI intervals (green)
    p <- add_segments(p,
                      x = by_val, y = data$PI_Lower[i],
                      xend = by_val, yend = data$PI_Upper[i],
                      line = list(color = '#006400', width = 1),
                      showlegend = i == 1,
                      name = "PI",
                      hoverinfo = "text",
                      text = sprintf("PI<br>By: %s<br>Lower: %.4f<br>Upper: %.4f<br>Median: %.4f",
                                     by_label, data$PI_Lower[i], data$PI_Upper[i], data$Median[i])
    ) %>%
      # Add PI caps
      add_segments(
        x = by_val - cap_width, y = data$PI_Lower[i],
        xend = by_val + cap_width, yend = data$PI_Lower[i],
        line = list(color = '#006400', width = 1.5),
        showlegend = FALSE
      ) %>%
      add_segments(
        x = by_val - cap_width, y = data$PI_Upper[i],
        xend = by_val + cap_width, yend = data$PI_Upper[i],
        line = list(color = '#006400', width = 1.5),
        showlegend = FALSE
      ) %>%
      # Add PI median point
      add_markers(
        x = by_val, y = data$Median[i],
        marker = list(color = '#006400', size = 3.5),
        showlegend = FALSE
      )

    # Add TI intervals (blue)
    p <- add_segments(p,
                      x = by_val + offset_scale, y = data$TI_Lower[i],
                      xend = by_val + offset_scale, yend = data$TI_Upper[i],
                      line = list(color = '#4232e7', width = 1),
                      showlegend = i == 1,
                      name = "TI",
                      hoverinfo = "text",
                      text = sprintf("TI<br>By: %s<br>Lower: %.4f<br>Upper: %.4f<br>Median: %.4f",
                                     by_label, data$TI_Lower[i], data$TI_Upper[i], data$Median[i])
    ) %>%
      # Add TI caps
      add_segments(
        x = by_val + offset_scale - cap_width, y = data$TI_Lower[i],
        xend = by_val + offset_scale + cap_width, yend = data$TI_Lower[i],
        line = list(color = '#4232e7', width = 1.5),
        showlegend = FALSE
      ) %>%
      add_segments(
        x = by_val + offset_scale - cap_width, y = data$TI_Upper[i],
        xend = by_val + offset_scale + cap_width, yend = data$TI_Upper[i],
        line = list(color = '#4232e7', width = 1.5),
        showlegend = FALSE
      ) %>%
      # Add TI median point
      add_markers(
        x = by_val + offset_scale, y = data$Median[i],
        marker = list(color = '#4232e7', size = 3.5),
        showlegend = FALSE
      )
  }

  # Calculate range padding based on number of points
  range_padding <- max(0.8, min(2.5, 6/n_by))

  # For single chain, replace the "1" with an empty string
  tick_text <- levels(data$By)
  if (is_single_chain) {
    tick_text <- ""
  }

  # Create layout
  p <- layout(p,
              title = "Bayesian Statistical Intervals",
              xaxis = list(
                title = x_axis_title,
                tickmode = "array",
                tickvals = 1:length(levels(data$By)),
                ticktext = tick_text,
                tickangle = 45,
                range = c(1 - range_padding, length(levels(data$By)) + range_padding)
              ),
              yaxis = list(title = "Value"),
              legend = list(x = 1.05, y = 0.5),
              margin = list(b = 100)
  )

  return(p)
}
