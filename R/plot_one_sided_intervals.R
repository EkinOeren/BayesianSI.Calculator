#' Plot One-Sided Statistical Intervals
#'
#' Creates interactive plots showing one-sided statistical intervals (CI, PI, TI)
#' with separate plots for upper and lower bounds.
#'
#' @param data A data frame containing interval data with columns:
#'        By, Median, CI_Lower, CI_Upper, PI_Lower, PI_Upper, TI_Lower, TI_Upper
#' @param show_reducible Logical, whether to show reducible uncertainty ribbons (default = FALSE)
#' @param log_normal Logical, whether values are in log-normal scale (default = FALSE)
#' @param selected_by_values Optional vector of specific by values to include in the plot
#'
#' @return A list containing two plotly objects:
#' \itemize{
#'   \item upper: Plot for upper bounds
#'   \item lower: Plot for lower bounds
#' }
#'
#' @examples
#' \dontrun{
#' # First calculate intervals
#' result <- bayesian_statistical_intervals(
#'   data = mcmc_samples,
#'   fixed_effects = "beta0",
#'   random_params = "sigma2",
#'   by = "Group"
#' )
#'
#' # Then plot one-sided intervals
#' plots <- plot_one_sided_intervals(result$data)
#'
#' # Display upper and lower plots
#' plots$upper
#' plots$lower
#' }
#'
#' @importFrom plotly plot_ly add_trace add_ribbons layout
#' @export
plot_one_sided_intervals <- function(data, show_reducible = FALSE,
                                     log_normal = FALSE, selected_by_values = NULL) {
  # Input validation
  required_cols <- c("By", "Median", "CI_Lower", "CI_Upper",
                     "TI_Lower", "TI_Upper", "PI_Lower", "PI_Upper")
  if (!all(required_cols %in% names(data))) {
    stop("Data must contain all required interval columns")
  }

  # Filter By values if specified
  if (!is.null(selected_by_values)) {
    data <- data[data$By %in% selected_by_values, ]
    if (nrow(data) == 0) {
      stop("No data remaining after filtering By values")
    }
  }

  # Convert By to factor and get number of unique values
  data$By <- factor(data$By)
  n_by <- length(unique(data$By))

  # Check if this is a single chain situation
  is_single_chain <- n_by == 1 && (as.character(data$By[1]) == "1" ||
                                     as.character(data$By[1]) == "SingleChain")

  # Determine x-axis title
  x_axis_title <- if (is_single_chain) {
    "By Identifier"
  } else {
    "By"
  }

  # Calculate plot range for extending to infinity
  all_values <- c(data$CI_Lower, data$CI_Upper, data$PI_Lower,
                  data$PI_Upper, data$TI_Lower, data$TI_Upper)
  y_range <- range(all_values, na.rm = TRUE)
  y_extend <- diff(y_range) * 0.3  # Extend by 30% of the range
  y_min_extended <- y_range[1] - y_extend
  y_max_extended <- y_range[2] + y_extend

  # Calculate scaling for visual elements
  base_cap_width <- 0.05
  base_offset <- 0.18
  cap_scaling <- 5 / (n_by + 4)
  cap_width <- max(0.02, min(0.1, cap_width <- base_cap_width * cap_scaling))
  offset_scaling <- 4 / (n_by + 3)
  offset_scale <- max(0.08, min(0.2, offset_scale <- base_offset * offset_scaling))

  # Create separate plots for upper and lower bounds
  upper_plot <- plotly::plot_ly()
  lower_plot <- plotly::plot_ly()

  # Add traces for each By value
  for (i in 1:nrow(data)) {
    by_val <- as.numeric(data$By[i])
    by_label <- as.character(data$By[i])

    # Add reducible uncertainty ribbons if enabled
    if (show_reducible) {
      ribbon_width <- 0.15

      # Upper plot ribbons
      upper_plot <- plotly::add_ribbons(
        upper_plot,
        x = c(by_val - ribbon_width, by_val + ribbon_width),
        ymin = rep(data$PI_Upper[i], 2),
        ymax = rep(data$TI_Upper[i], 2),
        fillcolor = 'rgba(255, 165, 0, 0.6)',
        line = list(color = 'rgba(255, 165, 0, 1)', width = 1),
        name = "Reducible Imprecision",
        showlegend = (i == 1),
        hoverinfo = "text",
        text = sprintf("Reducible Imprecision<br>By: %s<br>Range: %.4f to %.4f",
                       by_label, data$PI_Upper[i], data$TI_Upper[i])
      )

      # Lower plot ribbons
      lower_plot <- plotly::add_ribbons(
        lower_plot,
        x = c(by_val - ribbon_width, by_val + ribbon_width),
        ymin = rep(data$TI_Lower[i], 2),
        ymax = rep(data$PI_Lower[i], 2),
        fillcolor = 'rgba(255, 165, 0, 0.6)',
        line = list(color = 'rgba(255, 165, 0, 1)', width = 1),
        name = "Reducible Imprecision",
        showlegend = (i == 1),
        hoverinfo = "text",
        text = sprintf("Reducible Imprecision<br>By: %s<br>Range: %.4f to %.4f",
                       by_label, data$TI_Lower[i], data$PI_Lower[i])
      )
    }

    # Add CI Upper traces
    upper_plot <- plotly::add_trace(
      upper_plot,
      x = c(by_val - offset_scale),
      y = c(data$Median[i]),
      type = 'scatter',
      mode = 'markers',
      marker = list(color = '#de2f2f', size = 4),
      showlegend = FALSE,
      hoverinfo = "text",
      text = sprintf("CI Median: %.4f", data$Median[i])
    )

    upper_plot <- plotly::add_trace(
      upper_plot,
      x = c(by_val - offset_scale, by_val - offset_scale),
      y = c(data$Median[i], data$CI_Upper[i]),
      type = 'scatter',
      mode = 'lines',
      line = list(color = '#de2f2f', width = 2),
      name = "CI Upper",
      showlegend = i == 1,
      hoverinfo = "text",
      text = sprintf("CI Upper: %.4f", data$CI_Upper[i])
    )

    upper_plot <- plotly::add_trace(
      upper_plot,
      x = c(by_val - offset_scale, by_val - offset_scale),
      y = c(data$CI_Upper[i], y_min_extended),
      type = 'scatter',
      mode = 'lines',
      line = list(color = '#de2f2f', width = 2),
      showlegend = FALSE
    )

    upper_plot <- plotly::add_trace(
      upper_plot,
      x = c(by_val - offset_scale - cap_width, by_val - offset_scale + cap_width),
      y = c(data$CI_Upper[i], data$CI_Upper[i]),
      type = 'scatter',
      mode = 'lines',
      line = list(color = '#de2f2f', width = 2),
      showlegend = FALSE
    )

    # Add PI Upper traces
    upper_plot <- plotly::add_trace(
      upper_plot,
      x = c(by_val),
      y = c(data$Median[i]),
      type = 'scatter',
      mode = 'markers',
      marker = list(color = '#006400', size = 4),
      showlegend = FALSE,
      hoverinfo = "text",
      text = sprintf("PI Median: %.4f", data$Median[i])
    )

    upper_plot <- plotly::add_trace(
      upper_plot,
      x = c(by_val, by_val),
      y = c(data$Median[i], data$PI_Upper[i]),
      type = 'scatter',
      mode = 'lines',
      line = list(color = '#006400', width = 2),
      name = "PI Upper",
      showlegend = i == 1,
      hoverinfo = "text",
      text = sprintf("PI Upper: %.4f", data$PI_Upper[i])
    )

    upper_plot <- plotly::add_trace(
      upper_plot,
      x = c(by_val, by_val),
      y = c(data$PI_Upper[i], y_min_extended),
      type = 'scatter',
      mode = 'lines',
      line = list(color = '#006400', width = 2),
      showlegend = FALSE
    )

    upper_plot <- plotly::add_trace(
      upper_plot,
      x = c(by_val - cap_width, by_val + cap_width),
      y = c(data$PI_Upper[i], data$PI_Upper[i]),
      type = 'scatter',
      mode = 'lines',
      line = list(color = '#006400', width = 2),
      showlegend = FALSE
    )

    # Add TI Upper traces
    upper_plot <- plotly::add_trace(
      upper_plot,
      x = c(by_val + offset_scale),
      y = c(data$Median[i]),
      type = 'scatter',
      mode = 'markers',
      marker = list(color = '#4232e7', size = 4),
      showlegend = FALSE,
      hoverinfo = "text",
      text = sprintf("TI Median: %.4f", data$Median[i])
    )

    upper_plot <- plotly::add_trace(
      upper_plot,
      x = c(by_val + offset_scale, by_val + offset_scale),
      y = c(data$Median[i], data$TI_Upper[i]),
      type = 'scatter',
      mode = 'lines',
      line = list(color = '#4232e7', width = 2),
      name = "TI Upper",
      showlegend = i == 1,
      hoverinfo = "text",
      text = sprintf("TI Upper: %.4f", data$TI_Upper[i])
    )

    upper_plot <- plotly::add_trace(
      upper_plot,
      x = c(by_val + offset_scale, by_val + offset_scale),
      y = c(data$TI_Upper[i], y_min_extended),
      type = 'scatter',
      mode = 'lines',
      line = list(color = '#4232e7', width = 2),
      showlegend = FALSE
    )

    upper_plot <- plotly::add_trace(
      upper_plot,
      x = c(by_val + offset_scale - cap_width, by_val + offset_scale + cap_width),
      y = c(data$TI_Upper[i], data$TI_Upper[i]),
      type = 'scatter',
      mode = 'lines',
      line = list(color = '#4232e7', width = 2),
      showlegend = FALSE
    )

    # Add CI Lower traces
    lower_plot <- plotly::add_trace(
      lower_plot,
      x = c(by_val - offset_scale),
      y = c(data$Median[i]),
      type = 'scatter',
      mode = 'markers',
      marker = list(color = '#de2f2f', size = 4),
      showlegend = FALSE,
      hoverinfo = "text",
      text = sprintf("CI Median: %.4f", data$Median[i])
    )

    lower_plot <- plotly::add_trace(
      lower_plot,
      x = c(by_val - offset_scale, by_val - offset_scale),
      y = c(data$Median[i], data$CI_Lower[i]),
      type = 'scatter',
      mode = 'lines',
      line = list(color = '#de2f2f', width = 2),
      name = "CI Lower",
      showlegend = i == 1,
      hoverinfo = "text",
      text = sprintf("CI Lower: %.4f", data$CI_Lower[i])
    )

    lower_plot <- plotly::add_trace(
      lower_plot,
      x = c(by_val - offset_scale, by_val - offset_scale),
      y = c(data$CI_Lower[i], y_max_extended),
      type = 'scatter',
      mode = 'lines',
      line = list(color = '#de2f2f', width = 2),
      showlegend = FALSE
    )

    lower_plot <- plotly::add_trace(
      lower_plot,
      x = c(by_val - offset_scale - cap_width, by_val - offset_scale + cap_width),
      y = c(data$CI_Lower[i], data$CI_Lower[i]),
      type = 'scatter',
      mode = 'lines',
      line = list(color = '#de2f2f', width = 2),
      showlegend = FALSE
    )

    # Add PI Lower traces
    lower_plot <- plotly::add_trace(
      lower_plot,
      x = c(by_val),
      y = c(data$Median[i]),
      type = 'scatter',
      mode = 'markers',
      marker = list(color = '#006400', size = 4),
      showlegend = FALSE,
      hoverinfo = "text",
      text = sprintf("PI Median: %.4f", data$Median[i])
    )

    lower_plot <- plotly::add_trace(
      lower_plot,
      x = c(by_val, by_val),
      y = c(data$Median[i], data$PI_Lower[i]),
      type = 'scatter',
      mode = 'lines',
      line = list(color = '#006400', width = 2),
      name = "PI Lower",
      showlegend = i == 1,
      hoverinfo = "text",
      text = sprintf("PI Lower: %.4f", data$PI_Lower[i])
    )

    lower_plot <- plotly::add_trace(
      lower_plot,
      x = c(by_val, by_val),
      y = c(data$PI_Lower[i], y_max_extended),
      type = 'scatter',
      mode = 'lines',
      line = list(color = '#006400', width = 2),
      showlegend = FALSE
    )

    lower_plot <- plotly::add_trace(
      lower_plot,
      x = c(by_val - cap_width, by_val + cap_width),
      y = c(data$PI_Lower[i], data$PI_Lower[i]),
      type = 'scatter',
      mode = 'lines',
      line = list(color = '#006400', width = 2),
      showlegend = FALSE
    )

    # Add TI Lower traces
    lower_plot <- plotly::add_trace(
      lower_plot,
      x = c(by_val + offset_scale),
      y = c(data$Median[i]),
      type = 'scatter',
      mode = 'markers',
      marker = list(color = '#4232e7', size = 4),
      showlegend = FALSE,
      hoverinfo = "text",
      text = sprintf("TI Median: %.4f", data$Median[i])
    )

    lower_plot <- plotly::add_trace(
      lower_plot,
      x = c(by_val + offset_scale, by_val + offset_scale),
      y = c(data$Median[i], data$TI_Lower[i]),
      type = 'scatter',
      mode = 'lines',
      line = list(color = '#4232e7', width = 2),
      name = "TI Lower",
      showlegend = i == 1,
      hoverinfo = "text",
      text = sprintf("TI Lower: %.4f", data$TI_Lower[i])
    )

    lower_plot <- plotly::add_trace(
      lower_plot,
      x = c(by_val + offset_scale, by_val + offset_scale),
      y = c(data$TI_Lower[i], y_max_extended),
      type = 'scatter',
      mode = 'lines',
      line = list(color = '#4232e7', width = 2),
      showlegend = FALSE
    )

    lower_plot <- plotly::add_trace(
      lower_plot,
      x = c(by_val + offset_scale - cap_width, by_val + offset_scale + cap_width),
      y = c(data$TI_Lower[i], data$TI_Lower[i]),
      type = 'scatter',
      mode = 'lines',
      line = list(color = '#4232e7', width = 2),
      showlegend = FALSE
    )
  }

  # Calculate range padding
  range_padding <- max(0.8, min(2.5, 6/n_by))

  # For single chain, replace the "1" with an empty string
  tick_text <- levels(data$By)
  if (is_single_chain) {
    tick_text <- ""
  }

  # Set layout for upper plot
  upper_plot <- plotly::layout(
    upper_plot,
    title = "Upper Bounds",
    xaxis = list(
      title = x_axis_title,
      tickmode = "array",
      tickvals = 1:length(levels(data$By)),
      ticktext = tick_text,
      tickangle = 45,
      range = c(1 - range_padding, length(levels(data$By)) + range_padding)
    ),
    yaxis = list(title = "Upper Bound Values"),
    legend = list(x = 1.05, y = 0.5),
    margin = list(b = 100)
  )

  # Set layout for lower plot
  lower_plot <- plotly::layout(
    lower_plot,
    title = "Lower Bounds",
    xaxis = list(
      title = x_axis_title,
      tickmode = "array",
      tickvals = 1:length(levels(data$By)),
      ticktext = tick_text,
      tickangle = 45,
      range = c(1 - range_padding, length(levels(data$By)) + range_padding)
    ),
    yaxis = list(title = "Lower Bound Values"),
    legend = list(x = 1.05, y = 0.5),
    margin = list(b = 100)
  )

  return(list(upper = upper_plot, lower = lower_plot))
}
