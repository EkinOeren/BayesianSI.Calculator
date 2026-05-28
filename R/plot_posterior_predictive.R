#' Plot Posterior Predictive Distribution
#'
#' Creates an interactive plot of the posterior predictive distribution using plotly.
#' Its only a legacy function to keep compatibility with the older versions.
#'
#' @param cdf_data A dataframe from create_cdf_dataframe() containing CDF information, or a list of such dataframes
#' @param x_value Optional. Single numeric value or range [min,max] as character string (e.g., "[2,5]")
#' @param title Optional. Custom plot title. If NULL, auto-generates based on by-value
#' @param show_ci Logical. Whether to show confidence interval ribbon (default = TRUE)
#' @param line_colors List of colors for different elements (optional)
#' @param show_reference_lines Logical. Whether to show reference lines for x_value (default = TRUE)
#'
#' @return A plotly object containing the posterior predictive distribution plot
#'
#' @examples
#' \dontrun{
#' # Basic plot with list input (automatically uses "Single Chain")
#' pred_dist <- calculate_predicted_distributions(data, "fixed1", "random1",
#'                                              "Group", 95)
#' cdf_data <- create_cdf_dataframe(pred_dist)
#' plot_posterior_predictive(cdf_data)
#'
#' # Plot with specific by-value from list
#' plot_posterior_predictive(cdf_data[["A"]])
#'
#' # Plot with x-value
#' plot_posterior_predictive(cdf_data, x_value = "5")
#'
#' # Plot with range
#' plot_posterior_predictive(cdf_data, x_value = "[2,5]")
#' }
#'
#' @importFrom plotly plot_ly add_trace add_ribbons layout add_segments
#' @export
plot_posterior_predictive <- function(cdf_data, x_value = NULL, title = NULL,
                                      show_ci = TRUE,
                                      line_colors = list(
                                        main = 'rgba(0,100,80,1)',
                                        ci = 'red',
                                        ci_ribbon = 'rgba(255,0,0,0.3)',
                                        reference = 'black'
                                      ),
                                      show_reference_lines = TRUE) {

  # Handle input data format
  if (is.list(cdf_data) && !is.data.frame(cdf_data)) {
    # If it's a list but not a dataframe, try to get "Single Chain" by default
    if ("Single Chain" %in% names(cdf_data)) {
      cdf_data <- cdf_data[["Single Chain"]]
    } else {
      stop('When providing a list of dataframes, either specify the by-value (e.g., cdf_data[["your_by_value"]]) or ensure a dataframe named "Single Chain" exists in the list')
    }
  }

  # Input validation
  if (!all(c("x", "y_mean", "ci_lower", "ci_upper", "by_value") %in% names(cdf_data))) {
    stop("cdf_data must contain x, y_mean, ci_lower, ci_upper, and by_value columns")
  }

  # Rest of the function remains the same...
  # Generate title if not provided
  if (is.null(title)) {
    title <- if(cdf_data$by_value[1] == "Single Chain") {
      "Posterior Predictive Distribution"
    } else {
      paste("Posterior Predictive Distribution for", cdf_data$by_value[1])
    }
  }

  # Create base plot
  p <- plot_ly() %>%
    # Main CDF line
    add_trace(
      data = cdf_data,
      x = ~x,
      y = ~y_mean,
      type = 'scatter',
      mode = 'lines',
      name = "PP Distribution",
      line = list(color = line_colors$main, width = 2),
      hoverinfo = 'text',
      text = ~sprintf(
        "x: %.4f<br>P(X ≤ x): %.4f",
        x, y_mean
      )
    )

  # Add confidence interval ribbon if requested
  if (show_ci) {
    p <- p %>%
      add_trace(
        data = cdf_data,
        x = ~x,
        y = ~ci_lower,
        type = 'scatter',
        mode = 'lines',
        name = "CI Lower",
        line = list(color = line_colors$ci, width = 2)
      ) %>%
      add_trace(
        data = cdf_data,
        x = ~x,
        y = ~ci_upper,
        type = 'scatter',
        mode = 'lines',
        name = "CI Upper",
        line = list(color = line_colors$ci, width = 2)
      ) %>%
      add_ribbons(
        data = cdf_data,
        x = ~x,
        ymin = ~ci_lower,
        ymax = ~ci_upper,
        fillcolor = line_colors$ci_ribbon,
        line = list(color = 'rgba(255,0,0,0)'),
        name = "CI Range",
        showlegend = FALSE
      )
  }

  # Handle x_value if provided
  if (!is.null(x_value)) {
    parsed_input <- parse_x_value_input(x_value)

    if (!is.null(parsed_input)) {
      if (parsed_input$type == "single") {
        # Handle single value
        x_val <- parsed_input$value

        # Find corresponding y value through interpolation
        y_val <- approx(cdf_data$x, cdf_data$y_mean, xout = x_val)$y
        ci_lower <- approx(cdf_data$x, cdf_data$ci_lower, xout = x_val)$y
        ci_upper <- approx(cdf_data$x, cdf_data$ci_upper, xout = x_val)$y

        if (!is.na(y_val)) {
          # Add marker point
          p <- p %>%
            add_trace(
              x = x_val,
              y = y_val,
              type = 'scatter',
              mode = 'markers',
              marker = list(color = line_colors$reference, size = 8),
              name = "Selected X Value",
              text = sprintf(
                "x: %.4f<br>P(X ≤ x): %.4f<br>CI: [%.4f, %.4f]",
                x_val, y_val, ci_lower, ci_upper
              ),
              hoverinfo = 'text'
            )

          # Add reference lines if requested
          if (show_reference_lines) {
            p <- p %>%
              add_segments(
                x = min(cdf_data$x),
                xend = x_val,
                y = y_val,
                yend = y_val,
                line = list(color = line_colors$reference,
                            width = 1, dash = 'dot'),
                showlegend = FALSE
              ) %>%
              add_segments(
                x = x_val,
                xend = x_val,
                y = 0,
                yend = y_val,
                line = list(color = line_colors$reference,
                            width = 1, dash = 'dot'),
                showlegend = FALSE
              )
          }
        }
      } else if (parsed_input$type == "range") {
        # Handle range values
        xmin <- parsed_input$xmin
        xmax <- parsed_input$xmax

        # Find corresponding y values
        y_min <- approx(cdf_data$x, cdf_data$y_mean, xout = xmin)$y
        y_max <- approx(cdf_data$x, cdf_data$y_mean, xout = xmax)$y

        if (!is.na(y_min) && !is.na(y_max)) {
          # Add range markers
          p <- p %>%
            add_trace(
              x = c(xmin, xmax),
              y = c(y_min, y_max),
              type = 'scatter',
              mode = 'markers',
              marker = list(color = line_colors$reference, size = 8),
              name = "Range Bounds",
              text = c(
                sprintf("xmin: %.4f<br>P(X ≤ xmin): %.4f", xmin, y_min),
                sprintf("xmax: %.4f<br>P(X ≤ xmax): %.4f", xmax, y_max)
              ),
              hoverinfo = 'text'
            )

          # Add reference lines if requested
          if (show_reference_lines) {
            p <- p %>%
              add_segments(
                x = c(min(cdf_data$x), min(cdf_data$x)),
                xend = c(xmin, xmax),
                y = c(y_min, y_max),
                yend = c(y_min, y_max),
                line = list(color = line_colors$reference,
                            width = 1, dash = 'dot'),
                showlegend = FALSE
              ) %>%
              add_segments(
                x = c(xmin, xmax),
                xend = c(xmin, xmax),
                y = c(0, 0),
                yend = c(y_min, y_max),
                line = list(color = line_colors$reference,
                            width = 1, dash = 'dot'),
                showlegend = FALSE
              )
          }
        }
      }
    }
  }

  # Set layout
  p <- p %>% layout(
    title = title,
    xaxis = list(title = "Value"),
    yaxis = list(
      title = "Probability",
      range = c(0, 1)
    ),
    hovermode = 'closest',
    showlegend = TRUE
  )

  return(p)
}
