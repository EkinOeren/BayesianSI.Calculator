#' Generate Interactive Quantile Scatterplot with Ellipses
#'
#' This function creates an interactive scatterplot visualizing the relationship between lower and upper
#' quantiles of posterior predictive distributions from MCMC samples, with confidence ellipses.
#' It uses plotly for interactivity and can work with either raw data or pre-calculated distributions.
#'
#' @param data A data frame containing MCMC samples. Required if pred_dist is NULL.
#' @param fixed_effects Character vector of column names for fixed effects. Required if pred_dist is NULL.
#' @param random_params Character vector of column names for random parameters. Required if pred_dist is NULL.
#' @param by Character string specifying the grouping variable. Default is "Single Chain Dataset".
#' @param pred_dist Optional pre-calculated predicted distributions from calculate_predicted_distributions().
#'        If provided, data, fixed_effects, and random_params are ignored.
#' @param selected_by_values Optional vector of specific by values to include in the plot.
#'        If NULL, all by values are shown. Default is NULL.
#' @param tolerance_level Numeric value between 0 and 100 for tolerance interval percentage. Default is 95.
#' @param ellipse_confidence Numeric value between 0 and 100 for credible ellipses. Default is 95.
#' @param multiplication_factor Positive numeric value to adjust variance components. Default is 1.
#' @param log_normal Logical indicating whether to transform predictions to log-normal scale. Default is FALSE.
#' @param covariate_cols Character vector of column names for covariates. Default is NULL.
#' @param covariate_values Numeric vector of values to multiply with covariates. Default is NULL.
#'
#' @return A plotly object containing the interactive quantile scatterplot with ellipses.
#'
#' @details
#' The function creates a scatterplot where:
#' - Each point represents a single MCMC sample
#' - X-axis shows the lower quantile
#' - Y-axis shows the upper quantile
#' - Points are grouped by the specified 'by' variable
#' - Confidence ellipses show the concentration of points
#' - Diamond markers show the mean position for each group
#' - Interactive tooltips provide detailed information
#'
#' @examples
#' \dontrun{
#' # Example 1: From raw data without covariates
#' plot1 <- quantile_scatterplot(
#'   data = mcmc_samples,
#'   fixed_effects = "beta0",
#'   random_params = "sigma2",
#'   by = "Group",
#'   tolerance_level = 95,
#'   ellipse_confidence = 90
#' )
#'
#' # Example 2: From raw data with covariates
#' plot2 <- quantile_scatterplot(
#'   data = mcmc_samples,
#'   fixed_effects = "beta0",
#'   random_params = "sigma2",
#'   by = "Group",
#'   covariate_cols = "age",
#'   covariate_values = 65,
#'   tolerance_level = 90
#' )
#'
#' # Example 3: From pre-calculated distributions
#' pred_dist <- calculate_predicted_distributions(...)
#' plot3 <- quantile_scatterplot(
#'   pred_dist = pred_dist,
#'   tolerance_level = 95,
#'   ellipse_confidence = 95
#' )
#'
#' # Example 4: Filter specific groups
#' plot4 <- quantile_scatterplot(
#'   data = mcmc_samples,
#'   fixed_effects = "beta0",
#'   random_params = "sigma2",
#'   by = "Group",
#'   selected_by_values = c("A", "B"),
#'   tolerance_level = 90
#' )
#' }
#'
#' @import ggplot2
#' @importFrom stats cov
#' @importFrom plotly plot_ly add_trace layout
#'
#' @export
quantile_scatterplot <- function(data = NULL,
                                 fixed_effects = NULL,
                                 random_params = NULL,
                                 by = "Single Chain Dataset",
                                 pred_dist = NULL,
                                 selected_by_values = NULL,
                                 tolerance_level = 95,
                                 ellipse_confidence = 95,
                                 multiplication_factor = 1,
                                 log_normal = FALSE,
                                 covariate_cols = NULL,
                                 covariate_values = NULL) {

  # Check if pred_dist is provided or needs to be calculated
  if (is.null(pred_dist)) {
    # Validate required parameters for calculation
    if (is.null(data) || is.null(fixed_effects) || is.null(random_params)) {
      stop("When pred_dist is NULL, data, fixed_effects, and random_params must be provided")
    }

    # Calculate predicted distributions
    pred_dist <- calculate_predicted_distributions(
      data = data,
      fixed_effects = fixed_effects,
      random_params = random_params,
      by = by,
      tolerance_level = tolerance_level,
      multiplication_factor = multiplication_factor,
      covariate_cols = covariate_cols,
      covariate_values = covariate_values
    )
  } else {
    # Use the provided pred_dist
    if (!all(c("lower_quantile", "upper_quantile", "By") %in% names(pred_dist))) {
      stop("pred_dist must contain 'lower_quantile', 'upper_quantile', and 'By' columns")
    }
  }

  # Apply log-normal transformation if requested
  if (log_normal) {
    transform_cols <- c("lower_quantile", "upper_quantile")
    for (col in transform_cols) {
      if (col %in% names(pred_dist)) {
        pred_dist[[col]] <- exp(pred_dist[[col]])
      }
    }
  }

  # Filter By values if specified
  if (!is.null(selected_by_values)) {
    pred_dist <- pred_dist[pred_dist$By %in% selected_by_values, ]
    if (nrow(pred_dist) == 0) {
      stop("No data remaining after filtering By values")
    }
  }

  # Calculate percentiles for labels
  lower_percentile <- (100 - tolerance_level) / 2
  upper_percentile <- 100 - lower_percentile

  # Convert to data frame
  plot_data <- as.data.frame(pred_dist)

  # Create color palette
  n_colors <- length(unique(plot_data$By))
  colors <- colorRampPalette(c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd"))(n_colors)

  # Initialize plotly plot
  p <- plotly::plot_ly()

  # Process each By value
  for (i in seq_along(unique(plot_data$By))) {
    by_value <- unique(plot_data$By)[i]
    group_data <- plot_data[plot_data$By == by_value, ]

    if (nrow(group_data) <= 1) next

    # Add scatter points
    p <- plotly::add_trace(p,
                           data = group_data,
                           x = ~lower_quantile,
                           y = ~upper_quantile,
                           type = 'scatter',
                           mode = 'markers',
                           name = paste("By:", by_value),
                           marker = list(
                             size = 8,
                             color = colors[i],
                             opacity = 0.6
                           ),
                           hoverinfo = 'text',
                           text = ~paste(
                             "By:", by_value,
                             "<br>Lower Quantile:", round(lower_quantile, 4),
                             "<br>Upper Quantile:", round(upper_quantile, 4)
                           ),
                           showlegend = TRUE
    )

    # Calculate mean point
    mean_point <- colMeans(group_data[, c("lower_quantile", "upper_quantile")])

    # Calculate and add confidence ellipse
    ellipse_level <- ellipse_confidence / 100
    tryCatch({
      cov_matrix <- cov(group_data[, c("lower_quantile", "upper_quantile")])
      if (all(is.finite(cov_matrix)) && det(cov_matrix) > 0) {
        ellipse_points <- create_confidence_ellipse_points(
          cov_matrix = cov_matrix,
          centre = mean_point,
          level = ellipse_level,
          npoints = 100
        )

        # Create darker version of the color for the ellipse
        darker_color <- colorRampPalette(c(colors[i], "black"))(3)[2]

        # Add ellipse
        p <- plotly::add_trace(p,
                               x = ellipse_points[, "x"],
                               y = ellipse_points[, "y"],
                               type = 'scatter',
                               mode = 'lines',
                               name = paste(ellipse_confidence, "% Ellipse"),
                               line = list(
                                 color = darker_color,
                                 width = 2.5
                               ),
                               hoverinfo = 'text',
                               text = paste(ellipse_confidence, "% Confidence Ellipse for By:", by_value),
                               showlegend = (i == 1)  # Only show first ellipse in legend
        )
      }
    }, error = function(e) {
      warning(paste("Could not calculate ellipse for group", by_value, ":", e$message))
    })

    # Add mean point
    p <- plotly::add_trace(p,
                           x = mean_point[1],
                           y = mean_point[2],
                           type = 'scatter',
                           mode = 'markers',
                           name = "Mean",
                           marker = list(
                             size = 12,
                             color = colors[i],
                             symbol = 'diamond'
                           ),
                           hoverinfo = 'text',
                           text = paste(
                             "Mean for By:", by_value,
                             "<br>Lower Quantile:", round(mean_point[1], 4),
                             "<br>Upper Quantile:", round(mean_point[2], 4)
                           ),
                           showlegend = (i == 1)  # Only show first mean in legend
    )
  }

  # Set layout
  p <- plotly::layout(p,
                      title = sprintf("Tolerance Interval Quantile Scatterplot (%.1f%% Probability Level, %.1f%% Credible Level)",
                                      tolerance_level, ellipse_confidence),
                      xaxis = list(
                        title = sprintf("Lower Tolerance Interval Probability Level (%.1f%%)", lower_percentile),
                        zeroline = TRUE
                      ),
                      yaxis = list(
                        title = sprintf("Upper Tolerance Interval Probability Level (%.1f%%)", upper_percentile),
                        zeroline = TRUE
                      ),
                      hovermode = 'closest',
                      showlegend = TRUE,
                      legend = list(
                        orientation = "h",
                        xanchor = "center",
                        x = 0.5,
                        y = -0.2
                      )
  )

  return(p)
}
