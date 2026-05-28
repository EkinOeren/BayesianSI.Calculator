#' Format Intervals Table with Double Header
#'
#' Creates a formatted DT table with two header levels showing statistical intervals
#' in the same format as the Shiny app.
#'
#' @param result Output list from perform_calculations() containing data and summary
#' @param show_reducible Logical, whether to include reducible uncertainty columns (default = FALSE)
#'
#' @return A DT::datatable object with formatted headers showing interval percentages
#' @import DT
#' @export
format_intervals_table <- function(result, show_reducible = FALSE) {

  # Extract data
  data <- result$data
  summary_text <- result$summary

  # Extract percentages from summary text using regex
  ci_percent <- as.numeric(regmatches(summary_text, regexpr("\\d+(?=% credibility)", summary_text, perl = TRUE)))
  pi_percent <- as.numeric(regmatches(summary_text, regexpr("\\d+(?=% prediction)", summary_text, perl = TRUE)))
  ti_percent <- as.numeric(regmatches(summary_text, regexpr("\\d+(?=% tolerance)", summary_text, perl = TRUE)))
  conf_percent <- as.numeric(regmatches(summary_text, regexpr("\\d+(?=% confidence)", summary_text, perl = TRUE)))

  # Check if single chain
  is_single_chain <- length(unique(data$By)) == 1 &&
    (unique(data$By) == "SingleChain" || unique(data$By) == 1)

  # Prepare display data
  display_data <- data
  if (is_single_chain) {
    display_data$By <- "" # Empty the By column for display purposes
  }

  # Select and order columns based on show_reducible
  if(show_reducible) {
    display_columns <- c("By", "Median",
                         "CI_Lower", "CI_Upper",
                         "PI_Lower", "PI_Upper",
                         "TI_Lower", "TI_Upper",
                         "ReducibleLower", "ReducibleUpper")

    # Create header names with extracted percentages
    header_names <- c(
      "By Identifier",
      "Point Estimate",
      sprintf("%d%% CI", ci_percent),
      sprintf("%d%% PI", pi_percent),
      sprintf("%d%% TI (%d%% Credibility)", ti_percent, conf_percent),
      "Reducible Imprecision"
    )

    # Define column spans for each header
    header_colspans <- c(1, 1, 2, 2, 2, 2)

    # Create the container with double header
    container <- shiny::tags$table(
      class = 'display',
      shiny::tags$thead(
        shiny::tags$tr(
          shiny::tags$th(rowspan = 2, if (is_single_chain) "" else header_names[1]),
          shiny::tags$th(rowspan = 2, header_names[2]),
          do.call(
            tagList,
            mapply(
              function(name, colspan) shiny::tags$th(colspan = colspan, name),
              header_names[-c(1, 2)],
              header_colspans[-c(1, 2)],
              SIMPLIFY = FALSE
            )
          )
        ),
        shiny::tags$tr(
          shiny::tags$th("Lower"), shiny::tags$th("Upper"),  # CI
          shiny::tags$th("Lower"), shiny::tags$th("Upper"),  # PI
          shiny::tags$th("Lower"), shiny::tags$th("Upper"),  # TI
          shiny::tags$th("Lower"), shiny::tags$th("Upper")   # Reducible
        )
      )
    )

  } else {
    display_columns <- c("By", "Median",
                         "CI_Lower", "CI_Upper",
                         "PI_Lower", "PI_Upper",
                         "TI_Lower", "TI_Upper")

    # Create header names with extracted percentages
    header_names <- c(
      "By Identifier",
      "Point Estimate",
      sprintf("%d%% CI", ci_percent),
      sprintf("%d%% PI", pi_percent),
      sprintf("%d%% TI (%d%% Credibility)", ti_percent, conf_percent)
    )

    # Define column spans for each header
    header_colspans <- c(1, 1, 2, 2, 2)

    # Create the container with double header
    container <- shiny::tags$table(
      class = 'display',
      shiny::tags$thead(
        shiny::tags$tr(
          shiny::tags$th(rowspan = 2, if (is_single_chain) "" else header_names[1]),
          shiny::tags$th(rowspan = 2, header_names[2]),
          do.call(
            tagList,
            mapply(
              function(name, colspan) shiny::tags$th(colspan = colspan, name),
              header_names[-c(1, 2)],
              header_colspans[-c(1, 2)],
              SIMPLIFY = FALSE
            )
          )
        ),
        shiny::tags$tr(
          shiny::tags$th("Lower"), shiny::tags$th("Upper"),  # CI
          shiny::tags$th("Lower"), shiny::tags$th("Upper"),  # PI
          shiny::tags$th("Lower"), shiny::tags$th("Upper")   # TI
        )
      )
    )
  }

  # Create the DT table
  dt <- DT::datatable(
    display_data[, display_columns],
    options = list(
      pageLength = 50,
      dom = 't',
      scrollY = "400px",
      scrollCollapse = TRUE,
      paging = FALSE,
      ordering = TRUE,
      order = list(),
      columnDefs = list(
        list(className = 'dt-center', targets = '_all'),
        list(className = 'dt-left', targets = 0)
      )
    ),
    style = 'bootstrap4',
    container = container,
    rownames = FALSE
  ) %>%
    DT::formatRound(columns = setdiff(display_columns, "By"), digits = 4)

  return(dt)
}
