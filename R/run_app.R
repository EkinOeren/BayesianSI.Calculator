#' Launch the Shiny Application
#'
#' @param ... arguments to pass to shiny::runApp
#' @export
#'
#' @examples
#' if(interactive()){
#'   start_app_view()
#' }
start_app_view <- function(...) {
  app_dir <- system.file("shiny", package = "BayesianStatisticalIntervalsCalculator")
  if (app_dir == "") {
    stop("Could not find app directory. Try reinstalling `BayesianStatisticalIntervalsCalculator`.", call. = FALSE)
  }

  shiny::runApp(app_dir, ...)
}
