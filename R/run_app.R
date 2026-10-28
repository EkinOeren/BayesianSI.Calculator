#' Launch the Shiny Application
#'
#' Starts the interactive application for uploading MCMC output and
#' calculating and plotting statistical intervals. Uploads of up to 30 MB are
#' accepted; the previous value of the \code{shiny.maxRequestSize} option is
#' restored when the application is closed.
#'
#' @param ... arguments to pass to shiny::runApp
#' @return Called for its side effect of running the application; returns
#'   \code{NULL} invisibly when the application is closed.
#' @export
#'
#' @examples
#' if(interactive()){
#'   start_app_view()
#' }
start_app_view <- function(...) {
  app_dir <- system.file("shiny", package = "bayesintervals")
  if (app_dir == "") {
    stop("Could not find app directory. Try reinstalling `bayesintervals`.", call. = FALSE)
  }

  old_options <- options(shiny.maxRequestSize = 30 * 1024^2)
  on.exit(options(old_options), add = TRUE)

  shiny::runApp(app_dir, ...)
  invisible(NULL)
}
