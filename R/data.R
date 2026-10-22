#' Synthetic MCMC output for examples and tests
#'
#' A small, entirely synthetic data set that mimics the output of an MCMC
#' sampler for a linear mixed model. It contains no real study data.
#'
#' @format A data frame with 19 rows and 8 variables:
#' \describe{
#'   \item{iteration}{MCMC iteration number}
#'   \item{mu}{Intercept draw}
#'   \item{t1, t2}{Fixed-effect draws}
#'   \item{v1, v2}{Variance-component draws}
#'   \item{By}{Numeric grouping variable (three groups)}
#'   \item{Bytwo}{Character grouping variable}
#' }
#' @source Simulated.
"data_InputFileTester2"
