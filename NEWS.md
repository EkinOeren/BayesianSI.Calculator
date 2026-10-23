# BayesianStatisticalIntervalsCalculator 2.0.0

First CRAN release.

## New features

* `bayesian_statistical_intervals()` computes credible, prediction and
  tolerance intervals from MCMC draws of a Bayesian linear mixed model, with
  optional covariates (`covariate_cols`, `covariate_values`) and a log-normal
  back-transformation (`log_normal`).
* Two-sided tolerance intervals with asymmetric limits can be requested with
  `use_krishnamoorthy = TRUE`. `plot_krishnamoorthy()` and
  `quantile_scatterplot()` visualize the lower/upper quantile draws and the
  optimal point.
* Reducible imprecision (`ReducibleUpper`, `ReducibleLower`) is
  returned with the intervals, and `plot_one_sided_intervals()` can display
  them.
* `create_CDF()`, `plot_posterior_predictive()` and `get_probability_table()`
  provide posterior predictive distributions and probabilities for a single
  value or a range of values.
* `start_app_view()` launches an interactive Shiny application.
* The synthetic data set `data_InputFileTester2` is included for examples and
  tests.

## Changes

* The package is released under the Apache License 2.0.
* Dependencies were reduced: `ellipse`, `gridExtra`, `shinythemes` and
  `htmltools` are no longer used (confidence ellipses are computed
  internally and the application theme comes from `bslib`). `later` and
  `bslib` are now imported because the application needs them.
* Inputs are validated with clear error messages: interval levels, the multiplication factor, covariate arguments, column types, and the `by` column. Missing or infinite MCMC draws and negative variances now stop with an error; previously missing draws were silently treated as zero.
* The documentation cites Krishnamoorthy and Mathew (2009) for the tolerance interval methods.
* The native pipe `|>` replaces `%>%`.
* All examples are now runnable and use small synthetic data.
* `start_app_view()` restores the `shiny.maxRequestSize` option when the
  application is closed.
* The application no longer links to internal documentation.

## Bug fixes

* `quantile_scatterplot()` now draws confidence ellipses when called from
  the package. The helper that computes them previously existed only inside
  the Shiny application, so the ellipses were silently skipped.
* Fixed functions that used `%>%` without importing it and would fail in a
  clean R session.
* Removed non-ASCII characters from R code and fixed broken
  cross-references in the documentation.
