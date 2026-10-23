# format_intervals_table() and plot_intervals() read the interval levels from the
# summary text of the result. Both pipelines must work with them.
# Synthetic data only.
make_draws <- function() {
  set.seed(5)
  data.frame(G = rep(c("A", "B"), each = 40), b = rnorm(80, 10, 0.4), s2 = rgamma(80, 30, 15))
}

test_that("format_intervals_table works with bayesian_statistical_intervals() output", {
  res <- bayesian_statistical_intervals(make_draws(), "b", "s2", by = "G",
                                        tolerance_interval_level = 90, credible_interval_level = 85,
                                        prediction_interval_level = 90, credibility_of_tolerance_interval = 80)
  expect_no_warning(tbl <- format_intervals_table(res))
  expect_s3_class(tbl, "datatables")
  # the container header carries the right percentages
  html <- paste(as.character(tbl$x$container), collapse = " ")
  expect_match(html, "85")
})

test_that("format_intervals_table still works with perform_calculations() output", {
  res <- perform_calculations(make_draws(), "b", "s2", by = "G", tolerance_level = 90,
                              percent_for_ci = 85, percent_for_pi = 90, confidence_of_tolerance = 80)
  expect_no_warning(tbl <- format_intervals_table(res))
  expect_s3_class(tbl, "datatables")
})

test_that("plot_intervals works with both pipelines", {
  d <- make_draws()
  bsi <- bayesian_statistical_intervals(d, "b", "s2", by = "G", tolerance_interval_level = 90,
                                        prediction_interval_level = 90)
  pc <- perform_calculations(d, "b", "s2", by = "G", tolerance_level = 90, percent_for_pi = 90)
  expect_s3_class(plot_intervals(bsi), "plotly")
  expect_s3_class(plot_intervals(pc), "plotly")
})
