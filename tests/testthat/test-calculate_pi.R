# Synthetic data only: group A has mean 10 / variance 4, group B has mean 20 / variance 9.
make_pi_data <- function() {
  data.frame(
    By = rep(c("A", "B"), each = 3),
    mean = c(9.5, 10, 10.5, 19, 20, 21),
    variance = c(4, 4, 4, 9, 9, 9)
  )
}

test_that("calculate_PI matches the analytic normal quantiles per group", {
  res <- calculate_PI(make_pi_data(), percent_for_pi = 95, Eta = 1e-6)

  expect_s3_class(res, "data.frame")
  expect_named(res, c("By", "PI_Lower", "PI_Upper"))
  expect_equal(nrow(res), 2)

  res <- res[order(res$By), ]
  expect_equal(res$By, c("A", "B"))
  expect_equal(res$PI_Lower, c(10 - qnorm(0.975) * 2, 20 - qnorm(0.975) * 3), tolerance = 1e-3)
  expect_equal(res$PI_Upper, c(10 + qnorm(0.975) * 2, 20 + qnorm(0.975) * 3), tolerance = 1e-3)
})

test_that("calculate_PI handles a single group", {
  dat <- data.frame(By = "only", mean = c(0, 0), variance = c(1, 1))
  res <- calculate_PI(dat, percent_for_pi = 90, Eta = 1e-6)

  expect_equal(nrow(res), 1)
  expect_equal(res$PI_Lower, -qnorm(0.95), tolerance = 1e-3)
  expect_equal(res$PI_Upper, qnorm(0.95), tolerance = 1e-3)
})

test_that("calculate_PI intervals are ordered and widen with the level", {
  dat <- make_pi_data()
  narrow <- calculate_PI(dat, 80, Eta = 1e-6)
  wide <- calculate_PI(dat, 99, Eta = 1e-6)

  expect_true(all(narrow$PI_Lower < narrow$PI_Upper))
  expect_true(all(wide$PI_Lower < narrow$PI_Lower))
  expect_true(all(wide$PI_Upper > narrow$PI_Upper))
})

test_that("calculate_PI accepts a data.table input", {
  res <- calculate_PI(data.table::as.data.table(make_pi_data()), 95, Eta = 1e-6)
  expect_equal(nrow(res), 2)
})

test_that("calculate_PI rejects invalid input", {
  dat <- make_pi_data()
  expect_error(calculate_PI("not a data frame", 95), "Invalid input parameters")
  expect_error(calculate_PI(dat, 0), "Invalid input parameters")
  expect_error(calculate_PI(dat, 100), "Invalid input parameters")
  expect_error(calculate_PI(dat, "95"), "Invalid input parameters")
})
