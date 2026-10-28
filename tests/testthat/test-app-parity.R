# The Shiny application (inst/shiny/app.R) keeps its own copies of several
# package functions so that it can run as a single self-contained file. These
# tests load those copies and check that they give the same results as the
# package functions, so the two cannot drift apart unnoticed.
#
# Not compared directly: calculate_predicted_distributions() and
# lognorm_adjuster(). In the app they are reactive (they read the user's inputs),
# so they have a different signature from the package functions.

pkg <- "bayesintervals"

app_functions <- function() {
  app_file <- system.file("shiny", "app.R", package = pkg)
  if (!nzchar(app_file)) {
    # running from the source tree (e.g. devtools::test())
    app_file <- file.path("..", "..", "inst", "shiny", "app.R")
  }
  skip_if_not(file.exists(app_file), "app.R not found")
  exprs <- parse(app_file, keep.source = FALSE)
  defs <- list()
  walk <- function(e) {
    if (is.call(e)) {
      if (identical(e[[1]], as.name("<-")) && is.name(e[[2]]) && is.call(e[[3]]) &&
          identical(e[[3]][[1]], as.name("function"))) {
        defs[[as.character(e[[2]])]] <<- e[[3]]
      }
      for (i in seq_along(e)) tryCatch(walk(e[[i]]), error = function(err) NULL)
    }
  }
  for (e in exprs) walk(e)
  # Evaluate the app's definitions where data.table and the package imports are
  # visible, as they are when the app runs with these packages attached.
  env <- new.env(parent = asNamespace(pkg))
  # the app attaches data.table and dplyr (library(...)) and uses their functions
  # unqualified, so make the same exports visible here
  attach_exports <- function(package) {
    nms <- grep("^[.]__", getNamespaceExports(package), value = TRUE, invert = TRUE)
    vals <- lapply(nms, function(n) tryCatch(getExportedValue(package, n), error = function(e) NULL))
    names(vals) <- nms
    list2env(vals[!vapply(vals, is.null, NA)], envir = env)
  }
  attach_exports("data.table")
  attach_exports("dplyr")
  # In the app, calculate_gamma_difference() takes the target credibility from the
  # user interface (isolate(input$confidence_of_tolerance)); stand in for that here.
  env$isolate <- function(expr) expr
  env$input <- list(confidence_of_tolerance = 80)
  lapply(defs, function(d) eval(d, env))
}

synthetic_pred_dist <- function() {
  set.seed(7)
  n <- 150
  d <- data.frame(
    G = rep(c("A", "B", "C"), each = n),
    b = c(rnorm(n, 10, 0.4), rnorm(n, 12, 0.5), rnorm(n, 9, 0.3)),
    s2 = rgamma(3 * n, shape = 30, rate = 15)
  )
  calculate_predicted_distributions(d, "b", "s2", "G", tolerance_level = 90)
}

test_that("the app defines the functions this test relies on", {
  app <- app_functions()
  expected <- c("calculate_ci", "calculate_tolerance_interval", "calculate_PI",
                "calculate_gP_mean", "calculate_gamma_difference", "find_optimal_point",
                "parse_x_value_input", "create_confidence_ellipse_points",
                "check_positive", "check_draw_columns", "check_variance",
                "reducible_ratio", "add_reducible_imprecision")
  expect_true(all(expected %in% names(app)),
              info = paste("missing in app.R:", paste(setdiff(expected, names(app)), collapse = ", ")))
  # same argument names as the package versions
  for (nm in expected) {
    expect_identical(names(formals(app[[nm]])), names(formals(get(nm, envir = asNamespace(pkg)))),
                     info = nm)
  }
})

test_that("app and package give the same credible intervals", {
  app <- app_functions(); pd <- synthetic_pred_dist()
  expect_equal(app$calculate_ci(pd, "median", 95, "By"),
               calculate_ci(pd, "median", 95, "By"))
})

test_that("app and package give the same tolerance intervals", {
  app <- app_functions(); pd <- synthetic_pred_dist()
  expect_equal(app$calculate_tolerance_interval(pd, "lower_quantile", "upper_quantile", 80, "By"),
               calculate_tolerance_interval(pd, "lower_quantile", "upper_quantile", 80, "By"))
})

test_that("app and package give the same prediction intervals", {
  app <- app_functions(); pd <- synthetic_pred_dist()
  expect_equal(app$calculate_PI(pd, 90), calculate_PI(pd, 90))
  expect_equal(app$calculate_PI(pd[pd$By == "A", ], 80), calculate_PI(pd[pd$By == "A", ], 80))
})

test_that("app and package give the same Krishnamoorthy results", {
  app <- app_functions(); pd <- synthetic_pred_dist()
  gp_app <- app$calculate_gP_mean(pd, 90)
  gp_pkg <- calculate_gP_mean(pd, 90)
  expect_equal(gp_app, gp_pkg)

  # the app version prints its progress to the console; discard that output
  invisible(utils::capture.output(res_app <- app$find_optimal_point(pd, gp_pkg, 80)))
  expect_equal(res_app, find_optimal_point(pd, gp_pkg, 80))

  one <- pd[pd$By == "A", ]
  xs <- seq(min(one$lower_quantile), max(one$lower_quantile), length.out = 7)
  expect_equal(app$calculate_gamma_difference(one, gp_pkg[["A"]], xs, 80),
               calculate_gamma_difference(one, gp_pkg[["A"]], xs, 80))
})

test_that("app and package parse x-value input the same way", {
  app <- app_functions()
  for (txt in c("5.2", "-3", "[2.1, 7.8]", "[ 1 ,2 ]", "abc", "[3, 1]", "")) {
    expect_equal(suppressWarnings(suppressMessages(app$parse_x_value_input(txt))),
                 suppressWarnings(suppressMessages(parse_x_value_input(txt))), info = txt)
  }
})

test_that("app and package compute the same confidence ellipse", {
  app <- app_functions()
  cov_m <- matrix(c(2, 0.5, 0.5, 1), 2)
  expect_equal(app$create_confidence_ellipse_points(cov_m, c(1, -1), 0.9, 60),
               create_confidence_ellipse_points(cov_m, c(1, -1), 0.9, 60))
})

test_that("app and package validate draws identically", {
  app <- app_functions()
  bad <- data.frame(x = c(1, NA, 3), y = c("1", "a", "3"), v = c(1, -1, 2))
  cases <- list(
    function(f) f$check_draw_columns(bad, "x", "Fixed effect"),
    function(f) f$check_draw_columns(bad, "y", "Fixed effect"),
    function(f) f$check_draw_columns(bad[0, ], "x", "Fixed effect"),
    function(f) f$check_variance(bad$v),
    function(f) f$check_positive(-1, "multiplication_factor")
  )
  pkg_ns <- asNamespace(pkg)
  pkg_funs <- mget(c("check_draw_columns", "check_variance", "check_positive"), envir = pkg_ns)
  for (case in cases) {
    msg_app <- tryCatch({ case(app); "no error" }, error = function(e) conditionMessage(e))
    msg_pkg <- tryCatch({ case(pkg_funs); "no error" }, error = function(e) conditionMessage(e))
    expect_identical(msg_app, msg_pkg)
  }
  # and both accept valid input
  ok <- data.frame(x = c(1, 2, 3))
  expect_silent(app$check_draw_columns(ok, "x", "Fixed effect"))
  expect_silent(pkg_funs$check_draw_columns(ok, "x", "Fixed effect"))
})

test_that("app and package compute reducible imprecision identically, including the guard", {
  app <- app_functions()
  pkg_ns <- asNamespace(pkg)
  df <- data.frame(Median = c(2, 2, 1), TI_Upper = c(5, 2, 4), PI_Upper = c(4, 1, 3),
                   TI_Lower = c(-1, -1, -2), PI_Lower = c(0, 0, -1))

  for (percent in c(FALSE, TRUE)) {
    res_app <- suppressWarnings(app$add_reducible_imprecision(df, percent = percent))
    res_pkg <- suppressWarnings(pkg_ns$add_reducible_imprecision(df, percent = percent))
    expect_identical(res_app, res_pkg)
  }

  # both warn with the same text in the degenerate group (second row)
  msg_app <- tryCatch(app$add_reducible_imprecision(df), warning = function(w) conditionMessage(w))
  msg_pkg <- tryCatch(pkg_ns$add_reducible_imprecision(df), warning = function(w) conditionMessage(w))
  expect_identical(msg_app, msg_pkg)
  expect_match(msg_app, "not defined for 1 group")
})
