# Creates the images of the user guide by running the real bayesintervals Shiny app
# in headless Edge/Chrome on the synthetic example data in example_data/.
#
# Requirements: bayesintervals (installed), chromote, processx, ggplot2, ragg.
# Set CHROMOTE_CHROME to the browser executable if it is not found automatically.
#   Rscript make_screenshots.R

suppressMessages({ library(chromote); library(processx) })
if (!nzchar(Sys.getenv("CHROMOTE_CHROME"))) {
  edge <- "C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe"
  if (file.exists(edge)) Sys.setenv(CHROMOTE_CHROME = edge)
}

here <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile)), error = function(e) getwd())
if (!dir.exists(file.path(here, "example_data"))) here <- getwd()
data_dir <- file.path(here, "example_data")
img_dir <- file.path(here, "images")
dir.create(img_dir, showWarnings = FALSE)
port <- 4399

# ---- helpers ----------------------------------------------------------------------------
start_app <- function() {
  p <- process$new(file.path(R.home("bin"), "Rscript.exe"),
                   c("-e", sprintf("bayesintervals::start_app_view(port = %d, launch.browser = FALSE)", port)),
                   stdout = tempfile(), stderr = tempfile())
  for (i in 1:60) {
    ok <- tryCatch({ con <- url(sprintf("http://127.0.0.1:%d", port)); readLines(con, n = 1, warn = FALSE); close(con); TRUE },
                   error = function(e) FALSE)
    if (ok) break
    Sys.sleep(1)
  }
  p
}
open_page <- function(width = 1500, height = 1750) {
  b <- ChromoteSession$new(width = width, height = height)
  b$Page$navigate(sprintf("http://127.0.0.1:%d", port))
  Sys.sleep(5)
  b
}
js <- function(b, code) b$Runtime$evaluate(code, returnByValue = TRUE)$result$value
upload <- function(b, path) {
  doc <- b$DOM$getDocument()
  node <- b$DOM$querySelector(doc$root$nodeId, "#file")
  b$DOM$setFileInputFiles(files = list(normalizePath(path, winslash = "/")), nodeId = node$nodeId)
  Sys.sleep(3)
}
click <- function(b, selector, wait = 2) { js(b, sprintf("document.querySelector('%s').click(); true", selector)); Sys.sleep(wait) }
set_select <- function(b, id, values) {
  v <- paste0("[", paste(sprintf("'%s'", values), collapse = ","), "]")
  js(b, sprintf("(function(){var el=$('#%s')[0]; if(el.selectize){el.selectize.setValue(%s);} else {$(el).val(%s).trigger('change');} return true;})()", id, v, v))
  Sys.sleep(1)
}
set_num <- function(b, id, value) { js(b, sprintf("$('#%s').val('%s').trigger('input').trigger('change'); true", id, value)); Sys.sleep(1) }
set_slider <- function(b, id, value) {
  js(b, sprintf("var s=$('#%s').data('ionRangeSlider'); s.update({from:%s}); $('#%s').trigger('change'); true", id, value, id)); Sys.sleep(0.5)
}
tab <- function(b, name) { js(b, sprintf("$('a[data-value=\"%s\"]').tab('show'); true", name)); Sys.sleep(2) }
png_path <- function(name) file.path(img_dir, name)
shot_sel <- function(b, name, selector, expand = 8, wait = 1) {
  # "#main_tabs" is only the tab strip; capture it together with the visible tab content
  if (identical(selector, "#main_tabs")) {
    return(shot_between(b, name, "$('#main_tabs')", "$('.tab-content')", pad = 6, wait = wait))
  }
  Sys.sleep(wait); b$screenshot(png_path(name), selector = selector, expand = expand); invisible(name)
}
# union rectangle (page coordinates) of two elements given as jQuery expressions
shot_between <- function(b, name, expr_from, expr_to, pad = 10, wait = 1) {
  Sys.sleep(wait)
  r <- js(b, sprintf("(function(){var a=(%s)[0].getBoundingClientRect(), z=(%s)[0].getBoundingClientRect();
    return [Math.min(a.left,z.left)+window.scrollX, a.top+window.scrollY, Math.max(a.right,z.right)-Math.min(a.left,z.left), z.bottom-a.top];})()", expr_from, expr_to))
  r <- unlist(r)
  b$screenshot(png_path(name), cliprect = c(r[1] - pad, r[2] - pad, r[3] + 2 * pad, r[4] + 2 * pad))
  invisible(name)
}
add_widget_wait <- function() Sys.sleep(5)

# The module flowchart (images/00_module_flowchart.png) is made by make_module_flowchart.R


# ---- 1. Example 1: results by week (columns Week, Intercept, Baseline, TreatB, Residual.Var) ----
p <- start_app(); on.exit(try(p$kill(), silent = TRUE), add = TRUE)
b <- open_page()
shot_sel(b, "01_app_start.png", "html", expand = 0)
upload(b, file.path(data_dir, "example_by_week_mcmc.csv"))
click(b, "#load_data", wait = 4)
shot_sel(b, "02_data_preview.png", "#main_tabs")
tab(b, "Warning & Errors")
shot_sel(b, "03_warnings_tab.png", "#main_tabs")
tab(b, "Data Preview")

set_select(b, "fixed_var", c("Intercept", "TreatB"))
set_select(b, "covariate_1", "Baseline")
set_num(b, "covariate_value_1", 5)
set_select(b, "random_var", "Residual.Var")
set_select(b, "by", "Week")
shot_between(b, "04_model_setup.png", "$('#fixed_var').closest('.form-group')", "$('#by').closest('.form-group')")
tab(b, "Formula")
shot_sel(b, "05_formula_tab.png", "#main_tabs")

shot_between(b, "06_interval_settings.png", "$('#tolerance_level').closest('.form-group')", "$('#percent_for_ci').closest('.form-group')")
shot_sel(b, "07_calculate_button.png", "#calculate_ci", expand = 6)
click(b, "#calculate_ci", wait = 8)
shot_sel(b, "08_intervals_tab.png", "#main_tabs")
js(b, "$('#show_reducible').click(); true"); Sys.sleep(6)
shot_sel(b, "09_reducible_imprecision.png", "#main_tabs")
js(b, "$('#show_reducible').click(); true"); Sys.sleep(3)

click(b, "#plot_quantiles", wait = 14)
# show the confidence ellipses of Weeks 2 and 24 and the Krishnamoorthy plot of Week 24
js(b, "$('#show_scatter_2').click(); $('#show_scatter_24').click(); true"); Sys.sleep(4)
js(b, "$('input[name=krishnamoorthy_selected_by][value=24]').click(); true"); Sys.sleep(8)
shot_sel(b, "10_two_sided_tab.png", "#main_tabs")

shot_sel(b, "11_ppd_button.png", "#plot_distribution", expand = 6)
click(b, "#plot_distribution", wait = 8)
shot_sel(b, "12_ppd_controls.png", "#main_tabs")
js(b, "$('#show_24').click(); true"); Sys.sleep(2)
set_num(b, "points_24", 100)
set_num(b, "x_value_24", 2)
Sys.sleep(6)
shot_sel(b, "13_ppd_plot.png", "#main_tabs")
b$close()
try(p$kill(), silent = TRUE)

# ---- 2. Example 2: repeated measures with a random patient effect ------------------------------
p <- start_app(); on.exit(try(p$kill(), silent = TRUE), add = TRUE)
b <- open_page()
upload(b, file.path(data_dir, "example_repeated_measures_mcmc.csv"))
click(b, "#load_data", wait = 4)
set_select(b, "fixed_var", c("Intercept", "TreatB", "Visit4", "TreatB.Visit4"))
set_select(b, "covariate_1", "Baseline")
set_num(b, "covariate_value_1", 5)
set_select(b, "random_var", c("Random.Var", "Residual.Var"))
shot_between(b, "14_model_setup_random_effects.png", "$('#fixed_var').closest('.form-group')", "$('#by').closest('.form-group')")
tab(b, "Formula")
shot_sel(b, "15_formula_random_effects.png", "#main_tabs")
click(b, "#calculate_ci", wait = 8)
shot_sel(b, "16_intervals_random_effects.png", "#main_tabs")
b$close()
try(p$kill(), silent = TRUE)
cat("images written to", img_dir, ":\n"); print(list.files(img_dir))
