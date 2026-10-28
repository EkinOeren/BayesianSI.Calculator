# Redraws the two flowcharts of the original documentation in the same style, with corrections:
# typos fixed, outdated entries updated (dist_data removed, intervals_plot_upper/_lower), and the
# two-sided tolerance interval module added (V 3.0). Output goes to images/*_v3.png for review.
suppressMessages({ library(ggplot2); library(grid) })
here <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile)), error = function(e) getwd())
img_dir <- file.path(here, "images")
edge <- "#2f4b8f"; line_col <- "#555555"

pill <- function(cx, cy, w, h, fill, label, tcol = "white", size = 8.6, border = edge) {
  list(annotation_custom(roundrectGrob(r = unit(0.5, "snpc"), gp = gpar(fill = fill, col = border, lwd = 2)),
                         xmin = cx - w/2, xmax = cx + w/2, ymin = cy - h/2, ymax = cy + h/2),
       annotate("text", x = cx, y = cy, label = label, colour = tcol, fontface = "bold", size = size, lineheight = 0.95))
}
arrow_path <- function(x, y, col = line_col)
  geom_path(data = data.frame(x = x, y = y), aes(x, y), colour = col, linewidth = 1.3,
            arrow = arrow(length = unit(0.16, "inches"), type = "closed"), lineend = "butt", linejoin = "mitre")
path_only <- function(x, y, col = line_col)
  geom_path(data = data.frame(x = x, y = y), aes(x, y), colour = col, linewidth = 1.3)

# ---------------------------------------------------------------- module flowchart
H <- 2000
cols <- c(F = "#8ff0ff", hF = "#8ff0ff", E = "#fbe3c8", hE = "#fbe3c8", R = "#ffd2b8", hR = "#ffd2b8", O = "#ff8fc8", hO = "#ff8fc8", D = "#ffd2b8", hD = "#ffd2b8")
box_text <- function(x0, x1, y0, y1, items, step = 36, size = 8) {
  # items: character vector like "hF:Functions:" ; "" = blank line. y given in image pixels (top = small)
  n <- length(items); cx <- (x0 + x1) / 2; cyc <- (y0 + y1) / 2
  ys <- H - (cyc + ((seq_len(n) - 1) - (n - 1) / 2) * step)
  keep <- nzchar(items)
  type <- sub(":.*$", "", items[keep]); lab <- sub("^[^:]*:", "", items[keep])
  d <- data.frame(x = cx, y = ys[keep], label = lab, col = cols[type], face = ifelse(startsWith(type, "h"), "bold", "plain"), stringsAsFactors = FALSE)
  geom_text(data = d, aes(x, y, label = label, colour = col, fontface = face), size = size, inherit.aes = FALSE)
}
module <- function(m) {
  list(annotate("rect", xmin = m$box[1], xmax = m$box[2], ymin = H - m$box[4], ymax = H - m$box[3], fill = m$boxfill),
       pill(m$pill[1], H - m$pill[2], m$pill[3], m$pill[4], m$pillfill, m$title),
       box_text(m$box[1], m$box[2], m$box[3], m$box[4], m$items))
}
M <- list(
  data = list(title = "Data Loading and\nHandling", pill = c(1720, 77, 300, 112), pillfill = "#262626", boxfill = "#808080", box = c(1365, 1870, 135, 945),
    items = c("hF:Functions:", "F:by_updater()", "F:inspect_data()", "", "hE:Events:", "E:Observe event for Load Data button", "E:Observe event for input by", "",
              "hR:Reactive Values", "R:InputData", "R:DataInspectionResults", "R:originalByColumns", "R:byHelpText", "R:byHelpTextValue", "R:firstInputGiven", "R:reactivity_paused", "",
              "hO:Outputs:", "O:output$data_preview", "O:output$byHelpText")),
  formula = list(title = "Formula\nGeneration", pill = c(1211, 190, 255, 112), pillfill = "#262626", boxfill = "#808080", box = c(925, 1338, 248, 733),
    items = c("hF:Functions:", "F:formulaString()", "", "hE:Events:", "E:Observe events for fixed variable,", "E:random variable, covariates and", "E:multiplication factor inputs", "",
              "hR:Reactive Values:", "R:FormulaGenerated", "", "hO:Outputs:", "O:output$formula_output")),
  bg = list(title = "Background Statistical\nCalculations", pill = c(703, 410, 335, 140), pillfill = "#7030a0", boxfill = "#9170ab", box = c(455, 870, 467, 1046),
    items = c("hF:Functions:", "F:calculate_predicted_distributions()", "F:check_draw_columns()", "F:check_variance()  check_positive()", "", "hE:Events:",
              "E:Observe events for reactivity,", "E:credibility of tolerance and", "E:multiplication factor", "", "hR:Reactive Values:", "R:ConfidenceOfTolerance", "R:MultiplicationFactorVariances", "R:predictedDistributions")),
  ppdcalc = list(title = "Posterior Predictive\nCalculations", pill = c(229, 600, 335, 140), pillfill = "#843c0c", boxfill = "#bf5511", box = c(22, 395, 673, 1107),
    items = c("hF:Reactive Function:", "F:ForCDF1Dataframe()", "", "hE:Events:", "E:click event on \"Posterior", "E:Predictive Distribution\" button")),
  ppddisp = list(title = "Posterior Predictive\nDisplay", pill = c(694, 1163, 335, 112), pillfill = "#843c0c", boxfill = "#bf5511", box = c(106, 860, 1220, 1918),
    items = c("hF:Functions:", "F:parse_x_value_input()", "", "hE:Events:", "E:ForCDF1Dataframe-dependent monitoring of by-value", "E:controls to generate the dynamic outputs in real time.", "",
              "hD:Dynamic Outputs:", "D:cdf_plot_<by>", "D:cdf_table_<by>", "", "hO:Outputs:", "O:output$distribution_plots", "O:output$by_value_controls", "O:output$cdf_plots")),
  intcalc = list(title = "Intervals\nCalculations", pill = c(1178, 811, 280, 112), pillfill = "#375623", boxfill = "#548235", box = c(905, 1320, 868, 1918),
    items = c("hF:Functions:", "F:performCalculations()", "F:calculate_ci()", "F:calculate_tolerance_interval()", "F:calculate_PI()", "", "hE:Events:",
              "E:Calculate Intervals button click.", "E:Observe events for reactivity", "E:paused, input changes, and", "E:timer debounce.", "",
              "hR:Reactive Values:", "R:CICalculated", "R:intervalCalculationInitiated", "R:input_changed", "R:last_change_time", "R:update_timer", "R:IntervalsOutputInitial")),
  intdisp = list(title = "Intervals Display", pill = c(1730, 1046, 280, 112), pillfill = "#375623", boxfill = "#548235", box = c(1365, 1870, 1107, 1918),
    items = c("hF:Functions:", "F:lognorm_adjuster()", "F:initializeDataTable()", "", "hE:Events:", "E:Observe events for initial interval", "E:output as well as show_reducible and", "E:log-normal checkbox inputs", "",
              "hR:Reactive Values:", "R:IntervalsOutputActual", "R:plot_data", "", "hO:Outputs:", "O:output$ci_output", "O:output$intervals_plot_upper", "O:output$intervals_plot_lower")),
  two = list(title = "Two-Sided Tolerance\nIntervals (new in V 3.0)", pill = c(2190, 1046, 400, 112), pillfill = "#0f4c5c", boxfill = "#2a7f8f", box = c(1990, 2390, 1107, 1918),
    items = c("hF:Functions:", "F:calculate_gP_mean()", "F:calculate_gamma_difference()", "F:find_optimal_point()", "F:update_ti_values()", "", "hE:Events:", "E:click event on \"Two-Sided", "E:Tolerance Intervals\" button", "",
              "hR:Reactive Values:", "R:optimalPointResults", "R:originalOptimalPointResults", "R:two_sided_ti_initiated", "R:krishnamoorthy_selected_by", "", "hO:Outputs:", "O:output$quantile_scatter_plot", "O:output$krishnamoorthy_plot", "O:output$quantile_ci_output"))
)
Y <- function(y) H - y
p <- ggplot() +
  lapply(M, module) +
  # arrows (image pixel coordinates, y flipped)
  path_only(c(1570, 1211), Y(c(77, 77))) + arrow_path(c(1211, 1211), Y(c(77, 134))) +
  path_only(c(1083, 703), Y(c(190, 190))) + arrow_path(c(703, 703), Y(c(190, 340))) +
  path_only(c(870, 895, 895), Y(c(410, 410, 811))) + arrow_path(c(895, 1036), Y(c(811, 811))) +
  path_only(c(536, 229), Y(c(410, 410))) + arrow_path(c(229, 229), Y(c(410, 530))) +
  path_only(c(396, 430, 430, 694), Y(c(600, 600, 1071, 1071)), "#4472c4") + arrow_path(c(694, 694), Y(c(1071, 1108)), "#4472c4") +
  path_only(c(1320, 1345, 1345), Y(c(811, 811, 1046))) + arrow_path(c(1345, 1590), Y(c(1046, 1046))) +
  arrow_path(c(1870, 1990), Y(c(1046, 1046)), "#0f4c5c") +
  scale_colour_identity() + coord_fixed(xlim = c(0, 2400), ylim = c(0, H), expand = FALSE) + theme_void() +
  theme(plot.background = element_rect(fill = "white", colour = NA))
ragg::agg_png(file.path(img_dir, "flowchart_modules_v3.png"), width = 2400, height = H, res = 72)
print(p); invisible(dev.off())

# ---------------------------------------------------------------- user workflow
blue <- "#3d64ad"; green <- "#548235"; bluelt <- "#4472c4"
HW <- 758
Yw <- function(y) HW - y
node <- function(cx, cy, w, h, fill, label, circle = FALSE) {
  r <- if (circle) unit(0.5, "snpc") else unit(0.5, "snpc")
  list(annotation_custom(roundrectGrob(r = r, gp = gpar(fill = fill, col = if (circle) "#bfbfbf" else edge, lwd = 2.5)),
                         xmin = cx - w/2, xmax = cx + w/2, ymin = Yw(cy) - h/2, ymax = Yw(cy) + h/2),
       annotate("text", x = cx, y = Yw(cy), label = label, colour = "white", size = 11, lineheight = 0.98))
}
q <- ggplot() +
  node(155, 313, 290, 98, blue, "Loading Data") +
  node(585, 315, 400, 172, blue, "Defining your Linear\nMixed Model") +
  node(914, 139, 402, 172, blue, "Configuring Statistical\nIntervals") +
  node(1360, 139, 258, 258, green, "Visualizing\nStatistical\nIntervals", TRUE) +
  node(1760, 139, 258, 258, "#0f4c5c", "Visualizing\nTwo-Sided\nTolerance\nIntervals\n(new in V 3.0)", TRUE) +
  node(1360, 463, 258, 258, green, "Visualizing\nPosterior\nPredictive\nDistribution", TRUE) +
  node(1787, 463, 400, 172, blue, "Configuring Posterior\nPredictive Distribution\nAccuracy") +
  node(1787, 668, 340, 156, bluelt, "Investigating\nSpecific X Values") +
  arrow_path(c(290, 385), Yw(c(313, 313)), bluelt) +
  path_only(c(585, 585), Yw(c(229, 139)), bluelt) + arrow_path(c(585, 713), Yw(c(139, 139)), bluelt) +
  arrow_path(c(1115, 1231), Yw(c(139, 139)), bluelt) +
  arrow_path(c(1489, 1630), Yw(c(139, 139)), "#0f4c5c") +
  path_only(c(787, 808, 808), Yw(c(315, 315, 463)), bluelt) + arrow_path(c(808, 1231), Yw(c(463, 463)), bluelt) +
  path_only(c(914, 914), Yw(c(225, 463)), bluelt) +
  arrow_path(c(1489, 1587), Yw(c(463, 463)), bluelt) +
  arrow_path(c(1787, 1787), Yw(c(549, 590)), bluelt) +
  path_only(c(1620, 1360), Yw(c(668, 668)), bluelt) + arrow_path(c(1360, 1360), Yw(c(668, 592)), bluelt) +
  coord_fixed(xlim = c(0, 2000), ylim = c(0, HW), expand = FALSE) + theme_void() +
  theme(plot.background = element_rect(fill = "white", colour = NA))
ragg::agg_png(file.path(img_dir, "flowchart_user_workflow_v3.png"), width = 2000, height = HW, res = 72)
print(q); invisible(dev.off())
cat("written\n")
