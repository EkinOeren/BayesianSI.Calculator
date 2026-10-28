# bayesintervals user guide (V 3.0)

* `index.html` - the guide (published by GitHub Pages) (one self-contained file; open it in a browser)
* `bayesintervals_user_guide.qmd` - its source (render with Quarto; needs the bayesintervals package)
* `images/` - the pictures of the guide: the two flowcharts (redrawn from the originals for V 3.0) and screenshots made by running the application on the example data
* `code_samples/` - the function sources shown in the "Code Sample" dropdowns, taken from `inst/shiny/app.R`
* `example_data/` - synthetic example MCMC files and the script that creates them
* `make_screenshots.R` - recreates the screenshots (needs chromote, processx and a Chromium browser)
* `make_redrawn_flowcharts.R` - recreates the two flowcharts (needs ggplot2, ragg)

All data are simulated; no real study data were used.
