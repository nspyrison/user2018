# Download devtools if not already installed.
if (!is.element("devtools", installed.packages()[,1])) {
  install.packages("devtools", dep = TRUE)
  require(p, character.only = TRUE)
} else message("Package already installed.")

# Download dev version of plotly (to be submitted to CRAN later July 2018).
devtools::install_github("ropensci/plotly", dep = TRUE)

# Try to run the demo code. If you have dependency issues try the next line.
demo("crosstalk-highlight-epl-2", package = "plotly")

# If you still have dependency issues run the below and then run demo() again.
plotly_demo_pkgs <- c("engsoccerdata", "dplyr", "tidyr", "plotly")
install.packages(plotly_demo_pkgs)