# Przemyslaw Biecek, Tues 9-12:30pm "Shem-ez-lav Biee-tek"
browseURL("https://github.com/pbiecek/DALEX_docs/tree/master/workshops/UseR2018")
PB_pkgs <- c("DALEX", "breakDown", "ceterisParibus", 
             "live", "randomForest", "auditor")
install.packages(PB_pkgs,dependencies = T) 
#Also see modelDown!
devtools::install_github("MI2DataLab/modelDown")

# Stephanie Kovalchik, Tues 1:30-5pm
browseURL("https://github.com/skoval/UseR2018")
SK_pkgs <- c("rvest", "jsonlite", "dplyr", "tidyr", "stringr", "ggplot2",
             "ggthemes", "scales", "lubridate", "BradleyTerry2", "pitchRx",
             "mgcv", "rjags") 
  #as per README.md, different from abstract.pdf.
install.packages(SK_pkgs)
devtools::install_github("ropensci/RSelenium")
devtools::install_github("skoval/deuce")
browseURL("https://docs.docker.com/install/")
  # NOT AVAIABLE ON WINDOWS 10 HOME




# Carson Sivert, Wed 9-12:30pm
browseURL("https://tutorials.cpsievert.me/20180711/#1")
install.packages("plotly")
# FOR DEMO() to work:

devtools::install_github("ropensci/plotly")
CS_pkgs <- c("engsoccerdata", "dplyr", "tidyr", "plotly")
install.packages(CS_pkgs)
demo("crosstalk-highlight-epl-2", package = "plotly")



### Trouble with dev ver of plotly or demo()?
# Download devtools if not already installed:
if (!is.element("devtools", installed.packages()[,1])) {
  install.packages("devtools", dep = TRUE)
  require(p, character.only = TRUE)
} else message("Package already installed.")

# Download dev version of plotly (to be submitted to CRAN later July 2018.)
devtools::install_github("ropensci/plotly", dep = TRUE)

# Try to run the demo code. If you have dependancy issues try the next line.
demo("crosstalk-highlight-epl-2", package = "plotly")

# If you still have dependancy issues, try:
plotly_demo_pkgs <- c("engsoccerdata", "dplyr", "tidyr", "plotly")
install.packages(plotly_demo_pkgs)



