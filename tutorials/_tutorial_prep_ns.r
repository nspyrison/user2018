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
