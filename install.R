options(repos = c("CRAN" = "http://cran.ma.imperial.ac.uk"))
install.packages(c("tidyverse", "reshape2", "gridExtra", "pastecs"))

# start: set wd to folder containing server.R setwd("~/courses/cruk/IntroductionToStatisticalAnalysis/git_ShinyAppTwoSampleTest")
shiny::runApp(launch.browser = TRUE)