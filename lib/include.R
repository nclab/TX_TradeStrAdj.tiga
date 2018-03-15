# import library
library(chron)
library(MTS)
#library(lubridate)
source("lib/preprocessor.R")
source("lib/graphical.R")
source("lib/util.R")
source("lib/MTSoverwrite.R")

source("lib/accuracyVerif_model.R")
source("lib/filterByWindow_model.R")
source("lib/regression_model.R")
source("lib/gradientCompute_model.R")
source("lib/trendType_model.R")
source("lib/clustering_model.R")
source("lib/classify_model.R")
source("lib/verification_model.R")


datasets <- c('TFX')
setup <- function (res.path, datasets = c('TFX')) {
  # this path is the root directory for saving results
  res.path <<- res.path
  # load the datasets used
  for (dataset in datasets) {
    dta <- readRDS(make.path('data', paste0(dataset, '.rds')))
    assign(dataset, dta, envir=globalenv())
  }
}


# TFX <- readRDS("C:/Users/cageb/Desktop/TXtradingsystem.asga/data/TX_sample.rds")
