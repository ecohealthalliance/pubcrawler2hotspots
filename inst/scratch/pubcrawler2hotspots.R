library(raster)
library(readr)
library(purrr)
library(dplyr)
library(foreach)
library(doParallel)

# Functions
bind_csvs <- function(datadir, col_types = "icddcdic") {
  files <- list.files(datadir, recursive = TRUE, full.names = TRUE)
  bound <- files %>%
    map(read_csv, col_types = col_types) %>%
    reduce(rbind)
  return(bound)
}

# Script
registerDoParallel()


datadir <- "../pubcrawler/export/2016-10-03_14-26-52"
list.files(datadir)
datadirs <- file.path(datadir, list.files(datadir))

datadirs

locations <- foreach(d = datadirs, .combine = rbind) %dopar% {
  bind_csvs(d)
}

save(locations, file = "data/locations.RData")
