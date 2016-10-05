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

locations_list <- foreach(d = datadirs) %dopar% {
  bind_csvs(d)
}

locations <- bind_rows(locations_list)

save(locations, file = "cache/locations.RData")
save(locations_list, file = "cache/locations_list.RData")

