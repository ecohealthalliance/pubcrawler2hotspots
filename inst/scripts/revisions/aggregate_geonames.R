# Objective: Aggregate GeoNames allCountries file to the Hotspots grid to use as
# an offset in a Poisson model.

library(raster)
library(dplyr)
library(readr)
library(purrr)

data(template_raster)

# Load GeoNames data
path_to_allCountries <- "/Volumes/Transcend/datasets/geonames/2017-03-03/allCountries.zip"

col_names <- c("geonameid",
               "name",
               "asciiname",
               "alternatenames",
               "latitude",
               "longitude",
               "feature_class",
               "feature_code",
               "country_code",
               "cc2",
               "admin1_code",
               "admin2_code",
               "admin3_code",
               "admin4_code",
               "population",
               "elevation",
               "dem",
               "timezone",
               "modification_date")
col_types <- cols(geonameid = col_integer(),
                  name = col_character(),
                  asciiname = col_character(),
                  alternatenames = col_character(),
                  latitude = col_double(),
                  longitude = col_double(),
                  feature_class = col_character(),
                  feature_code = col_character(),
                  country_code = col_character(),
                  cc2 = col_character(),
                  admin1_code = col_character(),
                  admin2_code = col_character(),
                  admin3_code = col_character(),
                  admin4_code = col_character(),
                  population = col_integer(),
                  elevation = col_integer(),
                  dem = col_integer(),
                  timezone = col_character(),
                  modification_date = col_date(format = ""))
skip_nonessential_cols <- cols(geonameid = col_integer(),
                          name = col_skip(),
                          asciiname = col_skip(),
                          alternatenames = col_skip(),
                          latitude = col_double(),
                          longitude = col_double(),
                          feature_class = col_skip(),
                          feature_code = col_skip(),
                          country_code = col_character(),
                          cc2 = col_skip(),
                          admin1_code = col_skip(),
                          admin2_code = col_skip(),
                          admin3_code = col_skip(),
                          admin4_code = col_skip(),
                          population = col_skip(),
                          elevation = col_skip(),
                          dem = col_skip(),
                          timezone = col_skip(),
                          modification_date = col_skip())


geonames_raw <- read_tsv(file = path_to_allCountries,
                         col_names = col_names,
                         col_types = col_types,
                         quote = "",
                         na = character())

geonames_raw <- read_delim(file = path_to_allCountries,
                           delim = "\t",
                           col_names = col_names,
                           col_types = col_types,
                           quote = "",
                           na = character())

# Remove blocked entities to bring in line with ingested articles.
blocklist <- list.files("data-raw/blocklists", full.names = TRUE) %>%
  map(read_csv, col_types = "cc") %>%
  reduce(rbind)
blocklist2 <- c("American River", "Candida", "Research", "Centre", "Sigma", "Normal",
                "Middle", "Tukey", "The World", "Golgi", "Male", "Horizontal", "Teaching
                Hospital", "Cancer", "Altogether", "Delta", "Excel", "Chicken", "Basic",
                "Scheme")
geonames_filtered <- geonames_raw %>%
  filter(!is.na(longitude) & !is.na(latitude)) %>%
  filter(!(geonameid %in% blocklist$geonameid)) %>%
  filter(!(name %in% blocklist2))

# Convert to SpatialPixelsDataFrame; aggregate to template_raster.
geonames_spdf <- geonames_filtered
coordinates(geonames_spdf) <- c("longitude", "latitude")
geonames_raster <- rasterize(geonames_spdf, template_raster,
                                 field = "geonameid",
                                 fun = "count",
                                 background = 0,
                                 silent = FALSE)


writeRaster(geonames_raster, file = "cache/geonames_raster", overwrite = TRUE)
geonames_df <- as.data.frame(geonames_raster, xy = TRUE) %>%
  rename(lon = x, lat = y, geonames = layer) %>%
  inner_join(dplyr::select(hotspots_drivers, lon, lat))
quickmap(geonames_df, log(geonames))

save(geonames_df, file = "data/geonames_df.RData")
