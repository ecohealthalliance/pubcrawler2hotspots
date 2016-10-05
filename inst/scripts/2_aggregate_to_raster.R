library(raster)
library(dplyr)
library(readr)

if (interactive()) load_all()

load("cache/locations.RData")

# Methods of weighting
# w1a: Uniform
# w1b: Uniform over non-zero populations
# w2a: Mention count
# w2b: Mention count over non-zero populations
# w3: Population
# w4: Population * count

weighted <- locations %>%
  group_by(article_id) %>%
  mutate(pop_nonzero = if_else(population == 0, 0, 1),
         w1a = 1 / n(),
         w1b = w1a * pop_nonzero / sum(w1a * pop_nonzero),
         w2a = count / sum(count),
         w2b = w2a * pop_nonzero / sum(w2a * pop_nonzero),
         w3 = population / sum(population),
         w4 = population * count / sum(population * count)) %>%
  ungroup()

# There are some NAs generated with population weighting (if all matches are zero).
# In this case, we remove zero-population matches for now.
weighted[is.na(weighted)] <- 0

by_geonameid <- weighted %>%
  group_by(geonameid, latitude, longitude, name) %>%
  summarize(count = sum(count),
            w1a = sum(w1a),
            w1b = sum(w1b),
            w2a = sum(w2a),
            w2b = sum(w2b),
            w3 = sum(w3),
            w4 = sum(w4)) %>%
  ungroup()

# A lot of the top Geonames matches are in fact countries. We'll remove these.
# Actually, we should remove them at the prior stage.
countryInfo <- read_tsv("http://download.geonames.org/export/dump/countryInfo.txt",
                        comment = "#")


by_lonlat <- by_geonameid %>%
  mutate(lon = round(longitude + 0.5) - 0.5,
         lat = round(latitude + 0.5) - 0.5) %>%
  group_by(lon, lat) %>%
  summarize(count = sum(count),
            w1a = sum(w1a),
            w1b = sum(w1b),
            w2a = sum(w2a),
            w2b = sum(w2b),
            w3 = sum(w3),
            w4 = sum(w4))

# Make SpatialPointsDataFrame

by_geonameid_sp <- select(by_geonameid, -geonameid, -name)
by_geonameid_sp <- data.frame(by_geonameid_sp)
coordinates(by_geonameid_sp) <- c("longitude", "latitude")
proj4string(by_geonameid_sp) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

load("cache/template_raster.RData")
lonlat_raster <- rasterize(x = by_geonameid_sp, y = template_raster, fun = sum, mask = FALSE)


winsorize(lonlat_raster$w4)
