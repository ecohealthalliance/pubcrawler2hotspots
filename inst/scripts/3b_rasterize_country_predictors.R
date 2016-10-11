library(dplyr)
library(raster)
library(rgdal)
library(readr)
library(curl)


load_all()

pubs_hs <- raster("cache/pubs_hs")
data(hotspots_drivers)
data(country_predictors)

countryinfo <- read_tsv("data-raw/countryInfo.txt", skip = 50)

country_outlines <- readOGR("data-raw/shapes_simplified_low/shapes_simplified_low.json", layer = "OGRGeoJSON")
country_outlines <- merge(country_outlines, select(countryinfo, ISO3, geonameid), by.x = "geoNameId", by.y = "geonameid")
names(country_outlines) <- tolower(names(country_outlines))
country_outlines <- merge(country_outlines, country_predictors)

# 
# country_grid <- rasterize(country_outlines, pubs_hs, field = "geonameid")
# 
# country_grid <- rasterize(country_outlines, raster(resolution = (1/4)), field = c("all_causes", "diseases", "total_usd", "gdp"))


# This code will break at some point in the future.
layers <- list()
for (layer in c("all_causes", "diseases", "total_usd", "gdp")) {
  layers[layer] <- rasterize(country_outlines, raster(resolution = 1/4), field = layer)
}

country_grid_hires <- brick(layers)
writeRaster(country_grid_hires, file = "cache/country_grid_hires", overwrite = TRUE)

# country_grid_hsr <- resample(country_grid_hires, pubs_hs)
country_grid_hs <- aggregate(country_grid_hires, res(pubs_hs)/res(country_grid_hires))


writeRaster(country_grid_hs, file = "cache/country_grid_hs", overwrite = TRUE)



country_df <- as.data.frame(country_grid_hs, xy = TRUE) %>%
  rename(lon = x, lat = y) %>%
  right_join(select(hotspots_drivers, lon, lat, iso3, pop))


# pred <- hotspots_drivers %>%
#   select(lon, lat, iso3, pop) %>%
#   left_join(country_df)

# Distribute country vars by pop
country_df <- country_df %>%
  group_by(iso3) %>%
  mutate(pop_dist = pop / sum(pop)) %>%
  ungroup() %>%
  mutate(dalys_dist = all_causes * pop,
         health_exp_dist = total_usd * pop,
         gdp_dist = gdp * pop,
         diseases_dist = diseases * pop_dist)

save(country_df, file = "cache/country_df.RData")
