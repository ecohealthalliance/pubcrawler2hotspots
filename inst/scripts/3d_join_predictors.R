library(raster)
library(dplyr)


load("cache/country_df.RData")

acc_df <- as.data.frame(raster("cache/acc_hs"), xy = TRUE) %>%
  rename(lon = x, lat = y) %>%
  na.omit()

predictors <- left_join(country_df, acc_df) %>%
  select(lon, lat, pop, dalys = dalys_dist, health_exp = health_exp_dist, gdp = gdp_dist, acc_50k, diseases, diseases_dist)


# Join with two different urban land measures.
data(hotspots_drivers)

predictors <- predictors %>%
  left_join(select(predictors_old, gridid, lon, lat, urban_land)) %>%
  left_join(select(hotspots_drivers, gridid, earth9_urban))

save(predictors, file = "data/predictors.RData")
