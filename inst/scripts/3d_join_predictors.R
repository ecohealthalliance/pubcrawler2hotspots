library(raster)
library(dplyr)


load("cache/country_df.RData")

acc_df <- as.data.frame(raster("cache/acc_hs"), xy = TRUE) %>%
  rename(lon = x, lat = y) %>%
  na.omit()

predictors <- left_join(country_df, acc_df) %>%
  select(lon, lat, pop, dalys = dalys_dist, health_exp = health_exp_dist, gdp = gdp_dist, acc_50k, diseases = diseases_dist)

save(predictors, file = "data/predictors.RData")
