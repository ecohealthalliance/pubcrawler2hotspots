library(dplyr)
library(raster)
load_all()

data(hotspots_drivers)

pubs_hs <- raster("cache/pubs_hs")

acc_raw <- raster("data-raw/access_50k/acc_50k")

# acc_hs <- aggregate(acc_raw, fact = res(pubs_hs) / res(acc_raw))
acc_hs <- resample(acc_raw, pubs_hs, method = "bilinear")

writeRaster(acc_hs, "cache/acc_hs", overwrite = TRUE)
