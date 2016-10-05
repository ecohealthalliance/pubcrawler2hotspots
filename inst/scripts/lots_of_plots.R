quickmap(by_lonlat, count)
quickmap(by_lonlat, w1a)
quickmap(by_lonlat, w1b)
quickmap(by_lonlat, w2a)
quickmap(by_lonlat, w2b)
quickmap(by_lonlat, w3)
quickmap(by_lonlat, w4)

quickmap(by_lonlat, quantvar(count))
quickmap(by_lonlat, quantvar(w1a))
quickmap(by_lonlat, quantvar(w1b))
quickmap(by_lonlat, quantvar(w2a))
quickmap(by_lonlat, quantvar(w2b))
quickmap(by_lonlat, quantvar(w3))
quickmap(by_lonlat, quantvar(w4))

quickmap(by_lonlat, winsorize(count))
quickmap(by_lonlat, winsorize(w1a))
quickmap(by_lonlat, winsorize(w1b))
quickmap(by_lonlat, winsorize(w2a))
quickmap(by_lonlat, winsorize(w2b))
quickmap(by_lonlat, winsorize(w3))
quickmap(by_lonlat, winsorize(w4))


by_lonlat_hires <- by_geonameid %>%
  mutate(lon = round(longitude * 2) / 2,
         lat = round(latitude * 2) / 2) %>%
  group_by(lon, lat) %>%
  summarize(count = sum(count),
            w1a = sum(w1a),
            w1b = sum(w1b),
            w2a = sum(w2a),
            w2b = sum(w2b),
            w3 = sum(w3),
            w4 = sum(w4))




quickmap(by_lonlat_hires, count)
quickmap(by_lonlat_hires, w1a)
quickmap(by_lonlat_hires, w1b)
quickmap(by_lonlat_hires, w2a)
quickmap(by_lonlat_hires, w2b)
quickmap(by_lonlat_hires, w3)
quickmap(by_lonlat_hires, w4)

quickmap(by_lonlat_hires, quantvar(count))
quickmap(by_lonlat_hires, quantvar(w1a))
quickmap(by_lonlat_hires, quantvar(w1b))
quickmap(by_lonlat_hires, quantvar(w2a))
quickmap(by_lonlat_hires, quantvar(w2b))
quickmap(by_lonlat_hires, quantvar(w3))
quickmap(by_lonlat_hires, quantvar(w4))

quickmap(by_lonlat_hires, winsorize(count))
quickmap(by_lonlat_hires, winsorize(w1a))
quickmap(by_lonlat_hires, winsorize(w1b))
quickmap(by_lonlat_hires, winsorize(w2a))
quickmap(by_lonlat_hires, winsorize(w2b))
quickmap(by_lonlat_hires, winsorize(w3))
quickmap(by_lonlat_hires, winsorize(w4, 100))

library(GGally)
ggpairs(by_lonlat_hires[c(-1, -2)])