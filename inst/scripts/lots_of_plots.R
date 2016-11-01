load_all()

library(ggplot2)

pubs <- raster("cache/pubs_hires")




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

raster_plot(lonlat_raster$w1a, bg_color = "grey50")
raster_plot(hires_raster, bg_color = "grey50")











