library(sf)
library(ggplot2)
library(viridis)
library(plotly)

nc <- st_read(system.file("shape/nc.shp", package="sf"))
View(nc)
g1 <- ggplot(nc) + geom_sf(aes(fill=SID74)) +
  scale_fill_viridis() + theme_bw()
ggplotly(g1)

library(leaflet) # 4326
st_crs(nc)$epsg #4267

ncll <- st_transform(nc, 4326)
leaflet(ncll)
