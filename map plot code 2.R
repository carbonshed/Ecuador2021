# uncomment the **install** lines only once

#install.packages("rgdal", type="source")

#library(rgdal)
library(raster)
library(elevatr)
library(rasterVis)
library(rgl)
library(terra)
library(sf)
library(lwgeom)
library(dplyr)
library(sp)

#get raster data
get_elev_raster()

df <- read.csv(here::here("ProcessedData/upscaleFlux_allwatersheds_oct8.csv"))%>%
  rename(name=cathment_name)%>%rename(ha = catchment_ha)
df$name <- factor(df$name, levels = c("ante", "gavi", "colm"))
my_sf_2 <- st_as_sf(df, coords = c('lon', 'lat'))


## it will guess the driver automatically based on the .shp extension
#st_write(my_sf_2, here::here("spatial_data/upscale_flux_shapefile2.shp"))

#SpatialPolygonsDataFrame example
points <-  shapefile(here::here("spatial_data/upscale_flux_shapefile2.shp"))
head(points)

rnetwork <- points[points$name=="colm",]
plot(rnetwork, main="rnetwork", axes=TRUE)
plot(points, add=TRUE)
invisible(text(coordinates(points), labels=as.character(points$ele), cex=0.8))

#elevation raster
DEM <- raster(here::here("spatial_data/AP_27774_PLR_F7180_RT2/AP_27774_PLR_F7180_RT2.dem.tif"))

plot(DEM, main="This the downloaded DEM [meters]")
plot(rnetwork, add=TRUE)

