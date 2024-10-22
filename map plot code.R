#make a map for table 10
library(viridis)
library(sf)
library(raster)
library(here)


library(tidyverse)
library(sf)
library(mapview)
library(rvest)
library(httr)
library(terra)

# 1) Download the kml file
moh_chas_clinics <- GET("https://data.gov.sg/dataset/31e92629-980d-4672-af33-cec147c18102/download",
                        write_disk(here::here("moh_chas_clinics.zip"), overwrite = TRUE))

# 2) Unzip the downloaded zip file 
unzip(here::here("moh_chas_clinics.zip"))

# 3) Read the KML file as a Spatial object
moh_chas_clinics <- read_sf(here::here("dem/AP_27774_PLR_F7180_RT2.kmz"))

# Watch data
moh_chas_clinics %>%
  glimpse()

# See map
mapview(moh_chas_clinics)

# 4) Get the attributes for each observation

# Option a) Using a simple lapply
attributes <- lapply(X = 1:nrow(moh_chas_clinics), 
                     FUN = function(x) {
                       
                       moh_chas_clinics %>% 
                         slice(x) %>%
                         pull(Description) %>%
                         read_html() %>%
                         html_node("table") %>%
                         html_table(header = TRUE, trim = TRUE, dec = ".", fill = TRUE) %>%
                         as_tibble(.name_repair = ~ make.names(c("Attribute", "Value"))) %>% 
                         pivot_wider(names_from = Attribute, values_from = Value)
                       
                     })

# Option b) Using a Parallel lapply (faster)
future::plan("multisession")

attributes <- future.apply::future_lapply(X = 1:nrow(moh_chas_clinics), 
                                          FUN = function(x) {
                                            
                                            moh_chas_clinics %>% 
                                              slice(x) %>%
                                              pull(Description) %>%
                                              read_html() %>%
                                              html_node("table") %>%
                                              html_table(header = TRUE, trim = TRUE, dec = ".", fill = TRUE) %>%
                                              as_tibble(.name_repair = ~ make.names(c("Attribute", "Value"))) %>% 
                                              pivot_wider(names_from = Attribute, values_from = Value)
                                            
                                          })

# 5) Bind the attributes to each observation as new columns
moh_chas_clinics_attr <- 
  moh_chas_clinics %>%
  bind_cols(bind_rows(attributes)) %>%
  select(-Description)

# Watch new data
moh_chas_clinics_attr %>%
  glimpse()

# New map
mapview(moh_chas_clinics_attr, 
        zcol = "CLINIC_PROGRAMME_CODE", 
        layer.name = "Clinic Programme Code")




df <- read.csv(here::here("ProcessedData/upscaleFlux_allwatersheds_oct8.csv"))
df$cathment_name <- factor(df$cathment_name, levels = c("ante", "gavi", "colm"))

df_bin5 <- df%>%filter(catchment_ha>=1) %>% mutate(new_bin = cut(log10(catchment_ha), breaks=5))
df_bin5$cathment_name <- factor(df_bin5$cathment_name, levels = c("ante", "gavi", "colm"))


df$code_names <- NA
df[df$F_CO2_molperd_eq1 <= 0,]$code_names <- "< 0"
df[df$F_CO2_molperd_eq1 > 1.12,]$code_names <- "> 1.12"

my_sf_2 <- st_as_sf(df, coords = c('lon', 'lat'))


map2 <- ggplot(my_sf_2%>%filter(catchment_ha>=1)%>%filter(F_CO2_molperd_eq1>0 & F_CO2_molperd_eq1<=1.2)) + 
  geom_sf(aes(color=F_CO2_molperd_eq1),size=.05) +
  
  scale_color_viridis(name=expression(paste("CO"[2]~"Flux [mol ", day^-1,"]")),option="viridis",trans = "log1p")+
  
  geom_point(data = df%>%filter(catchment_ha>=1)%>%filter(F_CO2_molperd_eq1>1.2), aes(x = lon, y = lat, fill=code_names), size = .05,  color = "red") +
  geom_point(data = df%>%filter(catchment_ha>=1)%>%filter(F_CO2_molperd_eq1<0), aes(x = lon, y = lat,fill=code_names), size = 1,  color = "white") +scale_fill_manual(labels = c("< 0","> 1.2"), values = c("white", "red")) + 
  theme_dark(base_size = 16)+ theme(axis.text.x = element_text(angle = 25, vjust = 1, hjust=1))


#raster
DEM <- raster(here::here("dem/AP_27774_PLR_F7180_RT2/AP_27774_PLR_F7180_RT2_VH.tif"))
DEM <- raster(here::here("dem/AP_27774_PLR_F7180_RT2/AP_27774_PLR_F7180_RT2.dem.tif"))


plot(DEM, main="Digital Surface Model - HARV")
slope <- terrain(DEM, "slope", unit="radians")
aspect <- terrain(DEM, "aspect", unit="radians")
hill <- shade(slope, aspect, 45, 270)

plot(slope, col=grey(0:100/100), legend=FALSE, mar=c(2,2,1,4))

plot(DEM, col=terrain.colors(25, alpha=0.35), add=TRUE, main="HARV DSM with Hillshade")
g_tmax_map <- ggplot(data = DEM) +
  geom_raster(aes(x = x, y = y)) +
  scale_fill_viridis_c() +
  theme_void() +
  theme(
    legend.position = "bottom"
  )

ggplot()+
  geom_tile(data = DEM, aes(x = x, y = y, fill = elevation))+
  scale_fill_gradientn(colours= c("gray", "black"))#+
#  geom_sf(data = states, fill = NA)+
#  coord_sf(xlim = xlimit, ylim = ylimit)


my_sf_1 <- st_as_sf(df_bin5, coords = c('lon', 'lat'))

map1 <- ggplot(my_sf_1%>%filter(catchment_ha>=1)) + 
  geom_sf(aes(color=new_bin),size=3) +
  scale_color_brewer(name="Catchment size [ha]",labels = c('9.0 - 16.6','16.6 - 40.0','40.0 - 57.5','57.5 - 107','107 - 199'))+ theme(axis.text.x = element_text(angle = 25, vjust = 1, hjust=1))+theme_dark(base_size = 16)+ theme(axis.text.x = element_text(angle = 25, vjust = 1, hjust=1)) #+ guides(color="none")

