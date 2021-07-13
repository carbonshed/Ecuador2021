#This is for ploting synoptic data
library(here)

GAVI_df <- read.csv(here::here("Synoptic/GAVI_edit_2021-07-10.csv"))
GAVI_df$Date <- as.Date(GAVI_df$Date, format="%m/%d/%Y" )
GAVI_df$Date.as.fact <- as.factor(GAVI_df$Date)
GAVI_df$Tract.as.fact <- as.factor(GAVI_df$Tract_2)


#plot
synop_mapFlux <- qmplot(Lon, Lat, data = GAVI_df, zoom = 13,  maptype = "toner-background", color = Tract_2, shape = Date.as.fact)+
  scale_color_gradient(low="blue", high="red")
synop_mapCO2 <- qmplot(Lon, Lat, data = GAVI_df, zoom = 13,  
                       maptype = "toner-background", 
                       color = Tract.as.fact, 
                       #shape = Date.as.fact,
                       size = Flux_ave)+
#  scale_color_gradient(low="blue", high="red") +
  geom_path(data=NULL, aes(x=Lon, y=Lat, group=Tract_2), size=1)
  
  geom_curve(data=GAVI_df, aes(x=Lon, y=Lat, xend = xend, yend = yend, group = Tract_2), color="black", size=1)


  geom_curve(
    mapping = aes(Tract.as.fact),
    data = NULL,
    stat = "identity",
    position = "identity",
    ...,
    curvature = 0.5,
    angle = 90,
    ncp = 5,
    arrow = NULL,
    arrow.fill = NULL,
    lineend = "butt",
    na.rm = FALSE,
    show.legend = NA,
    inherit.aes = TRUE
  )


