
#Geomorphology for GAVI
library(here)
library(ggplot2)
library(tidyverse)
library(lubridate)
library("plot3D")
library(dplyr)
library(geosphere)
library(ggmap)


synop <- read.csv(here::here("Synoptic/GAVI_mainstem_2021-08-29Edit.csv"))

#merge waypoints and tracks

##WAYPPINTS

GAVI_geo_waypoints <-  read.csv(here::here("/Geomorphology/Gavilan/GAVI_AllWaypts.csv"), skip=22, header = TRUE, sep = ",",
                      na.strings=c("","NA"), quote = "\"",dec = ".", fill = TRUE, comment.char = "")[(1:26),c(2:5,8)]
colnames(GAVI_geo_waypoints) <- c("lat_wypt","lon_wypt","ele_wypt","time","name")

#convert time in UTC to local time
GAVI_geo_waypoints$time <- gsub("T"," ", GAVI_geo_waypoints$time)
GAVI_geo_waypoints$time <- gsub("Z","", GAVI_geo_waypoints$time)

GAVI_geo_waypoints$time <- as.POSIXct(GAVI_geo_waypoints$time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC" )
GAVI_geo_waypoints$time <-GAVI_geo_waypoints$time - 5*60*60 
#round to nearest half second
GAVI_geo_waypoints$time <- round_date(GAVI_geo_waypoints$time, unit = "30 second")

#not automatically formating in numeric
GAVI_geo_waypoints$lat_wypt <- as.numeric(format(GAVI_geo_waypoints$lat_wypt, digits = 20, nsmall = 20))
GAVI_geo_waypoints$lon_wypt <- as.numeric(format(GAVI_geo_waypoints$lon_wypt, digits = 20, nsmall = 20))
GAVI_geo_waypoints$ele_wypt <- as.numeric(format(GAVI_geo_waypoints$ele_wypt, digits = 20, nsmall = 20))
GAVI_geo_waypoints$name <- sub('.', '', GAVI_geo_waypoints$name)

#merge in distance btw wayppints
#ANTE_geoDist <-  read.csv(here::here("/Geomorphology/Atenas/ANTE_Wypt_and_dist.csv"), skip=3, header = TRUE, sep = ",",
#                          na.strings=c("","NA"), quote = "\"",dec = ".", fill = TRUE, comment.char = "")
#ANTE_geoDist$name <- as.character(ANTE_geoDist$name)

#ANTE_geo_waypoints <- full_join(ANTE_geo_waypoints,ANTE_geoDist, by = "name")
#rm(ANTE_geoDist)

##deal with TRACKS

GAVI_geo_tracks1 <-  read.csv(here::here("/Geomorphology/Gavilan/GAVI_2021-07-26 09_49_22 DAY.csv"), skip=42, header = TRUE, sep = ",",
                            na.strings=c("","NA"), quote = "\"",dec = ".", fill = TRUE, comment.char = "")[,1:6]
GAVI_geo_tracks2 <-  read.csv(here::here("/Geomorphology/Gavilan/GAVI_2021-07-27 10_13_40 DAY.csv"), skip=42, header = TRUE, sep = ",",
                             na.strings=c("","NA"), quote = "\"",dec = ".", fill = TRUE, comment.char = "")[,1:6]
GAVI_geo_tracks3 <-  read.csv(here::here("/Geomorphology/Gavilan/GAVI_2021-07-28 14_01_03 DAY.csv"), skip=42, header = TRUE, sep = ",",
                             na.strings=c("","NA"), quote = "\"",dec = ".", fill = TRUE, comment.char = "")[,1:6]


GAVI_geo_tracks <- rbind(GAVI_geo_tracks1,GAVI_geo_tracks2,GAVI_geo_tracks3)
rm(GAVI_geo_tracks1,GAVI_geo_tracks2,GAVI_geo_tracks3)

#convert time in UTC to local time
GAVI_geo_tracks$time <- gsub("T"," ", GAVI_geo_tracks$time)
GAVI_geo_tracks$time <- gsub("Z","", GAVI_geo_tracks$time)

GAVI_geo_tracks$time <- as.POSIXct(GAVI_geo_tracks$time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC" )
GAVI_geo_tracks$time <-GAVI_geo_tracks$time - 5*60*60 

#round to nearest half second
GAVI_geo_tracks$time <- round_date(GAVI_geo_tracks$time, unit = "30 second")


# When was I sampling GAVI??

GAVI_geo_tracks <- subset(GAVI_geo_tracks,
                         time > as.POSIXct('2021-07-26 09:50:00', tz="UTC") &
                           time <= as.POSIXct('2021-07-26 14:45:00', tz="UTC"))
#ANT_geo_tracks2 <- subset(ANT_geo_tracks,
#                         time > as.POSIXct('2021-07-12 10:39:00', tz="UTC") &
#                           time <= as.POSIXct('2021-07-12 12:50:00', tz="UTC"))


fig <- plot_ly(GAVI_geo_tracks, x = ~lat, y = ~lon, z = ~ele, size = 1
)
fig <- fig %>% add_markers()
fig <- fig %>% layout(scene = list(xaxis = list(title = 'lat'),
                                   yaxis = list(title = 'lon'),
                                   zaxis = list(title = 'elevation')))

fig


#merge waypoints and tracks
#ANTE_df <- full_join(ANT_geo_tracks2,ANTE_geo_waypoints[,4:5], by = "time")
ANTE_df <- full_join(ANT_geo_tracks2,ANTE_geo_waypoints, by = "time")
ANTE_df$dist <- as.numeric(ANTE_df$dist)

#write.csv(ANTE_df,here::here("/Geomorphology/Atenas/ANTENAS_GEOMORPH_2021-09-28.csv"))

TrackMap <- qmplot(lon, lat, data = ANTE_df, zoom = 13,
                   maptype = "toner-background", color=ele) 

TrackMap2 <- qmplot(lon_wypt, lat_wypt, data = ANTE_df%>%drop_na(lat_wypt), zoom = 13,  maptype = "toner-background")


TrackMap_elevation <- ggplot(ANTE_df, aes(x=time, y=ele)) +
  geom_point(color="black") +
  geom_text(aes(time,ele_wypt,label=dist,colour="black")) +
  theme_bw()


TrackMap_elevation <- ggplot(ANTE_df, aes(x=lon_wypt, y=ele_wypt)) +
  geom_point(color="black") +
  theme_bw()

TrackMap_elevation <- ggplot(ANTE_df, aes(x=lon, y=lat, color=ele)) +
  geom_point() +
  theme_bw()

TrackMap_elevation <- ggplot(ANTE_df, aes(x=dist, y=ele_wypt)) +geom_point() +
  geom_text(aes(dist,ele_wypt,label=dist,colour="black")) +
  theme_bw()

scatter3D(ANTE_df$lat, ANTE_df$lon, ANTE_df$ele, clab = c("Elevation", "(m)"))



###GEOMORPHOLOGY

ANTE_geo <-  read.csv(here::here("/Geomorphology/Atenas/Antenas_Notes_2021-08-12.csv"), skip=9, header = TRUE, sep = ",",
                      na.strings=c("","NA"), quote = "\"",dec = ".", fill = TRUE, comment.char = "")[,1:10]

p <- ggplot(ANTE_geo) + geom_text(aes(x, w, label = x, color = notes2), #check_overlap = TRUE
                                  ) #+ 
  geom_label(aes(x, 4240, label = SynopPoint), angle = 45) #+
#  geom_text(aes(x, 4240, label = notes), angle = 90)


  
  
