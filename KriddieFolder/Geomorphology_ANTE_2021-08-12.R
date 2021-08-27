#Geomorphology for Antenas
library(ggplot2)
library(tidyverse)
library(lubridate)
library("plot3D")
library(dplyr)
library(geosphere)

#merge waypoints and tracks

##WAYPPINTS
ANTE_geo_waypoints <-  read.csv(here::here("/Geomorphology/Atenas/Antenas_Waypoint_July12.csv"), skip=22, header = TRUE, sep = ",",
                      na.strings=c("","NA"), quote = "\"",dec = ".", fill = TRUE, comment.char = "")[(1:26),c(2:5,8)]
colnames(ANTE_geo_waypoints) <- c("lat_wypt","lon_wypt","ele_wypt","time","name")

#convert time in UTC to local time
ANTE_geo_waypoints$time <- gsub("T"," ", ANTE_geo_waypoints$time)
ANTE_geo_waypoints$time <- gsub("Z","", ANTE_geo_waypoints$time)

ANTE_geo_waypoints$time <- as.POSIXct(ANTE_geo_waypoints$time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC" )
ANTE_geo_waypoints$time <-ANTE_geo_waypoints$time - 5*60*60 
#round to nearest half second
ANTE_geo_waypoints$time <- round_date(ANTE_geo_waypoints$time, unit = "30 second")

#not automatically formating in numeric
ANTE_geo_waypoints$lat_wypt <- as.numeric(format(ANTE_geo_waypoints$lat_wypt, digits = 20, nsmall = 20))
ANTE_geo_waypoints$lon_wypt <- as.numeric(format(ANTE_geo_waypoints$lon_wypt, digits = 20, nsmall = 20))
ANTE_geo_waypoints$ele_wypt <- as.numeric(format(ANTE_geo_waypoints$ele_wypt, digits = 20, nsmall = 20))
ANTE_geo_waypoints$name <- sub('.', '', ANTE_geo_waypoints$name)

#merge in distance btw wayppints
ANTE_geoDist <-  read.csv(here::here("/Geomorphology/Atenas/ANTE_Wypt_and_dist.csv"), skip=3, header = TRUE, sep = ",",
                          na.strings=c("","NA"), quote = "\"",dec = ".", fill = TRUE, comment.char = "")
ANTE_geoDist$name <- as.character(ANTE_geoDist$name)

ANTE_geo_waypoints <- full_join(ANTE_geo_waypoints,ANTE_geoDist, by = "name")
rm(ANTE_geoDist)

##deal with TRACKS

ANT_geo_tracks <-  read.csv(here::here("/Geomorphology/Atenas/ANTENAS_2021-07-12 10_20_22 DAY.csv"), skip=42, header = TRUE, sep = ",",
                            na.strings=c("","NA"), quote = "\"",dec = ".", fill = TRUE, comment.char = "")[,1:6]

#convert time in UTC to local time
ANT_geo_tracks$time <- gsub("T"," ", ANT_geo_tracks$time)
ANT_geo_tracks$time <- gsub("Z","", ANT_geo_tracks$time)

ANT_geo_tracks$time <- as.POSIXct(ANT_geo_tracks$time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC" )
ANT_geo_tracks$time <-ANT_geo_tracks$time - 5*60*60 

#round to nearest half second
ANT_geo_tracks$time <- round_date(ANT_geo_tracks$time, unit = "30 second")

#subset between 10:26 and 12:50 on 2021-07-12
ANT_geo_tracks1 <- subset(ANT_geo_tracks,
                         time > as.POSIXct('2021-07-12 10:26:00', tz="UTC") &
                           time <= as.POSIXct('2021-07-12 10:39:00', tz="UTC"))
ANT_geo_tracks2 <- subset(ANT_geo_tracks,
                         time > as.POSIXct('2021-07-12 10:39:00', tz="UTC") &
                           time <= as.POSIXct('2021-07-12 12:50:00', tz="UTC"))

#merge waypoints and tracks
#ANTE_df <- full_join(ANT_geo_tracks2,ANTE_geo_waypoints[,4:5], by = "time")
ANTE_df <- full_join(ANT_geo_tracks2,ANTE_geo_waypoints, by = "time")
ANTE_df$dist <- as.numeric(ANTE_df$dist)

TrackMap <- qmplot(lon, lat, data = ANTE_df, zoom = 13,  maptype = "toner-background")
TrackMap2 <- qmplot(lon_wypt, lat_wypt, data = ANTE_df%>%drop_na(lat_wypt), zoom = 13,  maptype = "toner-background")

TrackMap_elevation <- ggplot(ANTE_df, aes(x=time, y=ele)) +
  geom_point(color="black") +
  geom_text(aes(time,ele_wypt,label=dist,colour="black")) +
  theme_bw()

TrackMap_elevation <- ggplot(ANTE_df, aes(x=dist, y=ele_wypt)) +geom_point() +
  geom_text(aes(dist,ele_wypt,label=dist,colour="black")) +
  theme_bw()

##run loop to take average of every point within 5 meters

###this works. save it.
#create empty df
df <- data.frame(1:258,1:2,"NA")
colnames(df) <- c("id","dist","time")
df$time <- as.POSIXct(df$time, format = "%Y-%m-%d %H:%M:%S")

count = 0
for (i in 1:nrow(ANTE_df)){
  count = count +1
  df[i,"time"] <- ANTE_df[i,"time"]
    df[i,1] <- i
 
 df[i,2] <- distm(c(ANTE_df[i,"lon"], ANTE_df[i,"lat"]), c(ANTE_df[i+1,"lon"], ANTE_df[i+1,"lat"]), fun = distHaversine) 
    
}
####

df <- data.frame(1:20,1:2)
colnames(df) <- c("id","dist")
synop_merge <- synop_merge
synop_merge2 <- as.data.frame(synop_merge[,1:9])

###this works. save it.
count = 0
for (i in 1:nrow(synop_merge2)){
  count = count +1
#  df[i,"Date"] <- synop_merge[i,"Date"]
#  df[i,1] <- i
  
  d <- distm(c(synop_merge2[i,"Lon"], synop_merge2[i,"Lat"]), c(synop_merge2[i+1,"Lon"], synop_merge2[i+1,"Lat"]), fun = distHaversine)
  sqrt(d^2 + (abs(synop_merge2[i,"elev"] - synop_merge2[i+1,"ele"]))^2)
}

distm(c(synop_merge[i,"Lon"], synop_merge[i,"Lat"]), c(synop_merge[i+1,"Lon"], synop_merge[i+1,"Lat"]), fun = distHaversine)


distm(c(lon1, lat1), c(lon2, lat2), fun = distHaversine)




###GEOMORPHOLOGY

ANTE_geo <-  read.csv(here::here("/Geomorphology/Atenas/Antenas_Notes_2021-08-12.csv"), skip=9, header = TRUE, sep = ",",
                      na.strings=c("","NA"), quote = "\"",dec = ".", fill = TRUE, comment.char = "")[,1:10]

p <- ggplot(ANTE_geo) + geom_text(aes(x, w, label = x, color = notes2), #check_overlap = TRUE
                                  ) #+ 
  geom_label(aes(x, 4240, label = SynopPoint), angle = 45) #+
#  geom_text(aes(x, 4240, label = notes), angle = 90)

