#Geomorphology for Antenas
library(ggplot2)
library(tidyverse)

#merge waypoints and tracks
ANTE_geo_waypoints <-  read.csv(here::here("/Geomorphology/Atenas/Antenas_Waypoint_July12.csv"), skip=9, header = TRUE, sep = ",",
                      na.strings=c("","NA"), quote = "\"",dec = ".", fill = TRUE, comment.char = "")

ANT_geo_tracks <-  read.csv(here::here("/Geomorphology/Atenas/ANTENAS_2021-07-12 10_20_22 DAY.csv"), skip=42, header = TRUE, sep = ",",
                            na.strings=c("","NA"), quote = "\"",dec = ".", fill = TRUE, comment.char = "")[,1:6]


ANTE_geo <-  read.csv(here::here("/Geomorphology/Atenas/Antenas_Notes_2021-08-12.csv"), skip=9, header = TRUE, sep = ",",
                      na.strings=c("","NA"), quote = "\"",dec = ".", fill = TRUE, comment.char = "")


ANTE_geo <- ANTE_geo[,1:10]


p <- ggplot(ANTE_geo) + geom_text(aes(x, w, label = x, color = notes2), #check_overlap = TRUE
                                  ) #+ 
  geom_label(aes(x, 4240, label = SynopPoint), angle = 45) #+
#  geom_text(aes(x, 4240, label = notes), angle = 90)

