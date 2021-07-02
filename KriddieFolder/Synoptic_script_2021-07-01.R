## This is where I will work on synoptic data

install.packages("ggmap")
library(ggmap)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(here)
# Install
install.packages("wesanderson")
# Load
library(wesanderson)

API_Key <- "AIzaSyAL9hbyMHAI_3a99HJyB4_LjI_M_pGMOGg"
register_google(key = API_Key, write = TRUE)

#June 18th

CO2 <-  read.csv(here::here("/Synoptic/June18/CO2_EOS1_synoptic_2021-06-18.csv"), skip=6, header = TRUE, sep = ",",
                            quote = "\"",dec = ".", fill = TRUE, comment.char = "")
colnames(CO2) <- c("Date","Time","CO2_ppm")
CO2$DateTime <- as.POSIXct(paste(CO2$Date, CO2$Time), format="%m/%d/%Y %I:%M:%S %p")
CO2$Date <- NULL
CO2$Time <- NULL

#June 22
CO2_eos1_June22 <-  read.csv(here::here("/Synoptic/June22/CO2_EOS1synoptic_2021-06-22.csv"), skip=6, header = TRUE, sep = ",",
                 quote = "\"",dec = ".", fill = TRUE, comment.char = "")
colnames(CO2_eos1_June22) <- c("Date","Time","CO2_ppm","Tract","Description","Point","Lon","Lat","WaterSample")

CO2_eos2_June22 <-  read.csv(here::here("/Synoptic/June22/CO2_EOS2synoptic_2021-06-22.csv"), skip=6, header = TRUE, sep = ",",
                 quote = "\"",dec = ".", fill = TRUE, comment.char = "")
CO2_eos2_June22$Voltage..ppm. <- NULL
colnames(CO2_eos2_June22) <- c("Date","Time","CO2_ppm","Tract","Description","Point","Lon","Lat","WaterSample")

CO2_June22 <- rbind(CO2_eos1_June22,CO2_eos2_June22)
CO2_June22$DateTime <- as.POSIXct(paste(CO2_June22$Date, CO2_June22$Time), format="%m/%d/%Y %I:%M:%S %p")
CO2_June22$Date <- as.Date(CO2_June22$Date, format="%m/%d/%Y" )
CO2_June22$Time <- NULL

CO2_June22_pivot <- CO2_June22  %>%
  drop_na(Lat)  %>%
  group_by(Lat, Lon, Date) %>%
  summarize(CO2_ppm_ave = mean(CO2_ppm, na.rm = TRUE),
            Tract = mean(Tract, na.rm = TRUE),
            Point = mean(Point, na.rm = TRUE))  

## June  23
CO2_eos1_June23 <-  read.csv(here::here("/Synoptic/June23/CO2_EOS1_Synoptics_2021-06-23.csv"), skip=6, header = TRUE, sep = ",",
                      quote = "\"",dec = ".", fill = TRUE, comment.char = "")
CO2_eos1_June23$Voltage..ppm. <- NULL
colnames(CO2_eos1_June23) <- c("Date","Time","CO2_ppm","Tract","Description","Point","Lon","Lat","WaterSample")

CO2_eos2_June23 <-  read.csv(here::here("/Synoptic/June23/CO2_EOS2_Synoptics_2021-06-23.csv"), skip=6, header = TRUE, sep = ",",
                             quote = "\"",dec = ".", fill = TRUE, comment.char = "")
colnames(CO2_eos2_June23) <- c("Date","Time","CO2_ppm","Tract","Description","Point","Lon","Lat","WaterSample")

CO2_June23 <- rbind(CO2_eos1_June23,CO2_eos2_June23)
CO2_June23$DateTime <- as.POSIXct(paste(CO2_June23$Date, CO2_June23$Time), format="%m/%d/%Y %I:%M:%S %p")
CO2_June23$Date <- as.Date(CO2_June23$Date, format="%m/%d/%Y" )
CO2_June23$Time <- NULL

CO2_June23_pivot <- CO2_June23  %>%
  drop_na(Lat)  %>%
  group_by(Lat, Lon, Date) %>%
  summarize(CO2_ppm_ave = mean(CO2_ppm, na.rm = TRUE),
            Tract = mean(Tract, na.rm = TRUE),
            Point = mean(Point, na.rm = TRUE)) 

## June  29
CO2_eos1_June29 <-  read.csv(here::here("/Synoptic/June29_Edited/EOS1_CO2_Edited_2021-06-29.csv"), skip=6, header = TRUE, sep = ",",
                             quote = "\"",dec = ".", fill = TRUE, comment.char = "")
colnames(CO2_eos1_June29) <- c("Date","Time","CO2_ppm","Tract","Description","Point","Lon","Lat","WaterSample","notes")
CO2_eos1_June29$notes <- NULL

#CO2_eos1_June29 <-  read.csv(here::here("/Synoptic/June23/CO2_EOS2_Synoptics_2021-06-23.csv"), skip=6, header = TRUE, sep = ",",
#                             quote = "\"",dec = ".", fill = TRUE, comment.char = "")
#colnames(CO2_eos1_June29) <- c("Date","Time","CO2_ppm","Tract","Description","Point","Lon","Lat","WaterSample")

#CO2_June29 <- rbind(CO2_eos1_June29,CO2_eos2_June23)
#CO2_June29$DateTime <- as.POSIXct(paste(CO2_June29$Date, CO2_June29$Time), format="%m/%d/%Y %I:%M:%S %p")
#CO2_June29$Date <- as.Date(CO2_June29$Date, format="%m/%d/%Y" )
#CO2_June29$Time <- NULL

CO2_June29_pivot <- CO2_June29  %>%
  drop_na(Lat)  %>%
  group_by(Lat, Lon, Date) %>%
  summarize(CO2_ppm_ave = mean(CO2_ppm, na.rm = TRUE),
            Tract = mean(Tract, na.rm = TRUE),
            Point = mean(Point, na.rm = TRUE)) 





qmplot(Long, Lat, data = CO2_pivot, zoom = 15,  maptype = "toner-background", color = CO2_ppm_ave)+
  scale_color_gradient(low="blue", high="red")



##this is cool - but can't zoom in enough


map <- get_map(location = c(long = -78.20378, lat =	 -0.3315833), zoom = 15,scale = "auto")

ggmap(map)





qmap(location = c(long = -78.20378, lat =	 -0.3315833), maprange = TRUE, zoom = 15,
     base_layer = ggplot(aes(x=Long, y=Lat), data = CO2_pivot)) +
  geom_point(color = CO2_ppm_ave)

