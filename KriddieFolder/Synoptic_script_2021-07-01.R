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

CO2_June29 <- CO2_eos1_June29
#rbind(CO2_eos1_June29,CO2_eos2_June23)
CO2_June29$DateTime <- as.POSIXct(paste(CO2_June29$Date, CO2_June29$Time), format="%m/%d/%Y %I:%M:%S %p")
CO2_June29$Date <- as.Date(CO2_June29$Date, format="%m/%d/%Y" )
CO2_June29$Time <- NULL

CO2_June29_pivot <- CO2_June29  %>%
  drop_na(Lat)  %>%
  group_by(Lat, Lon, Date) %>%
  summarize(CO2_ppm_ave = mean(CO2_ppm, na.rm = TRUE),
            Tract = mean(Tract, na.rm = TRUE),
            Point = mean(Point, na.rm = TRUE)) 


## June 30
#EOS1 data has missing coodinates for GPS, however we can approximaTE LOCATION BC IT WAS AROUND WHERE lIZ (eos2) STARTED ON THE SAME DAY
CO2_eos1_June30 <-  read.csv(here::here("/Synoptic/June30_Edited/EOS1_CO2_Edited_2021-06-30.csv"), skip=6, header = TRUE, sep = ",",
                             quote = "\"",dec = ".", fill = TRUE, comment.char = "")
colnames(CO2_eos1_June30) <- c("Date","Time","CO2_ppm","Tract","Description","Point","Lon","Lat","WaterSample","notes")
CO2_eos1_June30$notes <- NULL
#CO2_eos1_June30$EOS_num <- "1"
CO2_eos1_June30$Lon <- as.numeric(CO2_eos1_June30$Lon)
CO2_eos1_June30$Lat <- as.numeric(CO2_eos1_June30$Lat)

CO2_eos2_June30 <-  read.csv(here::here("/Synoptic/June30_Edited/EOS02_CO2_EDITED_2021-06-30.csv"), skip=6, header = TRUE, sep = ",",
                             quote = "\"",dec = ".", fill = TRUE, comment.char = "")
colnames(CO2_eos2_June30) <- c("Date","Time","CO2_ppm","Tract","Description","Point","Lon","Lat","WaterSample")
#CO2_eos2_June30$EOS_num <- "2"

CO2_June30 <- rbind(CO2_eos1_June30,CO2_eos2_June30)
CO2_June30$DateTime <- as.POSIXct(paste(CO2_June30$Date, CO2_June30$Time), format="%m/%d/%Y %I:%M:%S %p")
CO2_June30$Date <- as.Date(CO2_June30$Date, format="%m/%d/%Y" )
CO2_June30$Time <- NULL

CO2_June30_pivot <- CO2_June30  %>%
  drop_na(Lat)  %>%
  group_by(Lat, Lon, Date) %>%
  summarize(CO2_ppm_ave = mean(CO2_ppm, na.rm = TRUE),
            Tract = mean(Tract, na.rm = TRUE),
            Point = mean(Point, na.rm = TRUE)) 

CO2_synop <- rbind(CO2_June22_pivot,CO2_June23_pivot,CO2_June29_pivot,CO2_June30_pivot)


CO2_map <- qmplot(Lon, Lat, data = CO2_synop, zoom = 13,  maptype = "toner-background", color = CO2_ppm_ave)+
  scale_color_gradient(low="blue", high="red")

####EOS DATA #####

#June 18



# June 22
Flux_eos1_June22 <-  read.csv(here::here("/Synoptic/June22/EOS1_Synoptic_2021-06-22.csv"), skip=0, header = TRUE, sep = ",",
                              quote = "\"",dec = ".", fill = TRUE, comment.char = "")
Flux_eos1_June22 <- Flux_eos1_June22[,c(1:6,12:17)]
colnames(Flux_eos1_June22) <- c("Month","Day","Year","Time","Flux","Temp","Tract","Description","Point","Lon","Lat","WaterSample")
Flux_eos1_June22$Date <- as.Date(with(Flux_eos1_June22, paste(Year, Month, Day,sep="-")), "%y-%m-%d")

Flux_eos2_June22 <-  read.csv(here::here("/Synoptic/June22/EOS2_Synoptic_2021-06-22.csv"), skip=0, header = TRUE, sep = ",",
                              quote = "\"",dec = ".", fill = TRUE, comment.char = "")
Flux_eos2_June22 <- Flux_eos2_June22[,c(1:6,16:21)]
colnames(Flux_eos2_June22) <- c("Month","Day","Year","Time","Flux","Temp","Tract","Description","Point","Lon","Lat","WaterSample")
Flux_eos2_June22$Date <- as.Date(with(Flux_eos2_June22, paste(Year, Month, Day,sep="-")), "%y-%m-%d")

Flux_June22 <- rbind(Flux_eos1_June22,Flux_eos2_June22)

Flux_June22_pivot <- Flux_June22  %>%
  drop_na(Lat)  %>%
  group_by(Lat, Lon, Date) %>%
  summarize(Flux_ave = mean(Flux, na.rm = TRUE),
            Tract = mean(Tract, na.rm = TRUE),
            Point = mean(Point, na.rm = TRUE)) 

#June 23
Flux_eos1_June23 <-  read.csv(here::here("/Synoptic/June23/EOS1_Synoptics_2021-06-23.csv"), skip=0, header = TRUE, sep = ",",
                              quote = "\"",dec = ".", fill = TRUE, comment.char = "")
Flux_eos1_June23 <- Flux_eos1_June23[,c(1:6,12:17)]
colnames(Flux_eos1_June23) <- c("Month","Day","Year","Time","Flux","Temp","Tract","Description","Point","Lon","Lat","WaterSample")
Flux_eos1_June23$Date <- as.Date(with(Flux_eos1_June23, paste(Year, Month, Day,sep="-")), "%y-%m-%d")

Flux_eos2_June23 <-  read.csv(here::here("/Synoptic/June23/EOS2_Synoptics_2021-06-23.csv"), skip=0, header = TRUE, sep = ",",
                              quote = "\"",dec = ".", fill = TRUE, comment.char = "")
Flux_eos2_June23 <- Flux_eos2_June23[,c(1:6,16:21)]
colnames(Flux_eos2_June23) <- c("Month","Day","Year","Time","Flux","Temp","Tract","Description","Point","Lon","Lat","WaterSample")
Flux_eos2_June23$Date <- as.Date(with(Flux_eos2_June23, paste(Year, Month, Day,sep="-")), "%y-%m-%d")

Flux_June23 <- rbind(Flux_eos1_June23,Flux_eos2_June23)

Flux_June23_pivot <- Flux_June23  %>%
  drop_na(Lat)  %>%
  group_by(Lat, Lon, Date) %>%
  summarize(Flux_ave = mean(Flux, na.rm = TRUE),
            Tract = mean(Tract, na.rm = TRUE),
            Point = mean(Point, na.rm = TRUE)) 

#June 29
Flux_eos1_June29 <-  read.csv(here::here("/Synoptic/June29_Edited/EOS1_Edited_2021-06-29.csv"), skip=0, header = TRUE, sep = ",",
                              quote = "\"",dec = ".", fill = TRUE, comment.char = "")
Flux_eos1_June29 <- Flux_eos1_June29[,c(1:6,12:17)]
colnames(Flux_eos1_June29) <- c("Month","Day","Year","Time","Flux","Temp","Tract","Description","Point","Lon","Lat","WaterSample")
Flux_eos1_June29$Date <- as.Date(with(Flux_eos1_June29, paste(Year, Month, Day,sep="-")), "%y-%m-%d")

Flux_June23 <- Flux_eos1_June29
  #rbind(Flux_eos1_June23,Flux_eos2_June23)

Flux_June29_pivot <- Flux_June23  %>%
  drop_na(Lat)  %>%
  group_by(Lat, Lon, Date) %>%
  summarize(Flux_ave = mean(Flux, na.rm = TRUE),
            Tract = mean(Tract, na.rm = TRUE),
            Point = mean(Point, na.rm = TRUE))

Flux_map <- qmplot(Lon, Lat, data = Flux_June29_pivot, zoom = 13,  maptype = "toner-background", color = Flux_ave)+
  scale_color_gradient(low="blue", high="red")

#June 30
Flux_eos1_June30 <-  read.csv(here::here("/Synoptic/June30_Edited/EOS1_Edited_2021-06-30.csv"), skip=0, header = TRUE, sep = ",",
                              quote = "\"",dec = ".", fill = TRUE, comment.char = "")
Flux_eos1_June30 <- Flux_eos1_June30[,c(1:6,12:17)]
colnames(Flux_eos1_June30) <- c("Month","Day","Year","Time","Flux","Temp","Tract","Description","Point","Lon","Lat","WaterSample")
Flux_eos1_June30$Date <- as.Date(with(Flux_eos1_June30, paste(Year, Month, Day,sep="-")), "%y-%m-%d")

Flux_eos2_June30 <-  read.csv(here::here("/Synoptic/June30_Edited/EOS2_Synoptic_EDITED_2021-06-30.csv"), skip=0, header = TRUE, sep = ",",
                              quote = "\"",dec = ".", fill = TRUE, comment.char = "")
Flux_eos2_June30 <- Flux_eos2_June30[,c(1:6,16:21)]
colnames(Flux_eos2_June30) <- c("Month","Day","Year","Time","Flux","Temp","Tract","Description","Point","Lon","Lat","WaterSample")
Flux_eos2_June30$Date <- as.Date(with(Flux_eos2_June30, paste(Year, Month, Day,sep="-")), "%y-%m-%d")

Flux_June30 <- rbind(Flux_eos1_June30,Flux_eos2_June30)

Flux_June30_pivot <- Flux_June30  %>%
  drop_na(Lat)  %>%
  group_by(Lat, Lon, Date) %>%
  summarize(Flux_ave = mean(Flux, na.rm = TRUE),
            Tract = mean(Tract, na.rm = TRUE),
            Point = mean(Point, na.rm = TRUE)) 

Flux_synop <- rbind(Flux_June22_pivot, Flux_June23_pivot, Flux_June29_pivot, Flux_June30_pivot)


Flux_map <- qmplot(Lon, Lat, data = Flux_synop, zoom = 13,  maptype = "toner-background", color = Flux_ave)+
  scale_color_gradient(low="blue", high="red")
CO2_map <- qmplot(Lon, Lat, data = Flux_synop, zoom = 13,  maptype = "toner-background", color = Flux_ave)+
  scale_color_gradient(low="blue", high="red")


### Merge datua

synop_merge <- full_join(Flux_synop,CO2_synop, by = c("Lat","Lon","Tract","Point","Date"))

#plot
synop_map <- qmplot(Lon, Lat, data = synop_merge, zoom = 13,  maptype = "toner-background", color = Flux_ave)+
  scale_color_gradient(low="blue", high="red")
synop_map <- qmplot(Lon, Lat, data = synop_merge, zoom = 13, geom = "line", maptype = "toner-background", color = CO2_ppm_ave)+
  scale_color_gradient(low="blue", high="red")


Gmap <- get_map(location = c(long = -78.20378, lat =	 -0.3315833), zoom = 20,scale = "auto")

ggmap(Gmap) #+
  geom_segment(data = aux2, aes(x = Lon, y = Lat, 
                                xend = lon_coord.y, yend = lat_coord.y), 
               color = "yellow", arrow = arrow(length = unit(0.2,"cm"))) +
  geom_point(aes(x=Lon, y=Lat),data=aux2) 



##this is cool - but can't zoom in enough


map <- get_map(location = c(long = -78.20378, lat =	 -0.3315833), zoom = 15,scale = "auto")

ggmap(map)





qmap(location = c(long = -78.20378, lat =	 -0.3315833), maprange = TRUE, zoom = 15,
     base_layer = ggplot(aes(x=Long, y=Lat), data = CO2_pivot)) +
  geom_point(color = CO2_ppm_ave)

