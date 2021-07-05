## This is where I will work on synoptic data


# Install
install.packages("wesanderson")
install.packages("ggmap")

# Load libraries
library(ggmap)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(here)
library(wesanderson)

API_Key <- "AIzaSyAL9hbyMHAI_3a99HJyB4_LjI_M_pGMOGg"
register_google(key = API_Key, write = TRUE)

#June 18th
#missing GPS coordinates for 1 location
CO2_eos1_June18 <-  read.csv(here::here("/Synoptic/June18_Edited/CO2_EOS1_synoptic_EDITED_2021-06-18.csv"), skip=6, header = TRUE, sep = ",",
                            quote = "\"",dec = ".", fill = TRUE, comment.char = "")
colnames(CO2_eos1_June18) <- c("Date","Time","CO2_ppm","Tract","Description","Point","Lon","Lat","WaterSample", "Notes", "Notes_2")
CO2_June18 <- CO2_eos1_June18

#June 22
CO2_eos1_June22 <-  read.csv(here::here("/Synoptic/June22/CO2_EOS1synoptic_2021-06-22.csv"), skip=6, header = TRUE, sep = ",",
                 quote = "\"",dec = ".", fill = TRUE, comment.char = "")
colnames(CO2_eos1_June22) <- c("Date","Time","CO2_ppm","Tract","Description","Point","Lon","Lat","WaterSample")
CO2_eos1_June22$Notes <- NA
CO2_eos1_June22$Notes_2 <- NA

CO2_eos2_June22 <-  read.csv(here::here("/Synoptic/June22/CO2_EOS2synoptic_2021-06-22.csv"), skip=6, header = TRUE, sep = ",",
                 quote = "\"",dec = ".", fill = TRUE, comment.char = "")
CO2_eos2_June22$Voltage..ppm. <- NULL
colnames(CO2_eos2_June22) <- c("Date","Time","CO2_ppm","Tract","Description","Point","Lon","Lat","WaterSample")
CO2_eos2_June22$Notes <- NA
CO2_eos2_June22$Notes_2 <- NA

CO2_June22 <- rbind(CO2_eos1_June22,CO2_eos2_June22)

## June  23
CO2_eos1_June23 <-  read.csv(here::here("/Synoptic/June23/CO2_EOS1_Synoptics_2021-06-23.csv"), skip=6, header = TRUE, sep = ",",
                      quote = "\"",dec = ".", fill = TRUE, comment.char = "")
CO2_eos1_June23$Voltage..ppm. <- NULL
colnames(CO2_eos1_June23) <- c("Date","Time","CO2_ppm","Tract","Description","Point","Lon","Lat","WaterSample")
CO2_eos1_June23$Notes <- NA
CO2_eos1_June23$Notes_2 <- NA

CO2_eos2_June23 <-  read.csv(here::here("/Synoptic/June23/CO2_EOS2_Synoptics_2021-06-23.csv"), skip=6, header = TRUE, sep = ",",
                             quote = "\"",dec = ".", fill = TRUE, comment.char = "")
colnames(CO2_eos2_June23) <- c("Date","Time","CO2_ppm","Tract","Description","Point","Lon","Lat","WaterSample")
CO2_eos2_June23$Notes <- NA
CO2_eos2_June23$Notes_2 <- NA

CO2_June23 <- rbind(CO2_eos1_June23,CO2_eos2_June23)

## June  29
CO2_eos1_June29 <-  read.csv(here::here("/Synoptic/June29_Edited/EOS1_CO2_Edited_2021-06-29.csv"), skip=6, header = TRUE, sep = ",",
                             quote = "\"",dec = ".", fill = TRUE, comment.char = "")
colnames(CO2_eos1_June29) <- c("Date","Time","CO2_ppm","Tract","Description","Point","Lon","Lat","WaterSample","Notes")
CO2_eos1_June29$Notes_2 <- NA

CO2_eos2_June29 <-  read.csv(here::here("/Synoptic/June29_Edited/EOS2_CO2_Edited_2021-06-29.csv"), skip=6, header = TRUE, sep = ",",
                             quote = "\"",dec = ".", fill = TRUE, comment.char = "")
colnames(CO2_eos2_June29) <- c("Date","Time","CO2_ppm","Tract","Description","Point","Lon","Lat","WaterSample")
CO2_eos2_June29$Notes <- NA
CO2_eos2_June29$Notes_2 <- NA

CO2_June29 <- rbind(CO2_eos1_June29,CO2_eos2_June29)


## June 30
#EOS1 data has missing coodinates for GPS, however we can approximaTE LOCATION BC IT WAS AROUND WHERE lIZ (eos2) STARTED ON THE SAME DAY
CO2_eos1_June30 <-  read.csv(here::here("/Synoptic/June30_Edited/EOS1_CO2_Edited_2021-06-30.csv"), skip=6, header = TRUE, sep = ",",
                             quote = "\"",dec = ".", fill = TRUE, comment.char = "")
colnames(CO2_eos1_June30) <- c("Date","Time","CO2_ppm","Tract","Description","Point","Lon","Lat","WaterSample","Notes")
CO2_eos1_June30$Notes_2 <- NA
CO2_eos1_June30$Lon <- as.numeric(CO2_eos1_June30$Lon)
CO2_eos1_June30$Lat <- as.numeric(CO2_eos1_June30$Lat)

CO2_eos2_June30 <-  read.csv(here::here("/Synoptic/June30_Edited/EOS02_CO2_EDITED_2021-06-30.csv"), skip=6, header = TRUE, sep = ",",
                             quote = "\"",dec = ".", fill = TRUE, comment.char = "")
colnames(CO2_eos2_June30) <- c("Date","Time","CO2_ppm","Tract","Description","Point","Lon","Lat","WaterSample")
CO2_eos2_June30$Notes <- NA
CO2_eos2_June30$Notes_2 <- NA

CO2_June30 <- rbind(CO2_eos1_June30,CO2_eos2_June30)

CO2_synop <- rbind(CO2_June18, CO2_June22,CO2_June23,CO2_June29,CO2_June30)


CO2_synop$DateTime <- as.POSIXct(paste(CO2_synop$Date, CO2_synop$Time), format="%m/%d/%Y %I:%M:%S %p")
CO2_synop$Date <- as.Date(CO2_synop$Date, format="%m/%d/%Y" )
CO2_synop$Time <- NULL

CO2_synop_pivot <- CO2_synop  %>%
  drop_na(Lat)  %>%
  group_by(Lat, Lon, Date) %>%
  summarize(CO2_ppm_ave = mean(CO2_ppm, na.rm = TRUE),
            Tract = mean(Tract, na.rm = TRUE),
            Point = mean(Point, na.rm = TRUE)) 

#CO2_synop_pivot$Date.as.fact <- as.factor(CO2_synop_pivot$Date)

CO2_map <- qmplot(Lon, Lat, data = CO2_synop_pivot, zoom = 13,  maptype = "toner-background", color = CO2_ppm_ave)+
  scale_color_gradient(low="blue", high="red")

##################
####EOS DATA #####
##################

#June 18
Flux_eos1_June18 <-  read.csv(here::here("/Synoptic/June18_Edited/EOS1_Edited_2021-06-18_EditedAgain.csv"), skip=0, header = TRUE, sep = ",",
                              quote = "\"",dec = ".", fill = TRUE, comment.char = "")
Flux_eos1_June18 <- Flux_eos1_June18[,c(1:6,12:17)]
colnames(Flux_eos1_June18) <- c("Month","Day","Year","Time","Flux","Temp","Tract","Description","Point","Lon","Lat","WaterSample")
Flux_eos1_June18$Date <- as.Date(with(Flux_eos1_June18, paste(Year, Month, Day,sep="-")), "%y-%m-%d")

Flux_June18 <- Flux_eos1_June18

# June 22
#I'd like to look at the notebook to double check the times we took samples
Flux_eos1_June22 <-  read.csv(here::here("/Synoptic/June22/EOS1_Synoptic_2021-06-22_EditedAgain.csv"), skip=0, header = TRUE, sep = ",",
                              quote = "\"",dec = ".", fill = TRUE, comment.char = "")
Flux_eos1_June22 <- Flux_eos1_June22[,c(1:6,12:17)]
colnames(Flux_eos1_June22) <- c("Month","Day","Year","Time","Flux","Temp","Tract","Description","Point","Lon","Lat","WaterSample")
Flux_eos1_June22$Date <- as.Date(with(Flux_eos1_June22, paste(Year, Month, Day,sep="-")), "%y-%m-%d")

Flux_eos2_June22 <-  read.csv(here::here("/Synoptic/June22/EOS2_Synoptic_2021-06-22_EditedAgain.csv"), skip=0, header = TRUE, sep = ",",
                              quote = "\"",dec = ".", fill = TRUE, comment.char = "")
Flux_eos2_June22 <- Flux_eos2_June22[,c(1:6,16:21)]
colnames(Flux_eos2_June22) <- c("Month","Day","Year","Time","Flux","Temp","Tract","Description","Point","Lon","Lat","WaterSample")
Flux_eos2_June22$Date <- as.Date(with(Flux_eos2_June22, paste(Year, Month, Day,sep="-")), "%y-%m-%d")

Flux_June22 <- rbind(Flux_eos1_June22,Flux_eos2_June22)

#June 23
Flux_eos1_June23 <-  read.csv(here::here("/Synoptic/June23/EOS1_Synoptics_2021-06-23_EditedAgain.csv"), skip=0, header = TRUE, sep = ",",
                              quote = "\"",dec = ".", fill = TRUE, comment.char = "")
Flux_eos1_June23 <- Flux_eos1_June23[,c(1:6,12:17)]
colnames(Flux_eos1_June23) <- c("Month","Day","Year","Time","Flux","Temp","Tract","Description","Point","Lon","Lat","WaterSample")
Flux_eos1_June23$Date <- as.Date(with(Flux_eos1_June23, paste(Year, Month, Day,sep="-")), "%y-%m-%d")

Flux_eos2_June23 <-  read.csv(here::here("/Synoptic/June23/EOS2_Synoptics_2021-06-23_EditedAgain.csv"), skip=0, header = TRUE, sep = ",",
                              quote = "\"",dec = ".", fill = TRUE, comment.char = "")
Flux_eos2_June23 <- Flux_eos2_June23[,c(1:6,16:21)]
colnames(Flux_eos2_June23) <- c("Month","Day","Year","Time","Flux","Temp","Tract","Description","Point","Lon","Lat","WaterSample")
Flux_eos2_June23$Date <- as.Date(with(Flux_eos2_June23, paste(Year, Month, Day,sep="-")), "%y-%m-%d")

Flux_June23 <- rbind(Flux_eos1_June23,Flux_eos2_June23)

#June 29
Flux_eos1_June29 <-  read.csv(here::here("/Synoptic/June29_Edited/EOS1_Edited_2021-06-29_EditedAgain.csv"), skip=0, header = TRUE, sep = ",",
                              quote = "\"",dec = ".", fill = TRUE, comment.char = "")
Flux_eos1_June29 <- Flux_eos1_June29[,c(1:6,12:17)]
colnames(Flux_eos1_June29) <- c("Month","Day","Year","Time","Flux","Temp","Tract","Description","Point","Lon","Lat","WaterSample")
Flux_eos1_June29$Date <- as.Date(with(Flux_eos1_June29, paste(Year, Month, Day,sep="-")), "%y-%m-%d")

Flux_eos2_June29 <-  read.csv(here::here("/Synoptic/June29_Edited/EOS2_Edited_2021-06-29_EditedAgain.csv"), skip=0, header = TRUE, sep = ",",
                              quote = "\"",dec = ".", fill = TRUE, comment.char = "")
Flux_eos2_June29 <- Flux_eos2_June29[,c(1:6,16:21)]
colnames(Flux_eos2_June29) <- c("Month","Day","Year","Time","Flux","Temp","Tract","Description","Point","Lon","Lat","WaterSample")
Flux_eos2_June29$Date <- as.Date(with(Flux_eos2_June29, paste(Year, Month, Day,sep="-")), "%y-%m-%d")

Flux_June29 <- rbind(Flux_eos1_June29,Flux_eos2_June29)


#June 30
Flux_eos1_June30 <-  read.csv(here::here("/Synoptic/June30_Edited/EOS1_Edited_2021-06-30_EditedAgain.csv"), skip=0, header = TRUE, sep = ",",
                              quote = "\"",dec = ".", fill = TRUE, comment.char = "")
Flux_eos1_June30 <- Flux_eos1_June30[,c(1:6,12:17)]
colnames(Flux_eos1_June30) <- c("Month","Day","Year","Time","Flux","Temp","Tract","Description","Point","Lon","Lat","WaterSample")
Flux_eos1_June30$Date <- as.Date(with(Flux_eos1_June30, paste(Year, Month, Day,sep="-")), "%y-%m-%d")

Flux_eos2_June30 <-  read.csv(here::here("/Synoptic/June30_Edited/EOS2_Synoptic_EDITED_2021-06-30_EditedAgain.csv"), skip=0, header = TRUE, sep = ",",
                              quote = "\"",dec = ".", fill = TRUE, comment.char = "")
Flux_eos2_June30 <- Flux_eos2_June30[,c(1:6,16:21)]
colnames(Flux_eos2_June30) <- c("Month","Day","Year","Time","Flux","Temp","Tract","Description","Point","Lon","Lat","WaterSample")
Flux_eos2_June30$Date <- as.Date(with(Flux_eos2_June30, paste(Year, Month, Day,sep="-")), "%y-%m-%d")

Flux_June30 <- rbind(Flux_eos1_June30,Flux_eos2_June30)


Flux_June30_pivot2 <- Flux_eos2_June30  %>%
  drop_na(Lat)  %>%
  group_by(Lat, Lon, Date) %>%
  summarize(Flux_ave = mean(Flux, na.rm = TRUE),
            Tract = mean(Tract, na.rm = TRUE),
            Point = mean(Point, na.rm = TRUE)) 


Flux_map <- qmplot(Lon, Lat, data = Flux_June30_pivot, zoom = 13,  maptype = "toner-background", color = Flux_ave)+
  scale_color_gradient(low="blue", high="red")



Flux_synop <- rbind(Flux_June18, Flux_June22, Flux_June23, Flux_June29, Flux_June30)

Flux_synop_pivot <- Flux_synop  %>%
  drop_na(Lat)  %>%
  group_by(Lat, Lon, Date) %>%
  summarize(Flux_ave = mean(Flux, na.rm = TRUE),
            Tract = mean(Tract, na.rm = TRUE),
            Point = mean(Point, na.rm = TRUE)) 

Flux_map <- qmplot(Lon, Lat, data = Flux_synop_pivot, zoom = 13,  maptype = "toner-background", color = Flux_ave)+
  scale_color_gradient(low="blue", high="red")
CO2_map <- qmplot(Lon, Lat, data = CO2_synop_pivot, zoom = 13,  maptype = "toner-background", color = CO2_ppm_ave)+
  scale_color_gradient(low="blue", high="red")


### Merge data

synop_merge <- full_join(Flux_synop_pivot,CO2_synop_pivot, by = c("Lat","Lon","Date", "Tract", "Point"))

synop_merge$Date.as.fact <- as.factor(synop_merge$Date)


#plot
synop_mapFlux <- qmplot(Lon, Lat, data = synop_merge, zoom = 13,  maptype = "toner-background", color = Flux_ave, shape = Date.as.fact)+
  scale_color_gradient(low="blue", high="red")
synop_mapCO2 <- qmplot(Lon, Lat, data = synop_merge, zoom = 13,  maptype = "toner-background", color = CO2_ppm_ave, shape = Date.as.fact)+
  scale_color_gradient(low="blue", high="red")


#line graph?
synop_mapFlux <- qmplot(Lon, Lat, data = synop_merge, zoom = 13, geom = "line", maptype = "toner-background", color = CO2_ppm_ave, shape = Date.as.fact)+
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

