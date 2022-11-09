#Synoptic Script COLMILLO


# Load libraries
library(ggmap)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(here)
library(wesanderson)



#July 6th
#vaisala new
CO2_eos1_July6 <-  read.csv(here::here("/Synoptic/July6_Edited/EOS1_CO2_Edited2_2021-07-06.csv"), skip=1, header = TRUE, sep = ",",
                            quote = "\"",dec = ".", fill = TRUE, comment.char = "")
colnames(CO2_eos1_July6) <- c("Date","Time","CO2_ppm","Tract","Description","Point","lat","lon","ele","WaterSample","Notes","Wetland")

CO2_eos1_July6$EOS_no <- "EOS_1"
CO2_eos1_July6$Notes_2 <- NA
CO2_eos1_July6 <- CO2_eos1_July6 %>%
  filter(Wetland == "COLMILLO")

#Vaisala old
CO2_eos2_July6 <-  read.csv(here::here("/Synoptic/July6_Edited/CO2_EOS2_Edited2_2021-07-06.csv"), skip=6, header = TRUE, sep = ",",
                            quote = "\"",dec = ".", fill = TRUE, comment.char = "")
colnames(CO2_eos2_July6) <- c("Date","Time","CO2_ppm","Tract","Description","Point","lat","lon","ele","WaterSample","Wetland")
CO2_eos2_July6$EOS_no <- "EOS_2"
CO2_eos2_July6$Notes <- NA
CO2_eos2_July6$Notes_2 <- NA
CO2_eos2_July6 <- CO2_eos2_July6 %>%
  filter(Wetland == "COLMILLO")


CO2_July6 <- rbind(CO2_eos1_July6,CO2_eos2_July6)
rm(CO2_eos1_July6,CO2_eos2_July6)

CO2_July6$Wetland <- NULL


CO2_synop_pivot <- CO2_July6  %>%
  drop_na(lat)  %>%
  group_by(lat, lon, Date, EOS_no) %>%
  summarize(CO2_ppm_ave = mean(CO2_ppm, na.rm = TRUE),
            Tract = mean(Tract, na.rm = TRUE),
            Point = mean(Point, na.rm = TRUE)) 
CO2_synop_pivot$Date.as.fact <- as.factor(CO2_synop_pivot$Date)

CO2_map <- qmplot(lon, lat, data = CO2_synop_pivot, zoom = 13,  maptype = "toner-background", color = CO2_ppm_ave, shape = as.factor(EOS_no))+
  scale_color_gradient(low="blue", high="red")


#July 7th 

#there is a missing waypoint at pt 4 EOS 1
#Vaisala new
CO2_eos1_July7 <-  read.csv(here::here("/Synoptic/July7_Edited/CO2_EOS1_Edited_2021-07-07EDIT AGAIN.csv"), skip=1, header = TRUE, sep = ",",
                            quote = "\"",dec = ".", fill = TRUE, comment.char = "")
colnames(CO2_eos1_July7) <- c("Date","Time","CO2_ppm","Tract","Description","Point","lat","lon","ele","WaterSample","Notes")

CO2_eos1_July7$EOS_no <- "EOS_1"
CO2_eos1_July7$Notes_2 <- NA

#Vaisala old
CO2_eos2_July7 <-  read.csv(here::here("/Synoptic/July7_Edited/CO2_EOS2_Edited_2021-07-07EDIT AGAIN.csv"), skip=6, header = TRUE, sep = ",",
                            quote = "\"",dec = ".", fill = TRUE, comment.char = "")
colnames(CO2_eos2_July7) <- c("Date","Time","CO2_ppm","Tract","Description","Point","lat","lon","ele","WaterSample")

CO2_eos2_July7$EOS_no <- "EOS_2"
CO2_eos2_July7$Notes <- NA
CO2_eos2_July7$Notes_2 <- NA


CO2_July7 <- rbind(CO2_eos1_July7,CO2_eos2_July7)
rm(CO2_eos1_July7,CO2_eos2_July7)

#CO2_July7$DateTime <- as.POSIXct(paste(CO2_July7$Date, CO2_July7$Time), format="%m/%d/%Y %I:%M:%S %p")
#CO2_July7$Date <- as.Date(CO2_July7$Date, format="%m/%d/%Y" )
#CO2_July7$Time <- NULL

CO2_synop_pivot <- CO2_July7  %>%
  drop_na(lat)  %>%
  group_by(lat, lon, Date, EOS_no) %>%
  summarize(CO2_ppm_ave = mean(CO2_ppm, na.rm = TRUE),
            Tract = mean(Tract, na.rm = TRUE),
            Point = mean(Point, na.rm = TRUE)) 
CO2_synop_pivot$Date.as.fact <- as.factor(CO2_synop_pivot$Date)

CO2_map <- qmplot(lon, lat, data = CO2_synop_pivot, zoom = 13,  maptype = "toner-background", color = CO2_ppm_ave, shape = as.factor(EOS_no))+
  scale_color_gradient(low="blue", high="red")

#July 9

CO2_eos2_July9 <-  read.csv(here::here("/Synoptic/July9_Edited/CO2_EOS2_Synoptics_2021-07-09EDIT.csv"), skip=6, header = TRUE, sep = ",",
                            quote = "\"",dec = ".", fill = TRUE, comment.char = "")
colnames(CO2_eos2_July9) <- c("Date","Time","CO2_ppm","Tract","Description","Point","lat","lon","ele","WaterSample", "Notes")
CO2_eos2_July9$EOS_no <- "EOS_2"
CO2_eos2_July9$Notes_2 <- NA

CO2_synop <- CO2_eos2_July9
CO2_synop <- rbind(CO2_synop,CO2_July6)
CO2_synop <- rbind(CO2_synop,CO2_July7)

CO2_synop$DateTime <- as.POSIXct(paste(CO2_synop$Date, CO2_synop$Time), format="%m/%d/%Y %I:%M:%S %p")
CO2_synop$Date <- as.Date(CO2_synop$Date, format="%m/%d/%Y" )
CO2_synop$Time <- NULL

CO2_synop_pivot <- CO2_synop  %>%
  drop_na(lat)  %>%
  group_by(lat, lon, ele,Date, EOS_no) %>%
  summarize(CO2_ppm_ave = mean(CO2_ppm, na.rm = TRUE),
            Tract = mean(Tract, na.rm = TRUE),
            Point = mean(Point, na.rm = TRUE)) 


CO2_map <- qmplot(lon, lat, data = CO2_synop_pivot, zoom = 13,  maptype = "toner-background", color = CO2_ppm_ave, shape = Date.as.fact)+
  scale_color_gradient(low="blue", high="red")


Trouble <- qmplot(lon, lat, data = CO2_synop_pivot, zoom = 13,  maptype = "toner-background", color = Tract, shape = Date.as.fact)+
  scale_color_gradient(low="blue", high="red")

#####Now do flux

#July 6
Flux_eos1_July6 <-  read.csv(here::here("Synoptic/July6_Edited/EOS1_Edited_2021-07-06_EDITEDAGAIN.csv"), skip=0, header = TRUE, sep = ",",
                              quote = "\"",dec = ".", fill = TRUE, comment.char = "")
Flux_eos1_July6 <- Flux_eos1_July6[,c(1:6,12:20)]
colnames(Flux_eos1_July6) <- c("Month","Day","Year","Time","Flux","Temp","Tract","Description","Point","lat","lon","ele","WaterSample", "Notes","Wetland")
Flux_eos1_July6$EOS_no <- "EOS_1"
Flux_eos1_July6$Date <- as.Date(with(Flux_eos1_July6, paste(Year, Month, Day,sep="-")), "%y-%m-%d")
Flux_eos1_July6$Notes_2 <- NA
Flux_eos1_July6 <- Flux_eos1_July6 %>%
  filter(Wetland == "CORMILLO")

Flux_eos2_July6 <-  read.csv(here::here("/Synoptic/July6_Edited/EOS2_synoptic_Edited_2021-07-06_EDITEDAGAIN2.csv"), skip=0, header = TRUE, sep = ",",
                              quote = "\"",dec = ".", fill = TRUE, comment.char = "")
Flux_eos2_July6 <- Flux_eos2_July6[,c(1:6,16:23)]
colnames(Flux_eos2_July6) <- c("Month","Day","Year","Time","Flux","Temp","Tract","Description","Point","lat","lon","ele","WaterSample","Wetland")
Flux_eos2_July6$EOS_no <- "EOS_2"
Flux_eos2_July6$Notes <- NA
Flux_eos2_July6$Notes_2 <- NA
Flux_eos2_July6$Date <- as.Date(with(Flux_eos2_July6, paste(Year, Month, Day,sep="-")), "%y-%m-%d")
Flux_eos2_July6 <- Flux_eos2_July6 %>%
  filter(Wetland == "COLMILLO")

Flux_July6 <- rbind(Flux_eos1_July6,Flux_eos2_July6)
Flux_July6$Wetland <- NULL


#JULY 7
Flux_eos1_July7 <-  read.csv(here::here("Synoptic/July7_Edited/EOS1_Edited_2021-07-07EDITAGAIN.csv"), skip=0, header = TRUE, sep = ",",
                             quote = "\"",dec = ".", fill = TRUE, comment.char = "")
Flux_eos1_July7 <- Flux_eos1_July7[,c(1:6,12:19)]
colnames(Flux_eos1_July7) <- c("Month","Day","Year","Time","Flux","Temp","Tract","Description","Point","lat","lon","ele","WaterSample", "Notes")
Flux_eos1_July7$EOS_no <- "EOS_1"
Flux_eos1_July7$Date <- as.Date(with(Flux_eos1_July7, paste(Year, Month, Day,sep="-")), "%y-%m-%d")
Flux_eos1_July7$Notes_2 <- NA


Flux_eos2_July7 <-  read.csv(here::here("/Synoptic/July7_Edited/EOS2_synoptic_Edited_2021-07-07EDITAGAIN.csv"), skip=0, header = TRUE, sep = ",",
                             quote = "\"",dec = ".", fill = TRUE, comment.char = "")
Flux_eos2_July7 <- Flux_eos2_July7[,c(1:6,16:22)]
colnames(Flux_eos2_July7) <- c("Month","Day","Year","Time","Flux","Temp","Tract","Description","Point","lat","lon","ele","WaterSample")
Flux_eos2_July7$EOS_no <- "EOS_2"
Flux_eos2_July7$Notes <- NA
Flux_eos2_July7$Notes_2 <- NA
Flux_eos2_July7$Date <- as.Date(with(Flux_eos2_July7, paste(Year, Month, Day,sep="-")), "%y-%m-%d")

Flux_July7 <- rbind(Flux_eos1_July7,Flux_eos2_July7)

#July9
Flux_eos1_July9 <-  read.csv(here::here("Synoptic/July9_Edited/EOS_01_Edited_2021-07-09EDITAGAIN.csv"), skip=0, header = TRUE, sep = ",",
                             quote = "\"",dec = ".", fill = TRUE, comment.char = "")
Flux_eos1_July9 <- Flux_eos1_July9[,c(1:6,12:19)]
colnames(Flux_eos1_July9) <- c("Month","Day","Year","Time","Flux","Temp","Tract","Description","Point","lat","lon","ele","WaterSample", "Notes")
Flux_eos1_July9$EOS_no <- "EOS_1"
Flux_eos1_July9$Date <- as.Date(with(Flux_eos1_July9, paste(Year, Month, Day,sep="-")), "%y-%m-%d")
Flux_eos1_July9$Notes_2 <- NA


Flux_eos2_July9 <-  read.csv(here::here("/Synoptic/July9_Edited/EOS2_Edited_2021-07-09EDITAGAIN.csv"), skip=0, header = TRUE, sep = ",",
                             quote = "\"",dec = ".", fill = TRUE, comment.char = "")
Flux_eos2_July9 <- Flux_eos2_July9[,c(1:6,16:22)]
colnames(Flux_eos2_July9) <- c("Month","Day","Year","Time","Flux","Temp","Tract","Description","Point","lat","lon","ele","WaterSample")
Flux_eos2_July9$EOS_no <- "EOS_2"
Flux_eos2_July9$Notes <- NA
Flux_eos2_July9$Notes_2 <- NA
Flux_eos2_July9$Date <- as.Date(with(Flux_eos2_July9, paste(Year, Month, Day,sep="-")), "%y-%m-%d")

Flux_July9 <- rbind(Flux_eos1_July9,Flux_eos2_July9)

##bind together and pivot

Flux_synop <- rbind(Flux_July9,Flux_July7)
Flux_synop <- rbind(Flux_synop,Flux_July6)


Flux_synop_pivot <- Flux_synop  %>%
  drop_na(lat)  %>%
  group_by(lat, lon, ele,Date, EOS_no) %>%
  summarize(Flux_ave = mean(Flux, na.rm = TRUE),
            Tract = mean(Tract, na.rm = TRUE),
            Point = mean(Point, na.rm = TRUE)) 


Flux_map <- qmplot(lon, lat, data = Flux_synop_pivot, zoom = 13,  maptype = "toner-background", color = Flux_ave)+
  scale_color_gradient(low="blue", high="red")


Trouble <- qmplot(lon, lat, data = CO2_synop_pivot, zoom = 13,  maptype = "toner-background", color = Tract, shape = Date.as.fact)+
  scale_color_gradient(low="blue", high="red")



### Merge data
Flux_synop_pivot$Tract <- NULL 
CO2_synop_pivot$Tract <- NULL
synop_merge <- full_join(Flux_synop_pivot,CO2_synop_pivot, by = c("lat","lon","ele","Date", "Point","EOS_no"))



write.csv(synop_merge, here::here("Synoptic/COLMILLO_2021-08-30.csv"))

synop_merge$Date.as.fact <- as.factor(synop_merge$Date)


#plot
synop_mapFlux <- qmplot(lon, lat, data = synop_merge, zoom = 13,  maptype = "toner-background", color = Flux_ave, shape = Date.as.fact)+
  scale_color_gradient(low="blue", high="red")
synop_mapCO2 <- qmplot(lon, lat, data = synop_merge, zoom = 13,  maptype = "toner-background", color = CO2_ppm_ave, shape = EOS_no)+
  scale_color_gradient(low="blue", high="red")

