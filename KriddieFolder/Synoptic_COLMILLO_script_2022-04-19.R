#Synoptic Script COLMILLO


# Load libraries
library(ggmap)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(here)
library(lubridate)

ContinuousData <-  read.csv(here::here("/Synoptic/ContinuousData_forSynop_2022-01-27.csv"), skip=0, header = TRUE, sep = ",",
                            quote = "\"",dec = ".", fill = TRUE, comment.char = "")
ContinuousData$X <- NULL
ContinuousData$DateTime <- as.POSIXct(ContinuousData$DateTime,  format="%Y-%m-%d %H:%M:%S", tz = "UTC")


#July 6th
#vaisala new
CO2_eos1_July6 <-  read.csv(here::here("/Synoptic/July6_Edited/Vnew_EOS1_synoptic_2021-07-06.csv"), skip=6, header = TRUE, sep = ",",
                            quote = "\"",dec = ".", fill = TRUE, comment.char = "")
colnames(CO2_eos1_July6) <- c("Date","Time","CO2_ppm","Tract","Description","Point","lat","lon","ele","WaterSample","Notes","Wetland")

CO2_eos1_July6$EOS_no <- "EOS_1"
CO2_eos1_July6$VaisalaType <- "new"
CO2_eos1_July6$Notes_2 <- NA
CO2_eos1_July6 <- CO2_eos1_July6 %>%
  filter(Wetland == "COLMILLO")

#Vaisala old
CO2_eos2_July6 <-  read.csv(here::here("/Synoptic/July6_Edited/Vold_EOS2_synoptic_2021-07-06.csv"), skip=6, header = TRUE, sep = ",",
                            quote = "\"",dec = ".", fill = TRUE, comment.char = "")
colnames(CO2_eos2_July6) <- c("Date","Time","CO2_ppm","Tract","Description","Point","lat","lon","ele","WaterSample","Wetland","Notes")
CO2_eos2_July6$EOS_no <- "EOS_2"
CO2_eos2_July6$VaisalaType <- "old"
CO2_eos2_July6$Notes <- NA
CO2_eos2_July6$Notes_2 <- NA
CO2_eos2_July6 <- CO2_eos2_July6 %>%
  filter(Wetland == "COLMILLO")


CO2_July6 <- rbind(CO2_eos1_July6,CO2_eos2_July6)
rm(CO2_eos1_July6,CO2_eos2_July6)

CO2_July6$Wetland <- NULL

#July 7th 

#there is a missing waypoint at pt 4 EOS 1
#Vaisala new
CO2_eos1_July7 <-  read.csv(here::here("/Synoptic/July7_Edited/Vnew_EOS1_synoptic_2021-07-07.csv"), skip=7, header = TRUE, sep = ",",
                            quote = "\"",dec = ".", fill = TRUE, comment.char = "")
colnames(CO2_eos1_July7) <- c("Date","Time","CO2_ppm","Tract","Description","Point","lat","lon","ele","WaterSample","Notes")
CO2_eos1_July7$EOS_no <- "EOS_1"
CO2_eos1_July7$VaisalaType <- "new"
CO2_eos1_July7$Notes_2 <- NA

#Vaisala old
CO2_eos2_July7 <-  read.csv(here::here("/Synoptic/July7_Edited/Vold_EOS2_synoptic_2021-07-07.csv"), skip=6, header = TRUE, sep = ",",
                            quote = "\"",dec = ".", fill = TRUE, comment.char = "")
colnames(CO2_eos2_July7) <- c("Date","Time","CO2_ppm","Tract","Description","Point","lat","lon","ele","WaterSample")

CO2_eos2_July7$EOS_no <- "EOS_2"
CO2_eos2_July7$VaisalaType <- "old"
CO2_eos2_July7$Notes <- NA
CO2_eos2_July7$Notes_2 <- NA

CO2_July7 <- rbind(CO2_eos1_July7,CO2_eos2_July7)
rm(CO2_eos1_July7,CO2_eos2_July7)

#July 9
#Vaisala old
CO2_eos2_July9 <-  read.csv(here::here("/Synoptic/July9_Edited/Vold_EOS2_synoptic_2021-07-09.csv"), skip=6, header = TRUE, sep = ",",
                            quote = "\"",dec = ".", fill = TRUE, comment.char = "")
colnames(CO2_eos2_July9) <- c("Date","Time","CO2_ppm","Tract","Description","Point","lat","lon","ele","WaterSample", "Notes")

CO2_eos2_July9$EOS_no <- "EOS_2"
CO2_eos2_July9$VaisalaType <- "old"
CO2_eos2_July9$Notes_2 <- NA

CO2_synop <- rbind(CO2_eos2_July9,CO2_July6,CO2_July7)
CO2_synop$DateTime <- as.POSIXct(paste(CO2_synop$Date, CO2_synop$Time), format="%m/%d/%Y %I:%M:%S %p")
CO2_synop$Date <- as.Date(CO2_synop$Date, format="%m/%d/%Y" )
CO2_synop$DateTime <- round_date(CO2_synop$DateTime, unit = "15 minute")
CO2_synop$Time <- NULL


#adjust vaisala for temp and pressure
CO2_synop <- left_join(CO2_synop,ContinuousData,by="DateTime")

CO2_synop_pivot <- CO2_synop  %>%
  drop_na(lat)  %>%
  group_by(lat, lon, ele,Date, EOS_no,VaisalaType) %>%
  summarize(CO2_ppm_ave = mean(CO2_ppm, na.rm = TRUE),
            COLM_waterTempAve = mean(COLM_WaterTempAve, na.rm = TRUE),
            BaroPress_kpa = mean(Baro_kpa, na.rm = TRUE),
            AirTemp_c = mean(BaroTemp_c, na.rm = TRUE),
            Tract = mean(Tract, na.rm = TRUE),
            Point = mean(Point, na.rm = TRUE)) 


##Correct for temp and pressure
#assume 2 cm submersion of vaisala
CO2_synop_pivot$Total_hPa <- CO2_synop_pivot$BaroPress_kpa * 10 + 1*2.4884

old <- CO2_synop_pivot[CO2_synop_pivot$VaisalaType == "old", ]
new <- CO2_synop_pivot[CO2_synop_pivot$VaisalaType == "new", ]


old$adjusted_ppm <- 
  old$CO2_ppm_ave * (1 + (1013 - old$Total_hPa) * 0.0015) *
  (1 - (25 - old$COLM_waterTempAve) * 0.003)

new$CO2_ppm_ave_save <-  new$CO2_ppm_ave
new$CO2_ppm_ave <-  new$CO2_ppm_ave_save*2
new$CO2_ppm_ave[new$CO2_ppm_ave_save==10000] <- 10000

new$adjusted_ppm <- 
  new$CO2_ppm_ave * (1 + (1013 - new$Total_hPa) * 0.0015) * 
  (1 - (25 - new$COLM_waterTempAve) * 0.005)
new$CO2_ppm_ave_save <- NULL


CO2_synop_pivot <- rbind(old,new)

#CO2_synop_pivot$Date.as.fact <- as.factor(CO2_synop_pivot$Date)

CO2_map <- qmplot(lon,lat, data = CO2_synop_pivot, zoom = 13,  maptype = "toner-background", color = as.factor(Date)) #+
#scale_color_gradient(low="blue", high="red")


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
Flux_eos1_July9 <-  read.csv(here::here("Synoptic/July9_Edited/EOS_01_Edited_2021-07-09EDITAGAIN2.csv"), skip=0, header = TRUE, sep = ",",
                             quote = "\"",dec = ".", fill = TRUE, comment.char = "")
Flux_eos1_July9 <- Flux_eos1_July9[,c(1:6,12:19)]
colnames(Flux_eos1_July9) <- c("Month","Day","Year","Time","Flux","Temp","Tract","Description","Point","lat","lon","ele","WaterSample", "Notes")
Flux_eos1_July9$EOS_no <- "EOS_1"
Flux_eos1_July9$Date <- as.Date(with(Flux_eos1_July9, paste(Year, Month, Day,sep="-")), "%y-%m-%d")
Flux_eos1_July9$Notes_2 <- NA


Flux_eos2_July9 <-  read.csv(here::here("/Synoptic/July9_Edited/EOS2_Edited_2021-07-09EDITAGAIN2.csv"), skip=0, header = TRUE, sep = ",",
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



Flux_map <- qmplot(lon, lat, data = Flux_synop_pivot, zoom = 13,  maptype = "toner-background", color = EOS_no, shape=as.factor(Date))#+
#  scale_color_gradient(low="blue", high="red")



Trouble <- qmplot(lon, lat, data = CO2_synop_pivot, zoom = 13,  maptype = "toner-background", color = Tract, shape = Date.as.fact)+
  scale_color_gradient(low="blue", high="red")



### Merge data
Flux_synop_pivot$Tract <- NULL 
CO2_synop_pivot$Tract <- NULL
synop_merge <- full_join(Flux_synop_pivot,CO2_synop_pivot, by = c("lat","lon","ele","Date", "Point","EOS_no"))


#i edites the wrong version of colmillo, so this has been edited since 3/17
write.csv(synop_merge, here::here("Synoptic/COLMILLO_2022-04-19.csv"))


#plot
synop_mapFlux <- qmplot(lon, lat, data = synop_merge, zoom = 13,  maptype = "toner-background", color = Flux_ave, shape = as.factor(Date))+
  scale_color_gradient(low="blue", high="red")
synop_mapCO2 <- qmplot(lon, lat, data = synop_merge, zoom = 13,  maptype = "toner-background", color = log10(adjusted_ppm), shape = VaisalaType)+
  scale_color_gradient(low="blue", high="red")

