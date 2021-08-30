#Synoptic Script COLMILLO


# Load libraries
library(ggmap)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(here)
library(wesanderson)



#July 6th
CO2_eos1_July6 <-  read.csv(here::here("/Synoptic/July6_Edited/EOS1_CO2_Edited2_2021-07-06.csv"), skip=1, header = TRUE, sep = ",",
                            quote = "\"",dec = ".", fill = TRUE, comment.char = "")
colnames(CO2_eos1_July6) <- c("Date","Time","CO2_ppm","Tract","Description","Point","lon","lat","ele","WaterSample","Notes","Wetland")

CO2_eos1_July6$EOS_no <- "EOS_1"
CO2_eos1_July6$Notes_2 <- NA
CO2_eos1_July6 <- CO2_eos1_July6 %>%
  filter(Wetland == "COLMILLO")

CO2_eos2_July6 <-  read.csv(here::here("/Synoptic/July6_Edited/CO2_EOS2_Edited2_2021-07-06.csv"), skip=6, header = TRUE, sep = ",",
                            quote = "\"",dec = ".", fill = TRUE, comment.char = "")
colnames(CO2_eos2_July6) <- c("Date","Time","CO2_ppm","Tract","Description","Point","lon","lat","ele","WaterSample","Wetland")
CO2_eos2_July6$EOS_no <- "EOS_2"
CO2_eos2_July6$Notes <- NA
CO2_eos2_July6$Notes_2 <- NA
CO2_eos2_July6 <- CO2_eos2_July6 %>%
  filter(Wetland == "COLMILLO")


CO2_July6 <- rbind(CO2_eos1_July6,CO2_eos2_July6)

CO2_July6$ele <- NULL
CO2_July6$Wetland <- NULL

#July 7th 

#there is a missing waypoint 
CO2_eos1_July7 <-  read.csv(here::here("/Synoptic/July7_Edited/CO2_EOS1_Edited_2021-07-07EDIT AGAIN.csv"), skip=1, header = TRUE, sep = ",",
                            quote = "\"",dec = ".", fill = TRUE, comment.char = "")
colnames(CO2_eos1_July7) <- c("Date","Time","CO2_ppm","Tract","Description","Point","lon","lat","WaterSample","Notes")

CO2_eos1_July7$EOS_no <- "EOS_1"
CO2_eos1_July7$Notes_2 <- NA


CO2_eos2_July7 <-  read.csv(here::here("/Synoptic/July7_Edited/CO2_EOS2_Edited_2021-07-07.csv"), skip=6, header = TRUE, sep = ",",
                            quote = "\"",dec = ".", fill = TRUE, comment.char = "")
colnames(CO2_eos2_July7) <- c("Date","Time","CO2_ppm","Tract","Description","Point","lon","lat","WaterSample")

CO2_eos2_July7$EOS_no <- "EOS_2"
CO2_eos2_July7$Notes <- NA
CO2_eos2_July7$Notes_2 <- NA


CO2_July7 <- rbind(CO2_eos1_July7,CO2_eos2_July7)


#July 9
CO2_eos2_July9 <-  read.csv(here::here("/Synoptic/July9_Edited/CO2_EOS2_Synoptics_2021-07-09EDIT.csv"), skip=6, header = TRUE, sep = ",",
                            quote = "\"",dec = ".", fill = TRUE, comment.char = "")
colnames(CO2_eos2_July9) <- c("Date","Time","CO2_ppm","Tract","Description","Point","lon","lat","WaterSample", "Notes")

CO2_eos2_July9$EOS_no <- "EOS_2"
CO2_eos2_July9$Notes_2 <- NA

CO2_synop <- rbind(CO2_July6,CO2_July7)
CO2_synop <- rbind(CO2_synop,CO2_eos2_July9)


CO2_synop$DateTime <- as.POSIXct(paste(CO2_synop$Date, CO2_synop$Time), format="%m/%d/%Y %I:%M:%S %p")
CO2_synop$Date <- as.Date(CO2_synop$Date, format="%m/%d/%Y" )
CO2_synop$Time <- NULL

CO2_synop_pivot <- CO2_synop  %>%
  drop_na(lat)  %>%
  group_by(lat, lon, Date, EOS_no) %>%
  summarize(CO2_ppm_ave = mean(CO2_ppm, na.rm = TRUE),
            Tract = mean(Tract, na.rm = TRUE),
            Point = mean(Point, na.rm = TRUE)) 

CO2_synop_pivot$Date.as.fact <- as.factor(CO2_synop_pivot$Date)

CO2_map <- qmplot(lon, lat, data = CO2_synop_pivot, zoom = 13,  maptype = "toner-background", color = CO2_ppm_ave, shape = Date.as.fact)+
  scale_color_gradient(low="blue", high="red")

#####Now do flux