#this script is for synoptic data for atennas

# Load libraries
library(ggmap)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(here)
library(lubridate)
library(chron)

#July 5th
CO2_eos1_July5 <-  read.csv(here::here("/Synoptic/July5_Edited/EOS1_CO2_Synoptic_Edited_2021-07-05.csv"), skip=6, header = TRUE, sep = ",",
                             quote = "\"",dec = ".", fill = TRUE, comment.char = "")
colnames(CO2_eos1_July5) <- c("Date","Time","CO2_ppm","Tract","Description","Point","lon","lat","ele","WaterSample")
CO2_eos1_July5$EOS_no <- "EOS_1"
CO2_eos1_July5$Notes <- NA
CO2_eos1_July5$Notes_2 <- NA
CO2_eos1_July5$Wetland <- "ANTE"

CO2_eos2_July5 <-  read.csv(here::here("/Synoptic/July5_Edited/EOS2_CO2_Edited2_2021-07-05.csv"), skip=6, header = TRUE, sep = ",",
                             quote = "\"",dec = ".", fill = TRUE, comment.char = "")
#CO2_eos2_July5$Voltage..ppm. <- NULL
colnames(CO2_eos2_July5) <- c("Date","Time","CO2_ppm","Tract","Description","Point","lon","lat","ele","WaterSample","Notes")
CO2_eos2_July5$EOS_no <- "EOS_2"
CO2_eos2_July5$Notes_2 <- NA
CO2_eos2_July5$Wetland <- "ANTE"

CO2_July5 <- rbind(CO2_eos1_July5,CO2_eos2_July5)

#July 6th
CO2_eos1_July6 <-  read.csv(here::here("/Synoptic/July6_Edited/EOS1_CO2_Edited2_2021-07-06.csv"), skip=1, header = TRUE, sep = ",",
                            quote = "\"",dec = ".", fill = TRUE, comment.char = "")
colnames(CO2_eos1_July6) <- c("Date","Time","CO2_ppm","Tract","Description","Point","lon","lat","ele","WaterSample","Notes","Wetland")

CO2_eos1_July6$EOS_no <- "EOS_1"
CO2_eos1_July6$Notes_2 <- NA
CO2_eos1_July6 <- CO2_eos1_July6 %>%
  filter(Wetland == "ANTE")

CO2_eos2_July6 <-  read.csv(here::here("/Synoptic/July6_Edited/CO2_EOS2_Edited2_2021-07-06.csv"), skip=6, header = TRUE, sep = ",",
                            quote = "\"",dec = ".", fill = TRUE, comment.char = "")
colnames(CO2_eos2_July6) <- c("Date","Time","CO2_ppm","Tract","Description","Point","lon","lat","ele","WaterSample","Wetland")
CO2_eos2_July6$EOS_no <- "EOS_2"
CO2_eos2_July6$Notes <- NA
CO2_eos2_July6$Notes_2 <- NA
CO2_eos2_July6 <- CO2_eos2_July6 %>%
  filter(Wetland == "ANTE")

CO2_July6 <- rbind(CO2_eos1_July6,CO2_eos2_July6)

#bind

CO2_synop <- rbind(CO2_July5, CO2_July6)


CO2_synop$DateTime <- as.POSIXct(paste(CO2_synop$Date, CO2_synop$Time), format="%m/%d/%Y %I:%M:%S %p")
CO2_synop$Date <- as.Date(CO2_synop$Date, format="%m/%d/%Y" )
CO2_synop$Time <- NULL

CO2_synop_pivot <- CO2_synop  %>%
  drop_na(lat)  %>%
  group_by(lat, lon,ele, Date, EOS_no) %>%
  summarize(CO2_ppm_ave = mean(CO2_ppm, na.rm = TRUE),
            Tract = mean(Tract, na.rm = TRUE),
            Point = mean(Point, na.rm = TRUE)) 

CO2_synop_pivot$Date.as.fact <- as.factor(CO2_synop_pivot$Date)

CO2_map <- qmplot(lon, lat, data = CO2_synop_pivot, zoom = 13,  maptype = "toner-background", color = CO2_ppm_ave, shape = Date.as.fact)+
  scale_color_gradient(low="blue", high="red")

###FLUX Data
#July 5th

Flux_eos1_July5 <-  read.csv(here::here("Synoptic/July5_Edited/EOS1_Synoptic_Edited_2021-07-05.csv"), skip=0, header = TRUE, sep = ",",
                              quote = "\"",dec = ".", fill = TRUE, comment.char = "")
Flux_eos1_July5 <- Flux_eos1_July5[,c(1:6,12:18)]
colnames(Flux_eos1_July5) <- c("Month","Day","Year","Time","Flux","Temp","Tract","Description","Point","lon","lat","ele","WaterSample")
Flux_eos1_July5$EOS_no <- "EOS_1"
Flux_eos1_July5$Notes <- NA
Flux_eos1_July5$Date <- as.Date(with(Flux_eos1_July5, paste(Year, Month, Day,sep="-")), "%y-%m-%d")
Flux_eos1_July5$Wetland <- "ANTE"

Flux_eos2_July5 <-  read.csv(here::here("/Synoptic/July5_Edited/EOS2_Edited2_2021-07-05.csv"), skip=0, header = TRUE, sep = ",",
                              quote = "\"",dec = ".", fill = TRUE, comment.char = "")
Flux_eos2_July5 <- Flux_eos2_July5[,c(1:6,16:22)]
colnames(Flux_eos2_July5) <- c("Month","Day","Year","Time","Flux","Temp","Tract","Description","Point","lon","lat","ele","WaterSample")
Flux_eos2_July5$EOS_no <- "EOS_2"
Flux_eos2_July5$Notes <- NA
Flux_eos2_July5$Date <- as.Date(with(Flux_eos2_July5, paste(Year, Month, Day,sep="-")), "%y-%m-%d")
Flux_eos2_July5$Wetland <- "ANTE"

Flux_July5 <- rbind(Flux_eos1_July5,Flux_eos2_July5)


#July 6
Flux_eos1_July6 <-  read.csv(here::here("Synoptic/July6_Edited/EOS1_Edited_2021-07-06_EDITEDAGAIN.csv"), skip=0, header = TRUE, sep = ",",
                             quote = "\"",dec = ".", fill = TRUE, comment.char = "")
Flux_eos1_July6 <- Flux_eos1_July6[,c(1:6,12:20)]
colnames(Flux_eos1_July6) <- c("Month","Day","Year","Time","Flux","Temp","Tract","Description","Point","lon","lat","ele","WaterSample", "Notes","Wetland")
Flux_eos1_July6$EOS_no <- "EOS_1"
Flux_eos1_July6$Date <- as.Date(with(Flux_eos1_July6, paste(Year, Month, Day,sep="-")), "%y-%m-%d")
Flux_eos1_July6 <- Flux_eos1_July6 %>%
  filter(Wetland == "ANTE")


Flux_eos2_July6 <-  read.csv(here::here("/Synoptic/July6_Edited/EOS2_synoptic_Edited_2021-07-06_EDITEDAGAIN2.csv"), skip=0, header = TRUE, sep = ",",
                             quote = "\"",dec = ".", fill = TRUE, comment.char = "")
Flux_eos2_July6 <- Flux_eos2_July6[,c(1:6,16:23)]
colnames(Flux_eos2_July6) <- c("Month","Day","Year","Time","Flux","Temp","Tract","Description","Point","lon","lat","ele","WaterSample","Wetland")
Flux_eos2_July6$EOS_no <- "EOS_2"
Flux_eos2_July6$Notes <- NA
Flux_eos2_July6$Date <- as.Date(with(Flux_eos2_July6, paste(Year, Month, Day,sep="-")), "%y-%m-%d")
Flux_eos2_July6 <- Flux_eos2_July6 %>%
  filter(Wetland == "ANTE")

Flux_July6 <- rbind(Flux_eos1_July6,Flux_eos2_July6)

#bind flux

Flux_synop <- rbind(Flux_July5, Flux_July6)

y <- hms(Flux_synop$Time)
Flux_synop$Time <- hour(y) + minute(y) / 60 + second(y) / 360


Flux_synop_pivot <- Flux_synop  %>%
  drop_na(lat)  %>%
  group_by(lat, lon, ele, Date, EOS_no) %>%
  summarize(Flux_ave = mean(Flux, na.rm = TRUE),
            Tract = mean(Tract, na.rm = TRUE),
            Point = mean(Point, na.rm = TRUE),
            Time = mean(Time))


synop_merge <- full_join(Flux_synop_pivot,CO2_synop_pivot, by = c("lon","lat","ele","Date", "Tract", "Point","EOS_no"))

synop_merge$Time<- times(synop_merge$Time / 24)
synop_merge$DateTime <- as.POSIXct(paste(synop_merge$Date, synop_merge$Time))
              
##Water Chem

WaterChem <-  read.csv(here::here("/WaterChem/DOC_9-1-2021_UNC_EDIT.csv"), skip=0, header = TRUE, sep = ",",
                            quote = "\"",dec = ".", fill = TRUE, comment.char = "")
WaterChem$Date <- as.Date(WaterChem$Date, format = "%m/%d/%Y")
WaterChem$EOS_no <- gsub("_.*", "", WaterChem$sampleID)     

WaterChem <- WaterChem%>%
  filter(SampleType == "synoptic")%>%
  filter(Date == "2021-07-05" | Date == "2021-07-06")

WaterChem$DateTime <- as.POSIXct(paste(WaterChem$Date, WaterChem$Time), format = "%Y-%m-%d %H:%M")


#merge with synop 
synop_merge$EOS_no<-gsub("_","",synop_merge$EOS_no)

synop_merge$DateTime <- round_date(synop_merge$DateTime,unit="15 minutes")
WaterChem$DateTime <- round_date(WaterChem$DateTime,unit="15 minutes")

synop_merge <- full_join(synop_merge,WaterChem, by=c("DateTime","EOS_no","Date"))

synop_merge$Date.as.fact <- NULL
synop_merge$Time.y <- NULL
synop_merge$Time.x <- NULL
synop_merge$SampleType <- NULL

#Write OUt
#write.csv(synop_merge, here::here("Synoptic/ANTE_2022-01-09.csv"))

synop_merge$Date.as.fact <- as.factor(synop_merge$Date)


#plot
synop_mapFlux <- qmplot(lon, lat, data = synop_merge, zoom = 13,  maptype = "toner-background", color = Flux_ave, shape = Date.as.fact)+
  scale_color_gradient(low="blue", high="red")
synop_mapCO2 <- qmplot(lon, lat, data = synop_merge, zoom = 13,  maptype = "toner-background", color = CO2_ppm_ave, shape = EOS_no)+
  scale_color_gradient(low="blue", high="red") 



