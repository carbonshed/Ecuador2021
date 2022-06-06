## This is where I will work on synoptic data


# Install
#install.packages("wesanderson")
#install.packages("ggmap")

# Load libraries
library(ggmap)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(here)
library(lubridate)
library(chron)

API_Key <- "AIzaSyAL9hbyMHAI_3a99HJyB4_LjI_M_pGMOGg"
register_google(key = API_Key, write = TRUE)

#read in continuous data to correct vaisala
ContinuousData <-  read.csv(here::here("Synoptic/ContinuousData_forSynop_2022-01-27.csv"), skip=0, header = TRUE, sep = ",",
                             quote = "\"",dec = ".", fill = TRUE, comment.char = "")
ContinuousData$X <- NULL
ContinuousData$DateTime <- as.POSIXct(ContinuousData$DateTime,  format="%Y-%m-%d %H:%M:%S", tz = "UTC")

##June 18th
#missing GPS coordinates for 1 location
#this day only new Vaisala
CO2_eos1_June18 <-  read.csv(here::here("Synoptic/June18_Edited/Vnew_EOS1_synoptic_2021-06-18.csv"), skip=6, header = TRUE, sep = ",",
                            quote = "\"",dec = ".", fill = TRUE, comment.char = "")
colnames(CO2_eos1_June18) <- c("Date","Time","CO2_ppm","Tract","Description","Point","Lon","Lat","WaterSample", "Notes", "Notes_2")
CO2_eos1_June18$EOS_no <- "EOS_1"
CO2_eos1_June18$VaisalaType <- "new"
CO2_June18 <- CO2_eos1_June18
rm(CO2_eos1_June18)

#June 22
#new vaisala
CO2_eos1_June22 <-  read.csv(here::here("Synoptic/June22/Vnew_EOS1_synoptic_2021-06-22.csv"), skip=6, header = TRUE, sep = ",",
                 quote = "\"",dec = ".", fill = TRUE, comment.char = "")
colnames(CO2_eos1_June22) <- c("Date","Time","CO2_ppm","Tract","Description","Point","Lon","Lat","WaterSample")

CO2_eos1_June22$EOS_no <- "EOS_1"
CO2_eos1_June22$VaisalaType <- "new"
CO2_eos1_June22$Notes <- NA
CO2_eos1_June22$Notes_2 <- NA

#old vaisala
CO2_eos2_June22 <-  read.csv(here::here("Synoptic/June22/Vold_EOS2_synoptic_2021-06-22.csv"), skip=6, header = TRUE, sep = ",",
                 quote = "\"",dec = ".", fill = TRUE, comment.char = "")
CO2_eos2_June22$Voltage..ppm. <- NULL
colnames(CO2_eos2_June22) <- c("Date","Time","CO2_ppm","Tract","Description","Point","Lon","Lat","WaterSample")
CO2_eos2_June22$EOS_no <- "EOS_2"
CO2_eos2_June22$VaisalaType <- "old"
CO2_eos2_June22$Notes <- NA
CO2_eos2_June22$Notes_2 <- NA

CO2_June22 <- rbind(CO2_eos1_June22,CO2_eos2_June22)
rm(CO2_eos1_June22,CO2_eos2_June22)

## June  23
#old vaisala
CO2_eos1_June23 <-  read.csv(here::here("Synoptic/June23/Vold_EOS1_synoptic_2021-06-23.csv"), skip=6, header = TRUE, sep = ",",
                      quote = "\"",dec = ".", fill = TRUE, comment.char = "")
CO2_eos1_June23$Voltage..ppm. <- NULL
CO2_eos1_June23$DIFF <- NULL
colnames(CO2_eos1_June23) <- c("Date","Time","CO2_ppm","Tract","Description","Point","Lon","Lat","WaterSample")
CO2_eos1_June23$EOS_no <- "EOS_1"
CO2_eos1_June23$VaisalaType <- "old"
CO2_eos1_June23$Notes <- NA
CO2_eos1_June23$Notes_2 <- NA

#new Vaiala
CO2_eos2_June23 <-  read.csv(here::here("Synoptic/June23/Vnew_EOS2_synoptic_2021-06-23.csv"), skip=6, header = TRUE, sep = ",",
                             quote = "\"",dec = ".", fill = TRUE, comment.char = "")
colnames(CO2_eos2_June23) <- c("Date","Time","CO2_ppm","Tract","Description","Point","Lon","Lat","WaterSample")
CO2_eos2_June23$EOS_no <- "EOS_2"
CO2_eos2_June23$VaisalaType <- "new"
CO2_eos2_June23$Notes <- NA
CO2_eos2_June23$Notes_2 <- NA

CO2_June23 <- rbind(CO2_eos1_June23,CO2_eos2_June23)
rm(CO2_eos1_June23,CO2_eos2_June23)

## June  29

#SynopticNew - New Vaisala THERE IS A SWITCH IN VIASIALA but I don't know if the "new" vaisala is an old one or a new one
    # this happens 12:34 - 13:05
CO2_eos1_June29 <-  read.csv(here::here("Synoptic/June29_Edited/Vnew_EOS1_synoptic_2021-06-29.csv"), skip=6, header = TRUE, sep = ",",
                             quote = "\"",dec = ".", fill = TRUE, comment.char = "")
colnames(CO2_eos1_June29) <- c("Date","Time","CO2_ppm","Tract","Description","Point","Lon","Lat","WaterSample","Notes","Notes_2")
CO2_eos1_June29$EOS_no <- "EOS_1"
CO2_eos1_June29$VaisalaType <- "new"
CO2_eos1_June29$Notes <- NA
CO2_eos1_June29$Notes_2 <- NA

#SynopticOld - Old Vaisala and switch to New
  #vaisala sleeve ripped so switch to stn 2 Vaisala wich is New, at 
CO2_eos2_June29 <-  read.csv(here::here("Synoptic/June29_Edited/Vold_EOS2_synoptic_2021-06-29.csv"), skip=6, header = TRUE, sep = ",",
                             quote = "\"",dec = ".", fill = TRUE, comment.char = "")
colnames(CO2_eos2_June29) <- c("Date","Time","CO2_ppm","Tract","Description","Point","Lon","Lat","WaterSample","Notes")
CO2_eos2_June29$EOS_no <- "EOS_2"
CO2_eos2_June29$DateTime <- as.POSIXct(paste(CO2_eos2_June29$Date, CO2_eos2_June29$Time),format = "%m/%d/%Y %I:%M:%S %p", tz = "UTC" )
CO2_eos2_June29_1 <- subset(CO2_eos2_June29, DateTime < as.POSIXct('2021-06-29 12:00:00', tz="UTC"))
CO2_eos2_June29_1$VaisalaType <- "old"
CO2_eos2_June29_2 <- subset(CO2_eos2_June29, DateTime > as.POSIXct('2021-06-29 12:00:00', tz="UTC"))
CO2_eos2_June29_2$VaisalaType <- "new"
CO2_eos2_June29 <- rbind(CO2_eos2_June29_1,CO2_eos2_June29_2)
CO2_eos2_June29$DateTime <- NULL

rm(CO2_eos2_June29_1,CO2_eos2_June29_2)
CO2_eos2_June29$Notes <- NA
CO2_eos2_June29$Notes_2 <- NA

CO2_June29 <- rbind(CO2_eos1_June29,CO2_eos2_June29)
rm(CO2_eos1_June29,CO2_eos2_June29)


## June 30
#EOS1 data has missing coodinates for GPS, however we can approximaTE LOCATION BC IT WAS AROUND WHERE lIZ (eos2) STARTED ON THE SAME DAY

#SynopticNew -New Vaisala
CO2_eos1_June30 <-  read.csv(here::here("Synoptic/June30_Edited/Vnew_EOS1_synoptic_2021-06-30.csv"), skip=6, header = TRUE, sep = ",",
                             quote = "\"",dec = ".", fill = TRUE, comment.char = "")
colnames(CO2_eos1_June30) <- c("Date","Time","CO2_ppm","Tract","Description","Point","Lon","Lat","WaterSample","Notes")
CO2_eos1_June30$EOS_no <- "EOS_1"
CO2_eos1_June30$VaisalaType <- "new"
CO2_eos1_June30$Notes_2 <- NA
CO2_eos1_June30$Lon <- as.numeric(CO2_eos1_June30$Lon)
CO2_eos1_June30$Lat <- as.numeric(CO2_eos1_June30$Lat)

#SynoticOld - Old Vaisala 
CO2_eos2_June30 <-  read.csv(here::here("Synoptic/June30_Edited/Vold_EOS2_synoptic_2021-06-30.csv"), skip=6, header = TRUE, sep = ",",
                             quote = "\"",dec = ".", fill = TRUE, comment.char = "")
colnames(CO2_eos2_June30) <- c("Date","Time","CO2_ppm","Tract","Description","Point","Lon","Lat","WaterSample")
CO2_eos2_June30$EOS_no <- "EOS_2"
CO2_eos2_June30$VaisalaType <- "old"
CO2_eos2_June30$Notes <- NA
CO2_eos2_June30$Notes_2 <- NA

CO2_June30 <- rbind(CO2_eos1_June30,CO2_eos2_June30)
rm(CO2_eos1_June30,CO2_eos2_June30)

CO2_synop <- rbind(CO2_June18, CO2_June22,CO2_June23,CO2_June29,CO2_June30)
CO2_synop$DateTime <- as.POSIXct(paste(CO2_synop$Date, CO2_synop$Time), format="%m/%d/%Y %I:%M:%S %p")
CO2_synop$Date <- as.Date(CO2_synop$Date, format="%m/%d/%Y" )
CO2_synop$DateTime <- round_date(CO2_synop$DateTime, unit = "15 minute")
CO2_synop$Time <- NULL

#adjust vaisala for temp and pressure
CO2_synop <- left_join(CO2_synop,ContinuousData,by="DateTime")

CO2_synop$DateTime <- as.POSIXct(paste(CO2_synop$Date, CO2_synop$Time), format="%m/%d/%Y %I:%M:%S %p")
CO2_synop$Date <- as.Date(CO2_synop$Date, format="%m/%d/%Y" )
CO2_synop$DateTime <- round_date(CO2_synop$DateTime, unit = "15 minute")
CO2_synop$Time <- NULL


CO2_synop_pivot <- CO2_synop  %>%
  drop_na(Lat)  %>%
  group_by(Lat, Lon, Date, EOS_no, VaisalaType) %>%
  summarize(CO2_ppm_ave = mean(CO2_ppm, na.rm = TRUE),
            GAVI_waterTempAve = mean(GAVI_waterTempAve, na.rm = TRUE),
            BaroPress_kpa = mean(Baro_kpa, na.rm = TRUE),
            AirTemp_c = mean(BaroTemp_c, na.rm = TRUE),
            Tract = mean(Tract, na.rm = TRUE),
            Point = mean(Point, na.rm = TRUE))# %>%




##Correct for temp and pressure
#assume 2 cm submersion of vaisala
CO2_synop_pivot$Total_hPa <- CO2_synop_pivot$BaroPress_kpa * 10 + 1*2.4884

old <- CO2_synop_pivot[CO2_synop_pivot$VaisalaType == "old", ]
new <- CO2_synop_pivot[CO2_synop_pivot$VaisalaType == "new", ]

old$adjusted_ppm <- 
  old$CO2_ppm_ave * (1 + (1013 - old$Total_hPa) * 0.0015) *
  (1 - (25 - old$GAVI_waterTempAve) * 0.003)


#new vaisalas pressure set point (in 2021) is 700hpa, and they have their own internal temperature sensor
#also the units on the omega were wrong, so multiply by 2
new$CO2_ppm_ave_save <-  new$CO2_ppm_ave
new$CO2_ppm_ave <-  new$CO2_ppm_ave_save*2
new$CO2_ppm_ave[new$CO2_ppm_ave_save==10000] <- 10000

new$adjusted_ppm <- 
  new$CO2_ppm_ave * (1 + (700 - new$Total_hPa) * 0.0015) 
new$CO2_ppm_ave_save <- NULL


CO2_synop_pivot <- rbind(old,new)

#CO2_synop_pivot$Date.as.fact <- as.factor(CO2_synop_pivot$Date)

CO2_map <- qmplot(Lat, Lon, data = CO2_synop_pivot, zoom = 13,  maptype = "toner-background",color=CO2_ppm_ave) +
  scale_color_gradient(low="blue", high="red")

##################
####EOS DATA #####
##################

#June 18
Flux_eos1_June18 <-  read.csv(here::here("Synoptic/June18_Edited/EOS1_Edited_2021-06-18_EditedAgain.csv"), skip=0, header = TRUE, sep = ",",
                              quote = "\"",dec = ".", fill = TRUE, comment.char = "")
Flux_eos1_June18 <- Flux_eos1_June18[,c(1:6,12:17)]
colnames(Flux_eos1_June18) <- c("Month","Day","Year","Time","Flux","Temp","Tract","Description","Point","Lon","Lat","WaterSample")
Flux_eos1_June18$EOS_no <- "EOS_1"

Flux_eos1_June18$Date <- as.Date(with(Flux_eos1_June18, paste(Year, Month, Day,sep="-")), "%y-%m-%d")

Flux_June18 <- Flux_eos1_June18
rm(Flux_eos1_June18)
# June 22
#I'd like to look at the notebook to double check the times we took samples
Flux_eos1_June22 <-  read.csv(here::here("Synoptic/June22/EOS1_Synoptic_2021-06-22_EditedAgain.csv"), skip=0, header = TRUE, sep = ",",
                              quote = "\"",dec = ".", fill = TRUE, comment.char = "")
Flux_eos1_June22 <- Flux_eos1_June22[,c(1:6,12:17)]
colnames(Flux_eos1_June22) <- c("Month","Day","Year","Time","Flux","Temp","Tract","Description","Point","Lon","Lat","WaterSample")
Flux_eos1_June22$EOS_no <- "EOS_1"
Flux_eos1_June22$Date <- as.Date(with(Flux_eos1_June22, paste(Year, Month, Day,sep="-")), "%y-%m-%d")

Flux_eos2_June22 <-  read.csv(here::here("Synoptic/June22/EOS2_Synoptic_2021-06-22_EditedAgain.csv"), skip=0, header = TRUE, sep = ",",
                              quote = "\"",dec = ".", fill = TRUE, comment.char = "")
Flux_eos2_June22 <- Flux_eos2_June22[,c(1:6,16:21)]
colnames(Flux_eos2_June22) <- c("Month","Day","Year","Time","Flux","Temp","Tract","Description","Point","Lon","Lat","WaterSample")
Flux_eos2_June22$EOS_no <- "EOS_2"
Flux_eos2_June22$Date <- as.Date(with(Flux_eos2_June22, paste(Year, Month, Day,sep="-")), "%y-%m-%d")

Flux_June22 <- rbind(Flux_eos1_June22,Flux_eos2_June22)
rm(Flux_eos1_June22,Flux_eos2_June22)
#June 23
Flux_eos1_June23 <-  read.csv(here::here("Synoptic/June23/EOS1_Synoptics_2021-06-23_EditedAgain.csv"), skip=0, header = TRUE, sep = ",",
                              quote = "\"",dec = ".", fill = TRUE, comment.char = "")
Flux_eos1_June23 <- Flux_eos1_June23[,c(1:6,12:17)]
colnames(Flux_eos1_June23) <- c("Month","Day","Year","Time","Flux","Temp","Tract","Description","Point","Lon","Lat","WaterSample")
Flux_eos1_June23$EOS_no <- "EOS_1"
Flux_eos1_June23$Date <- as.Date(with(Flux_eos1_June23, paste(Year, Month, Day,sep="-")), "%y-%m-%d")

Flux_eos2_June23 <-  read.csv(here::here("Synoptic/June23/EOS2_Synoptics_2021-06-23_EditedAgain.csv"), skip=0, header = TRUE, sep = ",",
                              quote = "\"",dec = ".", fill = TRUE, comment.char = "")
Flux_eos2_June23 <- Flux_eos2_June23[,c(1:6,16:21)]
colnames(Flux_eos2_June23) <- c("Month","Day","Year","Time","Flux","Temp","Tract","Description","Point","Lon","Lat","WaterSample")
Flux_eos2_June23$EOS_no <- "EOS_2"
Flux_eos2_June23$Date <- as.Date(with(Flux_eos2_June23, paste(Year, Month, Day,sep="-")), "%y-%m-%d")

Flux_June23 <- rbind(Flux_eos1_June23,Flux_eos2_June23)
rm(Flux_eos1_June23,Flux_eos2_June23)
#June 29
Flux_eos1_June29 <-  read.csv(here::here("Synoptic/June29_Edited/EOS1_Edited_2021-06-29_EditedAgain.csv"), skip=0, header = TRUE, sep = ",",
                              quote = "\"",dec = ".", fill = TRUE, comment.char = "")
Flux_eos1_June29 <- Flux_eos1_June29[,c(1:6,12:17)]
colnames(Flux_eos1_June29) <- c("Month","Day","Year","Time","Flux","Temp","Tract","Description","Point","Lon","Lat","WaterSample")
Flux_eos1_June29$EOS_no <- "EOS_1"
Flux_eos1_June29$Date <- as.Date(with(Flux_eos1_June29, paste(Year, Month, Day,sep="-")), "%y-%m-%d")

Flux_eos2_June29 <-  read.csv(here::here("Synoptic/June29_Edited/EOS2_Edited_2021-06-29_EditedAgain.csv"), skip=0, header = TRUE, sep = ",",
                              quote = "\"",dec = ".", fill = TRUE, comment.char = "")
Flux_eos2_June29 <- Flux_eos2_June29[,c(1:6,16:21)]
colnames(Flux_eos2_June29) <- c("Month","Day","Year","Time","Flux","Temp","Tract","Description","Point","Lon","Lat","WaterSample")
Flux_eos2_June29$EOS_no <- "EOS_2"
Flux_eos2_June29$Date <- as.Date(with(Flux_eos2_June29, paste(Year, Month, Day,sep="-")), "%y-%m-%d")

Flux_June29 <- rbind(Flux_eos1_June29,Flux_eos2_June29)
rm(Flux_eos1_June29,Flux_eos2_June29)

#June 30
Flux_eos1_June30 <-  read.csv(here::here("Synoptic/June30_Edited/EOS1_Edited_2021-06-30_EditedAgain.csv"), skip=0, header = TRUE, sep = ",",
                              quote = "\"",dec = ".", fill = TRUE, comment.char = "")
Flux_eos1_June30 <- Flux_eos1_June30[,c(1:6,12:17)]
colnames(Flux_eos1_June30) <- c("Month","Day","Year","Time","Flux","Temp","Tract","Description","Point","Lon","Lat","WaterSample")
Flux_eos1_June30$EOS_no <- "EOS_1"
Flux_eos1_June30$Date <- as.Date(with(Flux_eos1_June30, paste(Year, Month, Day,sep="-")), "%y-%m-%d")

Flux_eos2_June30 <-  read.csv(here::here("Synoptic/June30_Edited/EOS2_Synoptic_EDITED_2021-06-30_EditedAgain.csv"), skip=0, header = TRUE, sep = ",",
                              quote = "\"",dec = ".", fill = TRUE, comment.char = "")
Flux_eos2_June30 <- Flux_eos2_June30[,c(1:6,16:21)]
colnames(Flux_eos2_June30) <- c("Month","Day","Year","Time","Flux","Temp","Tract","Description","Point","Lon","Lat","WaterSample")
Flux_eos2_June30$EOS_no <- "EOS_2"
Flux_eos2_June30$Date <- as.Date(with(Flux_eos2_June30, paste(Year, Month, Day,sep="-")), "%y-%m-%d")

Flux_June30 <- rbind(Flux_eos1_June30,Flux_eos2_June30)
rm(Flux_eos1_June30,Flux_eos2_June30)

#Flux_map <- qmplot(Lon, Lat, data = Flux_June30_pivot, zoom = 13,  maptype = "toner-background", color = Flux_ave)+
#  scale_color_gradient(low="blue", high="red")



Flux_synop <- rbind(Flux_June18, Flux_June22, Flux_June23, Flux_June29, Flux_June30)
Flux_synop <- Flux_synop%>% drop_na(Date)

#bind flux

y <- hms(Flux_synop$Time)
Flux_synop$Time <- hour(y) + minute(y) / 60 + second(y) / 360

Flux_synop_pivot <- Flux_synop  %>%
  drop_na(Lat)  %>%
  group_by(Lat, Lon, Date, EOS_no) %>%
  summarize(Flux_ave = mean(Flux, na.rm = TRUE),
            Tract = mean(Tract, na.rm = TRUE),
            Point = mean(Point, na.rm = TRUE),
            Time = mean(Time))


### Merge data
synop_merge <- full_join(Flux_synop_pivot,CO2_synop_pivot, by = c("Lon","Lat","Date", "Tract", "Point","EOS_no"))

synop_merge$Time<- times(synop_merge$Time / 24)
synop_merge$DateTime <- as.POSIXct(paste(synop_merge$Date, synop_merge$Time), format = "%Y-%m-%d %H:%M")



##Water Chem

WaterChem <-  read.csv(here::here("WaterChem/DOC_9-1-2021_UNC_EDIT.csv"), skip=0, header = TRUE, sep = ",",
                       quote = "\"",dec = ".", fill = TRUE, comment.char = "")
WaterChem$Date <- as.Date(WaterChem$Date, format = "%m/%d/%y")
#WaterChem$EOS_no <- WaterChem$ID
WaterChem$EOS_no <- gsub("_.*", "", WaterChem$sampleID)     

WaterChem <- WaterChem%>%
  filter(SampleType == "synoptic")%>%
  filter(Date <= "2021-06-30")

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


#write.csv(synop_merge, here::here("Synoptic/GAVI_2022-06-04.csv"))



###END###




#plot
synop_mapFlux <- qmplot(Lat, Lon, data = synop_merge, zoom = 13,  maptype = "toner-background", color = Flux_ave)+
  scale_color_gradient(low="blue", high="red")
synop_mapCO2 <- qmplot(Lat,Lon, data = synop_merge, zoom = 13,  maptype = "toner-background", color = CO2_ppm_ave)+
  scale_color_gradient(low="blue", high="red")

