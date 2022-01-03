##Water Level
#This is code is to merge water level data and correct for barometric pressure
#Authors Kriddie and Liz
#Date: 2021-12-15

library(lubridate) #package for our date parsing
library(here)
library(dplyr)
library(ggplot2)
library(reshape2)
library(purrr)
library(sjmisc)
library(plotly)
theme_set(theme_bw())

#First the Hobos

setwd(here::here("WaterLevel"))
all_files=list.files(pattern=".csv") #pulls out the csv files from CO2 folder
sites_rp = sub('_[^_]+$', '', all_files)
site_names=unique(sites_rp) #creates list of site names for following loop

site_names = site_names[c(2:6)]

#rm old files, if they exist
rm(WLData)
rm(Temp_WLData)

for (site in site_names){
  
  list1=list.files(pattern=site) #finds all files for the site
  sitelist_csv=grep(".csv",list1) #creates list of all files for site
  file_list=list1[sitelist_csv]
  
  #reads in files in list and appends
  for (file in file_list){
    if (!exists("WLData")){
      WLData <- read.csv(file, skip=1, header = FALSE, sep = ",",
                         quote = "\"",dec = ".", fill = TRUE, comment.char = "")
      
      if(str_contains(WLData[1,3],"Abs Pres, psi")){
        WLData <- WLData[-1,]
        colnames(WLData)=c("row","DateTime","WLPres_kpa","WLTemp_c")
        WLData <- WLData[2:4]
        WLData$WLPres_kpa <- as.numeric(as.character(WLData$WLPres_kpa), digits=6)
        WLData$WLTemp_c <- as.numeric(as.character(WLData$WLTemp_c), digits=5)
        WLData$WLPres_kpa <- WLData$WLPres_kpa*6.89476
        WLData$WLTemp_c <- (WLData$WLTemp_c - 32)/1.8000

      } else { 
        WLData <- WLData[-1,]
        colnames(WLData)=c("row","DateTime","WLPres_kpa","WLTemp_c")
        WLData <- WLData[2:4]
        WLData$WLPres_kpa <- as.numeric(as.character(WLData$WLPres_kpa), digits=6)
        WLData$WLTemp_c <- as.numeric(as.character(WLData$WLTemp_c), digits=5)
        }
      
      WLData$DateTime <- as.POSIXct(WLData$DateTime, format="%m/%d/%y %I:%M:%S %p", tz="UTC")
    
    }
    if (exists("WLData")){
      Temp_WLData <- read.csv(file, skip=1, header = FALSE, sep = ",",
                              quote = "\"",dec = ".", fill = TRUE, comment.char = "")  
#      
      
      if(str_contains(Temp_WLData[1,3],"Abs Pres, psi")){
        Temp_WLData <- Temp_WLData[-1,]
        colnames(Temp_WLData)=c("row","DateTime","WLPres_kpa","WLTemp_c")
        Temp_WLData <- Temp_WLData[2:4]
        Temp_WLData$WLPres_kpa <- as.numeric(as.character(Temp_WLData$WLPres_kpa), digits=6)
        Temp_WLData$WLTemp_c <- as.numeric(as.character(Temp_WLData$WLTemp_c), digits=5)
        Temp_WLData$WLPres_kpa <- Temp_WLData$WLPres_kpa*6.89476
        Temp_WLData$WLTemp_c <- (Temp_WLData$WLTemp_c - 32)/1.8000
        
        
      } else { 
        Temp_WLData <- Temp_WLData[-1,]
        colnames(Temp_WLData)=c("row","DateTime","WLPres_kpa","WLTemp_c")
        Temp_WLData <- Temp_WLData[2:4]
        Temp_WLData$WLPres_kpa <- as.numeric(as.character(Temp_WLData$WLPres_kpa), digits=6)
        Temp_WLData$WLTemp_c <- as.numeric(as.character(Temp_WLData$WLTemp_c), digits=5)
        }
      
      Temp_WLData$DateTime <- as.POSIXct(Temp_WLData$DateTime, format="%m/%d/%y %I:%M:%S %p", tz="UTC")
      
      WLData <- rbind(WLData, Temp_WLData)
      rm(Temp_WLData)
    }
    
  }
  WLData$DateTime <- round_date(WLData$DateTime, "15 mins")
  WLData$Station <- site
  WLData=unique(WLData)
  assign((paste(site,sep="_")),WLData) #creates object with new appended data
  rm(WLData) #removes WLdata so that multiple sites aren't appended together
}

############################################
#this loop is for the sites with solinists##
############################################

setwd(here::here("WaterLevel"))
all_files=list.files(pattern=".csv") #pulls out the csv files from CO2 folder
sites_rp = sub('_[^_]+$', '', all_files)
site_names=unique(sites_rp) #creates list of site names for following loop

site_names = site_names[-c(2:6)]

#rm old files, if they exist
rm(WLData)
rm(Temp_WLData)

for (site in site_names){
  
  list1=list.files(pattern=site) #finds all files for the site
  sitelist_csv=grep(".csv",list1) #creates list of all files for site
  file_list=list1[sitelist_csv]
  
  #reads in files in list and appends
  for (file in file_list){
    if (!exists("WLData")){
      Trash_WLData <- read.csv(file, skip=0, header = TRUE, sep = ",",
                         quote = "\"",dec = ".", fill = TRUE, comment.char = "")
      
      if(str_contains(Trash_WLData[6,1],"m")){
        print("m")
        WLData <- read.csv(file, skip=11, header = TRUE, sep = ",",
                    quote = "\"",dec = ".", fill = TRUE, comment.char = "")
        colnames(WLData)=c("Date","Time","ms","WLPres_kpa","WLTemp_c")
        WLData$WLPres_kpa <- as.numeric(as.character(WLData$WLPres_kpa), digits=6)
        WLData$WLTemp_c <- as.numeric(as.character(WLData$WLTemp_c), digits=5)
        WLData$WLPres_kpa <- WLData$WLPres_kpa*9.8064
#        WLData$WLTemp_c <- (WLData$WLTemp_c - 32)/1.8000
        } else { 
        print("kPa")
        WLData <- read.csv(file, skip=11, header = TRUE, sep = ",",
            quote = "\"",dec = ".", fill = TRUE, comment.char = "")
        colnames(WLData)=c("Date","Time","ms","WLPres_kpa","WLTemp_c")
        WLData$WLPres_kpa <- as.numeric(as.character(WLData$WLPres_kpa), digits=6)
        WLData$WLTemp_c <- as.numeric(as.character(WLData$WLTemp_c), digits=5)
      }
    }
    if (exists("WLData")){
      Trash_WLData <- read.csv(file, skip=0, header = FALSE, sep = ",",
                              quote = "\"",dec = ".", fill = TRUE, comment.char = "")  
      if(str_contains(Trash_WLData[6,1],"m")){
        print("m")
        Temp_WLData <- read.csv(file, skip=11, header = TRUE, sep = ",",
                           quote = "\"",dec = ".", fill = TRUE, comment.char = "")
        Temp_WLData <- Temp_WLData[c(-1),]
        colnames(Temp_WLData)=c("Date","Time","ms","WLPres_kpa","WLTemp_c")
        Temp_WLData$WLPres_kpa <- as.numeric(as.character(Temp_WLData$WLPres_kpa), digits=6)
        Temp_WLData$WLTemp_c <- as.numeric(as.character(Temp_WLData$WLTemp_c), digits=5)
        Temp_WLData$WLPres_kpa <- Temp_WLData$WLPres_kpa*9.8064
#        Temp_WLData$WLTemp_c <- (Temp_WLData$WLTemp_c - 32)/1.8000
        } else { 
        print("kpa")
        Temp_WLData <- read.csv(file, skip=11, header = TRUE, sep = ",",
                                quote = "\"",dec = ".", fill = TRUE, comment.char = "")
        Temp_WLData <- Temp_WLData[c(-1),]
        colnames(Temp_WLData)=c("Date","Time","ms","WLPres_kpa","WLTemp_c")
        Temp_WLData$WLPres_kpa <- as.numeric(as.character(Temp_WLData$WLPres_kpa), digits=6)
        Temp_WLData$WLTemp_c <- as.numeric(as.character(Temp_WLData$WLTemp_c), digits=5)
      }
      
      
      WLData <- rbind(WLData, Temp_WLData)
      rm(Temp_WLData)
    }
    
  }
  WLData=unique(WLData)
  WLData$DateTime <- paste(WLData$Date, WLData$Time)
  WLData$DateTime <- as.POSIXct(WLData$DateTime, format="%m/%d/%Y %I:%M:%S %p", tz="UTC")
  WLData$DateTime <- round_date(WLData$DateTime, "5 mins")
  WLData <- WLData[,c("DateTime","WLPres_kpa","WLTemp_c")]
  WLData$Station <- site
  assign((paste(site,sep="_")),WLData) #creates object with new appended data
  rm(WLData) #removes WLdata so that multiple sites aren't appended together
}


##clean data
plot_ly(WL_06, x = ~DateTime, y = ~WLPres_kpa, type = 'scatter', mode = 'markers') 

WL_01 <- WL_01%>%filter(WLPres_kpa > 63 & WLPres_kpa < 70)
WL_02 <- WL_02%>%filter(WLPres_kpa > 63)
WL_03 <- WL_03%>%filter(WLPres_kpa > 63.1)
WL_04 <- WL_04%>%filter(WLPres_kpa > 63)
WL_05 <- WL_05%>%filter(DateTime != as.POSIXct("2021-07-28 14:00:00", tz = "UTC"))
WL_06 <- WL_06%>%filter(DateTime != as.POSIXct("2021-07-16 14:00:00", tz = "UTC"))
WL_06 <- WL_06%>%filter(DateTime < as.POSIXct("2021-07-26 14:30:00", tz = "UTC")|
                          DateTime > as.POSIXct("2021-07-28 14:15:00", tz = "UTC"))

#Well 2 was moved up, it seams
#Add (64.49375 - 64.09757) for andything before July 27, 2021 10:40
WL_Well02_1 <- WL_Well02%>%filter(DateTime <= as.POSIXct("2021-07-27 10:40:00", tz = "UTC"))
WL_Well02_1$WLPres_kpa <- WL_Well02_1$WLPres_kpa + (64.49375 - 64.09757)
WL_Well02_2 <- WL_Well02%>%filter(DateTime > as.POSIXct("2021-07-27 10:40:00", tz = "UTC"))
WL_Well02 <- rbind(WL_Well02_1,WL_Well02_2)
rm(WL_Well02_1,WL_Well02_2)

#Well 01 looks fine

All_WL <- rbind(WL_01,WL_02,WL_03,WL_04,WL_05,WL_06,WL_Well01,WL_Well02)


###merge data with baro data###
setwd(here::here("Baro"))
all_files=list.files(pattern=".csv") #pulls out the csv files from WL folder in HOBOware folder
sites_rp=gsub("_.*","",all_files) #selects the correct pattern so as to seelct only desired files
site_names=unique(sites_rp) #creates list of site names for following loop

#rm old files, if they exsist
rm(BaroData)
rm(Temp_BaroData)

for (site in site_names){
  
  list1=list.files(pattern=site) #finds all files for the site
  sitelist_csv=grep(".csv",list1) #creates list of all files for site
  file_list=list1[sitelist_csv]
  
  #reads in files in list and appends
  for (file in file_list){
    if (!exists("BaroData")){
      BaroData <- read.csv(file, skip=10, header = TRUE, sep = ",",
                           quote = "\"",dec = ".", fill = TRUE, comment.char = "")
    }
    if (exists("BaroData")){
      Temp_BaroData <- read.csv(file, skip=10, header = TRUE, sep = ",",
                                quote = "\"",dec = ".", fill = TRUE, comment.char = "")  
      BaroData <- rbind(BaroData, Temp_BaroData)
      rm(Temp_BaroData)
    }
    
  }
  colnames(BaroData)=c("Date","Time","ms","Baro_kpa","BaroTemp_c")
  BaroData=unique(BaroData)
  BaroData$DateTime <- paste(BaroData$Date, BaroData$Time)
  BaroData$DateTime <- as.POSIXct(BaroData$DateTime, format="%m/%d/%Y %I:%M:%S %p", tz="UTC")
  BaroData$DateTime <- round_date(BaroData$DateTime, "15 mins")
  BaroData$ms <- NULL
  BaroData$Date <- NULL
  BaroData$Time <- NULL
  BaroData <- BaroData[,c(3,1,2)]
  BaroData=unique(BaroData)
  assign((paste(site,sep="_")),BaroData) #creates object with new appended data
  rm(BaroData) #removes WLdata so that multiple sites aren't appended together
}


##clean BaroData
plot_ly(Baro%>%filter(DateTime > as.POSIXct("2021-06-11 00:00:00", tz = "UTC")), x = ~DateTime, y = ~Baro_kpa, type = 'scatter', mode = 'markers') 

Baro <- Baro%>%filter(DateTime < as.POSIXct("2021-07-27 15:00:00", tz = "UTC")|
                          DateTime > as.POSIXct("2021-07-28 13:30:00", tz = "UTC"))


#correct for baro 

Baro_corrected <- left_join(Baro,All_WL,by="DateTime")

Baro_corrected$Corrected_kpa <- Baro_corrected$WLPres_kpa - Baro_corrected$Baro_kpa
#Baro_corrected <- Baro_corrected %>% filter(DateTime > "2021-06-10")

#convert kpa to meters of water
# constant, 1 kpa = 0.101972 m water
Baro_corrected$WL_m <- Baro_corrected$Corrected_kpa * 0.101972
Baro_corrected$Corrected_kpa <- NULL


##Now we can graph 
    ## will need to change the right limit accordingly

ggplot(data = Baro_corrected %>% filter(DateTime > "2021-06-10"), 
       aes(DateTime, WL_m)) +
  geom_line(color = "steelblue") +
  #  geom_point(color="steelblue") + 
#  scale_x_datetime(limits = c(as.POSIXct("2021-06-10 15:45:00 UTC"),
#                              as.POSIXct("2021-11-00 12:45:00 UTC")))+
  labs(#title = "CO2  stations",
    y = "Water Level [m]", x = "") + 
  facet_wrap(~ Station)




##find level at date time 

Baro_corrected %>% 
  filter(
    DateTime == 
      as.POSIXct("2021-07-28 14:30:00", tz = "UTC") &
                            Station == "WL_06")


plot_ly(Baro_corrected%>%filter(Station == "WL_06"), x = ~DateTime, y = ~WL_m  , type = 'scatter', mode = 'markers') 

