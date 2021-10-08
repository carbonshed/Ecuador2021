library(lubridate) #package for our date parsing
library(here)
library(dplyr)
library(ggplot2)
library(reshape2)
library(purrr)
library(plotly)
##set folder for site ##

setwd(here::here("WaterLevel"))
all_files=list.files(pattern=".csv") #pulls out the csv files from CO2 folder
sites_rp = sub('_[^_]+$', '', all_files)
site_names=unique(sites_rp) #creates list of site names for following loop
#site_names = site_names[-1]
site_names <- site_names[site_names != "WL_01"]
site_names <- site_names[site_names !=  "WL_Well2"]# remove solinst station

#rm old files, if they exist
rm(WLData)
rm(Temp_WLData)

###########################################################
for (site in site_names){
  
  list1=list.files(pattern=site) #finds all files for the site
  sitelist_csv=grep(".csv",list1) #creates list of all files for site
  file_list=list1[sitelist_csv]
  
  #reads in files in list and appends
  for (file in file_list){
    if (!exists("WLData")){
      WLData <- read.csv(file, skip=2, header = FALSE, sep = ",",
                         quote = "\"",dec = ".", fill = TRUE, comment.char = "")
      
    }
    if (exists("WLData")){
      Temp_WLData <- read.csv(file, skip=2, header = FALSE, sep = ",",
                              quote = "\"",dec = ".", fill = TRUE, comment.char = "")  
      
      WLData <- rbind(WLData, Temp_WLData)
      rm(Temp_WLData)
    }
    
  }
  colnames(WLData)=c("row","DateTime","WLPres_kpa","WLTemp_c")
  WLData=WLData[,2:4]
  WLData=unique(WLData)
  
  WLData$DateTime_1 <- WLData$DateTime
  WLData$DateTime <- as.POSIXct(WLData$DateTime_1, format = "%m/%d/%y %I:%M:%S %p", tz="UTC")
  WLData$DateTime[is.na(WLData$DateTime)] <- as.POSIXct(WLData$DateTime_1[is.na(WLData$DateTime)], format = "%m/%d/%Y %H:%M", tz="UTC")
  WLData$DateTime_1 <-NULL
  WLData$DateTime <- lubridate::round_date(WLData$DateTime, "15 minutes") 

     assign((paste(site,"WLdata",sep="_")),WLData) #creates object with new appended data
  rm(WLData) #removes WLdata so that multiple sites aren't appended together
}


######this loop is for the site with solinist
  #solinist is at WL_01 and WL_Well2

#rm old files, if they exist
rm(WLData)
rm(Temp_WLData)

site_names = c("WL_01","WL_Well2")

for (site in site_names){
  
  list1=list.files(pattern=site) #finds all files for the site
  sitelist_csv=grep(".csv",list1) #creates list of all files for site
  file_list=list1[sitelist_csv]
  
  #reads in files in list and appends
  for (file in file_list){
    if (!exists("WLData")){
      WLData <- read.csv(file, skip=11, header = TRUE, sep = ",",
                         quote = "\"",dec = ".", fill = TRUE, comment.char = "")
      colnames(WLData)=c("Date","Time","ms","kPa","Temp")
    }
    if (exists("WLData")){
      Temp_WLData <- read.csv(file, skip=11, header = TRUE, sep = ",",
                              quote = "\"",dec = ".", fill = TRUE, comment.char = "")  
      colnames(Temp_WLData) <- c("Date","Time","ms","kPa","Temp")
      WLData <- rbind(WLData, Temp_WLData)
      rm(Temp_WLData)
    }
    
  }
  colnames(WLData)=c("Date","Time","ms","WLPres_kpa","WLTemp_c")
  WLData=unique(WLData)
  WLData$DateTime <- as.POSIXct(paste(WLData$Date, WLData$Time), format="%m/%d/%Y %I:%M:%S %p", tz="UTC")
  WLData$DateTime <- lubridate::round_date(WLData$DateTime, "15 minutes") 
  WLData = subset(WLData, select = -c(Date,Time,ms) )
  assign((paste(site,"WLdata",sep="_")),WLData) #creates object with new appended data
  rm(WLData) #removes WLdata so that multiple sites aren't appended together
}


##Now adding a station column to each of datasets
WL_01_WLdata$Station <- 1
WL_02_WLdata$Station <- 2
WL_03_WLdata$Station <- 3
WL_04_WLdata$Station <- 4
WL_05_WLdata$Station <- 5
WL_06_WLdata$Station <- 6


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
  colnames(BaroData)=c("Date","Time","ms","BaroPres_kpa","AirTemp_c")
  BaroData=unique(BaroData)
  BaroData$DateTime <- as.POSIXct(paste(BaroData$Date, BaroData$Time), format="%m/%d/%Y %I:%M:%S %p", tz="UTC")
  BaroData$DateTime <- lubridate::round_date(BaroData$DateTime, "15 minutes") 
  BaroData = subset(BaroData, select = -c(Date,Time,ms) )
  assign((paste(site,"data",sep="_")),BaroData) #creates object with new appended data
  rm(BaroData) #removes WLdata so that multiple sites aren't appended together
}

WL_01_WLdata <-  left_join(WL_01_WLdata,Baro_data,by="DateTime")
WL_02_WLdata <-  left_join(WL_02_WLdata,Baro_data,by="DateTime")
WL_03_WLdata <-  left_join(WL_03_WLdata,Baro_data,by="DateTime")
WL_04_WLdata <-  left_join(WL_04_WLdata,Baro_data,by="DateTime")
WL_05_WLdata <-  left_join(WL_05_WLdata,Baro_data,by="DateTime")
WL_06_WLdata <-  left_join(WL_06_WLdata,Baro_data,by="DateTime")
WL_Well2_WLdata <-  left_join(WL_Well2_WLdata,Baro_data,by="DateTime")

### merge WL data with Baro data for outlet and convert from kPa to cm of water
WL_01_WLdata$WL_m <- (WL_01_WLdata$WLPres_kpa - WL_01_WLdata$BaroPres_kpa)*10.19716 
WL_01_WLdata <- WL_01_WLdata %>% subset(WL_m > 0)

WL_02_WLdata$WL_m <- (WL_02_WLdata$WLPres_kpa - WL_02_WLdata$BaroPres_kpa)*10.19716 
WL_02_WLdata <- WL_02_WLdata %>% subset(WL_m > 1)

WL_03_WLdata$WL_m <- (WL_03_WLdata$WLPres_kpa - WL_03_WLdata$BaroPres_kpa)*10.19716 
WL_03_WLdata <- WL_03_WLdata %>% subset(WL_m > 10)

WL_04_WLdata$WL_m <- (WL_04_WLdata$WLPres_kpa - WL_04_WLdata$BaroPres_kpa)*10.19716 
WL_04_WLdata <- WL_04_WLdata %>% subset(WL_m > 0)

WL_05_WLdata$WL_m <- (WL_05_WLdata$WLPres_kpa - WL_05_WLdata$BaroPres_kpa)*10.19716
WL_05_WLdata <- WL_05_WLdata %>% subset(WL_m > 0)

WL_06_WLdata$WL_m <- (WL_06_WLdata$WLPres_kpa - WL_06_WLdata$BaroPres_kpa)*10.19716 
WL_06_WLdata <- WL_06_WLdata %>% subset(DateTime < as.POSIXct("2021-07-26 14:30:00", tz="UTC"))
WL_06_WLdata <- WL_06_WLdata %>% subset(WL_m > 25)

WL_Well2_WLdata$WL_m <- (WL_Well2_WLdata$WLPres_kpa - WL_Well2_WLdata$BaroPres_kpa)*10.19716 


##Now we can graph 
ggplot(WL_01_WLdata, aes(x=DateTime,y=WL_m)) + geom_point()
fig <- plot_ly(data = WL_01_WLdata, x = ~DateTime, y = ~WL_m)

###for the discharge raing curve, do it fools and sons of fools
WL_06_WLdata %>% 
  filter(DateTime == 
           as.POSIXct("2021-07-26 14:15", tz="UTC"))
