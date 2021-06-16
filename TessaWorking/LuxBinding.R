#binding lux data
setwd(here::here("Lux"))

library(lubridate) #package for our date parsing
library(dplyr)
library(here)

##set folder for site ##

setwd(here::here("HOBOData/Beav"))

all_files=list.files(pattern=".csv") #pulls out the csv files from WL folder in HOBOware folder
sites_rp=gsub("_.*","",all_files) #selects the correct pattern so as to seelct only desired files
site_names=unique(sites_rp) #creates list of site names for following loop

#rm old files, if they exsist
rm(WLData)
rm(Temp_WLData)

for (site in site_names){
  
  list1=list.files(pattern=site) #finds all files for the site
  sitelist_csv=grep(".csv",list1) #creates list of all files for site
  file_list=list1[sitelist_csv]
  
  #reads in files in list and appends
  for (file in file_list){
    if (!exists("LuxData")){
      LuxData <- read.csv(file, skip=2, header = FALSE, sep = ",",
                         quote = "\"",dec = ".", fill = TRUE, comment.char = "")
      
    }
    if (exists("WLData")){
      Temp_LuxData <- read.csv(file, skip=2, header = FALSE, sep = ",",
                              quote = "\"",dec = ".", fill = TRUE, comment.char = "")  
      
      LuxData <- rbind(WLData, Temp_WLData)
      rm(Temp_WLData)
    }
    
  }
  colnames(WLData)=c("row","DateTime","WLPres_kpa","WLTemp_c")
  WLData=WLData[,2:4]
  WLData=unique(WLData)
  WLData$DateTime <- as.POSIXct(WLData$DateTime, format="%m/%d/%y %I:%M:%S %p", tz="UTC")
  assign((paste(site,"WL_data",sep="_")),WLData) #creates object with new appended data
  rm(WLData) #removes WLdata so that multiple sites aren't appended together
}



###merge data with baro data###
setwd(here::here("BaroData"))
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
      BaroData <- read.csv(file, skip=2, header = FALSE, sep = ",",
                           quote = "\"",dec = ".", fill = TRUE, comment.char = "")
      
    }
    if (exists("BaroData")){
      Temp_BaroData <- read.csv(file, skip=2, header = FALSE, sep = ",",
                                quote = "\"",dec = ".", fill = TRUE, comment.char = "")  
      
      BaroData <- rbind(BaroData, Temp_BaroData)
      rm(Temp_BaroData)
    }
    
  }
  colnames(BaroData)=c("row","DateTime","BaroPres_kpa","AirTemp_c")
  BaroData=BaroData[,2:4]
  BaroData=unique(BaroData)
  BaroData$DateTime <- as.POSIXct(BaroData$DateTime, format="%m/%d/%y %I:%M:%S %p", tz="UTC")
  assign((paste(site,"Baro_data",sep="_")),BaroData) #creates object with new appended data
  rm(BaroData) #removes WLdata so that multiple sites aren't appended together
}



### merge WL data with Baro data for middle marsh
BeaverCreekMarshMiddleWL_WL_data <- left_join(BeaverCreekMarshMiddleWL_WL_data, SEllerbe_Baro_data, by = "DateTime")
BeaverCreekMarshMiddleWL_WL_data$WL_Baroadjusted <- BeaverCreekMarshMiddleWL_WL_data$WLPres_kpa - BeaverCreekMarshMiddleWL_WL_data$BaroPres_kpa 
# again for the Outlet data 
BeaverCreekMarshOutletWL_WL_data <- left_join(BeaverCreekMarshOutletWL_WL_data, SEllerbe_Baro_data, by = "DateTime")
BeaverCreekMarshOutletWL_WL_data$WL_Baroadjusted <- BeaverCreekMarshOutletWL_WL_data$WLPres_kpa - BeaverCreekMarshOutletWL_WL_data$BaroPres_kpa


