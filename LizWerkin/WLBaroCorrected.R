library(lubridate) #package for our date parsing
library(here)
library(dplyr)
library(ggplot2)
library(reshape2)
library(purrr)
theme_set(theme_bw())

##set folder for site ##

setwd(here::here("WaterLevel"))
all_files=list.files(pattern=".csv") #pulls out the csv files from CO2 folder
sites_rp = sub('_[^_]+$', '', all_files)
site_names=unique(sites_rp) #creates list of site names for following loop
site_names = site_names[-1]

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
  WLData$DateTime <- as.POSIXct(WLData$DateTime, format="%m/%d/%y %I:%M:%S %p", tz="UTC")
  assign((paste(site,"WL_data",sep="_")),WLData) #creates object with new appended data
  rm(WLData) #removes WLdata so that multiple sites aren't appended together
}


#thisloop is for the site with solinist
  #solinist is at WL_01

site_names = "WL_01"

for (site in site_names){
  
  list1=list.files(pattern=site) #finds all files for the site
  sitelist_csv=grep(".csv",list1) #creates list of all files for site
  file_list=list1[sitelist_csv]
  
  #reads in files in list and appends
  for (file in file_list){
    if (!exists("WLData")){
      WLData <- read.csv(file, skip=11, header = TRUE, sep = ",",
                         quote = "\"",dec = ".", fill = TRUE, comment.char = "")
      WLData=WLData[,1:4]
      
    }
    if (exists("WLData")){
      Temp_WLData <- read.csv(file, skip=11, header = TRUE, sep = ",",
                              quote = "\"",dec = ".", fill = TRUE, comment.char = "")  
      Temp_WLData = Temp_WLData[,1:4]
      WLData=WLData[,1:4]
      WLData <- rbind(WLData, Temp_WLData)
      rm(Temp_WLData)
    }
    
  }
  #colnames(WLData)=c("Date","Time","ms","kPa","Temp")
  WLData=WLData[,1:4]
  WLData=unique(WLData)
  WLData$DateTime <- as.POSIXct(WLData$DateTime, format="%m/%d/%y %I:%M:%S %p", tz="UTC")
  assign((paste(site,"WL_data",sep="_")),WLData) #creates object with new appended data
  rm(WLData) #removes WLdata so that multiple sites aren't appended together
}



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
      BaroData <- read.csv(file, skip=2, header = FALSE, sep = ",",
                           quote = "\"",dec = ".", fill = TRUE, comment.char = "")
      BaroData=BaroData[,2:4]
    }
    if (exists("BaroData")){
      Temp_BaroData <- read.csv(file, skip=2, header = FALSE, sep = ",",
                                quote = "\"",dec = ".", fill = TRUE, comment.char = "")  
      Temp_BaroData = Temp_BaroData[,2:4]
      BaroData <- rbind(BaroData, Temp_BaroData)
      rm(Temp_BaroData)
    }
    
  }
  colnames(BaroData)=c("DateTime","BaroPres_kpa","AirTemp_c")
  BaroData=unique(BaroData)
  #BaroData=BaroData[,2:4]
  BaroData$DateTime <- as.POSIXct(BaroData$DateTime, format="%m/%d/%y %I:%M:%S %p", tz="UTC")
  assign((paste(site,"Baro_data",sep="_")),BaroData) #creates object with new appended data
  rm(BaroData) #removes WLdata so that multiple sites aren't appended together
}


### merge WL data with Baro data for outlet and convert from kPa to cm of water
BEAVOutletWL_merge <- left_join(BeaverCreekMarshOutletWL_WL_data, SEllerbe_Baro_data, by = "DateTime")
BEAVOutletWL_merge$WL_Baroadjusted <- BEAVOutletWL_merge$WLPres_kpa - BEAVOutletWL_merge$BaroPres_kpa 
BEAVOutletWL_merge$WL_true <- BEAVOutletWL_merge$WL_Baroadjusted*10.19716 

# again for the middle data 
BEAVmiddleWL_merge <- left_join(BeaverCreekMarshMiddleWL_WL_data, SEllerbe_Baro_data, by = "DateTime")
BEAVmiddleWL_merge$WL_Baroadjusted <- BEAVmiddleWL_merge$WLPres_kpa - BEAVmiddleWL_merge$BaroPres_kpa
BEAVmiddleWL_merge$WL_true <- BEAVmiddleWL_merge$WL_Baroadjusted*10.19716 


##Now we can graph 
beavoutlet<- ggplot(BEAVOutletWL_merge, aes(x=DateTime, y=WL_true))+
  geom_point(aes(y=WL_Baroadjusted))+
  labs(x=NULL,y="WL (cm H2O)")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank())
beavoutlet

beavmiddle <- ggplot(BEAVmiddleWL_merge, aes(x=DateTime, y=WL_true))+
  geom_point(aes(y=WL_Baroadjusted))+
  labs(x=NULL,y="WL (cm H2O)")


