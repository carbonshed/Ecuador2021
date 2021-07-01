#binding lux data
setwd(here::here("Lux"))


library(lubridate) #package for our date parsing
library(dplyr)
library(here)

##set folder for site ##

setwd(here::here("Lux"))

all_files=list.files(pattern=".csv") #pulls out the csv files from WL folder in HOBOware folder
#sites_rp=gsub("_.*","",all_files) #selects the correct pattern so as to seelct only desired files
sites_rp = sub('_[^_]+$', '', all_files)
site_names=unique(sites_rp) #creates list of site names for following loop


#rm old files, if they exist
rm(LuxData)
rm(Temp_LuxData)

for (site in site_names){
  
  list1=list.files(pattern=site) #finds all files for the site
  sitelist_csv=grep(".csv",list1) #creates list of all files for site
  file_list=list1[sitelist_csv]
  
  #reads in files in list and appends
  for (file in file_list){
    if (!exists("LuxData")){
      LuxData <- read.csv(file, skip=2, header = FALSE, sep = ",",
                         quote = "\"",dec = ".", fill = TRUE, comment.char = "")
      LuxData=LuxData[,2:4]
      colnames(LuxData)=c("DateTime","Temp_C","Lux")  
      
    }
    if (exists("LuxData")){
      Temp_LuxData <- read.csv(file, skip=2, header = FALSE, sep = ",",
                              quote = "\"",dec = ".", fill = TRUE, comment.char = "")  
      colnames(Temp_LuxData)=c("row","DateTime","Temp_C","Lux")  
      Temp_LuxData=Temp_LuxData[,2:4]
      
      LuxData <- rbind(LuxData, Temp_LuxData)
      rm(Temp_LuxData)
    }
    
  }
  #colnames(LuxData)=c("row","DateTime","Temp_C","Lux")
  #LuxData=LuxData[,2:4]
  LuxData=unique(LuxData)
  LuxData$DateTime <- as.POSIXct(LuxData$DateTime, format="%m/%d/%y %I:%M:%S %p", tz="UTC")
  assign((paste(site,"Lux_data",sep="_")),LuxData) #creates object with new appended data
  rm(LuxData) #removes WLdata so that multiple sites aren't appended together
}






