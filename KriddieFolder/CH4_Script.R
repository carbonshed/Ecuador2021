#Metano code
#Kriddie Whitmore
#2021-06-16

#first merge all methane files 

library(lubridate) #package for our date parsing
library(dplyr)
library(here)


##set folder for site ##

setwd(here::here("Lux"))

all_files=list.files(pattern=".csv") #pulls out the csv files from WL folder in HOBOware folder
sites_rp=gsub("_.*","",all_files) #selects the correct pattern so as to seelct only desired files
sites_rp = sub('_[^_]+$', '', all_files)

site_names=unique(sites_rp) #creates list of site names for following loop

#rm old files, if they exsist
rm(EOSData)
rm(Temp_EOSData)

for (site in site_names){
  
  list1=list.files(pattern=site) #finds all files for the site
  sitelist_csv=grep(".csv",list1) #creates list of all files for site
  file_list=list1[sitelist_csv]
  
  #reads in files in list and appends
  for (file in file_list){
    if (!exists("EOSData")){
      EOSData <- read.csv(file, skip=2, header = FALSE, sep = ",",
                         quote = "\"",dec = ".", fill = TRUE, comment.char = "")
      
    }
    if (exists("EOSData")){
      Temp_EOSData <- read.csv(file, skip=2, header = FALSE, sep = ",",
                              quote = "\"",dec = ".", fill = TRUE, comment.char = "")  
      
      EOSData <- rbind(EOSData, Temp_EOSData)
      rm(Temp_EOSData)
    }
    
  }
  colnames(EOSData)=c("row","DateTime","WLPres_kpa","WLTemp_c")
  EOSData=EOSData[,2:4]
  EOSData=unique(EOSData)
  EOSData$DateTime <- as.POSIXct(EOSData$DateTime, format="%m/%d/%y %I:%M:%S %p", tz="UTC")
  assign((paste(site,"WL_data",sep="_")),EOSData) #creates object with new appended data
  rm(EOSData) #removes WLdata so that multiple sites aren't appended together
}



