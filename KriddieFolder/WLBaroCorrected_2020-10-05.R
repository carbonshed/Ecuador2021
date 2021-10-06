library(lubridate) #package for our date parsing
library(here)
library(dplyr)
library(ggplot2)
library(reshape2)
library(purrr)
##set folder for site ##

setwd(here::here("WaterLevel"))
all_files=list.files(pattern=".csv") #pulls out the csv files from CO2 folder
sites_rp = sub('_[^_]+$', '', all_files)
site_names=unique(sites_rp) #creates list of site names for following loop
#site_names = site_names[-1]
site_names <- site_names[site_names != c("WL_01","WL_Well2")] # remove solinst station

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


#this loop is for the site with solinist
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
  colnames(WLData)=c("Date","Time","ms","kPa","Temp")
  WLData=unique(WLData)
  #WLData$DateTime <- as.POSIXct(WLData$DateTime, format="%m/%d/%y %I:%M:%S %p", tz="UTC")
  assign((paste(site,"WL_data",sep="_")),WLData) #creates object with new appended data
  rm(WLData) #removes WLdata so that multiple sites aren't appended together
}

## Now we need to make the WL Station 1 Data look like the other places
  # so we can merge them together later 
WL_01_WL_data$DateTime <- with(WL_01_WL_data, as.POSIXct(paste(Date, Time), format="%m/%d/%Y %I:%M:%S %p", tz = "UTC"))
WL_01_WL_data=WL_01_WL_data[,4:6]
colnames(WL_01_WL_data)=c("WLPres_kpa","WLTemp_c","DateTime")

##Now adding a station column to each of datasets
WL_01_WL_data$Station <- 1
WL_02_WL_data$Station <- 2
WL_03_WL_data$Station <- 3
WL_04_WL_data$Station <- 4
WL_05_WL_data$Station <- 5
WL_06_WL_data$Station <- 6

combined <- rbind(WL_01_WL_data,WL_02_WL_data,WL_03_WL_data,WL_04_WL_data,WL_05_WL_data,WL_06_WL_data)


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
  #colnames(BaroData)=c("DateTime","BaroPres_kpa","AirTemp_c")
  BaroData=unique(BaroData)
  #BaroData=BaroData[,2:4]
  #BaroData$DateTime <- as.POSIXct(BaroData$DateTime, format="%m/%d/%y %I:%M:%S %p", tz="UTC")
  assign((paste(site,"Baro_data",sep="_")),BaroData) #creates object with new appended data
  rm(BaroData) #removes WLdata so that multiple sites aren't appended together
}

## Now we make the Baro data look like the combined data so they can be joined 
Baro_Baro_data$DateTime <- with(Baro_Baro_data, as.POSIXct(paste(Date, Time), format="%m/%d/%Y %I:%M:%S %p", tz = "UTC"))
vector <- Baro_Baro_data$DateTime
vector <- round_date(vector,unit="15 minutes") #to round the values when the data was not on exact 15 min intervals
Baro_Baro_data$DateTime <- vector
Baro_Baro_data=Baro_Baro_data[,4:6]
colnames(Baro_Baro_data)=c("Baro_kpa","WLTemp_c","DateTime")


### merge WL data with Baro data for outlet and convert from kPa to cm of water
Merged <- left_join(combined, Baro_Baro_data, by = "DateTime")
Merged$WL_adjusted <- Merged$WLPres_kpa - Merged$Baro_kpa 
Merged$WL_true <- Merged$WL_adjusted*10.19716 


##Now we can graph 
    ## will need to change the right limit accordingly
ggplot(Merged, aes(x=DateTime,y=WL_true))+ 
  facet_grid(Station ~ . ,labeller="label_both", scales ="free_y")+
  scale_x_datetime(limits = c(as.POSIXct("2021-06-10 15:45:00 UTC"),as.POSIXct("2021-07-02 12:45:00 UTC")))+
  labs(x="Date", y="WL (cm)")+ 
  geom_point(aes(y=WL_true))

