#Metano code
#Kriddie Whitmore
#2021-06-16

#first merge all methane files 

library(lubridate) #package for our date parsing
library(dplyr)
library(tidyverse)
library(here)



##set folder for site ##

setwd(here::here("CH4"))

all_files=list.files(pattern=".csv") #pulls out the csv files from WL folder in HOBOware folder
sites_rp=gsub("_.*","",all_files) #selects the correct pattern so as to seelct only desired files
#sites_rp = sub('_[^_]+$', '', all_files)

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
      EOSData <- read.csv(file, skip=0, header = TRUE, sep = ",",
                         quote = "\"",dec = ".", fill = TRUE, comment.char = "")
      EOSData <- select(EOSData, c("Month", "Day", "Year", "Time","Flux","Temperature..C.","Mode","EOS.Num","Site","Trans.Num"))
#      colnames(EOSData)=c("Month","Day","Year","Time","Flux","Temperature_c","Mode","Eos_no","Site","Trans_no")
      }
    if (exists("EOSData")){
      Temp_EOSData <- read.csv(file, skip=0, header = TRUE, sep = ",",
                              quote = "\"",dec = ".", fill = TRUE, comment.char = "")  
#      Temp_EOSData <- read_csv(file)
#      Temp_EOSData[, c('Month', 'Day', 'Year', 'Time','Flux','`Temperature (C)`','`CO2 Soil (ppm)`','`CO2 Soil STD (ppm)`','`CO2 ATM (ppm)`','Mode','`EOS Num`','Site','`Trans Num`')]
      Temp_EOSData <- select(Temp_EOSData, c("Month", "Day", "Year", "Time","Flux","Temperature..C.","Mode","EOS.Num","Site","Trans.Num"))
 #     colnames(Temp_EOSData)=c("Month","Day","Year","Time","Flux","Temperature_c","Mode","Eos_no","Site","Trans_no")
      EOSData <- rbind(EOSData, Temp_EOSData)
      rm(Temp_EOSData)
    }
    
  }
  colnames(EOSData)=c("Month","Day","Year","Time","Flux","Temperature_c","Mode","Eos_no","Site","Trans_no")
#  EOSData=EOSData[,1:14]
  EOSData=unique(EOSData)
  EOSData$Date <- as.Date(with(EOSData, paste(EOSData$Year + 2000, EOSData$Month, EOSData$Day,sep="-")), "%Y-%m-%d")
  EOSData$DateTime <- as.POSIXct(paste(EOSData$Date, EOSData$Time), format="%Y-%m-%d %H:%M:%S")
  assign((paste(site,sep="_")),EOSData) #creates object with new appended data
  rm(EOSData)
}

EOS <- EOS  %>%
drop_na(Site)   

EOS_pivot <- EOS  %>%
group_by(Date, Site, Trans_no) %>%
  filter(Flux > 0)  %>%
  summarize(mean_Flux = mean(Flux, na.rm = TRUE),
            std_Flux = sd(Flux, na.rm = TRUE))


#Graph it up bitches

ggplot(EOS_pivot, aes(fill=Site, y=mean_Flux, x=Trans_no)) + 
  geom_bar(position="dodge", stat="identity")

ggplot(EOS_pivot, aes(y=mean_Flux, x=Trans_no)) + 
  geom_bar(position="dodge", stat="identity")

EOS_pivot$percent <- EOS_pivot$std_Flux / EOS_pivot$mean_Flux *100


ggplot(EOS_pivot, aes(fill=Site, y=percent, x=Trans_no)) + 
  geom_bar(position="dodge", stat="identity")

