library(dplyr)
library(ggplot2)
library(reshape2)
library(purrr)
theme_set(theme_bw()) #just a preference for the plots

setwd(here::here("CO2"))
all_files=list.files(pattern=".csv") #pulls out the csv files from CO2 folder
#sites_rp=gsub("_.*","",all_files) #selects the correct pattern so as to select only desired files
sites_rp = sub('_[^_]+$', '', all_files)
site_names=unique(sites_rp) #creates list of site names for following loop
#site_names=site_names[-2] #to get the campbell logger one out 
#site_names=site_names[-6]

#rm old files, if they exist
rm(CO2Data)
rm(Temp_CO2Data)

for (site in site_names){
  
  list1=list.files(pattern=site) #finds all files for the site
  sitelist_csv=grep(".csv",list1) #creates list of all files for site
  file_list=list1[sitelist_csv]
  
  #reads in files in list and appends
  for (file in file_list){
    if (!exists("CO2Data")){
      CO2Data <- read.csv(file, skip=6, header = TRUE)
      CO2Data=CO2Data[,1:3]
    }
    if (exists("CO2Data")) {
      Temp_CO2Data <- read.csv(file, skip=6, header = TRUE)  
      Temp_CO2Data=Temp_CO2Data[,1:3]
      CO2Data <- rbind(CO2Data, Temp_CO2Data)
      rm(Temp_CO2Data)
    }
    
  }
  colnames(CO2Data)=c("Date","Time","ppm")
  #CO2Data=CO2Data[,2:4]
  CO2Data=unique(CO2Data)
  CO2Data$DateTime <- as.POSIXct(paste(CO2Data$Date, CO2Data$Time), format="%m/%d/%Y %I:%M:%S %p", tz = "UTC")
  #CO2$DateTime <- as.POSIXct(CO2$DateTime, format="%m/%d/%y %I:%M:%S %p", tz="UTC")
  assign((paste(site,"CO2_data",sep="_")),CO2Data) #creates object with new appended data
  rm(CO2Data) #removes WLdata so that multiple sites aren't appended together
}



##Now we can rbind all the combined data to make one big data 
# it's not elegant but it's reusable 
#this is just cleaning up so we have the rows we want and making a Station variable for later
CO2_01_CO2_data$Station <- 1
CO2_01_CO2_data=CO2_01_CO2_data[,3:5]
CO2_02_CO2_data$Station <- 2
CO2_02_CO2_data=CO2_02_CO2_data[,3:5]
CO2_03_CO2_data$Station <- 3
CO2_03_CO2_data=CO2_03_CO2_data[,3:5]
CO2_04_CO2_data$Station <- 4
CO2_04_CO2_data=CO2_04_CO2_data[,3:5]
CO2_Well1_CO2_data$Station <- 1.5
CO2_Well1_CO2_data=CO2_Well1_CO2_data[,3:5]
CO2_Well2_CO2_data$Station <- 3.5
CO2_Well2_CO2_data=CO2_Well2_CO2_data[,3:5]

combined <- rbind(CO2_01_CO2_data, CO2_02_CO2_data, CO2_03_CO2_data,CO2_04_CO2_data, CO2_Well1_CO2_data, CO2_Well2_CO2_data)


#Now we graph 
ggplot(combined, aes(x=DateTime,y=ppm))+ 
  facet_grid(Station ~ . ,labeller="label_both")+
  labs(x="Date", y="CO2 ppm")+ 
  geom_point(aes(y=ppm))




