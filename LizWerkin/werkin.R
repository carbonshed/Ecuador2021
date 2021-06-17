library(dplyr)
library(ggplot2)
theme_set(theme_bw()) #just a preference for the plots

setwd(here::here("CO2"))
all_files=list.files(pattern=".csv") #pulls out the csv files from WL folder in HOBOware folder
#sites_rp=gsub("_.*","",all_files) #selects the correct pattern so as to seelct only desired files
sites_rp = sub('_[^_]+$', '', all_files)
site_names=unique(sites_rp) #creates list of site names for following loop
#site_names=site_names[-2]
#site_names=site_names[-6]

#rm old files, if they exist
rm(CO2)
rm(Temp_CO2Data)

for (site in site_names){
  
  list1=list.files(pattern=site) #finds all files for the site
  sitelist_csv=grep(".csv",list1) #creates list of all files for site
  file_list=list1[sitelist_csv]
  
  #reads in files in list and appends
  for (file in file_list){
    if (!exists("CO2")){
      CO2 <- read.csv(file, skip=6, header = TRUE)
      CO2=CO2[,1:3]
    }
    else {
      Temp_CO2Data <- read.csv(file, skip=6, header = TRUE)  
      Temp_CO2Data=Temp_CO2Data[,1:3]
      CO2 <- rbind(CO2, Temp_CO2Data)
      rm(Temp_CO2Data)
    }
    
  }
  colnames(CO2)=c("Date","Time","Voltage")
  #CO2Data=CO2Data[,2:4]
  CO2=unique(CO2)
  #CO2$DateTime <- as.POSIXct(CO2$DateTime, format="%m/%d/%y %I:%M:%S %p", tz="UTC")
  assign((paste(site,"CO2_data",sep="_")),CO2) #creates object with new appended data
  rm(CO2) #removes WLdata so that multiple sites aren't appended together
}


##Now we can graph 
ggplot(BEAVOutletWL_merge, aes(x=DateTime, y=WL_true))+
  geom_point(aes(y=WL_Baroadjusted))+
  labs(x=NULL,y="WL (cm H2O)")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank())

