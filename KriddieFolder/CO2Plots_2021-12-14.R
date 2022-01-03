library(dplyr)
library(ggplot2)
theme_set(theme_bw()) #just a preference for the plots

setwd(here::here("CO2"))
all_files=list.files(pattern=".csv") #pulls out the csv files from WL folder in HOBOware folder
#sites_rp=gsub("_.*","",all_files) #selects the correct pattern so as to seelct only desired files
sites_rp = sub('_[^_]+$', '', all_files)
site_names=unique(sites_rp) #creates list of site names for following loop
#site_names=site_names[-2]

#rm old files, if they exist
rm(CO2Data)
rm(Temp_CO2Data)

for (site in site_names){
 # if(site == "CO2_02"){

  list1=list.files(pattern=site) #finds all files for the site
  sitelist_csv=grep(".csv",list1) #creates list of all files for site
  file_list=list1[sitelist_csv]
  
  #reads in files in list and appends
  for (file in file_list){
    if (!exists("CO2Data")){
      CO2Data <- read.csv(file, skip=6, header = TRUE)
      CO2Data=CO2Data[,1:3]
      colnames(CO2Data) <- c("Date","Time","ppm")
      CO2Data$DateTime <- as.POSIXct(paste(CO2Data$Date, CO2Data$Time), format="%m/%d/%Y %I:%M:%S %p", tz = "UTC")
      CO2Data$Station <- site
    }
    if (exists("CO2Data")) {
      Temp_CO2Data <- read.csv(file, skip=6, header = TRUE)  
      Temp_CO2Data=Temp_CO2Data[,1:3]
      if(colnames(Temp_CO2Data)[1]=="Date"){
        colnames(Temp_CO2Data) <- c("Date","Time","ppm")
        Temp_CO2Data$DateTime <- as.POSIXct(paste(Temp_CO2Data$Date, Temp_CO2Data$Time), format="%m/%d/%Y %I:%M:%S %p", tz = "UTC")
        Temp_CO2Data$Station <- site
      } else {
#        Temp_CO2Data$Fecha <- as.Date(Temp_CO2Data$Fecha, format = "%d / %m / %Y")
        Temp_CO2Data$DateTime <- as.POSIXct(paste(Temp_CO2Data$Fecha, Temp_CO2Data$Tiempo), format="%d/%m/%Y %H:%M:%S", tz = "UTC")
        colnames(Temp_CO2Data) <- c("Date","Time","ppm","DateTime")
        Temp_CO2Data$Station <- site
      }
      CO2Data <- rbind(CO2Data, Temp_CO2Data)
      rm(Temp_CO2Data)
    }
    
  }

  CO2Data=unique(CO2Data)
  CO2Data$Date <- NULL
  CO2Data$Time <- NULL
  CO2Data <- CO2Data[,c(3,2,1)]
  assign((paste(site,sep="_")),CO2Data) #creates object with new appended data
  rm(CO2Data) #removes WLdata so that multiple sites aren't appended together
}

Stations

stations <- rbind(CO2_01,CO2_02,CO2_03,CO2_04)

wells <- rbind(CO2_Well01,CO2_Well02)


ggplot(data = stations, aes(DateTime, ppm)) +
  geom_point(color = "steelblue") +
#  geom_point(color="steelblue") + 
  labs(#title = "CO2  stations",
       y = "CO2 ppm", x = "") + 
  facet_wrap(~ Station)


ggplot(data = wells, aes(DateTime, ppm)) +
  geom_line(color = "steelblue") +
  #  geom_point(color="steelblue") + 
  labs(#title = "CO2  stations",
    y = "CO2 ppm", x = "") + 
  facet_wrap(~ Station)

