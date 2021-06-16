library(dplyr)
library(ggplot2)
theme_set(theme_bw()) #just a preference for the plots
library(readxl)

all_files=list.files(pattern=".xlsx") #pulls out the csv files from WL folder in HOBOware folder
sites_rp=gsub("_.*","",all_files) #selects the correct pattern so as to seelct only desired files
site_names=unique(sites_rp) #creates list of site names for following loop

path <- "~/Ecuador2021/Ecuador2021/CO2" 

CO2Data <- read_excel(path, 1, NA, TRUE)

for (site in site_names){
  
  list1=list.files(pattern=site) #finds all files for the site
  sitelist_xlsx=grep(".xlsx",list1) #creates list of all files for site
  file_list=list1[sitelist_xlsx]

  #reads in files in list and appends
  for (file in file_list){
    if (!exists("CO2Data")){
      CO2Data <- read_excel(path, 1, A7:C1000, TRUE) #the 1000 may have to change depending on how long the excel sheets are
      
    }
    if (exists("CO2Data")){
      Temp_CO2Data <- read_excel(path, 1, A7:C1000, TRUE)  
      CO2Data <- rbind(CO2Data, Temp_CO2Data)
      rm(Temp_CO2Data)
    }
    
  }
  #colnames(CO2Data)=c("row","DateTime","WLPres_kpa","WLTemp_c")
  CO2Data=CO2Data[,2:4]
  CO2Data=unique(CO2Data)
  CO2Data$DateTime <- as.POSIXct(CO2Data$DateTime, format="%m/%d/%y %I:%M:%S %p", tz="UTC")
  assign((paste(site,"CO2_Data",sep="_")),CO2Data) #creates object with new appended data
  rm(CO2Data) #removes CO2Data so that multiple sites aren't appended together
}
