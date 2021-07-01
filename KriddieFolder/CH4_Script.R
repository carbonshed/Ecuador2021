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
sites_rp = sub('_[^_]+$', '', all_files)

site_names=unique(sites_rp) #creates list of site names for following loop

#EOS 1 and EOS 2 have different column names, so we will do a loop for each one

#EOS1 loop
site <- site_names[1]

list1=list.files(pattern=site) #finds all files for the site
sitelist_csv=grep(".csv",list1) #creates list of all files for site
file_list=list1[sitelist_csv]

#rm old files, if they exsist
rm(EOSData)
rm(Temp_EOSData)

for (file in file_list){
  if (!exists("EOSData")){
    EOSData <- read.csv(file, skip=0, header = TRUE, sep = ",",
                        quote = "\"",dec = ".", fill = TRUE, comment.char = "")
    EOSData <- select(EOSData, c("Month", "Day", "Year", "Time","Flux","Temperature..C.","Mode","EOS.Num","Site","Trans.Num"))
          colnames(EOSData)=c("Month","Day","Year","Time","Flux","Temperature_c","Mode","Eos_no","Site","Trans_no")
  }
  if (exists("EOSData")){
    Temp_EOSData <- read.csv(file, skip=0, header = TRUE, sep = ",",
                             quote = "\"",dec = ".", fill = TRUE, comment.char = "")  
    Temp_EOSData <- select(Temp_EOSData, c("Month", "Day", "Year", "Time","Flux","Temperature..C.","Mode","EOS.Num","Site","Trans.Num"))
    colnames(Temp_EOSData)=c("Month","Day","Year","Time","Flux","Temperature_c","Mode","Eos_no","Site","Trans_no")
    EOSData <- rbind(EOSData, Temp_EOSData)
    rm(Temp_EOSData)
  }
#  EOSData=unique(EOSData)
  EOSData_01 <- EOSData 
#  rm(EOSData)
}
  
### EOS2 loop ####
site <- site_names[2]

list1=list.files(pattern=site) #finds all files for the site
sitelist_csv=grep(".csv",list1) #creates list of all files for site
file_list=list1[sitelist_csv]

#rm old files, if they exsist
rm(EOSData)
rm(Temp_EOSData)

for (file in file_list){
  if (!exists("EOSData")){
    EOSData <- read.csv(file, skip=0, header = TRUE, sep = ",",
                        quote = "\"",dec = ".", fill = TRUE, comment.char = "")
    EOSData <- select(EOSData, c("Month", "Day", "Year", "Time","Flux","Temperature..C.","Mode","EOS.Num","Site","Trans.Num"))
    colnames(EOSData)=c("Month","Day","Year","Time","Flux","Temperature_c","Mode","Eos_no","Site","Trans_no")
  }
  if (exists("EOSData")){
    Temp_EOSData <- read.csv(file, skip=0, header = TRUE, sep = ",",
                             quote = "\"",dec = ".", fill = TRUE, comment.char = "")  
    Temp_EOSData <- select(Temp_EOSData, c("Month", "Day", "Year", "Time","Flux","Temperature..C.","Mode","EOS.Num","Site","Trans.Num"))
    colnames(Temp_EOSData)=c("Month","Day","Year","Time","Flux","Temperature_c","Mode","Eos_no","Site","Trans_no")
    EOSData <- rbind(EOSData, Temp_EOSData)
    rm(Temp_EOSData)
  }
#  EOSData=unique(EOSData)
  EOSData_02 <- EOSData 
#  rm(EOSData)
}

EOSData <-  rbind(EOSData_01, EOSData_02)

EOSData$Date <- as.Date(with(EOSData, paste(Year, Month, Day,sep="-")), "%y-%m-%d")

EOSData <- EOSData  %>%
drop_na(Site)    

EOS_pivot <- EOSData  %>%
group_by(Date, Site, Trans_no) %>%
  #filter(Flux > 0)  %>%
  summarize(mean_Flux = mean(Flux, na.rm = TRUE),
            std_Flux = sd(Flux, na.rm = TRUE))

###QA/QC
EOS_pivot %>%
  group_by(Site) %>%
  summarize(n())

EOS_pivot$percent <- EOS_pivot$std_Flux / EOS_pivot$mean_Flux *100
#Graph it up bitches

ggplot(EOS_pivot, aes(fill=Site, y=mean_Flux, x=Trans_no)) + 
  geom_bar(position="dodge", stat="identity")

ggplot(EOS_pivot, aes(y=mean_Flux, x=Trans_no)) + 
  geom_bar(position="dodge", stat="identity")




ggplot(EOS_pivot, aes(fill=Site, y=percent, x=Trans_no)) + 
  geom_bar(position="dodge", stat="identity")

