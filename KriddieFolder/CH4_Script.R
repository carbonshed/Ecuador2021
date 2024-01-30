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
EOSData <- EOSData%>%drop_na(Site)
EOSData <- EOSData%>%drop_na(Trans_no)
EOSData <- unique(EOSData)

EOSData$Date <- as.Date(with(EOSData, paste(Year, Month, Day,sep="-")), "%y-%m-%d")

EOSData <- EOSData  %>%
drop_na(Site)    %>%
  drop_na(Date)    

EOSData$Site = toupper(EOSData$Site)
EOSData$Flux <- as.numeric(EOSData$Flux)
EOSData$Site[EOSData$Site=="ANT"]<-"ANTE"
EOSData$Site[EOSData$Site=="ANTI"]<-"ANTE"
EOSData$Temperature_c <- as.numeric(as.character(EOSData$Temperature_c))

EOSData <- EOSData[c("Date","Time","Site","Trans_no","Flux","Temperature_c")]
#a little bit of data cleaning is necessary for some measurments that are very high standard deviation
#use pivot table

EOSData[EOSData$Date=="2021-06-14" & 
               EOSData$Site=="GUAR" & 
               EOSData$Trans_no==1 & 
          EOSData$Flux==0.01,]$Flux <- NA
EOSData[EOSData$Date=="2021-06-14" & 
          EOSData$Site=="GAVI" & 
          EOSData$Trans_no==3 & 
          EOSData$Flux < 1.0,]$Flux <- NA
EOSData[EOSData$Date=="2021-06-14" & 
          EOSData$Site=="GAVI" & 
          EOSData$Trans_no==5 & 
          EOSData$Flux== 0,]$Flux <- NA
EOSData[EOSData$Date=="2021-06-14" & 
          EOSData$Site=="GAVI" & 
          EOSData$Trans_no==2 & 
          EOSData$Flux== .35,]$Flux <- NA
EOSData[EOSData$Date=="2021-07-20" & 
          EOSData$Site=="GAVI" & 
          EOSData$Trans_no==3 & 
          EOSData$Flux== 0.01,]$Flux <- NA
EOSData[EOSData$Date=="2021-07-13" & 
          EOSData$Site=="GAVI" & 
          EOSData$Trans_no==1 & 
          EOSData$Flux== 0.47,]$Flux <- NA
EOSData[EOSData$Date=="2021-07-13" & 
          EOSData$Site=="GAVI" & 
          EOSData$Trans_no==3 & 
          EOSData$Flux== 0.37,]$Flux <- NA
EOSData[EOSData$Date=="2021-07-26" & 
          EOSData$Site=="ANTE" & 
          EOSData$Trans_no==2 & 
          EOSData$Flux== 0.01,]$Flux <- NA
EOSData[EOSData$Date=="2021-07-16" & 
          EOSData$Site=="PARQ" & 
          EOSData$Trans_no==3 & 
          EOSData$Flux== 0.03,]$Flux <- NA
EOSData[EOSData$Date=="2021-07-26" & 
          EOSData$Site=="PARQ" & 
          EOSData$Trans_no==5 & 
          EOSData$Flux== 0.11,]$Flux <- NA
EOSData[EOSData$Date=="2021-07-14" & 
          EOSData$Site=="GUAR" & 
          EOSData$Trans_no==3 & 
          EOSData$Flux== 0.11,]$Flux <- NA
EOSData[EOSData$Date=="2021-07-12" & 
          EOSData$Site=="ANTE" & 
          EOSData$Trans_no==3 & 
          EOSData$Flux== 0.04,]$Flux <- NA
EOSData[EOSData$Date=="2021-07-26" & 
          EOSData$Site=="ANTE" & 
          EOSData$Trans_no==3 & 
          EOSData$Flux== 0.19,]$Flux <- NA
EOSData[EOSData$Date=="2021-07-12" & 
          EOSData$Site=="ANTE" & 
          EOSData$Trans_no==5 & 
          EOSData$Flux== 0.29,]$Flux <- NA


EOS_pivot <- EOSData  %>%
group_by(Date, Site, Trans_no) %>%
  #filter(Flux > 0)  %>%
  summarize(mean_soil_tempC = mean(Temperature_c,na.rm=TRUE),
    mean_Flux = mean(Flux, na.rm = TRUE),
            std_Flux = sd(Flux, na.rm = TRUE))  

EOS_pivot$percent <- EOS_pivot$std_Flux / EOS_pivot$mean_Flux *100


###QA/QC
EOS_pivot %>%
  group_by(Site) %>%
  summarize(n())

#Graph it up bitches



ggplot(subset(EOS_pivot, Date == "2021-06-14" | Date == "2021-06-16"), aes(fill=Site, y=mean_Flux, x=Trans_no)) + 
  geom_bar(position="dodge", stat="identity") 


ggplot(EOS_pivot, aes(y=mean_Flux, x=Trans_no)) + 
  geom_bar(position="dodge", stat="identity") 


#EOS_pivot$Date[EOS_pivot$Date=="2021-06-16"]  <- as.Date("2021-06-14")
#EOS_pivot$Date[EOS_pivot$Date=="2021-06-22"]  <- as.Date("2021-06-21")



ggplot(EOS_pivot , aes(y=mean_Flux, x=Trans_no, color=Site)) + 
  geom_bar(position="dodge", stat="identity") + 
  facet_wrap(Site ~ Date, ncol = 5) 


#write out 
#write.csv(EOS_pivot, here::here("KriddieFolder/WetlandSoils_CO2Flux.csv"))


