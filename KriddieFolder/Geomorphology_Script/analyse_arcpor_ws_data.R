#let's calculate flux for each pixel babay daby

library(here)
library(dplyr)

dataFrame <- read.csv(here::here("Geomorphology/Geomorph_from_ArcPro/colm_rnetwork_point.csv"))
dataFrame$NEAR_FID <- paste("seg",dataFrame$NEAR_FID,sep="_")

split_data <- split(dataFrame, f = dataFrame$NEAR_FID)   



for (i in split_data) {
  seg_name <- names(split_data[1])
  seg_name_2 <- noquote(seg_name)
  seg_df <- split_datadf
}



for (df in split_data) {
  #for each stream_seg_code, do these things:
  seg_df <- split_data[[df]]
  #order by accumulation
  seg_df <- seg_df[order(seg_df$flo_accu),]
  for(i in seg_df) {
    #find 3 "above", 3 "below" 
    if(i>3) {
      
    }
  }

  #calc distance between points
  #calc difference in elevation between points
  #calc slope
  #write out into table maybe. or maybe I should make a table and then 
}





for (i in 1:nrow(dataFrame)){
  
  list1=unique(dataFrame$NEAR_FID) #finds all distinct NEAR_FIDs
  #sitelist_csv=grep(".csv",list1) #creates list of all files for site
  #file_list=list1[sitelist_csv]
  
  #reads in files in list and appends
  for (item in list1){
    
    if (!exists("WLData")){
      WLData <- read.csv(file, skip=1, header = FALSE, sep = ",",
                         quote = "\"",dec = ".", fill = TRUE, comment.char = "")
      
      if(str_contains(WLData[1,3],"Abs Pres, psi")){
        WLData <- WLData[-1,]
        colnames(WLData)=c("row","DateTime","WLPres_kpa","WLTemp_c")
        WLData <- WLData[2:4]
        WLData$WLPres_kpa <- as.numeric(as.character(WLData$WLPres_kpa), digits=6)
        WLData$WLTemp_c <- as.numeric(as.character(WLData$WLTemp_c), digits=5)
        WLData$WLPres_kpa <- WLData$WLPres_kpa*6.89476
        WLData$WLTemp_c <- (WLData$WLTemp_c - 32)/1.8000
        
      } else { 
        WLData <- WLData[-1,]
        colnames(WLData)=c("row","DateTime","WLPres_kpa","WLTemp_c")
        WLData <- WLData[2:4]
        WLData$WLPres_kpa <- as.numeric(as.character(WLData$WLPres_kpa), digits=6)
        WLData$WLTemp_c <- as.numeric(as.character(WLData$WLTemp_c), digits=5)
      }
      
      #      WLData$DateTime <- as.POSIXct(WLData$DateTime, format="%m/%d/%y %I:%M:%S %p", tz="UTC")
      WLData$DateTime <- as.POSIXct(WLData$DateTime, tz="UTC",
                                    tryFormats = c("%m/%d/%y %I:%M:%S %p",
                                                   "%m/%d/%Y %H:%M"))
    }
    if (exists("WLData")){
      Temp_WLData <- read.csv(file, skip=1, header = FALSE, sep = ",",
                              quote = "\"",dec = ".", fill = TRUE, comment.char = "")  
      #      
      
      if(str_contains(Temp_WLData[1,3],"Abs Pres, psi")){
        Temp_WLData <- Temp_WLData[-1,]
        colnames(Temp_WLData)=c("row","DateTime","WLPres_kpa","WLTemp_c")
        Temp_WLData <- Temp_WLData[2:4]
        Temp_WLData$WLPres_kpa <- as.numeric(as.character(Temp_WLData$WLPres_kpa), digits=6)
        Temp_WLData$WLTemp_c <- as.numeric(as.character(Temp_WLData$WLTemp_c), digits=5)
        Temp_WLData$WLPres_kpa <- Temp_WLData$WLPres_kpa*6.89476
        Temp_WLData$WLTemp_c <- (Temp_WLData$WLTemp_c - 32)/1.8000
        
        
      } else { 
        Temp_WLData <- Temp_WLData[-1,]
        colnames(Temp_WLData)=c("row","DateTime","WLPres_kpa","WLTemp_c")
        Temp_WLData <- Temp_WLData[2:4]
        Temp_WLData$WLPres_kpa <- as.numeric(as.character(Temp_WLData$WLPres_kpa), digits=6)
        Temp_WLData$WLTemp_c <- as.numeric(as.character(Temp_WLData$WLTemp_c), digits=5)
      }
      
      #      Temp_WLData$DateTime <- as.POSIXct(Temp_WLData$DateTime, format="%m/%d/%y %I:%M:%S %p", tz="UTC")
      Temp_WLData$DateTime <- as.POSIXct(Temp_WLData$DateTime, tz="UTC",
                                         tryFormats = c("%m/%d/%y %I:%M:%S %p",
                                                        "%m/%d/%Y %H:%M"))
      
      
      WLData <- rbind(WLData, Temp_WLData)
      rm(Temp_WLData)
    }
    
  }
  WLData$DateTime <- round_date(WLData$DateTime, "15 mins")
  WLData$Station <- site
  WLData=unique(WLData)
  assign((paste(site,sep="_")),WLData) #creates object with new appended data
  rm(WLData) #removes WLdata so that multiple sites aren't appended together
}
