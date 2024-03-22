#DTW

library(reshape2)
library(dplyr)
library(readr)
library(ggplot2)

###merge data with baro data###
setwd("~/OneDrive - University of North Carolina at Chapel Hill/Dissertation papers/SynopPaper/DTW_data/DTW_clip_tables")

all_files=list.files(pattern=".csv") #pulls out the csv files from WL folder in HOBOware folder
sites_rp=sub("__WS_poly_","",all_files) #selects the correct pattern so as to seelct only desired files
sites_rp=sub("_[^_]+$", "", sites_rp)
site_names=unique(sites_rp) #creates list of site names for following loop

#rm old files, if they exsist
rm(DTWdata)
rm(Temp_DTWdata)


for (site in site_names){
  
  list1=list.files(pattern=site) #finds all files for the site
  sitelist_csv=grep(".csv",list1) #creates list of all files for site
  file_list=list1[sitelist_csv]
  
  
  #reads in files in list and appends
  for (file in file_list){
    if (!exists("DTWdata")){
      DTWdata <- read.csv(file, skip=0, header = TRUE, sep = ",",
                           quote = "\"",dec = ".", fill = TRUE, comment.char = "")[c(3,6)]
     
      filename=sub("__WS_poly_","",file)
      filename=sub(".csv","",filename) 
      
      DTWdata$Cathment_name <- NA
      DTWdata$Cathment_name <- filename
    }
    if (exists("DTWdata")){
      Temp_DTWdata <- read.csv(file, skip=0, header = TRUE, sep = ",",
                                quote = "\"",dec = ".", fill = TRUE, comment.char = "")[c(3,6)]
      filename=sub("__WS_poly_","",file)
      filename=sub(".csv","",filename) 
      
      Temp_DTWdata$Cathment_name <- NA
      Temp_DTWdata$Cathment_name <- filename
      
      DTWdata <- rbind(DTWdata, Temp_DTWdata)
      rm(Temp_DTWdata)
      
    }
    
  }
  DTWdata <- reshape(DTWdata, idvar = "Cathment_name", timevar = "gridcode", direction = "wide")
  colnames(DTWdata) <- c("Catchment_Name","Water_area","Wet_area","Dry_area")
  DTWdata$water_precent <- DTWdata$Water_area / rowSums(DTWdata[,2:4]) * 100
  DTWdata$wet_precent <- DTWdata$Wet_area / rowSums(DTWdata[,2:4]) * 100
  DTWdata$dry_precent <- DTWdata$Dry_area / rowSums(DTWdata[,2:4]) * 100
  
  assign((paste(site,"DTW",sep="_")),DTWdata) #creates object with new appended data
  rm(DTWdata) #removes WLdata so that multiple sites aren't appended together
}

# now combine with synop data
ALLSYNOPDATA_FINAL_2024_02_10 <- read_csv("~/Documents/Ecuador2021/ProcessedData/ALLSYNOPDATA_FINAL_2024-02-10.csv")
ANTE_synop <- ALLSYNOPDATA_FINAL_2024_02_10%>%filter(Wetland == "ANTE")
ANTE_synop <- ANTE_synop[order(ANTE_synop$dist),]
ANTE_synop$Join_ID <- seq.int(nrow(ANTE_synop))
ANTE_DTW$Join_ID <- as.integer(gsub('ANTE_', '', ANTE_DTW$Catchment_Name))
ANTE_DTW <- full_join(ANTE_synop,ANTE_DTW,by="Join_ID")

##ANTE
#plot
ggplot(ANTE_DTW, aes(x=wet_precent, y=log(adjusted_ppm))) + geom_point()  + ggtitle("ANTE")

#model 
model1 <- lm(log(adjusted_ppm) ~ wet_precent, data = ANTE_DTW)
summary(model1)
model1 <- lm(log(adjusted_ppm) ~ Wet_area, data = ANTE_DTW)
summary(model1)
model2 <- lm(log(adjusted_ppm) ~ dist, data = ANTE_DTW)
summary(model2)
model3 <- lm(log(adjusted_ppm) ~ wet_precent  + dist, data = ANTE_DTW)
summary(model3)
model4 <- lm(log(adjusted_ppm) ~ wet_precent  + CatchmentSize_ha, data = ANTE_DTW)
summary(model4)

##GAVI Outlet
# now combine with synop data
ALLSYNOPDATA_FINAL_2024_02_10 <- read_csv("~/Documents/Ecuador2021/ProcessedData/ALLSYNOPDATA_FINAL_2024-02-10.csv")
GAVIOutlet_synop <- ALLSYNOPDATA_FINAL_2024_02_10%>%filter(Wetland_5 == "Gavilan Outlet")
GAVIOutlet_synop <- GAVIOutlet_synop[order(GAVIOutlet_synop$dist),]
GAVIOutlet_synop$Join_ID <- seq.int(nrow(GAVIOutlet_synop))
GAVIoutlet_DTW$Join_ID <- as.integer(gsub('GAVIoutlet_', '', GAVIoutlet_DTW$Catchment_Name))
GAVIoutlet_DTW <- full_join(GAVIOutlet_synop,GAVIoutlet_DTW,by="Join_ID")
#plot
ggplot(GAVIoutlet_DTW, aes(x=wet_precent, y=log(adjusted_ppm))) + geom_point()  + ggtitle("GAVIoutlet")
#model 
model1 <- lm(log(adjusted_ppm) ~ Wet_area, data = GAVIoutlet_DTW)
summary(model1)
model2 <- lm(log(adjusted_ppm) ~ dist, data = GAVIoutlet_DTW)
summary(model2)
model3 <- lm(log(adjusted_ppm) ~ wet_precent  + dist, data = GAVIoutlet_DTW)
summary(model3)
model4 <- lm(log(adjusted_ppm) ~ wet_precent  + CatchmentSize_ha, data = GAVIoutlet_DTW)
summary(model4)

##GAVI INLET
# now combine with synop data
GAVIInlet_synop <- ALLSYNOPDATA_FINAL_2024_02_10%>%filter(Wetland_5 == "Gavilan Inlet")
GAVIInlet_synop <- GAVIInlet_synop[order(GAVIInlet_synop$dist),]
GAVIInlet_synop$Join_ID <- seq.int(nrow(GAVIInlet_synop))
GAVIinlet_DTW$Join_ID <- as.integer(gsub('GAVIinlet_', '', GAVIinlet_DTW$Catchment_Name))
GAVIinlet_DTW <- full_join(GAVIInlet_synop,GAVIinlet_DTW,by="Join_ID")

#plot
ggplot(GAVIinlet_DTW, aes(x=wet_precent, y=log(adjusted_ppm))) + geom_point()  + ggtitle("GAVIoutlet")
#model 
model1 <- lm(log(adjusted_ppm) ~ Wet_area, data = GAVIinlet_DTW)
summary(model1)
model2 <- lm(log(adjusted_ppm) ~ dist, data = GAVIinlet_DTW)
summary(model2)
model3 <- lm(log(adjusted_ppm) ~ wet_precent  + dist, data = GAVIinlet_DTW)
summary(model3)
model4 <- lm(log(adjusted_ppm) ~ wet_precent  + CatchmentSize_ha, data = GAVIinlet_DTW)
summary(model4)


##GAVI trib1
# now combine with synop data
GAVItrib1_synop <- ALLSYNOPDATA_FINAL_2024_02_10%>%filter(Wetland_4 == "GAVItrib1")
GAVItrib1_synop <- GAVItrib1_synop[order(GAVItrib1_synop$dist),]
GAVItrib1_synop$Join_ID <- seq.int(nrow(GAVItrib1_synop))
GaviTrib1_DTW$Join_ID <- as.integer(gsub('GaviTrib1_', '', GaviTrib1_DTW$Catchment_Name))
GaviTrib1_DTW <- full_join(GAVItrib1_synop,GaviTrib1_DTW,by="Join_ID")

#plot
ggplot(GaviTrib1_DTW, aes(x=wet_precent, y=log(adjusted_ppm))) + geom_point()  + ggtitle("GAVItrib1")
#model 
model1 <- lm(log(adjusted_ppm) ~ Wet_area, data = GaviTrib1_DTW)
summary(model1)
model2 <- lm(log(adjusted_ppm) ~ dist, data = GaviTrib1_DTW)
summary(model2)
model3 <- lm(log(adjusted_ppm) ~ wet_precent  + dist, data = GaviTrib1_DTW)
summary(model3)
model4 <- lm(log(adjusted_ppm) ~ Wet_area  + CatchmentSize_ha, data = GaviTrib1_DTW)
summary(model4)


##GAVI trib2
# now combine with synop data
GAVItrib2_synop <- ALLSYNOPDATA_FINAL_2024_02_10%>%filter(Wetland_4 == "GAVItrib1")
GAVItrib2_synop <- GAVItrib2_synop[order(GAVItrib2_synop$dist),]
GAVItrib2_synop$Join_ID <- seq.int(nrow(GAVItrib2_synop))
GaviTrib2_DTW$Join_ID <- as.integer(gsub('GaviTrib2_', '', GaviTrib2_DTW$Catchment_Name))
GaviTrib2_DTW <- full_join(GAVItrib2_synop,GaviTrib2_DTW,by="Join_ID")

#plot
ggplot(GaviTrib2_DTW, aes(x=wet_precent, y=log(adjusted_ppm))) + geom_point()  + ggtitle("GaviTrib2")
#model 
model1 <- lm(log(adjusted_ppm) ~ Wet_area, data = GaviTrib2_DTW)
summary(model1)
model2 <- lm(log(adjusted_ppm) ~ dist, data = GaviTrib2_DTW)
summary(model2)
model3 <- lm(log(adjusted_ppm) ~ wet_precent  + dist, data = GaviTrib2_DTW)
summary(model3)
model4 <- lm(log(adjusted_ppm) ~ wet_precent  + CatchmentSize_ha, data = GaviTrib2_DTW)
summary(model4)


##COLM
# now combine with synop data
COLM_synop <- ALLSYNOPDATA_FINAL_2024_02_10%>%filter(Wetland == "COLM")
COLM_synop <- COLM_synop[order(COLM_synop$dist),]
COLM_synop$Join_ID <- seq.int(nrow(COLM_synop))
COLM_DTW$Join_ID <- as.integer(gsub('COLM_', '', COLM_DTW$Catchment_Name))
COLM_DTW <- full_join(COLM_synop,COLM_DTW,by="Join_ID")

#plot
ggplot(COLM_DTW, aes(x=wet_precent, y=log(adjusted_ppm))) + geom_point()  + ggtitle("COLM")
#model 
model1 <- lm(log(adjusted_ppm) ~ Wet_area, data = COLM_DTW)
summary(model1)
model2 <- lm(log(adjusted_ppm) ~ dist, data = COLM_DTW)
summary(model2)
model3 <- lm(log(adjusted_ppm) ~ wet_precent  + dist, data = COLM_DTW)
summary(model3)
model4 <- lm(log(adjusted_ppm) ~ wet_precent  + CatchmentSize_ha, data = COLM_DTW)
summary(model4)


df_DTW <- rbind(ANTE_DTW,COLM_DTW,GAVIinlet_DTW,GAVIoutlet_DTW,GaviTrib1_DTW)
write.csv(df_DTW,"~/Documents/Ecuador2021/ProcessedData/DTW_df.csv")
#plot
ggplot(df_DTW, aes(x=wet_precent, y=log(adjusted_ppm))) + geom_point()  + ggtitle("ALL")
#model 
model1 <- lm(log(adjusted_ppm) ~ wet_precent, data = df_DTW)
summary(model1)
model2 <- lm(log(adjusted_ppm) ~ dist, data = df_DTW)
summary(model2)
model3 <- lm(log(adjusted_ppm) ~ wet_precent  + dist, data = df_DTW)
summary(model3)
model4 <- lm(log(adjusted_ppm) ~ wet_precent  + CatchmentSize_ha, data = df_DTW)
summary(model4)

ggplot(df_DTW, aes(x=CatchmentSize_ha, y=log(adjusted_ppm),color=log(wet_precent))) + 
  geom_point(size=3)  + 
  scale_colour_gradient(
    low = "red",
#    mid = "white",
    high = "blue") +
  ggtitle("ALL")



