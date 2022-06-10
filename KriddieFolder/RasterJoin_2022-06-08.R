#merge raster data with new synoptic data

RasterData <- read.csv(here::here("Synoptic/Synop_all_raster3.csv"))
#RasterData <- RasterData[,c("Date","Wetland","Flux_ave","FlowAccu","Flowlen","Slope")]
RasterData$EOS_no<-gsub("\\_","",RasterData$EOS_no)

RasterData$Date <- as.Date(RasterData$Date, format="%m/%d/%Y")

df <- read.csv(here::here("ProcessedData/ALL_synoptic_2022-06-08.csv"))
df$Date <- as.Date(df$Date, format="%Y-%m-%d")

df <- full_join(df,RasterData,by=c("Date","Wetland","Flux_ave"))
