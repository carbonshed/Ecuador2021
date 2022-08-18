#This Rscript is to merge raster data with new synoptic data
# I created the raster data set in arcpro using data that has the same locations, but vaisala concnetrations have been altered.
#This script will 

library(dplyr)
library(tidyverse)

#RasterData <- read.csv(here::here("Synoptic/Synop_all_raster3.csv"))
RasterData <- read.csv(here::here("ProcessedData/Output_Raster_Synoptic_20220410.csv"))
RasterData <- RasterData[,c("Date","Wetland","EOS_no","Flux_ave","FlowAccu","CatchmentSize_m2","CatchmentSize_km2")]
RasterData$Date <- as.Date(RasterData$Date,format="%m/%d/%Y")
levels(RasterData$EOS_no) <- c("EOS1", "EOS2","EOS1","EOS2")
levels(RasterData$Wetland) <- c("ANTE","COLM","Gavi-mainstem","GAVItrib1","GAVItrib2","GAVItrib3")

df <- read.csv(here::here("ProcessedData/ALL_synoptic_2022-06-08.csv"))
df <- df%>%drop_na(Date)
df$Date <- as.Date(df$Date,format="%Y-%m-%d")
levels(df$Wetland) <- c("ANTE","COLM","Gavi-mainstem","GAVItrib1","GAVItrib2","GAVItrib3")


df_2 <- full_join(df,RasterData,by=c("Date","Wetland","EOS_no","Flux_ave"))

#this is not a perfect join, so after I read this out, I will do some manual edits


write.csv(df_2, here::here("ProcessedData/SynopFlowAccu_20220626.csv"))




#Amanda
# july 5th-12th

#mimi and dad
# July 13th - July 21st
#maybe 13th-20th
#wednesday
#

#mindo
#