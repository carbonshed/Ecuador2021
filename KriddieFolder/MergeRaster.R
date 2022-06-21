#This Rscript is to merge raster data with new synoptic data
# I created the raster data set in arcpro using data that has the same locations, but vaisala concnetrations have been altered.
#This script will 

RasterData <- read.csv(here::here("Synoptic/Synop_all_raster3.csv"))
RasterData <- RasterData[
  
df <- read.csv(here::here("ProcessedData/ALL_synoptic_2022-06-08.csv"))
df <- df%>%drop_na(Date)

#Amanda
# july 5th-12th

#mimi and dad
# July 13th - July 21st
#maybe 13th-20th
#wednesday
#

#mindo
#