#Calc k600 

library(here)
library(dplyr) # for `rename`
library(tidyr) # for `gather`
library(ggplot2)
library(cowplot)
library(tidyverse)
library(rstatix)
library(ggpubr)

#read in df
ANTE <- read.csv(here::here("/ProcessedData/ANTE_synoptic_2022-02-11.csv"))
GAVI <- read.csv(here::here("/ProcessedData/GAVI_synoptic_2022-01-27.csv"))
COLM <- read.csv(here::here("/ProcessedData/COLMILLO_synoptic_2022-01-27.csv"))

ANTE <- ANTE[,c("lon","lat","ele","lon_fit","lat_fit","ele_fit","dist","Date","EOS_no","VaisalaType","Flux_ave","CO2_ppm_ave","adjusted_ppm","AirTemp_c","GAVI_waterTempAve","Total_hPa")]
COLM <- COLM[,c("lon_fit","lat_fit","ele_fit","dist","Date","EOS_no","Flux_ave","CO2_ppm_ave","adjusted_ppm")]
GAVI <- GAVI[,c("lon_fit","lat_fit","ele_fit","dist","Date","EOS_no","VaisalaType","Flux_ave","CO2_ppm_ave","adjusted_ppm","AirTemp_c","GAVI_waterTempAve","Total_hPa","DOC","TDN")]

ANTE <- unique(ANTE)

##run WaterLevel_Discharge.Rmd
WL_01 <- WL_01[,c(1,3)]
colnames(WL_01) <- c("DateTime","WL1_temp")
WL_02 <- WL_02[,c(1,3)]
colnames(WL_02) <- c("DateTime","WL2_temp")
WL_03 <- WL_03[,c(1,3)]
colnames(WL_03) <- c("DateTime","WL3_temp")
WL_04 <- WL_04[,c(1,3)]
colnames(WL_04) <- c("DateTime","WL4_temp")
WL_05 <- WL_05[,c(1,3)]
colnames(WL_05) <- c("DateTime","WL5_temp")
WL_06 <- WL_06[,c(1,3)]
colnames(WL_06) <- c("DateTime","WL5_temp")

WL_Gavi <- full_join(WL_01,WL_02, by="DateTime")
WL_Gavi <- full_join(WL_Gavi,WL_03, by="DateTime")
WL_Gavi <- full_join(WL_Gavi,WL_04, by="DateTime")
WL_Gavi$Gavi_temp <- rowMeans(WL_Gavi[ , c(2:5)], na.rm=TRUE)


WL_Colm <- full_join(WL_05,WL_06, by="DateTime")
WL_Colm$Colm_temp <- rowMeans(WL_Colm[ , c(2:3)], na.rm=TRUE)


