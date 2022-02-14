#Calc k600 

library(here)
library(dplyr) 
library(tidyr) 
library(ggplot2)
library(cowplot)
library(tidyverse)
library(rstatix)
library(ggpubr)

#read in df
ANTE <- read.csv(here::here("/ProcessedData/ANTE_synoptic_2022-02-13.csv"))
GAVI <- read.csv(here::here("/ProcessedData/GAVI_synoptic_2022-02-14.csv"))
COLM <- read.csv(here::here("/ProcessedData/COLMILLO_synoptic_2022-02-14.csv"))

ANTE <- ANTE[,c("lon_fit","lat_fit","ele_fit","dist","Date","EOS_no","VaisalaType","Flux_ave","CO2_ppm_ave","adjusted_ppm","AirTemp_c","GAVI_waterTempAve","Total_hPa")]
COLM <- COLM[,c("lon_fit","lat_fit","ele_fit","dist","Date","EOS_no","VaisalaType","Flux_ave","CO2_ppm_ave","adjusted_ppm","AirTemp_c","COLM_waterTempAve","Total_hPa")]
GAVI <- GAVI[,c("lon_fit","lat_fit","ele_fit","dist","Date","EOS_no","VaisalaType","Flux_ave","CO2_ppm_ave","adjusted_ppm","AirTemp_c","WaterTemp_c","Total_hPa")]


