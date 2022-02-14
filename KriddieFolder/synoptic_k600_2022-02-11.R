#Calc k600 

library(here)
library(dplyr) 
library(tidyr) 
library(ggplot2)
library(cowplot)
library(tidyverse)
library(rstatix)
library(ggpubr)
library(dplyr)
library(lubridate)

#read in df
ANTE <- read.csv(here::here("/ProcessedData/ANTE_synoptic_2022-02-13.csv"))
GAVI <- read.csv(here::here("/ProcessedData/GAVI_synoptic_2022-02-14.csv"))
COLM <- read.csv(here::here("/ProcessedData/COLMILLO_synoptic_2022-02-14.csv"))

ANTE <- ANTE[,c("lon_fit","lat_fit","ele_fit","dist","Date","EOS_no","VaisalaType","Flux_ave","CO2_ppm_ave","adjusted_ppm","AirTemp_c","GAVI_waterTempAve","Total_hPa")]
COLM <- COLM[,c("lon_fit","lat_fit","ele_fit","dist","Date","EOS_no","VaisalaType","Flux_ave","CO2_ppm_ave","adjusted_ppm","AirTemp_c","COLM_waterTempAve","Total_hPa")]
GAVI <- GAVI[,c("lon_fit","lat_fit","ele_fit","dist","Date","EOS_no","VaisalaType","Flux_ave","CO2_ppm_ave","adjusted_ppm","AirTemp_c","WaterTemp_c","Total_hPa")]

colnames(ANTE)[which(names(ANTE) == "GAVI_waterTempAve")] <- "WaterTemp_c"
colnames(COLM)[which(names(COLM) == "COLM_waterTempAve")] <- "WaterTemp_c"


#Remove unique
ANTE <- unique(ANTE)
GAVI <- unique(GAVI)
COLM <- unique(COLM)

#add weltand info
GAVI$Wetland <- "GAVI"
ANTE$Wetland <- "ANTE"
COLM$Wetland <- "COLM"

#need seperate columns for each stream profile
ANTE$dist_ANTE <- ANTE$dist
ANTE$dist_GAVI <- NA
ANTE$dist_COLM <- NA


GAVI$dist_GAVI <- GAVI$dist
GAVI$dist_ANTE <- NA
GAVI$dist_COLM <- NA

COLM$dist_COLM <- COLM$dist
COLM$dist_ANTE <- NA
COLM$dist_GAVI <- NA

synoptic.df <- rbind(ANTE,GAVI,COLM)

###calculate K600 using data collected from EOS and Viasala 
#calculate using direct units


### Calculate concentration (gCO2-C per Liter) in air using ###

#concentration of air
#from www.esrl.noaa.gov
#average of July and August
#410.86 ppm
CO2_air_ppm <- 410*10^-6
R=0.08205736608096
gCO2asC <- 12

#convert hpa to atm; 1hPa = 0.0009869233 atm
synoptic.df$air_pressure_atm <- synoptic.df$Total_hPa * 0.0009869233

synoptic.df$VaporPressure_atm <- 10^(5.40221-(1838.675/((synoptic.df$AirTemp_c + 273.15)-31.737)))

synoptic.df$TotalAir_atm <- synoptic.df$air_pressure_atm - synoptic.df$VaporPressure_atm
synoptic.df$Total_air_MolperL <- synoptic.df$TotalAir_atm/(R*(synoptic.df$AirTemp_c + 273.15)) 
synoptic.df$CO2_air_MolesPerLiter <- synoptic.df$Total_air_MolperL * CO2_air_ppm
# 12 grams of C in 1 mole of CO2
synoptic.df$CO2_air_gCO2asCPerLiter <- synoptic.df$CO2_air_MolesPerLiter * gCO2asC


### Calculate concentration CO2 in water (gCO2-C per Liter) using Henry's law ###
#Equation below can be used to adjust Henry's constant to the temperature of the environment
# *NOTE: we may need to do an additional calculation to adjust for low pressure in our environment?*
#**KH = KH(STP) x exp(D(1/T-1/T(STP)))**
#* KH(STP) = Henry's law constant at STP (0.035 mol/L)
#*NOTE: the cited literature say that this in mol/(kg x bar)*
#  * D = Temperature dependence constant (2400 K)
#* T = Water Temperature (K)
#* T(STP) = Temp at STP (298.15 K)

#set constants
kH_STP_mol.L.atm = .035
D_K = 2400 
synoptic.df$WaterTemp_K <- synoptic.df$WaterTemp_c + 273.15
T_STP_K = 298.15

#calculate henry's law constant using 
synoptic.df$KH_mol.L.atm <- kH_STP_mol.L.atm * exp(D_K*(1/synoptic.df$WaterTemp_K - 1/T_STP_K))


UatmToatm <- 10^6

#calculate mass equivalence of CO2 in water
synoptic.df$CO2_water_gCO2asCPerLiter <- (synoptic.df$adjusted_ppm / UatmToatm) * synoptic.df$KH_mol.L.atm * gCO2asC

#calculate concntration difference between CO2 in the water and in the air
#for some reason, unclear to me, Mcdowell multiplies by henry's constant AGAIN. Let's try it
LiterToCubicMeter = 1000

synoptic.df$deltaCO2_gCO2asCperM3 <-  (synoptic.df$CO2_water_gCO2asCPerLiter - synoptic.df$CO2_air_gCO2asCPerLiter)  *
  synoptic.df$KH_mol.L.atm  * LiterToCubicMeter


#convert flux to mole/m2/d
#seconds per day (86,400)
SecToDay <- 86400
#umole to mole
UmoleToMole <- 10^6
#convert moles to grams of Carbon dioxide as Carbon
#gCO2asC <- 12

synoptic.df$Flux_gCO2asCperM2perDay <- synoptic.df$Flux_ave * SecToDay /  UmoleToMole *gCO2asC

#Fick' Law: calculate K

#**F(aq) = K[pCO2(aq) - pCO2(air)] x KH**
#  * k = F/(pCO2(aq)-pC02(air))/KH
#* F = Flux mole/m2/d
#* K = gas transfer rate m/d
#* KH = henry's constant for molar concentration of CO2 in water 

#*rearrange equation to solve for k*
#**k = F(aq) / ([pCO2(aq) - pCO2(air)] x KH)**

#convert Liters to cubic meters
#LiterToCubicMeter <- 1000
#df$deltaCO2_gCO2asCPerCubicMeter <- df$deltaCO2_gCO2asCPerLiter * LiterToCubicMeter

#calculate

synoptic.df$k_m.d <- synoptic.df$Flux_gCO2asCperM2perDay  / synoptic.df$deltaCO2_gCO2asCperM3


#convert to k600
#k600 = kCO2 * (600/SC)^-0.5
#SC= schmidst constant, temperature dependent
#SC = 1911.1 - 118.11*T + 3.4527*T^2 - 0.04132*T^3
#T = temperature in c
synoptic.df$Sc <- 1911.1 - 118.11*synoptic.df$WaterTemp_c + 3.4527*(synoptic.df$WaterTemp_c)^2 - 0.04132*(synoptic.df$WaterTemp_c)^3

synoptic.df$K600.effective <- synoptic.df$k_m.d * (600/synoptic.df$Sc)^(-0.5)

synoptic.df <- synoptic.df%>%drop_na(EOS_no)

write.csv(synoptic.df, here::here("ProcessedData/ALL_synoptic_2022-02-14.csv"))
