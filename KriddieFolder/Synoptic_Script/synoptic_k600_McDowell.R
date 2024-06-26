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

#Do it the way that McDowell does it! Change Nothing!!

#read in df
synoptic.df <- read.csv(here::here("ProcessedData/ALL_synoptic_2022-06-08.csv"))
synoptic.df <- synoptic.df%>%drop_na(Date)%>%rename(air_pressure_hpa = Total_hPa)%>%select(
  lon_fit,lat_fit,ele_fit,dist,Date,Flux_ave,adjusted_ppm,AirTemp_c,WaterTemp_c,air_pressure_hpa,surface_area,
  flux_umolpers,Totalflux_umolpers,DOC,TDN,Wetland,dist_ANTE,dist_GAVI,dist_COLM,dist_GAVItrib1,dist_GAVItrib2,dist_GAVItrib3,
)
synoptic.df <- synoptic.df%>%rename(pCO2_ppm=adjusted_ppm)
###calculate K600 using data collected from EOS and Viasala 
#calculate using direct units


### Calculate concentration (gCO2-C per Liter) in air using ###

#concentration of air
#from www.esrl.noaa.gov
#mean of 2021
#416.45 ppm

#CO2_air_ppm <- 416.45*10^-6
synoptic.df$CO2_air_ppm <- 416.45
R=0.08205736608096
gCO2asC <- 12

#convert hpa to atm; 1hPa = 0.0009869233 atm
synoptic.df$air_pressure_atm <- synoptic.df$air_pressure_hpa * 0.0009869233
synoptic.df$pCO2_air_atm <- synoptic.df$air_pressure_atm * synoptic.df$CO2_air_ppm * 10^-6

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
kH_STP_mol.L.atm = .034
D_K = 2300 
synoptic.df$WaterTemp_K <- synoptic.df$WaterTemp_c + 273.15
T_STP_K = 298.15

#calculate henry's law constant using 
synoptic.df$KH_mol.L.atm <- kH_STP_mol.L.atm * exp(D_K*(1/synoptic.df$WaterTemp_K - 1/T_STP_K))


#synoptic.df$VaporPressure_atm <- 10^(5.40221-(1838.675/((synoptic.df$AirTemp_c + 273.15)-31.737)))

#synoptic.df$TotalAir_atm <- synoptic.df$air_pressure_atm - synoptic.df$VaporPressure_atm
synoptic.df$CO2_CO2asC_gperL <- synoptic.df$pCO2_ppm*synoptic.df$KH_mol.L.atm*12*10^-6
synoptic.df$deltaCO2 <- synoptic.df$KH_mol.L.atm*(synoptic.df$CO2_CO2asC_gperL - synoptic.df$pCO2_air_atm)

#change units of flux (umol/m2/s - > g C m−2 d−1)
#86400 seconds in a day
synoptic.df$Flux_gC_m2_s <- synoptic.df$Flux_ave * 10^-6 * 12 * 86400


synoptic.df$k_m_d <- synoptic.df$Flux_gC_m2_s / synoptic.df$deltaCO2 
synoptic.df$k_m_d_2 <- synoptic.df$Flux_gC_m2_s / synoptic.df$deltaCO2 /1000



#convert to k600
#k600 = kCO2 * (600/SC)^-0.5
#SC= schmidst constant, temperature dependent
#SC = 1923.6 - 125.06*T + 4.3773*T^2 - 0.085681*T^3 + 0.00070284*T^4
#T = temperature in c

#Wanninkhof 2014
synoptic.df$Sc <- 1923.6 - 125.06*synoptic.df$WaterTemp_c + 4.3773*(synoptic.df$WaterTemp_c)^2 - 0.085681*(synoptic.df$WaterTemp_c)^3 + 0.00070284 **(synoptic.df$WaterTemp_c)^4

synoptic.df$K600.effective <- synoptic.df$k_m_d_2 * (600/synoptic.df$Sc)^(-0.5)

write.csv(synoptic.df, here::here("ProcessedData/Conundrum2_whitmore.csv"))


#########SAVED FOR LATER
