#use this code to model flux for all river networks

library(here)
library(dplyr)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(sf)
library(raster)
library(tmap)
library(geosphere)

#read in direct measurments
k600_df <- read.csv(here::here("ProcessedData/AllSynoptic_raymondk600_Oct8.csv"))%>%drop_na(k600_eq1_final)%>%filter(site!="GaviTrib3")
synop_allsites <- read.csv(here::here("ProcessedData/synop_allsites_slope.csv"))
synop_allsites_nowetland <- read.csv(here::here("ProcessedData/synop_allsites_nowetland_slope.csv"))
XWD_allsites <- read.csv(here::here("ProcessedData/xwd_allsites_slope.csv"))
Q_df <- read.csv(here::here("ProcessedData/Q_synop.csv"))

#read in catchment data
gavi_df <- read.csv(here::here("ProcessedData/slope_gavi_oct3.csv"))
gavi_df$cathment_name <- "gavi"
colm_df <- read.csv(here::here("ProcessedData/slope_colm_oct3.csv"))
colm_df$cathment_name <- "colm"
ante_df <- read.csv(here::here("ProcessedData/slope_ante_oct3.csv"))
ante_df$cathment_name <- "ante"

#count haw many are negative slope?
colm_test_init_1ha <- colm_df%>%filter(flo_accu > 1/3/3/.0001)
colm_test1 <- colm_test_init_1ha%>%drop_na(slope_mid)%>%drop_na(slope_up20)
colm_test2 <- colm_df%>%filter(slope_mid > 0)%>%filter(slope_up20 > 0)
colm_test3 <- colm_test_init_1ha%>%filter(slope_mid < 0 | slope_up20 < 0)
#count haw many are negative slope?
gavi_test_init_1ha <- gavi_df%>%filter(flo_accu > 1/3/3/.0001)
gavi_test1 <- gavi_test_init_1ha%>%drop_na(slope_mid)%>%drop_na(slope_up20)
gavi_test2 <- gavi_df%>%filter(slope_mid > 0)%>%filter(slope_up20 > 0)
gavi_test3 <- gavi_test_init_1ha%>%filter(slope_mid < 0 | slope_up20 < 0)
#count haw many are negative slope?
ante_test_init_1ha <- ante_df%>%filter(flo_accu > 1/3/3/.0001)
ante_test1 <- ante_test_init_1ha%>%drop_na(slope_mid)%>%drop_na(slope_up20)
ante_test2 <- ante_df%>%filter(slope_mid > 0)%>%filter(slope_up20 > 0)
ante_test3 <- ante_test_init_1ha%>%filter(slope_mid < 0 | slope_up20 < 0)

#for colm, init 1ha, 8 na and 101 negative slope

all_data_bind <- rbind(colm_df,ante_df,gavi_df)
#all_data_bind <- rbind(all_data_bind,ante_df)

all_data_bind$catchment_ha <- all_data_bind$flo_accu*3*3*.0001
###now you can add in the width, depth, flux data

#anova
model_co2 <- lm(log(adjusted_ppm) ~ log(catchment_ha) + log1p(slope_up), data = synop_allsites%>%drop_na(slope_up))
model_co2_nowetland <- lm(log(adjusted_ppm) ~ log(catchment_ha) + log1p(slope_up), data = synop_allsites_nowetland%>%drop_na(slope_up))
model_w <- lm(log(w) ~ log(catchment_ha)  , data = XWD_allsites)
model_d <- lm(log(d) ~ log(catchment_ha) + log1p(slope_mid), data = XWD_allsites)
model_Q <- lm(log(Q_m3s) ~ log(catchment_ha), data = Q_df)


summ_model_co2 <- summary(model_co2)
summ_model_co2_nowetland <-summary(model_co2_nowetland)
summ_model_w <- summary(model_w)
summ_model_d <-summary(model_d)
summ_model_Q <- summary(model_Q)

all_data_bind$co2 <- exp(summ_model_co2$coefficients[1]+
                           summ_model_co2$coefficients[2]*log(all_data_bind$catchment_ha)+
                           summ_model_co2$coefficients[3]*log1p(all_data_bind$slope_up20))

all_data_bind$width <- exp(summ_model_w$coefficients[1]+
                             summ_model_w$coefficients[2]*log(all_data_bind$catchment_ha))

all_data_bind$depth <- exp(summ_model_d$coefficients[1]+
                             summ_model_d$coefficients[2]*log(all_data_bind$catchment_ha)+
                             summ_model_d$coefficients[3]*log1p(all_data_bind$slope_mid))

all_data_bind$Q <- exp(summ_model_Q$coefficients[1]+
                         summ_model_Q$coefficients[2]*log(all_data_bind$catchment_ha))


### now calc flux
#first check how many valuses are negative, and how many slope =0
all_data_bind_test1 <- all_data_bind%>%filter(slope_mid >= 0)%>%filter(slope_up20 >= 0)
all_data_bind_test2 <- all_data_bind%>%filter(slope_mid == 0 | slope_up20 == 0)
all_data_bind_test3 <- all_data_bind%>%filter(ele_diff_mid < -1 |ele_up20 < -1)

#set 0  slopes to low value
all_data_bind <- all_data_bind%>%drop_na(slope_mid)%>%drop_na(slope_up20)%>%
  filter(ele_diff_mid > -1)%>%
  filter(ele_diff_up20 > -1)
#all_data_bind <- all_data_bind%>%filter(slope_mid > 0)%>%filter(slope_up20 > 0)
all_data_bind$slope_mid[all_data_bind$slope_mid <= 0 ] <- (1/20)/2
all_data_bind$slope_up20[all_data_bind$slope_up20 <= 0 ] <- (1/20)/2
#calc velocity
#Q=(w+w)/2*d*v
#v=Q/w/d
all_data_bind$v_ms <- all_data_bind$Q / (all_data_bind$depth/100) / (all_data_bind$width/100)


#calc k600 using eD as in Ulseth
#first calc eD
#eD = gSV ; where g is acceleration due to gravity = 9.81 m/s/s ; S is slope ; V is velocity (m/s)) (units m2 s-3) - Raymond et al. 2012
#eD break point is 0.020 m2 s–3
all_data_bind$eD <- 9.81*all_data_bind$slope_mid*all_data_bind$v_ms

#then calc low energy based on eD
all_data_bind$k600_eq1 <- exp(.035*log(all_data_bind$eD) + 3.1)

#then calc high energy stream
all_data_bind$k600_eq1_ulsethcorrection <-   exp(1.18*log(all_data_bind$eD) + 6.43)

#to calculate the final, do peicewise base on 0.02 breakpoint
all_data_bind$k600_eq1_final <- NA

#all_data_bind <- all_data_bind%>%drop_na(eD)
for(i in 1:nrow(all_data_bind)) {
  if (is.na(all_data_bind$eD[i])){
    all_data_bind$k600_eq1_final[i] <- NA
  } else if (all_data_bind$eD[i] < .020 ){
    all_data_bind$k600_eq1_final[i] <- all_data_bind$k600_eq1[i]
  }else{
    all_data_bind$k600_eq1_final[i] <- all_data_bind$k600_eq1_ulsethcorrection[i]
  }
}

#adjust henry to temp: KH = KH(STP) x exp(D(1/T-1/T(STP)))
#use constants in  	Burkholder et al. (2019) and convert to desired units
#  k°H (mol/(kg*bar) = mol/l/atm
#d(ln(kH))/d(1/T) (K)
kH_STP_mol.l.atm = .035*1/0.986923
D_K = 2400 
T_STP_K = 298.15

all_data_bind$pCO2_air_ppm <- 418.53 # 2022 average manoa
#########
baro_hpa <- 627
all_data_bind$air_pressure_atm <- baro_hpa * 0.000987 
all_data_bind$water_pressure_atm <- baro_hpa * 0.000987 + 0.000967841

########

all_data_bind$pCO2_air_atm <-  all_data_bind$pCO2_air_ppm / 10^6  * all_data_bind$air_pressure_atm
all_data_bind$pCO2_w_atm <- all_data_bind$co2 / 10^6 #* all_data_bind$water_pressure_atm 

#henry's constant adjust for temp
watertemp_c <- mean(k600_df$WaterTemp_c,na.rm=TRUE)

all_data_bind$KH_mol.l.atm <- kH_STP_mol.l.atm * exp(D_K*(1/(watertemp_c+273.15) - 1/T_STP_K))
all_data_bind$KH_mol.m3.atm <- all_data_bind$KH_mol.l.atm * 1000

all_data_bind$Sc_co2 <- 1923.6 - 125.06*watertemp_c + 4.3773*(watertemp_c)^2 - 0.085681*(watertemp_c)^3 + 0.00070284 * (watertemp_c)^4
#convert to k [m/d]
all_data_bind$k.m.d_eq1 <- all_data_bind$k600_eq1_final / ((600/all_data_bind$Sc_co2)^(-.05))
#calc flux [umol/m2/d]
all_data_bind$F_mol_m2_d_eq1 <- all_data_bind$k.m.d * all_data_bind$KH_mol.m3.atm * (all_data_bind$pCO2_w_atm -  all_data_bind$pCO2_air_atm )
#calc flux [umol/d]
all_data_bind$F_CO2_molperd_eq1 <- all_data_bind$F_mol_m2_d_eq1 * 3 * all_data_bind$width/100

#redout data framw
#write.csv(all_data_bind,here::here("ProcessedData/upscaleFlux_allwatersheds_oct18.csv"))
