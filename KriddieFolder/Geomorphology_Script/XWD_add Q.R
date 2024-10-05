#add Q and k600 to xwd data
library(here)
library(dplyr)
library(tidyr)


XWD_allsites <- read.csv(here::here("ProcessedData/xwd_allsites_slope.csv"))
Q_df <- read.csv(here::here("ProcessedData/Q_synop.csv"))
synop_allsites <- read.csv(here::here("ProcessedData/synop_allsites_slope.csv"))


#model Q
model_Q <- lm(log(Q_m3s) ~ log(catchment_ha), data = Q_df)
summ_model_Q <- summary(model_Q)
XWD_allsites$Q <- exp(summ_model_Q$coefficients[1]+
                         summ_model_Q$coefficients[2]*log(XWD_allsites$catchment_ha))

#add co2


#set 0 or negative slopes to low value
XWD_allsites <- XWD_allsites%>%filter(slope_mid > 0)%>%filter(XWD_allsites > 0)
XWD_allsites$slope_mid[XWD_allsites$slope_mid == 0 ] <- (1/20)/2

#calc velocity
#Q=(w+w)/2*d*v
#v=Q/w/d
XWD_allsites$v_ms <- XWD_allsites$Q / (XWD_allsites$d/100) / (XWD_allsites$w/100)


#calc k600 using raymond (all raymond?)
#k600 = (V*S)^.89 * D^0.54 * 5037
#k600 = 5937 * (1 - 2.54 * Fr^2) * (VS)^0.89 * D^0.58
#k600 = 4725 – 445 * (VS)^0.86 · Q -0.14 – 0.012 · D 0.66 – 0.029
XWD_allsites$k600_eq1 <- (XWD_allsites$v_ms * XWD_allsites$slope_mid)^.89 * (XWD_allsites$d/100)^0.54 * 5037

#correct with ulseth
#eD = gSV ; where g is acceleration due to gravity = 9.81 m/s/s ; S is slope ; V is velocity (m/s)) (units m2 s-3) - Raymond et al. 2012
#eD break point is 0.020 m2 s–3
XWD_allsites$eD <- 9.81*XWD_allsites$slope_mid*XWD_allsites$v_ms
#allsites_df$k600_eq1_ulsethcorrection <- allsites_df$k600_eq1 * 1.58 - .54
XWD_allsites$k600_eq1_ulsethcorrection <- -0.54 + 1.58*
  exp(5.137 + .468*log(XWD_allsites$v_ms * XWD_allsites$slope_mid) + .242*log(XWD_allsites$d))

XWD_allsites$k600_eq1_final <- NA
#all_data_bind <- all_data_bind%>%drop_na(eD)

for(i in 1:nrow(XWD_allsites)) {
  if (is.na(XWD_allsites$eD[i])){
    XWD_allsites$k600_eq1_final[i] <- NA
  } else if (XWD_allsites$eD[i] < .020 ){
    XWD_allsites$k600_eq1_final[i] <- XWD_allsites$k600_eq1[i]
  }else{
    XWD_allsites$k600_eq1_final[i] <- XWD_allsites$k600_eq1_ulsethcorrection[i]
  }
}

#adjust henry to temp: KH = KH(STP) x exp(D(1/T-1/T(STP)))
#use constants in  	Burkholder et al. (2019) and convert to desired units
#  k°H (mol/(kg*bar) = mol/l/atm
#d(ln(kH))/d(1/T) (K)
kH_STP_mol.l.atm = .035*1/0.986923
D_K = 2400 
T_STP_K = 298.15

XWD_allsites$pCO2_air_ppm <- 418.53 # 2022 average manoa
#########
baro_hpa <- 627
XWD_allsites$air_pressure_atm <- baro_hpa * 0.000987 
XWD_allsites$water_pressure_atm <- baro_hpa * 0.000987 + 0.000967841

########

XWD_allsites$pCO2_air_atm <-  XWD_allsites$pCO2_air_ppm / 10^6  * XWD_allsites$air_pressure_atm
XWD_allsites$pCO2_w_atm <- XWD_allsites$co2 / 10^6 * XWD_allsites$water_pressure_atm 

#henry's constant adjust for temp
watertemp_c <- 6

all_data_bind$KH_mol.l.atm <- kH_STP_mol.l.atm * exp(D_K*(1/(watertemp_c+273.15) - 1/T_STP_K))
all_data_bind$KH_mol.m3.atm <- all_data_bind$KH_mol.l.atm * 1000

all_data_bind$Sc_co2 <- 1923.6 - 125.06*watertemp_c + 4.3773*(watertemp_c)^2 - 0.085681*(watertemp_c)^3 + 0.00070284 * (watertemp_c)^4
#convert to k [m/d]
all_data_bind$k.m.d_eq1 <- all_data_bind$k600_eq1_final / ((600/all_data_bind$Sc_co2)^(-.05))
#calc flux [umol/m2/d]
all_data_bind$F_mol_m2_d_eq1 <- all_data_bind$k.m.d * all_data_bind$KH_mol.m3.atm * (all_data_bind$pCO2_w_atm -  all_data_bind$pCO2_air_atm )
#calc flux [umol/d]
all_data_bind$F_CO2_molperd_eq1 <- all_data_bind$F_mol_m2_d_eq1 * 3 * all_data_bind$width/100

