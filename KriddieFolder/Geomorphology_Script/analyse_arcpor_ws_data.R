#let's calculate flux for each pixel babay daby

library(here)
library(dplyr)
library(tidyr)



dataFrame <- read.csv(here::here("Geomorphology/Geomorph_from_ArcPro/colm_rnetwork_point.csv"))
dataFrame$NEAR_FID <- paste("seg",dataFrame$NEAR_FID,sep="_")

split_data <- split(dataFrame, f = dataFrame$NEAR_FID)   

## slope up

rm(all_data_temp)
rm(all_data)

for (n in split_data) {
  #for each stream_seg_code, do these things:
  seg_df <- n
  #order by accumulation
  seg_df <- seg_df[order(seg_df$flo_accu),]
  for(i in 1:nrow(seg_df)) {
    seg_df$lat_up20[i] <- NA
    seg_df$lon_up20[i] <- NA
    seg_df$ele_up20[i] <- NA
    #find 6 "above"
    if(i < 7) {
      seg_df$lat_up20[i] <- NA
      seg_df$lon_up20[i] <- NA
      seg_df$ele_up20[i] <- NA
    }else {
      seg_df$lat_up20[i] <- seg_df$lat[i-6]
      seg_df$lon_up20[i] <- seg_df$lon[i-6]
      seg_df$ele_up20[i] <- seg_df$ele[i-6]
    }
    seg_df$dist_diff_Up20[i] <- distm(c(seg_df$lon[i], seg_df$lat[i]), 
                                   c(seg_df$lon_up20[i], seg_df$lat_up20[i]), 
                                   fun = distHaversine)
    seg_df$ele_diff_Up20[i] <- seg_df$ele_up20[i] - seg_df$ele[i]
    seg_df$slope_Up20[i] <- seg_df$ele_diff_Up20[i] / seg_df$dist_diff_Up20[i]
  }
  if (!exists("all_data")){
    all_data <- seg_df
  }else{
    all_data_temp <- seg_df
    all_data <- rbind(all_data, all_data_temp)
    rm(all_data_temp)
  }
}

all_data_slopeUp <- all_data


## slope mid

rm(all_data_temp)
rm(all_data)

for (n in split_data) {
  #for each stream_seg_code, do these things:
  seg_df <- n
  #order by accumulation
  seg_df <- seg_df[order(seg_df$flo_accu),]
  for(i in 1:nrow(seg_df)) {
    seg_df$lat_up10[i] <- NA
    seg_df$lon_up10[i] <- NA
    seg_df$ele_up10[i] <- NA
    
    seg_df$lat_down10[i] <- NA
    seg_df$lon_down10[i] <- NA
    seg_df$ele_down10[i] <- NA
    
    #find 6 "above"
    if(i <= 3) {
      seg_df$lat_up10[i] <- NA
      seg_df$lon_up10[i] <- NA
      seg_df$ele_up10[i] <- NA
      
      seg_df$lat_down10[i] <- seg_df$lat[i+3]
      seg_df$lon_down10[i] <- seg_df$lon[i+3]
      seg_df$ele_down10[i] <- seg_df$ele[i+3]
      
    }else if (i <= nrow(seg_df) -3){
      seg_df$lat_up10[i] <- seg_df$lat[i-3]
      seg_df$lon_up10[i] <- seg_df$lon[i-3]
      seg_df$ele_up10[i] <- seg_df$ele[i-3]
      
      seg_df$lat_down10[i] <- seg_df$lat[i+3]
      seg_df$lon_down10[i] <- seg_df$lon[i+3]
      seg_df$ele_down10[i] <- seg_df$ele[i+3]
      
    }else{
      seg_df$lat_up10[i] <- seg_df$lat[i-3]
      seg_df$lon_up10[i] <- seg_df$lon[i-3]
      seg_df$ele_up10[i] <- seg_df$ele[i-3]
      
      seg_df$lat_down10[i] <- NA
      seg_df$lon_down10[i] <- NA
      seg_df$ele_down10[i] <- NA
    }
    seg_df$dist_diff_mid[i] <- distm(c(seg_df$lon_up10[i], seg_df$lat_up10[i]), 
                                      c(seg_df$lon_down10[i], seg_df$lat_down10[i]), 
                                      fun = distHaversine)
    seg_df$ele_diff_mid[i] <- seg_df$ele_up10[i] - seg_df$ele_down10[i]
    seg_df$slope_mid[i] <- seg_df$ele_diff_mid[i] / seg_df$dist_diff_mid[i]
    
  }
  if (!exists("all_data")){
    all_data <- seg_df
  }else{
    all_data_temp <- seg_df
    all_data <- rbind(all_data, all_data_temp)
    rm(all_data_temp)
  }
}


all_data_slopeMid <- all_data

all_data_bind <- full_join(all_data_slopeMid,all_data_slopeUp,by=c("OBJECTID","pointid","ele","stream_seg_code","lat","lon","flo_accu","NEAR_FID"))

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
                           summ_model_co2$coefficients[3]*log1p(all_data_bind$slope_Up20))

all_data_bind$width <- exp(summ_model_w$coefficients[1]+
                             summ_model_w$coefficients[2]*log(all_data_bind$catchment_ha))

all_data_bind$depth <- exp(summ_model_d$coefficients[1]+
                             summ_model_d$coefficients[2]*log(all_data_bind$catchment_ha)+
                             summ_model_d$coefficients[3]*log1p(all_data_bind$slope_mid))

all_data_bind$Q <- exp(summ_model_Q$coefficients[1]+
                         summ_model_Q$coefficients[2]*log(all_data_bind$catchment_ha))


### now calc flux

#set 0 or negative slopes to low value
all_data_bind$slope_mid[all_data_bind$slope_mid <= 0 ] <- .001
all_data_bind$slope_Up20[all_data_bind$slope_Up20 <= 0 ] <- .001
#calc velocity
#Q=(w+w)/2*d*v
#v=Q/w/d
all_data_bind$v_ms <- all_data_bind$Q / (all_data_bind$depth/100) / (all_data_bind$width/100)


#calc k600 using raymond (all raymond?)
#k600 = (V*S)^.89 * D^0.54 * 5037
#k600 = 5937 * (1 - 2.54 * Fr^2) * (VS)^0.89 * D^0.58
#k600 = 4725 – 445 * (VS)^0.86 · Q -0.14 – 0.012 · D 0.66 – 0.029
all_data_bind$k600_eq1 <- (all_data_bind$v_ms * all_data_bind$slope_mid)^.89 * (all_data_bind$depth/100)^0.54 * 5037

#correct with ulseth
#eD = gSV ; where g is acceleration due to gravity = 9.81 m/s/s ; S is slope ; V is velocity (m/s)) (units m2 s-3) - Raymond et al. 2012
#eD break point is 0.020 m2 s–3
all_data_bind$eD <- 9.81*all_data_bind$slope_mid*all_data_bind$v_ms
#allsites_df$k600_eq1_ulsethcorrection <- allsites_df$k600_eq1 * 1.58 - .54
all_data_bind$k600_eq1_ulsethcorrection <- -0.54 + 1.58*
  exp(5.137 + .468*log(all_data_bind$v_ms * all_data_bind$slope_mid) + .242*log(all_data_bind$depth))

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
all_data_bind$pCO2_w_atm <- all_data_bind$co2 / 10^6 * all_data_bind$water_pressure_atm 

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
all_data_bind$F_CO2_molpers_eq1 <- all_data_bind$F_mol_m2_d_eq1 * 3 * all_data_bind$width/100


#make some plots right quick, bin data
allsites_df_bin5 <- all_data_bind %>% mutate(new_bin = cut(log10(catchment_ha), breaks=5))
allsites_df_bin5_summary <- allsites_df_bin5%>%group_by(new_bin)%>%
  summarise(
    co2_mean = mean(co2,na.rm =TRUE),
    slope_mid_mean = mean(slope_mid,na.rm =TRUE),
    k600_eq1_mean = mean(k600_eq1_final,na.rm =TRUE),
    F_mol_m2_d_eq1_mean = mean(F_mol_m2_d_eq1,na.rm =TRUE),
    F_CO2_molpers_eq1_mean = mean(F_CO2_molpers_eq1,na.rm =TRUE),
    F_CO2_molpers_eq1_sum = sum(F_CO2_molpers_eq1,na.rm =TRUE)
    
  )


p<-ggplot(data=allsites_df_bin5_summary, aes(x=new_bin, y=co2_mean)) +
  geom_bar(stat="identity") + xlab("log10 catchment bin (ha)") + ylab("mean pCO2") +theme_bw(base_size = 16)
p

p<-ggplot(data=allsites_df_bin5_summary, aes(x=new_bin, y=slope_mid_mean)) +
  geom_bar(stat="identity") + xlab("log10 catchment bin (ha)") + ylab("mean slope") +theme_bw(base_size = 16)
p

p<-ggplot(data=allsites_df_bin5_summary, aes(x=new_bin, y=k600_eq1_mean))+
  geom_bar(stat="identity")  + xlab("log10 catchment bin (ha)") + ylab("mean k600") +theme_bw(base_size = 16)
p

p<-ggplot(data=allsites_df_bin5_summary, aes(x=new_bin, y=F_mol_m2_d_eq1_mean)) +
  geom_bar(stat="identity") + xlab("log10 catchment bin (ha)") + ylab("mean flux mol*m2/day") +theme_bw(base_size = 16)
p

p<-ggplot(data=allsites_df_bin5_summary, aes(x=new_bin, y=F_CO2_molpers_eq1_mean)) +
  geom_bar(stat="identity")+ xlab("log10 catchment bin (ha)") + ylab("mean flux mol/s") +theme_bw(base_size = 16)
p

p<-ggplot(data=allsites_df_bin5_summary, aes(x=new_bin, y=F_CO2_molpers_eq1_sum)) +
  geom_bar(stat="identity")+ xlab("log10 catchment bin (ha)") + ylab("mean flux mol/s") +theme_bw(base_size = 16)
p



