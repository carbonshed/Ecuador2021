#interpolate slope and Q for all files:
#Gavi-down
#Gavi-up
#COLM
#ANTE
#Gavi-trib1
#Gavi-trib2
#Gavi-trib3

#library
library(here)
library(dplyr)
library(zoo)
library(ggplot2)

round_to_first_decimal <- function(number) {
  return(round(number, 1))
}

#read in data

GaviDown <- read.csv(here::here("ProcessedData/export_to_r/gavi-down_XWD_export_CO2.csv"))%>%mutate(d=as.numeric(d))
GaviUp <- read.csv(here::here("ProcessedData/export_to_r/gavi-up_XWD_export_eledata_co2.csv"))
GaviTrib1 <- read.csv(here::here("ProcessedData/export_to_r/gavi-trib1_XWD_export_eledata_co2.csv")) 
GaviTrib2 <- read.csv(here::here("ProcessedData/export_to_r/gavi-trib2_XWD_export_eledata_co2.csv"))%>%rename(Q_m3s=Q_m_s)
GaviTrib3 <- read.csv(here::here("ProcessedData/export_to_r/gavi-trib3_XWD_export_eledata_Edit.csv")) 
Colm <- read.csv(here::here("ProcessedData/export_to_r/colm_XWD_export_eledata_co2.csv"))%>%rename(Q_m3s=Q_m_s) 
Ante <- read.csv(here::here("ProcessedData/export_to_r/ante_XWD_export_eledata_co2.csv"))%>%rename(Q_m3s=Q_m_s)

##########
#build df#
##########

#Colm
Colm$x_pred <- round_to_first_decimal(Colm$x_pred)
Colm_join_df <- data.frame(x_pred=seq(from = min(Colm$x_pred), to = max(Colm$x_pred), by = .1))
Colm_join_df <- left_join(Colm_join_df,Colm,by="x_pred")
Colm_join_df <- Colm_join_df[order(Colm_join_df$x_pred),]

Colm_join_df <- Colm_join_df %>%
  mutate(Q_m3s = na.approx(Q_m3s))%>%
  mutate(adjusted_ppm = na.approx(adjusted_ppm))%>%
  mutate(Flux_ave = na.approx(Flux_ave))%>%
  mutate(WaterTemp_c = na.approx(WaterTemp_c))%>%
  mutate(Total_hPa = na.approx(Total_hPa))%>%
  mutate(AirTemp_c = na.approx(AirTemp_c))



#Ante
Ante$x_pred <- round_to_first_decimal(Ante$x_pred)
Ante_join_df <- data.frame(x_pred=seq(from = min(Ante$x_pred), to = max(Ante$x_pred), by = .1))
Ante_join_df <- left_join(Ante_join_df,Ante,by="x_pred")
Ante_join_df <- Ante_join_df[order(Ante_join_df$x_pred),]

Ante_join_df <- Ante_join_df %>%
  mutate(Q_m3s = na.approx(Q_m3s))%>%
  mutate(adjusted_ppm = na.approx(adjusted_ppm))%>%
  mutate(Flux_ave = na.approx(Flux_ave))%>%
  mutate(WaterTemp_c = na.approx(WaterTemp_c))%>%
  mutate(Total_hPa = na.approx(Total_hPa))%>%
  mutate(AirTemp_c = na.approx(AirTemp_c))


#GaviDown
GaviDown$x_pred <- round_to_first_decimal(GaviDown$x_pred)
GaviDown_join_df <- data.frame(x_pred=seq(from = min(GaviDown$x_pred), to = max(GaviDown$x_pred), by = .1))
GaviDown_join_df <- left_join(GaviDown_join_df,GaviDown,by="x_pred")
GaviDown_join_df <- GaviDown_join_df[order(GaviDown_join_df$x_pred),]

GaviDown_join_df <- GaviDown_join_df %>%
  mutate(Q_m3s = na.approx(Q_m3s))%>%
  mutate(adjusted_ppm = na.approx(adjusted_ppm))%>%
  mutate(Flux_ave = na.approx(Flux_ave))%>%
  mutate(WaterTemp_c = na.approx(WaterTemp_c))%>%
  mutate(Total_hPa = na.approx(Total_hPa))%>%
  mutate(AirTemp_c = na.approx(AirTemp_c))


#gaviup
GaviUp$x_pred <- round_to_first_decimal(GaviUp$x_pred)
GaviUp_join_df <- data.frame(x_pred=seq(from = min(GaviUp$x_pred), to = max(GaviUp$x_pred), by = .1))
GaviUp_join_df <- left_join(GaviUp_join_df,GaviUp,by="x_pred")
GaviUp_join_df <- GaviUp_join_df[order(GaviUp_join_df$x_pred),]

GaviUp_join_df <- GaviUp_join_df %>%
  mutate(Q_m3s = na.approx(Q_m3s))%>%
  mutate(adjusted_ppm = na.approx(adjusted_ppm))%>%
  mutate(Flux_ave = na.approx(Flux_ave))%>%
  mutate(WaterTemp_c = na.approx(WaterTemp_c))%>%
  mutate(Total_hPa = na.approx(Total_hPa))%>%
  mutate(AirTemp_c = na.approx(AirTemp_c))



#gavi trib 1
GaviTrib1$x_pred <- round_to_first_decimal(GaviTrib1$x_pred)
GaviTrib1_join_df <- data.frame(x_pred=seq(from = min(GaviTrib1$x_pred,na.rm = TRUE), to = max(GaviTrib1$x_pred,na.rm = TRUE), by = .1))
GaviTrib1_join_df <- left_join(GaviTrib1_join_df,GaviTrib1,by="x_pred")
GaviTrib1_join_df <- GaviTrib1_join_df[order(GaviTrib1_join_df$x_pred),]

GaviTrib1_join_df <- GaviTrib1_join_df %>%
  mutate(Q_m3s = na.approx(Q_m3s))%>%
  mutate(adjusted_ppm = na.approx(adjusted_ppm))%>%
  mutate(Flux_ave = na.approx(Flux_ave))%>%
  mutate(WaterTemp_c = na.approx(WaterTemp_c))%>%
  mutate(Total_hPa = na.approx(Total_hPa))%>%
  mutate(AirTemp_c = na.approx(AirTemp_c))


#gavi trib 2
GaviTrib2$x_pred <- round_to_first_decimal(GaviTrib2$x_pred)
GaviTrib2_join_df <- data.frame(x_pred=seq(from = min(GaviTrib2$x_pred), to = max(GaviTrib2$x_pred), by = .1))
GaviTrib2_join_df <- left_join(GaviTrib2_join_df,GaviTrib2,by="x_pred")
GaviTrib2_join_df <- GaviTrib2_join_df[order(GaviTrib2_join_df$x_pred),]

GaviTrib2_join_df <- GaviTrib2_join_df %>%
  mutate(Q_m3s = na.approx(Q_m3s))%>%
  mutate(adjusted_ppm = na.approx(adjusted_ppm))%>%
  mutate(Flux_ave = na.approx(Flux_ave))%>%
  mutate(WaterTemp_c = na.approx(WaterTemp_c))%>%
  mutate(Total_hPa = na.approx(Total_hPa))%>%
  mutate(AirTemp_c = na.approx(AirTemp_c))


#add name column
GaviDown_join_df$name <- "GaviDown"
GaviUp_join_df$name <- "GaviUp"
GaviTrib1_join_df$name <- "GaviTrib1"
GaviTrib2_join_df$name <- "GaviTrib2"
GaviTrib3$name <- "GaviTrib3"
Colm_join_df$name <- "Colm"
Ante_join_df$name <- "Ante"

#clean up dataframes
GaviDown_join_df <- GaviDown_join_df%>%select(name,notes,lat_fit,lon_fit,x_pred,ele_fit,ele_arcpro,dist_diff,slope_mid,slope_up,x,d,w,date,VaisalaType,EOS_no,DOC,TDN,Flux_ave,adjusted_ppm,Q_m3s,WaterTemp_c,AirTemp_c,Total_hPa)
GaviUp_join_df <- GaviUp_join_df%>%select(name,notes,lat_fit,lon_fit,x_pred,ele_fit,ele_arcpro,dist_diff,slope_mid,slope_up,x,d,w,Date,VaisalaType,EOS_no,DOC,TDN,Flux_ave,adjusted_ppm,Q_m3s,WaterTemp_c,AirTemp_c,Total_hPa)%>%rename(date=Date)
GaviTrib1_join_df <- GaviTrib1_join_df%>%select(name,notes,lat_fit,lon_fit,x_pred,ele_fit,ele_arcpro,dist_diff,slope_mid,slope_up,x,d,w,Date,VaisalaType,EOS_no,DOC,TDN,Flux_ave,adjusted_ppm,Q_m3s,WaterTemp_c,AirTemp_c,Total_hPa)%>%rename(date=Date)
GaviTrib2_join_df <- GaviTrib2_join_df%>%select(name,notes,lat_fit,lon_fit,x_pred,ele_fit,ele_arcpro,dist_diff,slope_mid,slope_up,x,d,w,Date,VaisalaType,EOS_no,Flux_ave,adjusted_ppm,WaterTemp_c,Q_m3s,AirTemp_c,Total_hPa)%>%rename(date=Date)
GaviTrib2_join_df$DOC <- NA 
GaviTrib2_join_df$TDN <- NA
Colm_join_df <- Colm_join_df%>%select(name,notes,lat_fit,lon_fit,x_pred,ele_fit,ele_arcpro,dist_diff,slope_mid,slope_up,x,d,w,date,VaisalaType,EOS_no,DOC,TDN,Flux_ave,adjusted_ppm,Q_m3s,WaterTemp_c,AirTemp_c,Total_hPa)
Ante_join_df <- Ante_join_df%>%select(name,notes,lat_fit,lon_fit,x_pred,ele_fit,ele_arcpro,dist_diff,slope_mid,slope_up,x,d,w,date,VaisalaType,EOS_no,DOC,TDN,Flux_ave,adjusted_ppm,Q_m3s,WaterTemp_c,AirTemp_c,Total_hPa)

#rm(Ante,Colm,GaviDown,GaviUp,GaviTrib1,GaviTrib2)

#join all data frames
allsites_df <- rbind(GaviDown_join_df,GaviUp_join_df,GaviTrib1_join_df,GaviTrib2_join_df,Colm_join_df,Ante_join_df)

#set 0 or negative slopes to low value
allsites_df$slope_mid[allsites_df$slope_mid <= 0 ] <- .001
allsites_df$slope_up[allsites_df$slope_up <= 0 ] <- .001
#calc velocity
#Q=(w+w)/2*d*v
#v=Q/w/d
allsites_df$v_ms <- allsites_df$Q_m3s / (allsites_df$d/100) / (allsites_df$w/100)

#adjust henry to temp: KH = KH(STP) x exp(D(1/T-1/T(STP)))
#use constants in  	Burkholder et al. (2019) and convert to desired units
#  k°H (mol/(kg*bar) = mol/l/atm
#d(ln(kH))/d(1/T) (K)
kH_STP_mol.l.atm = .035*1/0.986923
D_K = 2400 
T_STP_K = 298.15

allsites_df$pCO2_air_ppm <- 418.53 # 2022 average manoa
#########

allsites_df$air_pressure_atm <- allsites_df$Total_hPa * 0.000987 - 0.000967841
allsites_df$water_pressure_atm <- allsites_df$Total_hPa * 0.000987

########

allsites_df$pCO2_air_atm <-  allsites_df$pCO2_air_ppm / 10^6  * allsites_df$air_pressure_atm
allsites_df$pCO2_w_atm <- allsites_df$adjusted_ppm / 10^6 * allsites_df$water_pressure_atm 

#henry's constant adjust for temp
allsites_df$KH_mol.l.atm <- kH_STP_mol.l.atm * exp(D_K*(1/(allsites_df$WaterTemp_c+273.15) - 1/T_STP_K))
allsites_df$KH_mol.m3.atm <- allsites_df$KH_mol.l.atm * 1000

allsites_df$CO2_sat_mol.L <- allsites_df$KH_mol.l.atm*allsites_df$pCO2_air_atm
allsites_df$CO2_mol.L <- allsites_df$KH_mol.l.atm*allsites_df$pCO2_w_atm
allsites_df$CO2_umol.L <- allsites_df$CO2_mol.L*10^6

allsites_df$CO2_sat_percent <- allsites_df$CO2_mol.L/allsites_df$CO2_sat_mol.L*100

#change units of flux (umol/m2/s - > mol/m2/d)
#86400 seconds in a day
allsites_df$Flux_mol_m2_d_chamber <- allsites_df$Flux_ave / 10^6 * 86400

#now calculate k in m/d
allsites_df$k_m.d_chamber <- allsites_df$Flux_mol_m2_d / (allsites_df$KH_mol.m3.atm * (allsites_df$pCO2_w_atm -  allsites_df$pCO2_air_atm )) 

allsites_df$Sc_co2 <- 1923.6 - 125.06*allsites_df$WaterTemp_c + 4.3773*(allsites_df$WaterTemp_c)^2 - 0.085681*(allsites_df$WaterTemp_c)^3 + 0.00070284 * (allsites_df$WaterTemp_c)^4

allsites_df$k600_chamber <- allsites_df$k_m.d * (600/allsites_df$Sc_co2)^(-.57)


#calc k600 using raymond (all raymond?)
#k600 = (V*S)^.89 * D^0.54 * 5037
#k600 = 5937 * (1 - 2.54 * Fr^2) * (VS)^0.89 * D^0.58
#k600 = 4725 – 445 * (VS)^0.86 · Q -0.14 – 0.012 · D 0.66 – 0.029
allsites_df$k600_eq1 <- (allsites_df$v_ms * allsites_df$slope_mid)^.89 * (allsites_df$d/100)^0.54 * 5037
allsites_df$k600_eq1_dummySlope <- (allsites_df$v_ms * .01)^.89 * (allsites_df$d/100)^0.54 * 5037

#correct with ulseth
#eD = gSV ; where g is acceleration due to gravity = 9.81 m/s/s ; S is slope ; V is velocity (m/s)) (units m2 s-3) - Raymond et al. 2012
#eD break point is 0.020 m2 s–3
allsites_df$eD <- 9.81*allsites_df$slope_mid*allsites_df$v_ms
allsites_df$k600_eq1_ulsethcorrection <- exp(5.137 + log(.468*allsites_df$v_ms * allsites_df$slope_mid) *.242*log(allsites_df$d))

allsites_df$k600_eq1_final <- NA

for(i in 1:nrow(allsites_df)) {
  if (allsites_df$eD < .020 ){
    allsites_df$k600_eq1_final[i] <- allsites_df$k600_eq1[i]
  }else{
    allsites_df$k600_eq1_final[i] <- allsites_df$k600_eq1_ulsethcorrection[i]
  }
}
  
#convert to k [m/d]
allsites_df$k.m.d_eq1 <- allsites_df$k600_eq1_final / ((600/allsites_df$Sc_co2)^(-.05))
#calc flux [umol/m2/d]
allsites_df$F_mol_m2_d_eq1 <- allsites_df$k.m.d * allsites_df$KH_mol.m3.atm * (allsites_df$pCO2_w_atm -  allsites_df$pCO2_air_atm )
#calc flux [umol/d]
allsites_df$F_CO2_molpers_eq1 <- allsites_df$F_mol_m2_d_eq1 * allsites_df$dist_diff * allsites_df$w/100

#seperate by site so that you can do comulative flux
colm <- allsites_df%>%filter(name=="Colm")%>%drop_na(F_CO2_molpers_eq1)
ante <- allsites_df%>%filter(name=="Ante")%>%drop_na(F_CO2_molpers_eq1)
gaviup <- allsites_df%>%filter(name=="GaviUp")%>%drop_na(F_CO2_molpers_eq1)
gavidown <- allsites_df%>%filter(name=="GaviDown")%>%drop_na(F_CO2_molpers_eq1)
gavitrib1 <- allsites_df%>%filter(name=="GaviTrib1")%>%drop_na(F_CO2_molpers_eq1)
gavitrib2 <- allsites_df%>%filter(name=="GaviTrib2")%>%drop_na(F_CO2_molpers_eq1)

colm <- colm[order(colm$x_pred),]
ante <- ante[order(ante$x_pred),]
gaviup <- gaviup[order(gaviup$x_pred),]
gavidown <- gavidown[order(gavidown$x_pred),]
gavitrib1 <- gavitrib1[order(gavitrib1$x_pred),]
gavitrib2 <- gavitrib2[order(gavitrib2$x_pred),]

colm$flux_sum <- NA
ante$flux_sum <- NA
gaviup$flux_sum <- NA
gavidown$flux_sum <- NA
gavitrib1$flux_sum <- NA
gavitrib2$flux_sum <- NA

for(i in 1:nrow(colm)) {
  if (i==1){
    colm$flux_sum[i] <- colm$F_CO2_molpers_eq1[i]
  }else{
    colm$flux_sum[i] <- colm$flux_sum[i-1] + colm$F_CO2_molpers_eq1[i]
  }
}

for(i in 1:nrow(ante)) {
  if (i==1){
    ante$flux_sum[i] <- ante$F_CO2_molpers_eq1[i]
  }else{
    ante$flux_sum[i] <- ante$flux_sum[i-1] + ante$F_CO2_molpers_eq1[i]
  }
}


for(i in 1:nrow(gaviup)) {
  if (i==1){
    gaviup$flux_sum[i] <- gaviup$F_CO2_molpers_eq1[i]
  }else{
    gaviup$flux_sum[i] <- gaviup$flux_sum[i-1] + gaviup$F_CO2_molpers_eq1[i]
  }
}


for(i in 1:nrow(gavidown)) {
  if (i==1){
    gavidown$flux_sum[i] <- gavidown$F_CO2_molpers_eq1[i]
  }else{
    gavidown$flux_sum[i] <- gavidown$flux_sum[i-1] + gavidown$F_CO2_molpers_eq1[i]
  }
}


for(i in 1:nrow(gavitrib1)) {
  if (i==1){
    gavitrib1$flux_sum[i] <- gavitrib1$F_CO2_molpers_eq1[i]
  }else{
    gavitrib1$flux_sum[i] <- gavitrib1$flux_sum[i-1] + gavitrib1$F_CO2_molpers_eq1[i]
  }
}


for(i in 1:nrow(gavitrib2)) {
  if (i==1){
    gavitrib2$flux_sum[i] <- gavitrib2$F_CO2_molpers_eq1[i]
  }else{
    gavitrib2$flux_sum[i] <- gavitrib2$flux_sum[i-1] + gavitrib2$F_CO2_molpers_eq1[i]
  }
}

#write out
#write.csv(allsites_df, here::here("ProcessedData/raymond_data_allsites_2024-09-13.csv"))

#plot

allsites_df_plot <- allsites_df%>%drop_na(EOS_no)
allsites_df_plot <- allsites_df_plot%>%drop_na(k600_eq1)

ggplot(allsites_df, aes(x=Flux_mol_m2_d,F_mol_m2_d_Raymond,color= slope)) + geom_point() + scale_y_log10() + scale_x_log10()
ggplot(allsites_df, aes(x=w,F_mol_m2_d_Raymond,color= name)) + geom_point()
ggplot(allsites_df, aes(x=slope,F_mol_m2_d_Raymond,color= name)) + geom_point()

ggplot(allsites_df, aes(x=slope,adjusted_ppm,color= name)) + geom_point() +
  scale_y_log10() + scale_x_log10()


#what is the most important factor in flux?
#is it slope? width? velocity?

ggplot(allsites_df%>%filter(name=="GaviDown"), aes(x=adjusted_ppm,F_CO2)) + geom_point()
ggplot(allsites_df%>%filter(name=="GaviDown"), aes(x=adjusted_ppm,F_CO2,color=slope)) + geom_point()

ggplot(allsites_df%>%filter(name=="Colm"), aes(x=adjusted_ppm,F_CO2)) + geom_point()
ggplot(allsites_df%>%filter(name=="Ante"), aes(x=adjusted_ppm,F_CO2)) + geom_point()
ggplot(allsites_df%>%filter(name=="GaviUp"), aes(x=adjusted_ppm,F_CO2)) + geom_point()
ggplot(allsites_df%>%filter(name=="GaviTrib1"), aes(x=adjusted_ppm,F_CO2)) + geom_point()
ggplot(allsites_df%>%filter(name=="GaviTrib2"), aes(x=adjusted_ppm,F_CO2)) + geom_point()


ggplot(allsites_df%>%filter(name!="Colm"), aes(x=slope,F_CO2,color=name)) + 
  geom_point() + scale_y_log10()
ggplot(allsites_df%>%filter(name=="GaviUp")
       , aes(x=slope,F_CO2,color=name)) + 
  geom_point() + scale_y_log10() + scale_x_log10()+ 
  geom_smooth(method='lm', formula= y~x)


ggplot(allsites_df, aes(x=slope,k600_eq1)) +
  geom_point()
ggplot(allsites_df_plot, aes(x=k600_chamber,y=k600_eq1_dummySlope,color=name)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1)


ggplot(allsites_df
       %>%filter(name!="Colm")%>%filter(name!="GaviDown"),
       aes(x=slope,log(adjusted_ppm),color=name)) + geom_point()+ 
  geom_smooth(method='lm', formula= y~x)

ggplot(allsites_df, aes(x=x,slope,color=x)) + geom_point()

#bball plots
ggplot(allsites_df_plot,aes(x=x_pred,y=F_CO2_umolpers,color=name)) + geom_line()

#adjust to ulseth

#calc flux
GaviDown_join_df$k_co2 <- 
GaviDown_join_df$flux_eq1 <- GaviDown_join_df$adjusted_ppm * GaviDown_join_df$k600_eq1

ggplot(allsites_df%>%filter(name=="GaviDown")
       , aes(x=slope,F_CO2_umolpers,color=name)) + 
  geom_point() + scale_y_log10() + scale_x_log10()

ggplot(allsites_df%>%filter(name=="Colm")
       , aes(x=slope,Flux_ave,color=name)) + 
  geom_point() + scale_y_log10() + scale_x_log10()

#################
#plots of worth?#
#################
ggplot(allsites_df %>%filter(name=="Colm")
       , aes(x=slope,F_CO2_umolpers,color=name)) + 
  geom_point() + scale_y_log10() + scale_x_log10()
#take-away: slope is a really strong predictor of flux
ggplot(allsites_df %>%filter(name=="Colm")
       , aes(x=slope,F_CO2_umolpers,color=name)) + 
  geom_point() + scale_y_log10() + scale_x_log10()
