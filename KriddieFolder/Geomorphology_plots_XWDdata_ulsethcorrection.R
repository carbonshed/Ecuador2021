#geomorphology script
#k. whitmore
#9/19/2024

library(here)
library(dplyr)
library(ggplot2)
library(zoo)

round_to_first_decimal <- function(number) {
  return(round(number, 1))
}
#read in gobs of data

#discharge
Q_df <- read.csv(here::here("Discharge/discharge_synop.csv"))

Ante_XWD <- read.csv(here::here("ProcessedData/export_to_r/ante_XWD_export_eledata_CO2.csv"))%>%select(x_pred,x,d,w,ele_arcpro,dist_diff,slope_mid,slope_up,catchment_ha)
Colm_XWD <- read.csv(here::here("ProcessedData/export_to_r/colm_XWD_export_eledata_CO2.csv"))%>%select(x_pred,x,d,w,ele_arcpro,dist_diff,slope_mid,slope_up,catchment_ha)
GaviDown_XWD <- read.csv(here::here("ProcessedData/export_to_r/gavi-down_XWD_export_co2.csv"))%>%select(x_pred,x,d,w,ele_arcpro,dist_diff,slope_mid,slope_up,catchment_ha)%>%mutate(d = as.numeric(d))
GaviUp_XWD <- read.csv(here::here("ProcessedData/export_to_r/gavi-up_XWD_export_eledata_co2.csv"))%>%select(x_pred,x,d,w,ele_arcpro,dist_diff,slope_mid,slope_up,catchment_ha)
GaviTrib1_XWD <- read.csv(here::here("ProcessedData/export_to_r/gavi-trib1_XWD_export_eledata_co2.csv"))%>%select(x_pred,x,d,w,ele_arcpro,dist_diff,slope_mid,slope_up,catchment_ha)
GaviTrib2_XWD <- read.csv(here::here("ProcessedData/export_to_r/gavi-trib2_XWD_export_eledata_CO2.csv"))%>%select(x_pred,x,d,w,ele_arcpro,dist_diff,slope_mid,slope_up,catchment_ha)
GaviTrib3_XWD <- read.csv(here::here("ProcessedData/export_to_r/gavi-trib3_XWD_export_eledata_CO2.csv"))%>%select(x_pred,x,d,w,ele_arcpro,dist_diff,slope_mid,slope_up,catchment_ha)

Ante_XWD$site <- "Ante"
Colm_XWD$site <- "Colm"
GaviDown_XWD$site <- "GaviDown"
GaviUp_XWD$site <- "GaviUp"
GaviTrib1_XWD$site <- "GaviTrib1"
GaviTrib2_XWD$site <- "GaviTrib2"
GaviTrib3_XWD$site <- "GaviTrib3"

#clean
#delete these points because the are in an area where there were lots of little channels
Ante_XWD <- Ante_XWD%>%filter(x!=445)%>%filter(x!=450)

XWD_allsites <- rbind(Ante_XWD,Colm_XWD,GaviDown_XWD,GaviUp_XWD,GaviTrib1_XWD,GaviTrib2_XWD,GaviTrib3_XWD)


#check real quick how many are 0 slope
XWD_allsites_test1 <- XWD_allsites%>%drop_na(slope_mid)%>%drop_na(slope_up)
XWD_allsites_test2 <- XWD_allsites_test1%>%filter(slope_mid == 0)
XWD_allsites_test3 <- XWD_allsites_test1%>%filter(slope_up == 0)
XWD_allsites_test4 <- XWD_allsites_test1%>%filter(slope_up == 0|slope_mid == 0)

Ante_synop <- read.csv(here::here("ProcessedData/export_to_r/ante_XWD_export_eledata_synop.csv"))%>%select(x_pred,x,ele_arcpro,Flux_ave,adjusted_ppm,WaterTemp_c,AirTemp_c,Total_hPa,slope_mid,slope_up,catchment_ha)
Colm_synop <- read.csv(here::here("ProcessedData/export_to_r/colm_XWD_export_eledata_synop.csv"))%>%select(x_pred,x,ele_arcpro,Flux_ave,adjusted_ppm,WaterTemp_c,AirTemp_c,Total_hPa,slope_mid,slope_up,catchment_ha)
GaviDown_synop <- read.csv(here::here("ProcessedData/export_to_r/gavi-down_XWD_export_synop.csv"))%>%select(x_pred,x,ele_arcpro,Flux_ave,adjusted_ppm,WaterTemp_c,AirTemp_c,Total_hPa,slope_mid,slope_up,catchment_ha)
GaviUp_synop <- read.csv(here::here("ProcessedData/export_to_r/gavi-up_XWD_export_eledata_synop.csv"))%>%select(x_pred,x,ele_arcpro,Flux_ave,adjusted_ppm,WaterTemp_c,AirTemp_c,Total_hPa,slope_mid,slope_up,catchment_ha)
GaviTrib1_synop <- read.csv(here::here("ProcessedData/export_to_r/gavi-trib1_XWD_export_eledata_synop.csv"))%>%select(x_pred,x,ele_arcpro,Flux_ave,adjusted_ppm,WaterTemp_c,AirTemp_c,Total_hPa,slope_mid,slope_up,catchment_ha)
GaviTrib2_synop <- read.csv(here::here("ProcessedData/export_to_r/gavi-trib2_XWD_export_eledata_synop.csv"))%>%select(x_pred,x,ele_arcpro,Flux_ave,adjusted_ppm,WaterTemp_c,AirTemp_c,Total_hPa,slope_mid,slope_up,catchment_ha)
GaviTrib3_synop <- read.csv(here::here("ProcessedData/export_to_r/gavi-trib3_XWD_export_eledata_synop.csv"))%>%select(x_pred,x,ele_arcpro,Flux_ave,adjusted_ppm,AirTemp_c,Total_hPa,slope_mid,slope_up,catchment_ha)

Ante_synop$site <- "Ante"
Colm_synop$site <- "Colm"
GaviDown_synop$site <- "GaviDown"
GaviUp_synop$site <- "GaviUp"
GaviTrib1_synop$site <- "GaviTrib1"
GaviTrib2_synop$site <- "GaviTrib2"
GaviTrib3_synop$site <- "GaviTrib3"
GaviTrib3_synop$WaterTemp_c <- NA

#clean
#delete co2 just below the wetland. start after waterfall
GaviDown_synop_nowetland <- GaviDown_synop%>%filter(x > 114)

synop_allsites <- rbind(Ante_synop,Colm_synop,GaviDown_synop,GaviUp_synop,GaviTrib1_synop,GaviTrib2_synop,GaviTrib3_synop)
synop_allsites_nowetland <- rbind(Ante_synop,Colm_synop,GaviDown_synop_nowetland,GaviUp_synop,GaviTrib1_synop,GaviTrib2_synop,GaviTrib3_synop)

synop_allsites_test1 <- synop_allsites%>%drop_na(slope_mid)%>%drop_na(slope_up)
synop_allsites_test2 <- synop_allsites_test1%>%filter(slope_mid == 0)
synop_allsites_test3 <- synop_allsites_test1%>%filter(slope_up == 0)
synop_allsites_test4 <- synop_allsites_test1%>%filter(slope_up == 0|slope_mid == 0)

rm(synop_allsites_test1,synop_allsites_test2,synop_allsites_test3,synop_allsites_test4)
rm(XWD_allsites_test1,XWD_allsites_test2,XWD_allsites_test3,XWD_allsites_test4)
#write out df
#write.csv(synop_allsites,here::here("ProcessedData/synop_allsites_slope.csv"))
#write.csv(synop_allsites_nowetland,here::here("ProcessedData/synop_allsites_nowetland_slope.csv"))
#write.csv(XWD_allsites,here::here("ProcessedData/xwd_allsites_slope.csv"))
#write.csv(Q_df,here::here("ProcessedData/Q_synop.csv"))

#predict Q based on catchment size

model_Q <- lm(log(Q_m3s) ~ log(catchment_ha), data = Q_df)
summ_model_Q <- summary(model_Q)
synop_allsites$Q_m3s <- exp(summ_model_Q$coefficients[1]+
                         summ_model_Q$coefficients[2]*log(synop_allsites$catchment_ha))
XWD_allsites$Q_m3s <- exp(summ_model_Q$coefficients[1]+
                              summ_model_Q$coefficients[2]*log(XWD_allsites$catchment_ha))

#bind
synop_allsites$d <- NA
synop_allsites$w <- NA
synop_allsites$dist_diff <- NA
synop_allsites <- synop_allsites%>%
  select(site,x_pred,x,d,w,ele_arcpro,slope_mid,slope_up,dist_diff,catchment_ha,Q_m3s,Flux_ave,adjusted_ppm,WaterTemp_c,Total_hPa,AirTemp_c)

XWD_allsites$Flux_ave <- NA
XWD_allsites$adjusted_ppm <- NA
XWD_allsites$WaterTemp_c <- NA
XWD_allsites$AirTemp_c <- NA
XWD_allsites$Total_hPa <- NA

XWD_allsites <- XWD_allsites%>%
  select(site,x_pred,x,d,w,ele_arcpro,slope_mid,slope_up,dist_diff,catchment_ha,Q_m3s,Flux_ave,adjusted_ppm,WaterTemp_c,Total_hPa,AirTemp_c)

all_df <- rbind(synop_allsites,XWD_allsites)

#seperate by site and interpolate
#Colm
Colm <- all_df%>%filter(site=="Colm")
Colm$x_pred <- round_to_first_decimal(Colm$x_pred)
Colm_join_df <- data.frame(x_pred=seq(from = min(Colm$x_pred), to = max(Colm$x_pred), by = .1))
Colm_join_df <- full_join(Colm_join_df,Colm,by="x_pred")
Colm_join_df <- Colm_join_df[order(Colm_join_df$x_pred),]

adjusted_ppm_vector <- Colm_join_df$adjusted_ppm
adjusted_ppm_vector <- adjusted_ppm_vector[!is.na(adjusted_ppm_vector)]
Flux_ave_vector <- Colm_join_df$Flux_ave
Flux_ave_vector <- Flux_ave_vector[!is.na(Flux_ave_vector)]
WaterTemp_c_vector <- Colm_join_df$WaterTemp_c
WaterTemp_c_vector <- WaterTemp_c_vector[!is.na(WaterTemp_c_vector)]
Total_hPa_vector <- Colm_join_df$Total_hPa
Total_hPa_vector <- Total_hPa_vector[!is.na(Total_hPa_vector)]
AirTemp_c_vector <- Colm_join_df$AirTemp_c
AirTemp_c_vector <- AirTemp_c_vector[!is.na(AirTemp_c_vector)]

#edit first and last row in dataframe
Colm_join_df[1,]$adjusted_ppm <- adjusted_ppm_vector[1]
Colm_join_df[1,]$Flux_ave <- Flux_ave_vector[1]
Colm_join_df[1,]$WaterTemp_c <- WaterTemp_c_vector[1]
Colm_join_df[1,]$Total_hPa <- Total_hPa_vector[1]
Colm_join_df[1,]$AirTemp_c <- AirTemp_c_vector[1]

Colm_join_df[nrow(Colm_join_df),]$adjusted_ppm <- tail(adjusted_ppm_vector, n=1)
Colm_join_df[nrow(Colm_join_df),]$Flux_ave <- tail(Flux_ave_vector, n=1)
Colm_join_df[nrow(Colm_join_df),]$WaterTemp_c <- tail(WaterTemp_c_vector, n=1)
Colm_join_df[nrow(Colm_join_df),]$Total_hPa <- tail(Total_hPa_vector, n=1)
Colm_join_df[nrow(Colm_join_df),]$AirTemp_c <- tail(AirTemp_c_vector, n=1)


Colm_join_df <- Colm_join_df %>%
  mutate(adjusted_ppm = na.approx(adjusted_ppm))%>%
  mutate(Flux_ave = na.approx(Flux_ave))%>%
  mutate(WaterTemp_c = na.approx(WaterTemp_c))%>%
  mutate(Total_hPa = na.approx(Total_hPa))%>%
  mutate(AirTemp_c = na.approx(AirTemp_c))



#Ante
Ante <- all_df%>%filter(site=="Ante")
Ante$x_pred <- round_to_first_decimal(Ante$x_pred)
Ante_join_df <- data.frame(x_pred=seq(from = min(Ante$x_pred), to = max(Ante$x_pred), by = .1))
Ante_join_df <- full_join(Ante_join_df,Ante,by="x_pred")
Ante_join_df <- Ante_join_df[order(Ante_join_df$x_pred),]

adjusted_ppm_vector <- Ante_join_df$adjusted_ppm
adjusted_ppm_vector <- adjusted_ppm_vector[!is.na(adjusted_ppm_vector)]
Flux_ave_vector <- Ante_join_df$Flux_ave
Flux_ave_vector <- Flux_ave_vector[!is.na(Flux_ave_vector)]
WaterTemp_c_vector <- Ante_join_df$WaterTemp_c
WaterTemp_c_vector <- WaterTemp_c_vector[!is.na(WaterTemp_c_vector)]
Total_hPa_vector <- Ante_join_df$Total_hPa
Total_hPa_vector <- Total_hPa_vector[!is.na(Total_hPa_vector)]
AirTemp_c_vector <- Ante_join_df$AirTemp_c
AirTemp_c_vector <- AirTemp_c_vector[!is.na(AirTemp_c_vector)]

#edit first and last row in dataframe
Ante_join_df[1,]$adjusted_ppm <- adjusted_ppm_vector[1]
Ante_join_df[1,]$Flux_ave <- Flux_ave_vector[1]
Ante_join_df[1,]$WaterTemp_c <- WaterTemp_c_vector[1]
Ante_join_df[1,]$Total_hPa <- Total_hPa_vector[1]
Ante_join_df[1,]$AirTemp_c <- AirTemp_c_vector[1]

Ante_join_df[nrow(Ante_join_df),]$adjusted_ppm <- tail(adjusted_ppm_vector, n=1)
Ante_join_df[nrow(Ante_join_df),]$Flux_ave <- tail(Flux_ave_vector, n=1)
Ante_join_df[nrow(Ante_join_df),]$WaterTemp_c <- tail(WaterTemp_c_vector, n=1)
Ante_join_df[nrow(Ante_join_df),]$Total_hPa <- tail(Total_hPa_vector, n=1)
Ante_join_df[nrow(Ante_join_df),]$AirTemp_c <- tail(AirTemp_c_vector, n=1)


Ante_join_df <- Ante_join_df %>%
  mutate(adjusted_ppm = na.approx(adjusted_ppm))%>%
  mutate(Flux_ave = na.approx(Flux_ave))%>%
  mutate(WaterTemp_c = na.approx(WaterTemp_c))%>%
  mutate(Total_hPa = na.approx(Total_hPa))%>%
  mutate(AirTemp_c = na.approx(AirTemp_c))



#GaviDown
GaviDown <- all_df%>%filter(site=="GaviDown")
GaviDown$x_pred <- round_to_first_decimal(GaviDown$x_pred)
GaviDown_join_df <- data.frame(x_pred=seq(from = min(GaviDown$x_pred), to = max(GaviDown$x_pred), by = .1))
GaviDown_join_df <- full_join(GaviDown_join_df,GaviDown,by="x_pred")
GaviDown_join_df <- GaviDown_join_df[order(GaviDown_join_df$x_pred),]

adjusted_ppm_vector <- GaviDown_join_df$adjusted_ppm
adjusted_ppm_vector <- adjusted_ppm_vector[!is.na(adjusted_ppm_vector)]
Flux_ave_vector <- GaviDown_join_df$Flux_ave
Flux_ave_vector <- Flux_ave_vector[!is.na(Flux_ave_vector)]
WaterTemp_c_vector <- GaviDown_join_df$WaterTemp_c
WaterTemp_c_vector <- WaterTemp_c_vector[!is.na(WaterTemp_c_vector)]
Total_hPa_vector <- GaviDown_join_df$Total_hPa
Total_hPa_vector <- Total_hPa_vector[!is.na(Total_hPa_vector)]
AirTemp_c_vector <- GaviDown_join_df$AirTemp_c
AirTemp_c_vector <- AirTemp_c_vector[!is.na(AirTemp_c_vector)]

#edit first and last row in dataframe
GaviDown_join_df[1,]$adjusted_ppm <- adjusted_ppm_vector[1]
GaviDown_join_df[1,]$Flux_ave <- Flux_ave_vector[1]
GaviDown_join_df[1,]$WaterTemp_c <- WaterTemp_c_vector[1]
GaviDown_join_df[1,]$Total_hPa <- Total_hPa_vector[1]
GaviDown_join_df[1,]$AirTemp_c <- AirTemp_c_vector[1]

GaviDown_join_df[nrow(GaviDown_join_df),]$adjusted_ppm <- tail(adjusted_ppm_vector, n=1)
GaviDown_join_df[nrow(GaviDown_join_df),]$Flux_ave <- tail(Flux_ave_vector, n=1)
GaviDown_join_df[nrow(GaviDown_join_df),]$WaterTemp_c <- tail(WaterTemp_c_vector, n=1)
GaviDown_join_df[nrow(GaviDown_join_df),]$Total_hPa <- tail(Total_hPa_vector, n=1)
GaviDown_join_df[nrow(GaviDown_join_df),]$AirTemp_c <- tail(AirTemp_c_vector, n=1)


GaviDown_join_df <- GaviDown_join_df %>%
  mutate(adjusted_ppm = na.approx(adjusted_ppm))%>%
  mutate(Flux_ave = na.approx(Flux_ave))%>%
  mutate(WaterTemp_c = na.approx(WaterTemp_c))%>%
  mutate(Total_hPa = na.approx(Total_hPa))%>%
  mutate(AirTemp_c = na.approx(AirTemp_c))



#GaviUp
GaviUp <- all_df%>%filter(site=="GaviUp")
GaviUp$x_pred <- round_to_first_decimal(GaviUp$x_pred)
GaviUp_join_df <- data.frame(x_pred=seq(from = min(GaviUp$x_pred,na.rm =TRUE), to = max(GaviUp$x_pred,na.rm =TRUE), by = .1))
GaviUp_join_df <- full_join(GaviUp_join_df,GaviUp,by="x_pred")
GaviUp_join_df <- GaviUp_join_df[order(GaviUp_join_df$x_pred),]

adjusted_ppm_vector <- GaviUp_join_df$adjusted_ppm
adjusted_ppm_vector <- adjusted_ppm_vector[!is.na(adjusted_ppm_vector)]
Flux_ave_vector <- GaviUp_join_df$Flux_ave
Flux_ave_vector <- Flux_ave_vector[!is.na(Flux_ave_vector)]
WaterTemp_c_vector <- GaviUp_join_df$WaterTemp_c
WaterTemp_c_vector <- WaterTemp_c_vector[!is.na(WaterTemp_c_vector)]
Total_hPa_vector <- GaviUp_join_df$Total_hPa
Total_hPa_vector <- Total_hPa_vector[!is.na(Total_hPa_vector)]
AirTemp_c_vector <- GaviUp_join_df$AirTemp_c
AirTemp_c_vector <- AirTemp_c_vector[!is.na(AirTemp_c_vector)]

#edit first and last row in dataframe
GaviUp_join_df[1,]$adjusted_ppm <- adjusted_ppm_vector[1]
GaviUp_join_df[1,]$Flux_ave <- Flux_ave_vector[1]
GaviUp_join_df[1,]$WaterTemp_c <- WaterTemp_c_vector[1]
GaviUp_join_df[1,]$Total_hPa <- Total_hPa_vector[1]
GaviUp_join_df[1,]$AirTemp_c <- AirTemp_c_vector[1]

GaviUp_join_df[nrow(GaviUp_join_df),]$adjusted_ppm <- tail(adjusted_ppm_vector, n=1)
GaviUp_join_df[nrow(GaviUp_join_df),]$Flux_ave <- tail(Flux_ave_vector, n=1)
GaviUp_join_df[nrow(GaviUp_join_df),]$WaterTemp_c <- tail(WaterTemp_c_vector, n=1)
GaviUp_join_df[nrow(GaviUp_join_df),]$Total_hPa <- tail(Total_hPa_vector, n=1)
GaviUp_join_df[nrow(GaviUp_join_df),]$AirTemp_c <- tail(AirTemp_c_vector, n=1)


GaviUp_join_df <- GaviUp_join_df %>%
  mutate(adjusted_ppm = na.approx(adjusted_ppm))%>%
  mutate(Flux_ave = na.approx(Flux_ave))%>%
  mutate(WaterTemp_c = na.approx(WaterTemp_c))%>%
  mutate(Total_hPa = na.approx(Total_hPa))%>%
  mutate(AirTemp_c = na.approx(AirTemp_c))



#GaviTrib1
GaviTrib1 <- all_df%>%filter(site=="GaviTrib1")
GaviTrib1$x_pred <- round_to_first_decimal(GaviTrib1$x_pred)
GaviTrib1_join_df <- data.frame(x_pred=seq(from = min(GaviTrib1$x_pred,na.rm =TRUE), to = max(GaviTrib1$x_pred,na.rm =TRUE), by = .1))
GaviTrib1_join_df <- full_join(GaviTrib1_join_df,GaviTrib1,by="x_pred")
GaviTrib1_join_df <- GaviTrib1_join_df[order(GaviTrib1_join_df$x_pred),]

adjusted_ppm_vector <- GaviTrib1_join_df$adjusted_ppm
adjusted_ppm_vector <- adjusted_ppm_vector[!is.na(adjusted_ppm_vector)]
Flux_ave_vector <- GaviTrib1_join_df$Flux_ave
Flux_ave_vector <- Flux_ave_vector[!is.na(Flux_ave_vector)]
WaterTemp_c_vector <- GaviTrib1_join_df$WaterTemp_c
WaterTemp_c_vector <- WaterTemp_c_vector[!is.na(WaterTemp_c_vector)]
Total_hPa_vector <- GaviTrib1_join_df$Total_hPa
Total_hPa_vector <- Total_hPa_vector[!is.na(Total_hPa_vector)]
AirTemp_c_vector <- GaviTrib1_join_df$AirTemp_c
AirTemp_c_vector <- AirTemp_c_vector[!is.na(AirTemp_c_vector)]

#edit first and last row in dataframe
GaviTrib1_join_df[1,]$adjusted_ppm <- adjusted_ppm_vector[1]
GaviTrib1_join_df[1,]$Flux_ave <- Flux_ave_vector[1]
GaviTrib1_join_df[1,]$WaterTemp_c <- WaterTemp_c_vector[1]
GaviTrib1_join_df[1,]$Total_hPa <- Total_hPa_vector[1]
GaviTrib1_join_df[1,]$AirTemp_c <- AirTemp_c_vector[1]

GaviTrib1_join_df[nrow(GaviTrib1_join_df),]$adjusted_ppm <- tail(adjusted_ppm_vector, n=1)
GaviTrib1_join_df[nrow(GaviTrib1_join_df),]$Flux_ave <- tail(Flux_ave_vector, n=1)
GaviTrib1_join_df[nrow(GaviTrib1_join_df),]$WaterTemp_c <- tail(WaterTemp_c_vector, n=1)
GaviTrib1_join_df[nrow(GaviTrib1_join_df),]$Total_hPa <- tail(Total_hPa_vector, n=1)
GaviTrib1_join_df[nrow(GaviTrib1_join_df),]$AirTemp_c <- tail(AirTemp_c_vector, n=1)


GaviTrib1_join_df <- GaviTrib1_join_df %>%
  mutate(adjusted_ppm = na.approx(adjusted_ppm))%>%
  mutate(Flux_ave = na.approx(Flux_ave))%>%
  mutate(WaterTemp_c = na.approx(WaterTemp_c))%>%
  mutate(Total_hPa = na.approx(Total_hPa))%>%
  mutate(AirTemp_c = na.approx(AirTemp_c))


###########
#GaviTrib2#
###########
GaviTrib2 <- all_df%>%filter(site=="GaviTrib2")
GaviTrib2$x_pred <- round_to_first_decimal(GaviTrib2$x_pred)
GaviTrib2_join_df <- data.frame(x_pred=seq(from = min(GaviTrib2$x_pred,na.rm =TRUE), to = max(GaviTrib1$x_pred,na.rm =TRUE), by = .1))
GaviTrib2_join_df <- full_join(GaviTrib2_join_df,GaviTrib2,by="x_pred")
GaviTrib2_join_df <- GaviTrib2_join_df[order(GaviTrib2_join_df$x_pred),]

adjusted_ppm_vector <- GaviTrib2_join_df$adjusted_ppm
adjusted_ppm_vector <- adjusted_ppm_vector[!is.na(adjusted_ppm_vector)]
Flux_ave_vector <- GaviTrib2_join_df$Flux_ave
Flux_ave_vector <- Flux_ave_vector[!is.na(Flux_ave_vector)]
WaterTemp_c_vector <- GaviTrib2_join_df$WaterTemp_c
WaterTemp_c_vector <- WaterTemp_c_vector[!is.na(WaterTemp_c_vector)]
Total_hPa_vector <- GaviTrib2_join_df$Total_hPa
Total_hPa_vector <- Total_hPa_vector[!is.na(Total_hPa_vector)]
AirTemp_c_vector <- GaviTrib2_join_df$AirTemp_c
AirTemp_c_vector <- AirTemp_c_vector[!is.na(AirTemp_c_vector)]

#edit first and last row in dataframe
GaviTrib2_join_df[1,]$adjusted_ppm <- adjusted_ppm_vector[1]
GaviTrib2_join_df[1,]$Flux_ave <- Flux_ave_vector[1]
GaviTrib2_join_df[1,]$WaterTemp_c <- WaterTemp_c_vector[1]
GaviTrib2_join_df[1,]$Total_hPa <- Total_hPa_vector[1]
GaviTrib2_join_df[1,]$AirTemp_c <- AirTemp_c_vector[1]

GaviTrib2_join_df[nrow(GaviTrib2_join_df),]$adjusted_ppm <- tail(adjusted_ppm_vector, n=1)
GaviTrib2_join_df[nrow(GaviTrib2_join_df),]$Flux_ave <- tail(Flux_ave_vector, n=1)
GaviTrib2_join_df[nrow(GaviTrib2_join_df),]$WaterTemp_c <- tail(WaterTemp_c_vector, n=1)
GaviTrib2_join_df[nrow(GaviTrib2_join_df),]$Total_hPa <- tail(Total_hPa_vector, n=1)
GaviTrib2_join_df[nrow(GaviTrib2_join_df),]$AirTemp_c <- tail(AirTemp_c_vector, n=1)


GaviTrib2_join_df <- GaviTrib2_join_df %>%
  mutate(adjusted_ppm = na.approx(adjusted_ppm))%>%
  mutate(Flux_ave = na.approx(Flux_ave))%>%
  mutate(WaterTemp_c = na.approx(WaterTemp_c))%>%
  mutate(Total_hPa = na.approx(Total_hPa))%>%
  mutate(AirTemp_c = na.approx(AirTemp_c))

###########
#GaviTrib3#
###########
GaviTrib3 <- all_df%>%filter(site=="GaviTrib3")
GaviTrib3$x_pred <- round_to_first_decimal(GaviTrib3$x_pred)
GaviTrib3_join_df <- data.frame(x_pred=seq(from = min(GaviTrib3$x_pred,na.rm =TRUE), to = max(GaviTrib1$x_pred,na.rm =TRUE), by = .1))
GaviTrib3_join_df <- full_join(GaviTrib3_join_df,GaviTrib3,by="x_pred")
GaviTrib3_join_df <- GaviTrib3_join_df[order(GaviTrib3_join_df$x_pred),]

adjusted_ppm_vector <- GaviTrib3_join_df$adjusted_ppm
adjusted_ppm_vector <- adjusted_ppm_vector[!is.na(adjusted_ppm_vector)]
Flux_ave_vector <- GaviTrib3_join_df$Flux_ave
Flux_ave_vector <- Flux_ave_vector[!is.na(Flux_ave_vector)]
WaterTemp_c_vector <- GaviTrib3_join_df$WaterTemp_c
WaterTemp_c_vector <- WaterTemp_c_vector[!is.na(WaterTemp_c_vector)]
Total_hPa_vector <- GaviTrib3_join_df$Total_hPa
Total_hPa_vector <- Total_hPa_vector[!is.na(Total_hPa_vector)]
AirTemp_c_vector <- GaviTrib3_join_df$AirTemp_c
AirTemp_c_vector <- AirTemp_c_vector[!is.na(AirTemp_c_vector)]

#edit first and last row in dataframe
GaviTrib3_join_df[1,]$adjusted_ppm <- adjusted_ppm_vector[1]
GaviTrib3_join_df[1,]$Flux_ave <- Flux_ave_vector[1]
GaviTrib3_join_df[1,]$WaterTemp_c <- WaterTemp_c_vector[1]
GaviTrib3_join_df[1,]$Total_hPa <- Total_hPa_vector[1]
GaviTrib3_join_df[1,]$AirTemp_c <- AirTemp_c_vector[1]

GaviTrib3_join_df[nrow(GaviTrib3_join_df),]$adjusted_ppm <- tail(adjusted_ppm_vector, n=1)
GaviTrib3_join_df[nrow(GaviTrib3_join_df),]$Flux_ave <- tail(Flux_ave_vector, n=1)
#GaviTrib3_join_df[nrow(GaviTrib3_join_df),]$WaterTemp_c <- tail(WaterTemp_c_vector, n=1)
GaviTrib3_join_df[nrow(GaviTrib3_join_df),]$Total_hPa <- tail(Total_hPa_vector, n=1)
GaviTrib3_join_df[nrow(GaviTrib3_join_df),]$AirTemp_c <- tail(AirTemp_c_vector, n=1)


GaviTrib3_join_df <- GaviTrib3_join_df %>%
  mutate(adjusted_ppm = na.approx(adjusted_ppm))%>%
  mutate(Flux_ave = na.approx(Flux_ave))%>%
#  mutate(WaterTemp_c = na.approx(WaterTemp_c))%>%
  mutate(Total_hPa = na.approx(Total_hPa))%>%
  mutate(AirTemp_c = na.approx(AirTemp_c))

all_data_bind <- rbind(Ante_join_df,Colm_join_df,GaviDown_join_df,GaviUp_join_df,GaviTrib1_join_df,GaviTrib2_join_df,GaviTrib3_join_df)
all_data_bind <- all_data_bind%>%rename(slope_up20=slope_up)%>%rename(depth=d)%>%rename(width=w)
#

### now calc k600

#check real quick how many are 0 slope
all_data_bind_test1 <- all_data_bind%>%drop_na(slope_mid)%>%drop_na(slope_up20)
all_data_bind_test2 <- all_data_bind_test1%>%filter(slope_mid == 0)
all_data_bind_test3 <- all_data_bind_test1%>%filter(slope_up20 == 0)
all_data_bind_test4 <- all_data_bind_test1%>%filter(slope_up20 == 0|slope_mid == 0)


#set 0 or negative slopes to low value
all_data_bind$slope_mid[all_data_bind$slope_mid <= 0 ] <- .01/20/2
all_data_bind$slope_up20[all_data_bind$slope_up20 <= 0 ] <- .01/20/2

test <- all_data_bind%>%filter(slope_mid > 0)%>%filter(slope_up20 > 0)
colm <- test%>%filter(site=="Colm")
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
all_data_bind$pCO2_w_atm <- all_data_bind$adjusted_ppm / 10^6 * all_data_bind$water_pressure_atm 

#henry's constant adjust for temp

all_data_bind$KH_mol.l.atm <- kH_STP_mol.l.atm * exp(D_K*(1/(all_data_bind$WaterTemp_c+273.15) - 1/T_STP_K))
all_data_bind$KH_mol.m3.atm <- all_data_bind$KH_mol.l.atm * 1000

all_data_bind$Sc_co2 <- 1923.6 - 125.06*all_data_bind$WaterTemp_c + 4.3773*(all_data_bind$WaterTemp_c)^2 - 0.085681*(all_data_bind$WaterTemp_c)^3 + 0.00070284 * (all_data_bind$WaterTemp_c)^4
#convert to k [m/d]
all_data_bind$k.m.d_eq1 <- all_data_bind$k600_eq1_final / ((600/all_data_bind$Sc_co2)^(-.05))
#calc flux [umol/m2/d]
all_data_bind$F_mol_m2_d_eq1 <- all_data_bind$k.m.d * all_data_bind$KH_mol.m3.atm * (all_data_bind$pCO2_w_atm -  all_data_bind$pCO2_air_atm )
#calc flux [umol/d]
all_data_bind$F_CO2_molperd_eq1 <- all_data_bind$F_mol_m2_d_eq1 * 3 * all_data_bind$width/100

#read out this dataframe
#write.csv(all_data_bind,here::here("ProcessedData/AllSynoptic_raymondk600_Oct8.csv"))


################## 
##cumulative flux
#seperate by site so that you can do 
allsites_df <- all_data_bind%>%rename(name=site)
allsites_df$dist <- NA
colm <- allsites_df%>%filter(name=="Colm")%>%drop_na(F_CO2_molperd_eq1)
ante <- allsites_df%>%filter(name=="Ante")%>%drop_na(F_CO2_molperd_eq1)
gaviup <- allsites_df%>%filter(name=="GaviUp")%>%drop_na(F_CO2_molperd_eq1)
gavidown <- allsites_df%>%filter(name=="GaviDown")%>%drop_na(F_CO2_molperd_eq1)
gavitrib1 <- allsites_df%>%filter(name=="GaviTrib1")%>%drop_na(F_CO2_molperd_eq1)
gavitrib2 <- allsites_df%>%filter(name=="GaviTrib2")%>%drop_na(F_CO2_molperd_eq1)


colm <- colm[order(colm$x),]
ante <- ante[order(ante$x),]
gaviup <- gaviup[order(gaviup$x),]
gavidown <- gavidown[order(gavidown$x),]
gavitrib1 <- gavitrib1[order(gavitrib1$x),]
gavitrib2 <- gavitrib2[order(gavitrib2$x),]

colm$flux_sum <- NA
ante$flux_sum <- NA
gaviup$flux_sum <- NA
gavidown$flux_sum <- NA
gavitrib1$flux_sum <- NA
gavitrib2$flux_sum <- NA

for(i in 1:nrow(colm)) {
  if (i==1){
    colm$flux_sum[i] <- colm$F_CO2_molperd_eq1[i]
  }else{
    colm$flux_sum[i] <- colm$flux_sum[i-1] + colm$F_CO2_molperd_eq1[i]
  }
}


for(i in 1:nrow(ante)) {
  if (i==1){
    ante$flux_sum[i] <- ante$F_CO2_molperd_eq1[i]
  }else{
    ante$flux_sum[i] <- ante$flux_sum[i-1] + ante$F_CO2_molperd_eq1[i]
  }
}


for(i in 1:nrow(gaviup)) {
  if (i==1){
    gaviup$flux_sum[i] <- gaviup$F_CO2_molperd_eq1[i]
  }else{
    gaviup$flux_sum[i] <- gaviup$flux_sum[i-1] + gaviup$F_CO2_molperd_eq1[i]
  }
}


for(i in 1:nrow(gavidown)) {
  if (i==1){
    gavidown$flux_sum[i] <- gavidown$F_CO2_molperd_eq1[i]
  }else{
    gavidown$flux_sum[i] <- gavidown$flux_sum[i-1] + gavidown$F_CO2_molperd_eq1[i]
  }
}


for(i in 1:nrow(gavitrib1)) {
  if (i==1){
    gavitrib1$flux_sum[i] <- gavitrib1$F_CO2_molperd_eq1[i]
  }else{
    gavitrib1$flux_sum[i] <- gavitrib1$flux_sum[i-1] + gavitrib1$F_CO2_molperd_eq1[i]
  }
}


for(i in 1:nrow(gavitrib2)) {
  if (i==1){
    gavitrib2$flux_sum[i] <- gavitrib2$F_CO2_molperd_eq1[i]
  }else{
    gavitrib2$flux_sum[i] <- gavitrib2$flux_sum[i-1] + gavitrib2$F_CO2_molperd_eq1[i]
  }
}


allsites_df <- rbind(colm,ante,gaviup,gavidown,gavitrib1,gavitrib2)

#read out cumulative flux dataframe
#write.csv(allsites_df,here::here("ProcessedData/CumulativeFlux_raymondk600_Oct8.csv"))



#first plot w v catchment
p_Q <- ggplot(Q_df,aes(x=catchment_ha,y=Q_m3s*1000)) + geom_point(size=3) + scale_y_log10() + scale_x_log10()+ 
  geom_smooth(method='lm', formula= y~x) +
  ylab("Q (L/s)") + xlab("Catchment size (ha)") +
  annotate("text", x=10, y=20, label= "r2 = 0.70  \npvalue = <.001",size=6,color="red") + 
  theme_bw(base_size = 18)

p_width <- ggplot(XWD_allsites,aes(x=catchment_ha,y=w)) + geom_point(size=3) + scale_y_log10() + scale_x_log10()+ 
  geom_smooth(method='lm', formula= y~x) + 
  ylab("Width (cm)") + xlab("Catchment size (ha)") +
  annotate("text", x=10, y=700, label= "r2 = 0.3395  \npvalue = <.001",size=6,color="red") + 
  theme_bw(base_size = 18)

p_depth <- ggplot(XWD_allsites,aes(x=catchment_ha,y=d,fill=log1p(slope_mid))) + geom_point(size=3,shape=21) + scale_y_log10() + scale_x_log10()+ 
  geom_smooth(method='lm', formula= y~x)+ 
  ylab("Depth (cm)") + xlab("Catchment size (ha)") + 
  annotate("text", x=10, y=100, label= "r2 = 0.5016 \npvalue = <.001",size=6,color="red") + 
  scale_fill_gradient(low = "yellow", high = "darkgreen", na.value = NA) +
  theme_bw(base_size = 18)


p_co2 <- ggplot(synop_allsites%>%drop_na(slope_up),aes(x=catchment_ha,y=adjusted_ppm,fill=slope_up)) + 
  geom_point(size=3,shape=21) + scale_y_log10() + scale_x_log10()+ 
  geom_smooth(method='lm', formula= y~x)+
  scale_fill_gradient(low = "yellow", high = "darkgreen", na.value = NA) +
  annotate("text", x=10, y=10000, label= "r2 = 0.2686  \npvalue = <.001",size=6,color="red") + 
  ylab(expression(italic(p)~CO[2] ~'(ppm)')) + xlab("") + xlab("Catchment size (ha)") +
  theme_bw(base_size = 16)

p_co2_nowetland <- ggplot(synop_allsites_nowetland%>%drop_na(slope_up),aes(x=catchment_ha,y=adjusted_ppm,fill=slope_up)) + 
  geom_point(size=3,shape=21) + scale_y_log10() + scale_x_log10()+ 
  geom_smooth(method='lm', formula= y~x)+
  scale_fill_gradient(low = "yellow", high = "darkgreen", na.value = NA) +
  annotate("text", x=10, y=10000, label= "r2 = 0.4178  \npvalue = <.001",size=6,color="red") + 
  ylab(expression(italic(p)~CO[2] ~'(ppm)')) + xlab("") + xlab("Catchment size (ha)") +
  theme_bw(base_size = 16)



p_co2_ntest <- ggplot(synop_test%>%drop_na(slope_up),aes(x=catchment_ha,y=adjusted_ppm,fill=slope_up)) + 
  geom_point(size=3,shape=21) + scale_y_log10() + scale_x_log10()+ 
  geom_smooth(method='lm', formula= y~x)+
  scale_fill_gradient(low = "yellow", high = "darkgreen", na.value = NA) +
  annotate("text", x=10, y=10000, label= "r2 = 0.4178  \npvalue = <.001",size=6,color="red") + 
  ylab(expression(italic(p)~CO[2] ~'(ppm)')) + xlab("") + xlab("Catchment size (ha)") +
  theme_bw(base_size = 16)


#anova
model_co2 <- lm(log(adjusted_ppm) ~ log(catchment_ha) + log1p(slope_up), data = synop_allsites%>%drop_na(slope_up))

model_co2_nowetland <- lm(log(adjusted_ppm) ~ log(catchment_ha) + log1p(slope_up), data = synop_allsites_nowetland%>%drop_na(slope_up))

#model_co2_test <- lm(log(adjusted_ppm) ~ log(catchment_ha) + log1p(slope_up), data = synop_test%>%drop_na(slope_up))

model_w <- lm(log(w) ~ log(catchment_ha)  , data = XWD_allsites)
model_d <- lm(log(d) ~ log(catchment_ha) + log1p(slope_mid), data = XWD_allsites)
model_Q <- lm(log(Q_m3s) ~ log(catchment_ha), data = Q_df)


summary(model_co2)
summary(model_co2_nowetland)
#summary(model_co2_test)
summary(model_w)
summary(model_d)
summary(model_Q)
