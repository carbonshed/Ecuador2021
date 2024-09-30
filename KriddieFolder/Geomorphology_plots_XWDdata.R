#geomorphology script
#k. whitmore
#9/19/2024

library(here)
library(dplyr)
library(ggplot2)

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


Ante_synop <- read.csv(here::here("ProcessedData/export_to_r/ante_XWD_export_eledata_synop.csv"))%>%select(x_pred,x,ele_arcpro,date,Flux_ave,adjusted_ppm,slope_mid,slope_up,catchment_ha)
Colm_synop <- read.csv(here::here("ProcessedData/export_to_r/colm_XWD_export_eledata_synop.csv"))%>%select(x_pred,x,ele_arcpro,date,Flux_ave,adjusted_ppm,slope_mid,slope_up,catchment_ha)
GaviDown_synop <- read.csv(here::here("ProcessedData/export_to_r/gavi-down_XWD_export_synop.csv"))%>%select(x_pred,x,ele_arcpro,date,Flux_ave,adjusted_ppm,slope_mid,slope_up,catchment_ha)
GaviUp_synop <- read.csv(here::here("ProcessedData/export_to_r/gavi-up_XWD_export_eledata_synop.csv"))%>%select(x_pred,x,ele_arcpro,Date,Flux_ave,adjusted_ppm,slope_mid,slope_up,catchment_ha)%>%rename(date=Date)
GaviTrib1_synop <- read.csv(here::here("ProcessedData/export_to_r/gavi-trib1_XWD_export_eledata_synop.csv"))%>%select(x_pred,x,ele_arcpro,Date,Flux_ave,adjusted_ppm,slope_mid,slope_up,catchment_ha)%>%rename(date=Date)
GaviTrib2_synop <- read.csv(here::here("ProcessedData/export_to_r/gavi-trib2_XWD_export_eledata_synop.csv"))%>%select(x_pred,x,ele_arcpro,Date,Flux_ave,adjusted_ppm,slope_mid,slope_up,catchment_ha)%>%rename(date=Date)
GaviTrib3_synop <- read.csv(here::here("ProcessedData/export_to_r/gavi-trib3_XWD_export_eledata_synop.csv"))%>%select(x_pred,x,ele_arcpro,Date,Flux_ave,adjusted_ppm,slope_mid,slope_up,catchment_ha)%>%rename(date=Date)

Ante_synop$site <- "Ante"
Colm_synop$site <- "Colm"
GaviDown_synop$site <- "GaviDown"
GaviUp_synop$site <- "GaviUp"
GaviTrib1_synop$site <- "GaviTrib1"
GaviTrib2_synop$site <- "GaviTrib2"
GaviTrib3_synop$site <- "GaviTrib3"

#clean
#delete co2 just below the wetland. start after waterfall
GaviDown_synop_nowetland <- GaviDown_synop%>%filter(x > 114)
#delete co2 just below the wetland. start after waterfall
GaviUp_test <- GaviUp_synop%>%filter(date != "6/29/2021")

synop_allsites <- rbind(Ante_synop,Colm_synop,GaviDown_synop,GaviUp_synop,GaviTrib1_synop,GaviTrib2_synop,GaviTrib3_synop)
synop_allsites_nowetland <- rbind(Ante_synop,Colm_synop,GaviDown_synop_nowetland,GaviUp_synop,GaviTrib1_synop,GaviTrib2_synop,GaviTrib3_synop)
synop_test <- rbind(Ante_synop,Colm_synop,GaviDown_synop_nowetland,GaviUp_test,GaviTrib1_synop,GaviTrib2_synop,GaviTrib3_synop)



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
