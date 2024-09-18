library(BAMMtools)

df1<- read_csv(here::here("ProcessedData/ALLSYNOPDATA_FINAL_2024-05-07.csv"))%>%rename(adjusted_ppm=pCO2_ppm)
ggplot(df1%>%filter(Wetland=="ANTE"),aes(x=dist,y=adjusted_ppm,color=Wetland)) + 
  geom_point() + scale_y_log10()
ggplot(df1%>%filter(Wetland=="GAVItrib1"),aes(x=dist,y=adjusted_ppm,color=Wetland)) + 
  geom_point() + scale_y_log10()
ggplot(df1%>%filter(Wetland=="GAVItrib1"|Wetland=="GAVItrib3"|Wetland=="ANTE"),aes(x=CatchmentSize_ha,y=adjusted_ppm,color=Wetland)) + 
  geom_point() + scale_y_log10() +scale_x_log10()
ggplot(df1,aes(x=CatchmentSize_ha,y=adjusted_ppm,color=Wetland)) + 
  geom_point() + scale_y_log10() #+scale_x_log10()

df <-read.csv(here::here("ProcessedData/raymond_data_allsites_2024-09-16.csv"),na.strings=c("","NA"))
df$name_2 <- df$name
df$name_2[which(df$name_2 == "GaviTrib1")] = "GaviTribs"
df$name_2[which(df$name_2 == "GaviTrib2")] = "GaviTribs"

df$water_subtract_air <- df$pCO2_w_atm - df$pCO2_air_atm


getJenksBreaks(df$adjusted_ppm, 5, subset = NULL)
# 320.1599  1499.8411  3042.7583  5341.6114 10343.6565

ggplot(df%>%filter(name=="Ante"),aes(x=x_pred,y=ele_arcpro,color=log(adjusted_ppm))) + geom_point() + scale_color_gradient(low="blue", high="red")
ggplot(df%>%filter(name=="GaviTrib1"),aes(x=x_pred,y=ele_arcpro,color=log(adjusted_ppm))) + geom_point() + scale_color_gradient(low="blue", high="red")
ggplot(df%>%filter(name=="GaviTrib2"),aes(x=x_pred,y=ele_arcpro,color=log(adjusted_ppm))) + geom_point() + scale_color_gradient(low="blue", high="red")
ggplot(df%>%filter(name=="GaviUp"),aes(x=x_pred,y=ele_arcpro,color=log(adjusted_ppm))) + geom_point() + scale_color_gradient(low="blue", high="red")
ggplot(df%>%filter(name=="GaviDown"),aes(x=x_pred,y=ele_arcpro,color=log(adjusted_ppm))) + geom_point() + scale_color_gradient(low="blue", high="red")


ggplot(df%>%filter(adjusted_ppm <= 320.1599),aes(x=adjusted_ppm,y=slope_up,color=name)) + geom_point()

ggplot(df%>%filter(catchment_ha>2.5),aes(x=slope_up,y=adjusted_ppm,color=log(catchment_ha))) + geom_point() + scale_y_log10()

ggplot(df,aes(x=catchment_ha,y=Q_m3s,color=name)) + geom_point()
ggplot(df,aes(x=catchment_ha,y=w,color=name)) + geom_point() + scale_y_log10()
ggplot(df,aes(x=catchment_ha,y=d,color=name)) + geom_point() + scale_y_log10()


CO2 <- ggplot(df, aes(x = name_2, y = adjusted_ppm, fill = name_2)) +
  geom_boxplot() +
  geom_jitter(shape=18,position=position_jitter(0.1)) +
  stat_summary(fun = "mean", geom = "point", shape = 23, size = 3, color = "black",fill="white")+
  geom_hline(yintercept= log10(air_ppm),color="red",linetype="twodash", linewidth=1) +
  scale_y_log10() +
  MyTheme



ggplot(df_bins,aes(x=Catchment_bin,y=w,color=name)) + geom_point() + scale_y_log10()
ggplot(df_bins,aes(x=Catchment_bin,y=d,color=name)) + geom_point()+ scale_y_log10()
ggplot(df_bins,aes(x=Catchment_bin,y=slope_mid,color=name)) + geom_point() + scale_y_log10()
ggplot(df_bins,aes(x=Catchment_bin,y=Q_m3s,color=name)) + geom_point() + scale_y_log10()
ggplot(df_bins,aes(x=Catchment_bin,y=k600_eq1_final,color=name)) + geom_point()+ scale_y_log10()
ggplot(df_bins,aes(x=Catchment_bin,y=adjusted_ppm,color=name)) + geom_point()+ scale_y_log10()
ggplot(df_bins,aes(x=Catchment_bin,y=F_mol_m2_d_eq1,color=name)) + geom_point()+ scale_y_log10()
ggplot(df_bins,aes(x=Catchment_bin,y=F_CO2_molpers_eq1,color=name)) + geom_point()+ scale_y_log10()


ggplot(df_bins,aes(x=catchment_ha,y=w,color=name)) + geom_point() + scale_y_log10()
ggplot(df_bins,aes(x=catchment_ha,y=d,color=name)) + geom_point()+ scale_y_log10()
ggplot(df_bins,aes(x=catchment_ha,y=slope_mid,color=name)) + geom_point() + scale_y_log10()
ggplot(df_bins,aes(x=catchment_ha,y=Q_m3s,color=name)) + geom_point() + scale_y_log10()
ggplot(df_bins,aes(x=catchment_ha,y=k600_eq1_final,color=name)) + geom_point()+ scale_y_log10()
ggplot(df_bins,aes(x=catchment_ha,y=adjusted_ppm,color=name)) + geom_point()+ scale_y_log10()
ggplot(df_bins,aes(x=catchment_ha,y=F_mol_m2_d_eq1,color=name)) + geom_point()+ scale_y_log10()
ggplot(df_bins,aes(x=catchment_ha,y=F_CO2_molpers_eq1,color=name)) + geom_point()+ scale_y_log10()



ggplot(df_bins%>%filter(catchment_ha > 2.5)
       %>%filter(slope_up > .05),
       aes(x=slope_up,y=adjusted_ppm)) + 
  geom_point()+ scale_y_log10() + geom_smooth(method='lm')


ggplot(df_bins#%>%filter(catchment_ha > 2.5)
       #%>%filter(slope_up > .001)
       ,
       aes(x=slope_mid,y=k600_eq1_final,color=Catchment_bin )) + 
  geom_point()+ scale_y_log10() + geom_smooth(method='lm')


#the first ___ of a stream is worth ___ flux

df_bins$Catchment_bin2 <- as.character(df_bins$Catchment_bin)
df_bins$Catchment_bin3 <- gsub("]", "", df_bins$Catchment_bin2) 
df_bins$Catchment_bin4 <- sub('.', '', df_bins$Catchment_bin3)
df_bins[c('min_log', 'max_log')] <- str_split_fixed(df_bins$Catchment_bin4, ',', 2)
df_bins$min_log <- as.numeric(df_bins$min_log )
df_bins$max_log <- as.numeric(df_bins$max_log )

df_bins$min <- signif(exp(df_bins$min_log),digits =2)
df_bins$max <- signif(exp(df_bins$max_log),digits =2)

df_bins$bins_true  <- paste(df_bins$min, "-", df_bins$max)

df_bins$bins_true = factor(df_bins$bins_true, levels=c("0.11 - 0.5","0.5 - 2.3","2.3 - 10","10 - 47","47 - 210"))


### I am trying to come up with some things showing that really small streams act differenct from other streams

ggplot(df%>%filter(catchment_ha>0),aes(x=slope_up,y=adjusted_ppm,color=log(catchment_ha))) + geom_point() + scale_y_log10()
ggplot(df%>%filter(catchment_ha>2.5),aes(x=slope_up,y=adjusted_ppm,color=log(catchment_ha))) + geom_point() + scale_y_log10()
ggplot(df%>%filter(catchment_ha>12.5),aes(x=slope_up,y=adjusted_ppm,color=log(catchment_ha))) + geom_point() + scale_y_log10()
ggplot(df%>%filter(catchment_ha>20),aes(x=slope_up,y=adjusted_ppm,color=log(catchment_ha))) + geom_point() + scale_y_log10()

ggplot(df_bins,aes(x=slope_mid,y=adjusted_ppm,color=name)) + geom_point() #+ scale_y_log10()

ggplot(df_bins,aes(x=Catchment_bin,y=adjusted_ppm)) + geom_boxplot() + scale_y_log10()
ggplot(df_bins,aes(x=Catchment_bin,y=F_mol_m2_d_eq1)) + geom_boxplot() + scale_y_log10()
ggplot(df_bins,aes(x=Catchment_bin,y=F_CO2_molpers_eq1)) + geom_boxplot() + scale_y_log10()


df1<- read_csv(here::here("ProcessedData/ALLSYNOPDATA_FINAL_2024-05-07.csv"))%>%rename(adjusted_ppm=pCO2_ppm)
ggplot(df1%>%filter(Wetland=="GAVItrib1"|Wetland=="GAVItrib3"|Wetland=="ANTE"),aes(x=CatchmentSize_ha,y=adjusted_ppm,color=Wetland)) + 
  geom_point() + scale_y_log10() +scale_x_log10()
ggplot(df1#%>%filter(Wetland=="GAVItrib1"|Wetland=="GAVItrib3"|Wetland=="ANTE")
       ,aes(x=CatchmentSize_ha,y=adjusted_ppm,color=Wetland)) + 
  geom_point() + scale_y_log10() +scale_x_log10()

#take-away: slope is a really strong predictor of flux for COlM
ggplot(df %>%filter(name=="Colm")
       , aes(x=slope_mid,F_mol_m2_d_eq1,color=adjusted_ppm)) + 
  geom_point() + scale_y_log10() + scale_x_log10()
ggplot(df %>%filter(name=="Colm")
       , aes(x=slope_mid,F_CO2_molpers_eq1,color=adjusted_ppm)) + 
  geom_point() + scale_y_log10() + scale_x_log10()
ggplot(df %>%filter(name=="Colm")
       , aes(x=slope_mid,k600_eq1,color=adjusted_ppm)) + 
  geom_point() + scale_y_log10() + scale_x_log10()

