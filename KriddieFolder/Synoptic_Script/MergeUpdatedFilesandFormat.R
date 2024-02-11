#Merge k600 and df

#this scrip is meant to:
#1. merge the df that contains updated flow accumulation  and the k600 data frame that has updated k600 data. 
#2. organize the column on the file so that they are more usable
#3. write out to csv

synop <- read.csv(here::here("ProcessedData/ALL_synoptic_2022-06-08.csv"))
#df <- read.csv(here::here("ProcessedData/SynopFlowAccu_20220626.csv"))
df <- read.csv(here::here("ProcessedData/SynopFlowAccu_20240131.csv"))
df <- df%>%select(lon_fit,lat_fit,ele_fit,dist,slope50m,Date,EOS_no,VaisalaType,Flux_ave,CO2_ppm_ave,adjusted_ppm,AirTemp_c,WaterTemp_c,Total_hPa,surface_area,flux_umolpers,Totalflux_umolpers,DOC,TDN,Wetland,dist_ANTE,dist_GAVI,dist_COLM,dist_GAVItrib1,dist_GAVItrib2,dist_GAVItrib3,CatchmentSize2_m2,K600.effective)
df$Date <- as.Date(df$Date, format="%m/%d/%Y")
df$k600_old <- df$K600.effective
df$K600.effective <- NULL

#k600 data frame contains update k600 values
k600_df <- read.csv(here::here("ProcessedData/ALL_synoptic_2022-08-26.csv"))
k600_df <- k600_df%>%select(Date,EOS_no,VaisalaType,Flux_ave,air_pressure_atm,VaporPressure_atm,TotalAir_atm,Total_air_MolperL,CO2_air_MolesPerLiter,CO2_water_gCO2asCPerLiter,WaterTemp_K,KH_mol.L.atm,CO2_water_gCO2asCPerLiter,deltaCO2_gCO2asCperM3,Flux_gCO2asCperM2perDay,k_m.d,Sc,K600.effective)
k600_df$Date <- as.Date(k600_df$Date, format="%Y-%m-%d")


#now we bind them
df_test <- full_join(df,k600_df,by=c("Date","VaisalaType","EOS_no","Flux_ave"))

#that almost worked - let's just read out the csv so that we can do this manualy - I think that will be easier
#write.csv(df_test,here::here("ProcessedData/df_test.csv"))

#now read it back in
df_2 <- read.csv(here::here("ProcessedData/df_test.csv"))

rm(k600_df,df,df_test)

df_2$k600_old <- NULL
df_2$X <- NULL

#is there anything we can do to bring in the width and depth info?
#ANTE DATA
ANTE_XWD <- read.csv(here::here("ProcessedData/Ante_XWD.csv"))
ANTE_XWD <- ANTE_XWD[c("dist","x_pred","w","d","notes")]
ANTE_XWD$dist <- ANTE_XWD$x_pred

df_ANTE <- full_join(df_2%>%filter(Wetland=="ANTE"),ANTE_XWD,by="dist")

#write out 
#write.csv(df_ANTE,here::here("ProcessedData/df_ANTE_test.csv"))

#COLMILLO DATA
COLM_XWD <- read.csv(here::here("ProcessedData/Colm_XWD.csv"))
COLM_XWD <- COLM_XWD[c("dist","x_pred","w","d","notes")]
COLM_XWD$dist <- COLM_XWD$x_pred

df_COLM <- full_join(df_2%>%filter(Wetland=="COLM"),COLM_XWD,by="dist")
#write out and then read in
#write.csv(df_COLM,here::here("ProcessedData/df_COLM_test.csv"))

#GAVI DATA
GAVI_XWD <- read.csv(here::here("ProcessedData/Colm_XWD.csv"))
GAVI_XWD <- GAVI_XWD[c("dist","x_pred","w","d","notes")]
GAVI_XWD$dist <- GAVI_XWD$x_pred

df_GAVI <- full_join(df_2%>%filter(Wetland=="Gavi-mainstem"),GAVI_XWD,by="dist")
#write out and then read in
#write.csv(df_GAVI,here::here("ProcessedData/df_GAVI_test.csv"))

#GAVItrib1 DATA
GAVItrib1_XWD <- read.csv(here::here("ProcessedData/Gavitrib1_XWD.csv"))
GAVItrib1_XWD <- GAVItrib1_XWD[c("dist","x_pred","w","d","notes")]
GAVItrib1_XWD$dist <- GAVItrib1_XWD$x_pred

df_GAVItrib1 <- full_join(df_2%>%filter(Wetland=="GAVItrib1"),GAVItrib1_XWD,by="dist")
#write out and then read in
#write.csv(df_GAVItrib1,here::here("ProcessedData/df_GAVItrib1_test.csv"))

#GAVItrib2 DATA
GAVItrib2_XWD <- read.csv(here::here("ProcessedData/Gavitrib2_XWD.csv"))
GAVItrib2_XWD <- GAVItrib2_XWD[c("dist","x_pred","w","d","notes")]
GAVItrib2_XWD$dist <- GAVItrib2_XWD$x_pred

df_GAVItrib2 <- full_join(df_2%>%filter(Wetland=="GAVItrib2"),GAVItrib2_XWD,by="dist")
#write out and then read in
#write.csv(df_GAVItrib2,here::here("ProcessedData/df_GAVItrib2_test.csv"))

#GAVItrib3 DATA
GAVItrib3_XWD <- read.csv(here::here("ProcessedData/Gavitrib3_XWD.csv"))
GAVItrib3_XWD <- GAVItrib3_XWD[c("dist","x_pred","w","d","notes")]
GAVItrib3_XWD$dist <- GAVItrib3_XWD$x_pred

df_GAVItrib3 <- full_join(df_2%>%filter(Wetland=="GAVItrib3"),GAVItrib3_XWD,by="dist")
#write out and then read in
#write.csv(df_GAVItrib3,here::here("ProcessedData/df_GAVItrib3_test.csv"))




#read back in
df_ANTE <- read.csv(here::here("ProcessedData/df_ANTE_test2.csv"))
df_COLM <- read.csv(here::here("ProcessedData/df_COLM_test3.csv"))
df_GAVI <- read.csv(here::here("ProcessedData/df_GAVI_test2.csv"))
df_GAVItrib1 <- read.csv(here::here("ProcessedData/df_GAVItrib1_test2.csv"))
df_GAVItrib2 <- read_csv(here::here("ProcessedData/df_GAVItrib2_test2.csv"))
df_GAVItrib3 <- read.csv(here::here("ProcessedData/df_GAVItrib3_test2.csv"))

df_3 <- rbind(df_ANTE,df_COLM,df_GAVI,df_GAVItrib1,df_GAVItrib2,df_GAVItrib3)
df <- df_3
#add more wetland info, this is for future plotting

df$Wetland_2 <- df$Wetland
df$Wetland_2 <- as.character(df$Wetland_2)

df$Wetland_2[df$Wetland == "GAVItrib1"] <- "Gavilan Tributaries"
df$Wetland_2[df$Wetland == "GAVItrib2"] <- "Gavilan Tributaries"
df$Wetland_2[df$Wetland == "GAVItrib3"] <- "Gavilan Tributaries"
df$Wetland_2[df$Wetland == "Gavi-mainstem"] <- "Gavilan Mainstem"
df$Wetland_2[df$Wetland == "ANTE"] <- "Antenas"
df$Wetland_2[df$Wetland == "COLM"] <- "Colmillo"
df$Wetland_2 <- as.factor(df$Wetland_2)

df$Wetland_3 <- df$Wetland

df$Wetland_3 <- as.character(df$Wetland_3)

df$Wetland_3[df$Wetland == "GAVItrib1"] <- "Gavilan River Network"
df$Wetland_3[df$Wetland == "GAVItrib2"] <- "Gavilan River Network"
df$Wetland_3[df$Wetland == "GAVItrib3"] <- "Gavilan River Network"
df$Wetland_3[df$Wetland == "Gavi-mainstem"] <- "Gavilan River Network"


df$Wetland_3 <- as.factor(df$Wetland_3)


df$Wetland_4 <- df$Wetland
df_fig2_1 <- df%>%filter(Wetland=="Gavi-mainstem")
df_fig2_2 <- df%>%filter(Wetland!="Gavi-mainstem")
GAVI_up <- df_fig2_1%>%filter(dist < 300)
GAVI_up$Wetland_4 <- "Gavilan Inlet"
GAVI_down <- df_fig2_1%>%filter(dist > 400)
GAVI_down$Wetland_4 <- "Gavilan Outlet"
df <- rbind(df_fig2_2,GAVI_up,GAVI_down)

df$Wetland_5 <- df$Wetland_4
df$Wetland_5[df$Wetland == "GAVItrib1"] <- "GavilanTribs"
df$Wetland_5[df$Wetland == "GAVItrib2"] <- "GavilanTribs"
df$Wetland_5[df$Wetland == "GAVItrib3"] <- "GavilanTribs"


df$CatchmentSize_m2 <-df$CatchmentSize2_m2
df$CatchmentSize2_m2 <- NULL
df$CatchmentSize_km2 <- df$CatchmentSize_m2 / 1000000
df$CatchmentSize_ha <- df$CatchmentSize_m2 / 10000

df$depth <- as.numeric(df$d)
df$width <- as.numeric(df$w)


#now that is done, let's make everything look a little nicer
df_4 <- df%>%select(lon_fit,lat_fit,ele_fit,width,depth,dist,dist_ANTE,dist_GAVI,dist_COLM,dist_GAVItrib1,dist_GAVItrib2,dist_GAVItrib3,
                      slope50m,Wetland,Wetland_2,Wetland_3,Wetland_4,Wetland_5,
                    CatchmentSize_ha,CatchmentSize_m2,CatchmentSize_km2,Date,EOS_no,VaisalaType,Date,
                      CO2_ppm_ave,adjusted_ppm,Flux_ave,flux_umolpers,Flux_gCO2asCperM2perDay,Totalflux_umolpers,
                      WaterTemp_c,AirTemp_c,Total_hPa,air_pressure_atm,VaporPressure_atm,
                    KH_mol.L.atm, k_m.d, Sc, K600.effective,
                    surface_area, DOC,TDN)

write.csv(df_4,here::here("ProcessedData/ALLSYNOPDATA_FINAL_2024-02-10.csv"))
