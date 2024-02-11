#Merge k600 and df

#this scrip is meant to:
#1. merge the df that contains updated flow accumulation  and the k600 data frame that has updated k600 data. 
#2. organize the column on the file so that they are more usable
#3. write out to csv

synop <- read.csv(here::here("ProcessedData/ALL_synoptic_2022-06-08.csv"))
#df <- read.csv(here::here("ProcessedData/SynopFlowAccu_20220626.csv"))
df <- read.csv(here::here("ProcessedData/SynopFlowAccu_20240131.csv"))

#remove old flow accumulation info
df$FlowAccu <- NULL
df$CatchmentSize_m2 <- NULL
df$CatchmentSize_km2 <- NULL
df$X <- NULL

#convert new flow accumulation to different units
df$CatchmentSize_km2 <- df$CatchmentSize2_m2 / 1000000
df$CatchmentSize_ha <- df$CatchmentSize2_m2 / 10000

#remove old k600 values and format date
df$K600.effective <- df$K600.effective_old
df$K600.effective <- NULL

df$k_m.d <- NULL
df$Date <- as.Date(df$Date, format="%m/%d/%Y")

#k600 data frame contains update k600 values
k600_df <- read.csv(here::here("ProcessedData/ALL_synoptic_2022-08-26.csv"))
k600_df$Date <- as.Date(k600_df$Date, format="%Y-%m-%d")

#now we bind them
df_test <- full_join(df,k600_df[c("Date","VaisalaType","EOS_no","Flux_ave","k_m.d","K600.effective")],by=c("Date","VaisalaType","EOS_no","Flux_ave"))

#that almost worked - let's just read out the csv so that we can do this manualy - I think that will be easier



#total flux for gavi tribs to add to bball plota
Trib1 <- read.csv(here::here("ProcessedData/GAVItrib1_synopticGeom_2022-08-30.csv"))
Trib2 <- read.csv(here::here("ProcessedData/GAVItrib2_synopticGeom_2022-08-30.csv"))



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
df_fig2 <- rbind(df_fig2_2,GAVI_up,GAVI_down)


######
#k600#
######


k600_df$Wetland_2 <- k600_df$Wetland
k600_df$Wetland_2 <- as.character(k600_df$Wetland_2)

k600_df$Wetland_2[k600_df$Wetland == "GAVItrib1"] <- "Gavilan Tributaries"
k600_df$Wetland_2[k600_df$Wetland == "GAVItrib2"] <- "Gavilan Tributaries"
k600_df$Wetland_2[k600_df$Wetland == "GAVItrib3"] <- "Gavilan Tributaries"
k600_df$Wetland_2[k600_df$Wetland == "GAVI"] <- "Gavilan Mainstem"
k600_df$Wetland_2[k600_df$Wetland == "ANTE"] <- "Antenas"
k600_df$Wetland_2[k600_df$Wetland == "COLM"] <- "Colmillo"
k600_df$Wetland_2 <- as.factor(k600_df$Wetland_2)


#tribstuff
Trib1$Wetland <- "Tributary 1"
Trib2$Wetland <- "Tributary 2"
Tribs <- rbind(Trib1,Trib2)