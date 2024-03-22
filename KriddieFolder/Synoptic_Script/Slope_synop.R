library(dplyr)
library(readr)
library(ggplot2)
library(lme4)
library(lmerTest)
library(jtools)
library(extraoperators)
library(JWileymisc)
library(multilevelTools)
library("Hmisc")
library(car)
library(GGally)
library(MCMCglmm)


#We found that pCO2 decreased with increasing catchment size within the river network of the Gavilán catchment (p-value < 0.0001) (Figure 2A & B). 
#We also found a pattern of decreasing pCO2 concentrations with increasing distance downstream in the smallest streams measured: Antenas, and the two smaller tributaries to Gavilán (Figure 2C). 
#The relationship is not statistically significant in Gavilán tributary 2, but it is shown here because it exhibits consistent decrease in pCO2 with distance. 
#We did not observe these same patterns in a third tributary to Gavilán, nor in the largest streams measured in this study, reaches Gavilán and Colmillo. 
#However, when the river reach draining the Gavilán catchment was split into two river reaches, upstream of the large wetland and downstream of the large wetland, we observed a significant decrease in pCO2 from upstream to downstream along the wetland outlet (Figure 2C). 

#slope

GaviTrib1_synop_Slope <-  read.csv("~/OneDrive - University of North Carolina at Chapel Hill/Dissertation papers/SynopPaper/DTW_data/Slope_tables/GaviTrib1_synop_Slope.csv")
GaviTrib1_synop_Slope$Stream_name <- "GaviTrib1"
GaviTrib2_synop_Slope <-  read.csv("~/OneDrive - University of North Carolina at Chapel Hill/Dissertation papers/SynopPaper/DTW_data/Slope_tables/GaviTrib2_synop_Slope.csv")
GaviTrib2_synop_Slope$Stream_name <- "GaviTrib2"
GaviOutlet_synop_Slope <-  read.csv("~/OneDrive - University of North Carolina at Chapel Hill/Dissertation papers/SynopPaper/DTW_data/Slope_tables/GaviOutlet_synop_Slope.csv")
GaviOutlet_synop_Slope$Stream_name <- "GaviOutlet"
GaviInlet_synop_Slope <-  read.csv("~/OneDrive - University of North Carolina at Chapel Hill/Dissertation papers/SynopPaper/DTW_data/Slope_tables/GaviInlet_synop_Slope.csv")
GaviInlet_synop_Slope$Stream_name <- "GaviInlet"
Antenas_synop_Slope <-  read.csv("~/OneDrive - University of North Carolina at Chapel Hill/Dissertation papers/SynopPaper/DTW_data/Slope_tables/Antenas_synop_Slope.csv")
Colmillo_synop_Slope <-  read.csv("~/OneDrive - University of North Carolina at Chapel Hill/Dissertation papers/SynopPaper/DTW_data/Slope_tables/Colmillo_synop_Slope.csv")
Colmillo_synop_Slope$Location1 <- NULL
Colmillo_synop_Slope$Stream_name <- "Colmillo"


df <- rbind(Antenas_synop_Slope,Colmillo_synop_Slope,GaviInlet_synop_Slope,GaviOutlet_synop_Slope,GaviTrib1_synop_Slope,GaviTrib2_synop_Slope)
df$ID <- paste(df$Stream_name,df$Sample_ID, sep = "_")
df <- df%>%select(ID,SampleType,Z)
df_wide <- reshape(df, idvar = "ID", timevar = "SampleType", direction = "wide")
colnames(df_wide) <- c("ID","Ele_downstream","Ele_upstream")
rm(df)
df_wide$Stream_name <- gsub('_.*','',df_wide$ID)
df_wide$Ele_diff <- df_wide$Ele_upstream - df_wide$Ele_downstream 

df_wide$reach_length <- NA
for(i in 1:nrow(df_wide)){
  if(df_wide$Stream_name[i]=="GaviTrib2"){df_wide$reach_length[i] <- 10}
  else if(df_wide$Stream_name[i]=="GaviTrib1"){df_wide$reach_length[i] <- 10}
  else if(df_wide$Stream_name[i]=="GaviInlet"){df_wide$reach_length[i] <- 20}
  else if(df_wide$Stream_name[i]=="GaviOutlet"){df_wide$reach_length[i] <- 30}
  else if(df_wide$Stream_name[i]=="Colmillo"){df_wide$reach_length[i] <- 180}
  else if(df_wide$Stream_name[i]=="Antenas"){df_wide$reach_length[i] <- 90}
  
}

df_wide$slope <- df_wide$Ele_diff/df_wide$reach_length

##
ALLSYNOPDATA_FINAL_2024_02_10 <- read_csv("~/Documents/Ecuador2021/ProcessedData/ALLSYNOPDATA_FINAL_2024-02-10.csv")

##
ANTE_synop <- ALLSYNOPDATA_FINAL_2024_02_10%>%filter(Wetland == "ANTE")
ANTE_synop <- ANTE_synop[order(ANTE_synop$dist),]
ANTE_synop$Join_ID <- seq.int(nrow(ANTE_synop))
df_wide_Antenas <- df_wide%>%filter(Stream_name=="Antenas")
df_wide_Antenas$Join_ID <- as.integer(gsub('Antenas_', '', df_wide_Antenas$ID))
df_wide_Antenas <- full_join(ANTE_synop,df_wide_Antenas,by="Join_ID")


GAVIOutlet_synop <- ALLSYNOPDATA_FINAL_2024_02_10%>%filter(Wetland_5 == "Gavilan Outlet")
GAVIOutlet_synop <- GAVIOutlet_synop[order(GAVIOutlet_synop$dist),]
GAVIOutlet_synop$Join_ID <- seq.int(nrow(GAVIOutlet_synop))
df_wide_GaviOutlet <- df_wide%>%filter(Stream_name=="GaviOutlet")
df_wide_GaviOutlet$Join_ID <- as.integer(gsub('GaviOutlet_', '', df_wide_GaviOutlet$ID))
df_wide_GaviOutlet <- full_join(GAVIOutlet_synop,df_wide_GaviOutlet,by="Join_ID")

GaviInlet_synop <- ALLSYNOPDATA_FINAL_2024_02_10%>%filter(Wetland_5 == "Gavilan Inlet")
GaviInlet_synop <- GaviInlet_synop[order(GaviInlet_synop$dist),]
GaviInlet_synop$Join_ID <- seq.int(nrow(GaviInlet_synop))
df_wide_GaviInlet <- df_wide%>%filter(Stream_name=="GaviInlet")
df_wide_GaviInlet$Join_ID <- as.integer(gsub('GaviInlet_', '', df_wide_GaviInlet$ID))
df_wide_GaviInlet <- full_join(GaviInlet_synop,df_wide_GaviInlet,by="Join_ID")

GaviTrib1_synop <- ALLSYNOPDATA_FINAL_2024_02_10%>%filter(Wetland == "GAVItrib1")
GaviTrib1_synop <- GaviTrib1_synop[order(GaviTrib1_synop$dist),]
GaviTrib1_synop$Join_ID <- seq.int(nrow(GaviTrib1_synop))
df_wide_GaviTrib1 <- df_wide%>%filter(Stream_name=="GaviTrib1")
df_wide_GaviTrib1$Join_ID <- as.integer(gsub('GaviTrib1_', '', df_wide_GaviTrib1$ID))
df_wide_GaviTrib1 <- full_join(GaviTrib1_synop,df_wide_GaviTrib1,by="Join_ID")

GaviTrib2_synop <- ALLSYNOPDATA_FINAL_2024_02_10%>%filter(Wetland == "GAVItrib2")
GaviTrib2_synop <- GaviTrib2_synop[order(GaviTrib2_synop$dist),]
GaviTrib2_synop$Join_ID <- seq.int(nrow(GaviTrib2_synop))
df_wide_GaviTrib2 <- df_wide%>%filter(Stream_name=="GaviTrib2")
df_wide_GaviTrib2$Join_ID <- as.integer(gsub('GaviTrib2_', '', df_wide_GaviTrib2$ID))
df_wide_GaviTrib2 <- full_join(GaviTrib2_synop,df_wide_GaviTrib2,by="Join_ID")


Colmillo_synop <- ALLSYNOPDATA_FINAL_2024_02_10%>%filter(Wetland == "COLM")
Colmillo_synop <- Colmillo_synop[order(Colmillo_synop$dist),]
Colmillo_synop$Join_ID <- seq.int(nrow(Colmillo_synop))
df_wide_Colmillo <- df_wide%>%filter(Stream_name=="Colmillo")
df_wide_Colmillo$Join_ID <- as.integer(gsub('Colmillo_', '', df_wide_Colmillo$ID))
df_wide_Colmillo <- full_join(Colmillo_synop,df_wide_Colmillo,by="Join_ID")


df_wide_all <- rbind(df_wide_Antenas,df_wide_GaviOutlet,df_wide_GaviInlet,df_wide_Colmillo,df_wide_GaviTrib1,df_wide_GaviTrib2)

ggplot(df_wide_Antenas, aes(x=slope,y=log(adjusted_ppm))) + geom_point()
ggplot(df_wide_GaviOutlet, aes(x=slope,y=log(adjusted_ppm))) + geom_point()
ggplot(df_wide_GaviInlet, aes(x=slope,y=log(adjusted_ppm))) + geom_point()
ggplot(df_wide_Colmillo, aes(x=slope,y=log(adjusted_ppm))) + geom_point()
ggplot(df_wide_GaviTrib1, aes(x=slope,y=log(adjusted_ppm))) + geom_point()
ggplot(df_wide_GaviTrib2, aes(x=slope,y=log(adjusted_ppm))) + geom_point()

#k600 v slope not interesting

ggplot(df_wide_all, aes(x=slope,y=log(adjusted_ppm))) + geom_point()
ggplot(df_wide_all%>%filter(Wetland_3 == "Gavilan River Network"), 
       aes(x=slope,y=log(adjusted_ppm), color=Wetland_5)) +
  geom_point()
ggplot(df_wide_all%>%filter(Wetland_4 != "GAVItrib1"&Wetland_4 != "GAVItrib2"), 
       aes(x=slope,y=log(adjusted_ppm), color=Wetland_5)) +
  geom_point()

ggplot(df_wide_all %>%filter(Wetland_4 != "GAVItrib1"&Wetland_4 != "GAVItrib2"), 
       aes(x=CatchmentSize_ha,y=log(adjusted_ppm), color=Wetland_5)) +
  geom_point()

ggplot(df_wide_all %>%filter(Wetland_4 != "GAVItrib1"&Wetland_4 != "GAVItrib2"), 
       aes(x=CatchmentSize_ha,y=log(adjusted_ppm), color=Wetland_5)) +
  geom_point()

ggplot(df_wide_all %>%filter(Wetland_4 != "GAVItrib1"&Wetland_4 != "GAVItrib2")
       , 
       aes(x=slope,y=log(adjusted_ppm), color=CatchmentSize_ha)) +
  geom_point() +
  geom_smooth(method = "lm"#, se = FALSE
              )


#model 
model1.1 <- lm(log(adjusted_ppm) ~ scale(slope) + scale(dist), data = df_wide_all %>%filter(Wetland_4 != "GAVItrib1"&Wetland_4 != "GAVItrib2")
               )
summary(model1.1)
model1.2 <- lm(log(adjusted_ppm) ~ scale(slope) + scale(CatchmentSize_ha) , data = df_wide_all %>%filter(Wetland_4 != "GAVItrib1"&Wetland_4 != "GAVItrib2"))
summary(model1.2)
model1.2 <- lmer(log(adjusted_ppm) ~ slope + CatchmentSize_ha + (1|Wetland_4) , data = df_wide_all %>%filter(Wetland_4 != "GAVItrib1"&Wetland_4 != "GAVItrib2"))
summary(model1.2)
model1.3 <- lm(log(adjusted_ppm) ~ slope + CatchmentSize_ha , 
               data = df_wide_all %>%
                 filter(Wetland_4 != "GAVItrib1"&Wetland_4 != "GAVItrib2"))
summary(model1.3)

model2.1 <- lm(log(adjusted_ppm) ~ slope + dist , data = df_wide_all %>%filter(Stream_name == "Antenas"))
summary(model2.1)
model2.1 <- lm(log(adjusted_ppm) ~ slope + dist  , data = df_wide_all %>%filter(Stream_name == "Colmillo"))
summary(model2.1)
model2.1 <- lm(log(adjusted_ppm) ~ slope + dist , data = df_wide_all %>%filter(Stream_name == "GaviOutlet"))
summary(model2.1)
model2.1 <- lm(log(adjusted_ppm) ~ slope+ dist  , data = df_wide_all %>%filter(Stream_name == "GaviInlet"))
summary(model2.1)
model2.1 <- lm(log(adjusted_ppm) ~ slope+ dist  , data = df_wide_all %>%filter(Stream_name == "GaviTrib1"))
summary(model2.1)
model2.1 <- lm(log(adjusted_ppm) ~ slope + dist , data = df_wide_all %>%filter(Stream_name == "GaviTrib2"))
summary(model2.1)



model1 <- lm(log(adjusted_ppm) ~ CatchmentSize_ha, data = df_wide_Colmillo)
summary(model1)
model1 <- lm(log(adjusted_ppm) ~ slope + CatchmentSize_ha, data = df_wide_Colmillo)
summary(model1)

model1 <- lm(log(adjusted_ppm) ~ slope, data = df_wide_GaviInlet)
summary(model1)
model2 <- lm(log(adjusted_ppm) ~ slope, data = df_wide_GaviOutlet)
summary(model2)

ggplot(df_wide_all%>%filter(Wetland_4 != "GAVItrib1"&Wetland_4 != "GAVItrib2"), 
       aes(x=slope,y=log(adjusted_ppm), color=slope)) + geom_point()
ggplot(df_wide_all, aes(x=dist,y=log(adjusted_ppm), color=slope)) + geom_point()
ggplot(df_wide_GaviOutlet, aes(x=CatchmentSize_ha,y=log(adjusted_ppm), color=slope)) + geom_point()
ggplot(df_wide_GaviOutlet, aes(x=dist,y=log(adjusted_ppm), color=slope)) + geom_point()

ggplot(df_wide_GaviInlet, aes(x=slope,y=log(adjusted_ppm), color=slope)) + geom_point()


model1 <- lm(Flux_ave ~ slope, data = df_wide_all)
summary(model1)
