# this script is to 

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

DTW_df <- read_csv(here::here("ProcessedData/ALLSYNOPDATA_FINAL_2024-02-10_withDTW.csv"))
DTW_df <- DTW_df%>%select(Wetland,Wetland_4, ele_fit,dist,CatchmentSize_m2,Date,adjusted_ppm,Water,DTW_morethan0,DTW_lessthen0)
DTW_df <- DTW_df[order(DTW_df$dist),]
DTW_df$TotalArea_difference <- DTW_df$Water + DTW_df$DTW_morethan0 + DTW_df$DTW_lessthen0
DTW_df$Water_perc <- DTW_df$Water / DTW_df$TotalArea_difference * 100
DTW_df$DTWmorethan0_perc <- DTW_df$DTW_morethan0 / DTW_df$TotalArea_difference * 100
DTW_df$DTWlessthen0_perc <- DTW_df$DTW_lessthen0 / DTW_df$TotalArea_difference * 100

DTW_ANTE <- DTW_df%>%filter(Wetland=="ANTE")
DTW_GAVIinlet <- DTW_df%>%filter(Wetland_4=="Gavilan Inlet")
DTW_GAVIoutlet <- DTW_df%>%filter(Wetland_4=="Gavilan Outlet")
DTW_COLM <- DTW_df%>%filter(Wetland=="COLM")
DTW_GAVItrib1 <- DTW_df%>%filter(Wetland=="GAVItrib1")
DTW_GAVItrib2 <- DTW_df%>%filter(Wetland=="GAVItrib2")
DTW_GAVItrib3 <- DTW_df%>%filter(Wetland=="GAVItrib3")


#lag

DTW_ANTE$dist_prev <- lag(DTW_ANTE$dist, 1)
DTW_ANTE$CO2_prev <- lag(DTW_ANTE$adjusted_ppm, 1)
DTW_ANTE$CO2_change <- (DTW_ANTE$CO2_prev - DTW_ANTE$adjusted_ppm) / (DTW_ANTE$dist - DTW_ANTE$dist_prev)
DTW_ANTE$DTWlessthan0_total <- NA
DTW_ANTE$DTWlessthan0_total[1] <- DTW_ANTE$DTW_lessthen0[1]
for(i in 2:length(DTW_ANTE$DTWlessthan0_total)){
  DTW_ANTE$DTWlessthan0_total[i] <- DTW_ANTE$DTW_lessthen0[i] + DTW_ANTE$DTWlessthan0_total[i-1]
}
DTW_ANTE$TotalArea <- NA
DTW_ANTE$TotalArea[1] <- DTW_ANTE$TotalArea_difference[1]
for(i in 2:length(DTW_ANTE$TotalArea)){
  DTW_ANTE$TotalArea[i] <- DTW_ANTE$TotalArea_difference[i] + DTW_ANTE$TotalArea[i-1]
}

##
DTW_GAVIinlet$dist_prev <- lag(DTW_GAVIinlet$dist, 1)
DTW_GAVIinlet$CO2_prev <- lag(DTW_GAVIinlet$adjusted_ppm, 1)
DTW_GAVIinlet$CO2_change <- (DTW_GAVIinlet$adjusted_ppm - DTW_GAVIinlet$CO2_prev) / (DTW_GAVIinlet$dist - DTW_GAVIinlet$dist_prev)
DTW_GAVIinlet$DTWlessthan0_total <- NA
DTW_GAVIinlet$DTWlessthan0_total[1] <- DTW_GAVIinlet$DTW_lessthen0[1]
for(i in 2:length(DTW_GAVIinlet$DTWlessthan0_total)){
  DTW_GAVIinlet$DTWlessthan0_total[i] <- DTW_GAVIinlet$DTW_lessthen0[i] + DTW_GAVIinlet$DTWlessthan0_total[i-1]
}
DTW_GAVIinlet$TotalArea <- NA
DTW_GAVIinlet$TotalArea[1] <- DTW_GAVIinlet$TotalArea_difference[1]
for(i in 2:length(DTW_GAVIinlet$TotalArea)){
  DTW_GAVIinlet$TotalArea[i] <- DTW_GAVIinlet$TotalArea_difference[i] + DTW_GAVIinlet$TotalArea[i-1]
}

##
DTW_GAVIoutlet$dist_prev <- lag(DTW_GAVIoutlet$dist, 1)
DTW_GAVIoutlet$CO2_prev <- lag(DTW_GAVIoutlet$adjusted_ppm, 1)
DTW_GAVIoutlet$CO2_change <- (DTW_GAVIoutlet$adjusted_ppm - DTW_GAVIoutlet$CO2_prev) / (DTW_GAVIoutlet$dist - DTW_GAVIoutlet$dist_prev)
DTW_GAVIoutlet$DTWlessthan0_total <- NA
DTW_GAVIoutlet$DTWlessthan0_total[1] <- DTW_GAVIoutlet$DTW_lessthen0[1]
for(i in 2:length(DTW_GAVIoutlet$DTWlessthan0_total)){
  DTW_GAVIoutlet$DTWlessthan0_total[i] <- DTW_GAVIoutlet$DTW_lessthen0[i] + DTW_GAVIoutlet$DTWlessthan0_total[i-1]
}
DTW_GAVIoutlet$TotalArea <- NA
DTW_GAVIoutlet$TotalArea[1] <- DTW_GAVIoutlet$TotalArea_difference[1]
for(i in 2:length(DTW_GAVIoutlet$TotalArea)){
  DTW_GAVIoutlet$TotalArea[i] <- DTW_GAVIoutlet$TotalArea_difference[i] + DTW_GAVIoutlet$TotalArea[i-1]
}

##
DTW_COLM$dist_prev <- lag(DTW_COLM$dist, 1)
DTW_COLM$CO2_prev<- lag(DTW_COLM$adjusted_ppm, 1)
DTW_COLM$CO2_change<- (DTW_COLM$adjusted_ppm - DTW_COLM$CO2_prev) / (DTW_COLM$dist - DTW_COLM$dist_prev)
DTW_COLM$DTWlessthan0_total <- NA
DTW_COLM$DTWlessthan0_total[1] <- DTW_COLM$DTW_lessthen0[1]
for(i in 2:length(DTW_COLM$DTWlessthan0_total)){
  DTW_COLM$DTWlessthan0_total[i] <- DTW_COLM$DTW_lessthen0[i] + DTW_COLM$DTWlessthan0_total[i-1]
}
DTW_COLM$TotalArea <- NA
DTW_COLM$TotalArea[1] <- DTW_COLM$TotalArea_difference[1]
for(i in 2:length(DTW_COLM$TotalArea)){
  DTW_COLM$TotalArea[i] <- DTW_COLM$TotalArea_difference[i] + DTW_COLM$TotalArea[i-1]
}

##
DTW_GAVItrib1$dist_prev <- lag(DTW_GAVItrib1$dist, 1)
DTW_GAVItrib1$CO2_prev <- lag(DTW_GAVItrib1$adjusted_ppm, 1)
DTW_GAVItrib1$CO2_change <- (DTW_GAVItrib1$adjusted_ppm - DTW_GAVItrib1$CO2_prev) / (DTW_GAVItrib1$dist - DTW_GAVItrib1$dist_prev)
DTW_GAVItrib1$DTWlessthan0_total <- NA
DTW_GAVItrib1$DTWlessthan0_total[1] <- DTW_GAVItrib1$DTW_lessthen0[1]
for(i in 2:length(DTW_GAVItrib1$DTWlessthan0_total)){
  DTW_GAVItrib1$DTWlessthan0_total[i] <- DTW_GAVItrib1$DTW_lessthen0[i] + DTW_GAVItrib1$DTWlessthan0_total[i-1]
}
DTW_GAVItrib1$TotalArea <- NA
DTW_GAVItrib1$TotalArea[1] <- DTW_GAVItrib1$TotalArea_difference[1]
for(i in 2:length(DTW_GAVItrib1$TotalArea)){
  DTW_GAVItrib1$TotalArea[i] <- DTW_GAVItrib1$TotalArea_difference[i] + DTW_GAVItrib1$TotalArea[i-1]
}

##
DTW_GAVItrib2$dist_prev <- lag(DTW_GAVItrib2$dist, 1)
DTW_GAVItrib2$CO2_prev <- lag(DTW_GAVItrib2$adjusted_ppm, 1)
DTW_GAVItrib2$CO2_change <- (DTW_GAVItrib2$adjusted_ppm - DTW_GAVItrib2$CO2_prev) / (DTW_GAVItrib2$dist - DTW_GAVItrib2$dist_prev)
DTW_GAVItrib2$DTWlessthan0_total <- NA
DTW_GAVItrib2$DTWlessthan0_total[1] <- DTW_GAVItrib2$DTW_lessthen0[1]
for(i in 2:length(DTW_GAVItrib2$DTWlessthan0_total)){
  DTW_GAVItrib2$DTWlessthan0_total[i] <- DTW_GAVItrib2$DTW_lessthen0[i] + DTW_GAVItrib2$DTWlessthan0_total[i-1]
}
DTW_GAVItrib2$TotalArea <- NA
DTW_GAVItrib2$TotalArea[1] <- DTW_GAVItrib2$TotalArea_difference[1]
for(i in 2:length(DTW_GAVItrib2$TotalArea)){
  DTW_GAVItrib2$TotalArea[i] <- DTW_GAVItrib2$TotalArea_difference[i] + DTW_GAVItrib2$TotalArea[i-1]
}

##
DTW_GAVItrib3$dist_prev <- lag(DTW_GAVItrib3$dist, 1)
DTW_GAVItrib3$CO2_prev <- lag(DTW_GAVItrib3$adjusted_ppm, 1)
DTW_GAVItrib3$CO2_change <- (DTW_GAVItrib3$adjusted_ppm - DTW_GAVItrib3$CO2_prev) / (DTW_GAVItrib3$dist - DTW_GAVItrib3$dist_prev)
DTW_GAVItrib3$DTWlessthan0_total <- NA
DTW_GAVItrib3$DTWlessthan0_total[1] <- DTW_GAVItrib3$DTW_lessthen0[1]
for(i in 2:length(DTW_GAVItrib3$DTWlessthan0_total)){
  DTW_GAVItrib3$DTWlessthan0_total[i] <- DTW_GAVItrib3$DTW_lessthen0[i] + DTW_GAVItrib3$DTWlessthan0_total[i-1]
}
DTW_GAVItrib3$TotalArea <- NA
DTW_GAVItrib3$TotalArea[1] <- DTW_GAVItrib3$TotalArea_difference[1]
for(i in 2:length(DTW_GAVItrib3$TotalArea)){
  DTW_GAVItrib3$TotalArea[i] <- DTW_GAVItrib3$TotalArea_difference[i] + DTW_GAVItrib3$TotalArea[i-1]
}



df <- rbind(DTW_ANTE,DTW_COLM,DTW_GAVIinlet,DTW_GAVIoutlet,DTW_GAVItrib1,DTW_GAVItrib2,DTW_GAVItrib3)
df$Total_DTWlessthan0_perc <- df$DTWlessthan0_total / df$TotalArea * 100


#plot lets see it want to see it!!

ggplot(df%>%filter(Wetland=="ANTE"),aes(x=DTWlessthen0_perc,y=CO2_change)) + geom_point()
ggplot(df%>%filter(Wetland=="GAVItrib1"),aes(x=DTWlessthen0_perc,y=CO2_change)) + geom_point()
ggplot(df%>%filter(Wetland=="COLM"),aes(x=DTWlessthen0_perc,y=CO2_change)) + geom_point()

ggplot(df,aes(x=DTWlessthen0_perc,y=log(adjusted_ppm),color=Wetland_4)) + geom_point()


ggplot(df,aes(x=DTWlessthen0_perc,y=CO2_change,color=Wetland_4)) + geom_point()
ggplot(df,aes(x=DTWlessthen0_perc,y=adjusted_ppm,color=Wetland_4)) + geom_point()


ggplot(df%>%filter(Wetland=="GAVItrib1"),aes(x=DTWlessthen0_perc,y=adjusted_ppm,color=Wetland_4)) + geom_point()
ggplot(df%>%filter(Wetland=="GAVItrib2"),aes(x=DTWlessthen0_perc,y=adjusted_ppm,color=Wetland_4)) + geom_point()
ggplot(df%>%filter(Wetland_4=="Gavilan Outlet"),aes(x=DTWlessthen0_perc,y=adjusted_ppm,color=Wetland_4)) + geom_point()
ggplot(df%>%filter(Wetland_4=="Gavilan Inlet"),aes(x=DTWlessthen0_perc,y=adjusted_ppm,color=Wetland_4)) + geom_point()
ggplot(df%>%filter(Wetland_4=="COLM"),aes(x=DTWlessthen0_perc,y=adjusted_ppm,color=Wetland_4)) + geom_point()


ggplot(df%>%
         filter(Total_DTWlessthan0_perc < 100)%>%
                  filter(Wetland_4=="Gavilan Outlet"),aes(x=Total_DTWlessthan0_perc,y=adjusted_ppm,color=Wetland_4)) + geom_point()


model_1 <- lm(adjusted_ppm ~ Total_DTWlessthan0_perc, data = df)
model_1 <- lm(adjusted_ppm  ~ DTWlessthen0_perc + CO2_prev, data = df)
summary(model_1
        )
