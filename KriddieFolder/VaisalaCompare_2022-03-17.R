## I don't know if vaisalas are being corrected correctly booooo
#so let's mess around with data see what we got.

library(here)
library(dplyr)
library(lubridate)
library(plotly)
library(tidyr)
#read in some data 
####6/18/21####
##GAVI
NewV <- read.csv(here::here("/Synoptic/VaisalaCheck/Vnew_EOS1_synoptic_2021-06-18.csv"), skip = 6)
NewV <- NewV[,c(1:3,6)]
colnames(NewV) <- c("Date","Time","ppm","point")
NewV$vaisala_type <- "new"
NewV$DateTime <- as.POSIXct(paste(NewV$Date, NewV$Time), format = "%m/%d/%Y %I:%M:%S %p", tz = "UTC")
NewV_0618 <- NewV %>% replace_na(list(point = 'air'))

fig <- plot_ly(NewV_0618%>%filter(point=="air")%>%filter(ppm>50)%>%filter(ppm<800),
               x = ~DateTime, y = ~ppm, size = 1, color = ~vaisala_type)
fig

####6/22/21####
##GAVI
OldV <- read.csv(here::here("/Synoptic/VaisalaCheck/Vold_EOS2_synoptic_2021-06-22.csv"), skip = 6)
#colnames(OldV) <- c("Date","Time","ppm","Tract","Description","Point","lat","lon","ele","WaterSample")
OldV <- OldV[,c(1,2,4,7)]
colnames(OldV) <- c("Date","Time","ppm","point")
OldV$vaisala_type <- "old"
OldV$DateTime <- as.POSIXct(paste(OldV$Date, OldV$Time), format = "%m/%d/%Y %I:%M:%S %p", tz = "UTC")


NewV <- read.csv(here::here("/Synoptic/VaisalaCheck/Vnew_EOS1_synoptic_2021-06-22.csv"), skip = 6)
NewV <- NewV[,c(1:3,6)]
colnames(NewV) <- c("Date","Time","ppm","point")
NewV$vaisala_type <- "new"
NewV$DateTime <- as.POSIXct(paste(NewV$Date, NewV$Time), format = "%m/%d/%Y %I:%M:%S %p", tz = "UTC")

df_V <- rbind(NewV,OldV)
df_V_0622 <- df_V %>% replace_na(list(point = 'air'))

fig <- plot_ly(df_V_0622%>%filter(point=="air")%>%filter(ppm>50)%>%filter(ppm<800),
               x = ~DateTime, y = ~ppm, size = 1, color = ~vaisala_type)
fig


###6/23/21#####
##GAVI
OldV <- read.csv(here::here("/Synoptic/VaisalaCheck/Vold_EOS1_synoptic_2021-06-23.csv"), skip = 6)
#colnames(OldV) <- c("Date","Time","ppm","Tract","Description","Point","lat","lon","ele","WaterSample")
OldV <- OldV[,c(1,2,4,7)]
colnames(OldV) <- c("Date","Time","ppm","point")
OldV$vaisala_type <- "old"
OldV$DateTime <- as.POSIXct(paste(OldV$Date, OldV$Time), format = "%m/%d/%Y %I:%M:%S %p", tz = "UTC")

NewV <- read.csv(here::here("/Synoptic/VaisalaCheck/Vnew_EOS2_synoptic_2021-06-23.csv"), skip = 6)
NewV <- NewV[,c(1:3,6)]
colnames(NewV) <- c("Date","Time","ppm","point")
NewV$vaisala_type <- "new"
NewV$DateTime <- as.POSIXct(paste(NewV$Date, NewV$Time), format = "%m/%d/%Y %I:%M:%S %p", tz = "UTC")

df_V <- rbind(NewV,OldV)
df_V_0623 <- df_V %>% replace_na(list(point = 'air'))

fig <- plot_ly(df_V_0623%>%filter(point=="air")%>%filter(ppm>50)%>%filter(ppm<800),
               x = ~DateTime, y = ~ppm, size = 1, color = ~vaisala_type)
fig <- plot_ly(df_V%>%drop_na(point)#%>%filter(point=="air")%>%filter(ppm>50)%>%filter(ppm<800)
               ,
               x = ~DateTime, y = ~ppm, size = 1, color = ~vaisala_type)
fig


####6/29/21####
##GAVI
OldV <- read.csv(here::here("/Synoptic/VaisalaCheck/Vold_EOS2_synoptic_2021-06-29.csv"), skip = 6)
#change in vaisala from old to new 6/29/2021	11:30:44
OldV <- OldV[,c(1:3,6)]
colnames(OldV) <- c("Date","Time","ppm","point")
OldV$DateTime <- as.POSIXct(paste(OldV$Date, OldV$Time), format = "%m/%d/%Y %I:%M:%S %p", tz = "UTC")
OldV_1 <- subset(OldV, DateTime < as.POSIXct("2021-06-29 11:30:44", tz="UTC"))
OldV_1$vaisala_type <- "GMP222"
OldV_2 <- subset(OldV, DateTime > as.POSIXct("2021-06-29 11:30:44", tz="UTC"))
OldV_2$vaisala_type <- "GMP252_2"

NewV <- read.csv(here::here("/Synoptic/VaisalaCheck/Vnew_EOS1_synoptic_2021-06-29.csv"), skip = 6)
NewV <- NewV[,c(1:3,6)]
colnames(NewV) <- c("Date","Time","ppm","point")
NewV$vaisala_type <- "GMP252_1"
NewV$DateTime <- as.POSIXct(paste(NewV$Date, NewV$Time), format = "%m/%d/%Y %I:%M:%S %p", tz = "UTC")

df_V <- rbind(NewV,OldV_1,OldV_2)
#df_V$point <- as.factor(df_V$point)
df_V$point[is.na(df_V$point)] <- "air"
df_V_0629 <- df_V%>%filter(point=="air")%>%filter(ppm>50)%>%filter(ppm<600)

fig <- plot_ly(df_V #%>%filter(point=="air")%>%filter(ppm>50)%>%filter(ppm<800)
               ,
               x = ~DateTime, y = ~CO2_ppm, size = 1, color = ~Vaisala)
fig

df_V_0629$DateTime <- lubridate::round_date(df_V_0629$DateTime, "5 seconds") 


df_V_0629 <- left_join(df_V_0629,Baro,by="DateTime")

#write.csv(df_V_0629, here::here("/Synoptic/VaisalaCheck/VaisalaAtAltitude.csv",sep=""),
#          row.names = FALSE)

df_V <- read.csv(here::here("/Synoptic/VaisalaCheck/VaisalaAtAltitude.csv"))
df_V$DateTime <- as.POSIXct(df_V$DateTime, format = "%m/%d/%Y %H:%M", tz = "UTC")

fig <- plot_ly(df_V #%>%filter(point=="air")%>%filter(ppm>50)%>%filter(ppm<800)
               ,
               x = ~DateTime, y = ~CO2_ppm, size = 1, color = ~Vaisala)
fig
####6/30/21####
##GAVI
OldV <- read.csv(here::here("/Synoptic/VaisalaCheck/Vold_EOS2_synoptic_2021-06-30.csv"), skip = 6)
#colnames(OldV) <- c("Date","Time","ppm","Tract","Description","Point","lat","lon","ele","WaterSample")
OldV <- OldV[,c(1:3,6)]
colnames(OldV) <- c("Date","Time","ppm","point")
OldV$vaisala_type <- "old"
OldV$DateTime <- as.POSIXct(paste(OldV$Date, OldV$Time), format = "%m/%d/%Y %I:%M:%S %p", tz = "UTC")

NewV <- read.csv(here::here("/Synoptic/VaisalaCheck/Vnew_EOS1_synoptic_2021-06-30.csv"), skip = 6)
NewV <- NewV[,c(1:3,6)]
colnames(NewV) <- c("Date","Time","ppm","point")
NewV$vaisala_type <- "new"
NewV$DateTime <- as.POSIXct(paste(NewV$Date, NewV$Time), format = "%m/%d/%Y %I:%M:%S %p", tz = "UTC")

df_V <- rbind(NewV,OldV)
df_V_0630 <- df_V %>% replace_na(list(point = 'air'))

fig <- plot_ly(df_V_0630%>%filter(point=="air")%>%filter(ppm>50)%>%filter(ppm<800),
               x = ~DateTime, y = ~ppm, size = 1, color = ~vaisala_type)
fig




###7/05/21###
OldV <- read.csv(here::here("/Synoptic/VaisalaCheck/Vold_EOS2_synoptic_2021-07-05.csv"), skip = 6)
#colnames(OldV) <- c("Date","Time","ppm","Tract","Description","Point","lat","lon","ele","WaterSample")
OldV <- OldV[,c(1:3,6)]
colnames(OldV) <- c("Date","Time","ppm","point")
OldV$vaisala_type <- "old"
OldV$DateTime <- as.POSIXct(paste(OldV$Date, OldV$Time), format = "%m/%d/%Y %I:%M:%S %p", tz = "UTC")

NewV <- read.csv(here::here("/Synoptic/VaisalaCheck/Vnew_EOS1_synoptic_2021-07-05.csv"), skip = 6)
NewV <- NewV[,c(1:3,6)]
colnames(NewV) <- c("Date","Time","ppm","point")
NewV$vaisala_type <- "new"
NewV$DateTime <- as.POSIXct(paste(NewV$Date, NewV$Time), format = "%m/%d/%Y %I:%M:%S %p", tz = "UTC")

df_V <- rbind(NewV,OldV)
df_V_0705 <- df_V %>% replace_na(list(point = 'air'))

fig <- plot_ly(df_V_0705%>%filter(point=="air")%>%filter(ppm>50)%>%filter(ppm<800)
               ,
               x = ~DateTime, y = ~ppm, size = 1, color = ~vaisala_type)
fig


###7/06/21###
#switch from ante to colm
OldV <- read.csv(here::here("/Synoptic/VaisalaCheck/Vold_EOS2_synoptic_2021-07-06.csv"), skip = 6)
OldV <- OldV[,c(1:3,6)]
colnames(OldV) <- c("Date","Time","ppm","point")
OldV$vaisala_type <- "old"
OldV$DateTime <- as.POSIXct(paste(OldV$Date, OldV$Time), format = "%m/%d/%Y %I:%M:%S %p", tz = "UTC")

NewV <- read.csv(here::here("/Synoptic/VaisalaCheck/Vnew_EOS1_synoptic_2021-07-06.csv"), skip = 6)
NewV <- NewV[,c(1:3,6)]
colnames(NewV) <- c("Date","Time","ppm","point")
NewV$vaisala_type <- "new"
NewV$DateTime <- as.POSIXct(paste(NewV$Date, NewV$Time), format = "%m/%d/%Y %I:%M:%S %p", tz = "UTC")

df_V <- rbind(NewV,OldV)
df_V_0706 <- df_V %>% replace_na(list(point = 'air'))

fig <- plot_ly(df_V_0706%>%filter(point=="air")%>%filter(ppm>50)%>%filter(ppm<800),
               x = ~DateTime, y = ~ppm, size = 1, color = ~vaisala_type)
fig


###7/07/21###
OldV <- read.csv(here::here("/Synoptic/VaisalaCheck/Vold_EOS2_synoptic_2021-07-07.csv"), skip = 6)
#colnames(OldV) <- c("Date","Time","ppm","Tract","Description","Point","lat","lon","ele","WaterSample")
OldV <- OldV[,c(1:3,6)]
colnames(OldV) <- c("Date","Time","ppm","point")
OldV$vaisala_type <- "old"
OldV$DateTime <- as.POSIXct(paste(OldV$Date, OldV$Time), format = "%m/%d/%Y %I:%M:%S %p", tz = "UTC")


NewV <- read.csv(here::here("/Synoptic/VaisalaCheck/Vnew_EOS1_synoptic_2021-07-07.csv"), skip = 6)
NewV <- NewV[,c(1:3,6)]
colnames(NewV) <- c("Date","Time","ppm","point")
NewV$vaisala_type <- "new"
NewV$DateTime <- as.POSIXct(paste(NewV$Date, NewV$Time), format = "%m/%d/%Y %I:%M:%S %p", tz = "UTC")

df_V <- rbind(NewV,OldV)
df_V_0707 <- df_V %>% replace_na(list(point = 'air'))

fig <- plot_ly(df_V%>%drop_na(point),
               x = ~DateTime, y = ~ppm, size = 1, color = ~vaisala_type)
fig


fig <- plot_ly(df_V_0707%>%filter(point=="air")%>%filter(ppm>50)%>%filter(ppm<400),
               x = ~DateTime, y = ~ppm, size = 1, color = ~vaisala_type)
fig


###7/09/21###
OldV <- read.csv(here::here("/Synoptic/VaisalaCheck/Vold_EOS2_synoptic_2021-07-09.csv"), skip = 6)
#colnames(OldV) <- c("Date","Time","ppm","Tract","Description","Point","lat","lon","ele","WaterSample")
OldV <- OldV[,c(1:3,6)]
colnames(OldV) <- c("Date","Time","ppm","point")
OldV$vaisala_type <- "old"
OldV$DateTime <- as.POSIXct(paste(OldV$Date, OldV$Time), format = "%m/%d/%Y %I:%M:%S %p", tz = "UTC")

NewV <- read.csv(here::here("/Synoptic/VaisalaCheck/Vnew_EOS1_synoptic_2021-07-09.csv"), skip = 6)
#NewV <- NewV[,c(1:3,6)]
colnames(NewV) <- c("Date","Time","ppm")
NewV$vaisala_type <- "new"
NewV$DateTime <- as.POSIXct(paste(NewV$Date, NewV$Time), format = "%m/%d/%Y %I:%M:%S %p", tz = "UTC")
NewV$point <- NA

df_V <- rbind(NewV,OldV)
df_V_0709 <- df_V %>% replace_na(list(point = 'air'))

fig <- plot_ly(df_V_0709%>%filter(point=="air")%>%filter(ppm>50)%>%filter(ppm<800),
               x = ~DateTime, y = ~ppm, size = 1, color = ~vaisala_type)
fig

####all together####
df_V <- rbind(NewV_0618,df_V_0622,df_V_0623,df_V_0629,df_V_0630,df_V_0705,df_V_0706,df_V_0707,df_V_0709)
df_V_GAVI <- rbind(NewV_0618,df_V_0622,df_V_0623,df_V_0629,df_V_0630)
df_V_ANTE <- rbind(df_V_0705,df_V_0706)
df_V_COLM <- rbind(df_V_0706,df_V_0707,df_V_0709)

fig <- plot_ly(df_V_GAVI%>%filter(point=="air")%>%filter(ppm>50)%>%filter(ppm<500),
               x = ~DateTime, y = ~ppm, size = 1, color = ~vaisala_type)
fig

###stats and histograms
stats <- df_V


fig <- plot_ly(df_V_0622%>%filter(point=="air")%>%filter(ppm>50)%>%filter(ppm<500),
               x = ~DateTime, y = ~ppm, size = 1, color = ~vaisala_type)
fig

ggplot(NewV_0618%>%filter(point=="air")%>%filter(ppm>50)%>%filter(ppm<500),
       aes(x=ppm, color=vaisala_type, fill=vaisala_type)) +
  geom_histogram(bins = 10)

ggplot(df_V_0622%>%filter(point=="air")%>%filter(ppm>50)%>%filter(ppm<500),
       aes(x=ppm, color=vaisala_type, fill=vaisala_type)) +
  geom_histogram(bins = 10)

ggplot(df_V_0623%>%filter(point=="air")%>%filter(ppm>50)%>%filter(ppm<500),
       aes(x=ppm, color=vaisala_type, fill=vaisala_type)) +
  geom_histogram(bins = 10)

ggplot(df_V_0629%>%filter(point=="air")%>%filter(ppm>50)%>%filter(ppm<500),
       aes(x=ppm, color=vaisala_type, fill=vaisala_type)) +
  geom_histogram(bins = 10)

ggplot(df_V_0630%>%filter(point=="air")%>%filter(ppm>50)%>%filter(ppm<500),
       aes(x=ppm, color=vaisala_type, fill=vaisala_type)) +
  geom_histogram(bins = 10)

ggplot(df_V_0705%>%filter(point=="air")%>%filter(ppm>50)%>%filter(ppm<500),
       aes(x=ppm, color=vaisala_type, fill=vaisala_type)) +
  geom_histogram(bins = 10)

ggplot(df_V_0706%>%filter(point=="air")%>%filter(ppm>50)%>%filter(ppm<500),
       aes(x=ppm, color=vaisala_type, fill=vaisala_type)) +
  geom_histogram(bins = 10)

ggplot(df_V_0707%>%filter(point=="air")%>%filter(ppm>50)%>%filter(ppm<500),
       aes(x=ppm, color=vaisala_type, fill=vaisala_type)) +
  geom_histogram(bins = 10)

ggplot(df_V_0709%>%filter(point=="air")%>%filter(ppm>50)%>%filter(ppm<500),
       aes(x=ppm, color=vaisala_type, fill=vaisala_type)) +
  geom_histogram(bins = 10)
