library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

#synoptic discharge
# I would like to calculate average, min, max for each stream while sampling

#first, I need find when each stream was sampled
#second we will average discharge for gavi and colmillo, since we have those loggers installed. 
    #lets use data stored in the "LongTermData_CayambeCoca" project. it'll be easier that way

#GAVI- MAINSTEM 
#6/18/2021 11:00 to 16:00
#6/22/2021 11:20 to 14:30 
#6/23/2021 13:00:00 to 15:15:00

Stn01_2023_03_29 <- read_csv("~/Documents/LongTermData_CayambeCoca/MergedFiles/Stn01_2023-03-29.csv", 
                             col_types = cols(DateTime = col_datetime(format = "%Y-%m-%d %H:%M:%S")))

Stn04_df_2023_12_20 <- read_csv("~/Documents/LongTermData_CayambeCoca/MergedFiles/Stn04_df_2023-12-20.csv", 
                                col_types = cols(DateTime = col_datetime(format = "%Y-%m-%d %H:%M:%S")))

#filter
Stn01_sub1<- Stn01_2023_03_29%>%filter(DateTime > as.POSIXct("2021-06-18 11:30:00",tz="UTC")&
                                        DateTime < as.POSIXct("2021-06-18 16:00:00",tz="UTC"))
Stn01_sub2<- Stn01_2023_03_29%>%filter(DateTime > as.POSIXct("2021-06-22 11:20:00",tz="UTC")&
                                         DateTime < as.POSIXct("2021-06-22 14:30:00",tz="UTC"))
Stn01_sub3<- Stn01_2023_03_29%>%filter(DateTime > as.POSIXct("2021-06-23 13:00:00",tz="UTC")&
                                         DateTime < as.POSIXct("2021-06-23 15:15:00",tz="UTC"))
Stn01 <- rbind(Stn01_sub1,Stn01_sub2,Stn01_sub3)
rm(Stn01_sub1,Stn01_sub2,Stn01_sub3)

p1 <- ggplot(Stn01 , aes(x=DateTime, y=Q_m3s)) + geom_point()

Stn04_sub1<- Stn04_df_2023_12_20%>%filter(DateTime > as.POSIXct("2021-06-18 11:30:00",tz="UTC")&
                                         DateTime < as.POSIXct("2021-06-18 16:00:00",tz="UTC"))
Stn04_sub2<- Stn04_df_2023_12_20%>%filter(DateTime > as.POSIXct("2021-06-22 11:20:00",tz="UTC")&
                                         DateTime < as.POSIXct("2021-06-22 14:30:00",tz="UTC"))
#Stn04_sub3<- Stn04_df_2023_12_20%>%filter(DateTime > as.POSIXct("2021-06-23 13:00:00",tz="UTC")&
#                                         DateTime < as.POSIXct("2021-06-23 15:15:00",tz="UTC"))

Stn04 <- rbind(Stn04_sub1,Stn04_sub2)
rm(Stn04_sub1,Stn04_sub2)

p2 <- ggplot(Stn04 , aes(x=DateTime, y=Q_m3s)) + geom_point()

sum04 <- summary(Stn04%>%select(Q_m3s))
sum01 <- summary(Stn01%>%select(Q_m3s))


#GAVI - TRIBS
#6/22/2021 13:40:00 to 15:50:00
#6/23/2021 10:30:00 to 15:30:00
#6/29/2021 09:55:00 to 14:20:00
#6/30/2021 10:00:00 to 13:00

#Atenas
#7/5/2021 13:00:00 to 14:30
#7/6/2021 10:30:00 to 12:20:00

#COLMILLO
#7/6/2021 14:05 to 15:45
#7/7/2021 10:30:00 to 14:10:00
#7/9/2021 12:55:00 to 14:40:00

Stn05_df_2023_12_19 <- read_csv("~/Documents/LongTermData_CayambeCoca/MergedFiles/Stn05_df_2023-12-19.csv", 
                                col_types = cols(DateTime = col_datetime(format = "%Y-%m-%d %H:%M:%S")))
Stn06_df_2023_12_19 <- read_csv("~/Documents/LongTermData_CayambeCoca/MergedFiles/Stn06_df_2023-12-19.csv", 
                                col_types = cols(DateTime = col_datetime(format = "%Y-%m-%d %H:%M:%S")))

Stn05_sub1<- Stn05_df_2023_12_19%>%filter(DateTime > as.POSIXct("2021-07-06 14:05:00",tz="UTC")&
                                            DateTime < as.POSIXct("2021-07-06 15:45:00",tz="UTC"))
Stn05_sub2<- Stn05_df_2023_12_19%>%filter(DateTime > as.POSIXct("2021-07-07 10:30:00",tz="UTC")&
                                            DateTime < as.POSIXct("2021-07-07 14:10:00",tz="UTC"))
Stn05_sub3<- Stn05_df_2023_12_19%>%filter(DateTime > as.POSIXct("2021-07-09 12:55:00",tz="UTC")&
                                            DateTime < as.POSIXct("2021-07-09 14:40:00",tz="UTC"))
Stn05 <- rbind(Stn05_sub1,Stn05_sub2,Stn05_sub3)
rm(Stn05_sub1,Stn05_sub2,Stn05_sub3)

Stn06_sub1<- Stn06_df_2023_12_19%>%filter(DateTime > as.POSIXct("2021-07-06 14:05:00",tz="UTC")&
                                            DateTime < as.POSIXct("2021-07-06 15:45:00",tz="UTC"))
Stn06_sub2<- Stn06_df_2023_12_19%>%filter(DateTime > as.POSIXct("2021-07-07 10:30:00",tz="UTC")&
                                            DateTime < as.POSIXct("2021-07-07 14:10:00",tz="UTC"))
Stn06_sub3<- Stn06_df_2023_12_19%>%filter(DateTime > as.POSIXct("2021-07-09 12:55:00",tz="UTC")&
                                            DateTime < as.POSIXct("2021-07-09 14:40:00",tz="UTC"))
Stn06 <- rbind(Stn06_sub1,Stn06_sub2,Stn06_sub3)
rm(Stn06_sub1,Stn06_sub2,Stn06_sub3)

p5 <- ggplot(Stn05 , aes(x=DateTime, y=Q_m3s)) + geom_point()
p6 <- ggplot(Stn06 , aes(x=DateTime, y=Q_m3s)) + geom_point()

sum06 <- summary(Stn06%>%select(Q_m3s))

