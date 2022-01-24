## In this Skript we combine all data that we might need for
#correcting viaslas during synoptic sampling events

#we need:
#barometric pressure
#water temp from all wl

colnames(WL_01) <- c("DateTime","stn1_WLPress_kpa","stn1_WLTemp_c","Station")
WL_01$Station <- NULL
colnames(WL_02) <- c("DateTime","stn2_WLPress_kpa","stn2_WLTemp_c","Station")
WL_02$Station <- NULL
colnames(WL_03) <- c("DateTime","stn3_WLPress_kpa","stn3_WLTemp_c","Station")
WL_03$Station <- NULL
colnames(WL_04) <- c("DateTime","stn4_WLPress_kpa","stn4_WLTemp_c","Station")
WL_04$Station <- NULL
colnames(WL_05) <- c("DateTime","stn5_WLPress_kpa","stn5_WLTemp_c","Station")
WL_05$Station <- NULL
colnames(WL_06) <- c("DateTime","stn6_WLPress_kpa","stn6_WLTemp_c","Station")
WL_06$Station <- NULL

data <- full_join(WL_01,WL_02,by="DateTime")
data <- full_join(data,WL_03,by="DateTime")
data <- full_join(data,WL_04,by="DateTime")
data <- full_join(data,WL_05,by="DateTime")
data <- full_join(data,WL_06,by="DateTime")

data <- full_join(data,Baro, by="DateTime")

data <- data%>%filter(DateTime > as.POSIXct("2021-06-18 08:00:00", tz = "UTC")&
                        DateTime < as.POSIXct("2021-07-09 20:00:00", tz = "UTC"))


write.csv(data, here::here("Synoptic/ContinuousData_forSynop_2022-01-16.csv"))
