library(plotly)
library(lubridate)




new_press<- 700
new_temp <- 6.0
old_press <- 1013
old_temp <- 25

new <- 344
old <- 376
AirTemp <- 7.2
Pressure <- 628

old*(1+(old_press-Pressure)*0.0015)*(1-(old_temp-AirTemp)*0.003)

new*(1+(new_press-Pressure)*0.0015)*(1-(new_temp-AirTemp)*0.005)
  




CO2_June22$DateTime <- as.POSIXct(paste(CO2_June22$Date, CO2_June22$Time), format = "%m/%d/%Y %I:%M:%S %p", tz = "UTC")

CO2_June18$DateTime <- as.POSIXct(paste(CO2_June18$Date, CO2_June18$Time), format = "%m/%d/%Y %I:%M:%S %p", tz = "UTC")
CO2_June23$DateTime <- as.POSIXct(paste(CO2_June23$Date, CO2_June23$Time), format = "%m/%d/%Y %I:%M:%S %p", tz = "UTC")
CO2_June29$DateTime <- as.POSIXct(paste(CO2_June29$Date, CO2_June29$Time), format = "%m/%d/%Y %I:%M:%S %p", tz = "UTC")
CO2_June30$DateTime <- as.POSIXct(paste(CO2_June30$Date, CO2_June30$Time), format = "%m/%d/%Y %I:%M:%S %p", tz = "UTC")

CO2_June30$DateTime <- as.POSIXct(paste(CO2_June30$Date, CO2_June30$Time), format = "%m/%d/%Y %I:%M:%S %p", tz = "UTC")
CO2_June30$DateTime <- as.POSIXct(paste(CO2_June30$Date, CO2_June30$Time), format = "%m/%d/%Y %I:%M:%S %p", tz = "UTC")

CO2_July6$DateTime <- as.POSIXct(paste(CO2_July6$Date, CO2_July6$Time), format = "%m/%d/%Y %I:%M:%S %p", tz = "UTC")
CO2_July7$DateTime <- as.POSIXct(paste(CO2_July7$Date, CO2_July7$Time), format = "%m/%d/%Y %I:%M:%S %p", tz = "UTC")
CO2_eos2_July9$DateTime <- as.POSIXct(paste(CO2_eos2_July9$Date, CO2_eos2_July9$Time), format = "%m/%d/%Y %I:%M:%S %p", tz = "UTC")



###
CO2_synop <- CO2_eos2_July9
CO2_synop$DateTime <- as.POSIXct(paste(CO2_synop$Date, CO2_synop$Time), format="%m/%d/%Y %I:%M:%S %p")
CO2_synop$Date <- as.Date(CO2_synop$Date, format="%m/%d/%Y" )
#CO2_synop$DateTime <- round_date(CO2_synop$DateTime, unit = "15 minute")
CO2_synop$Time <- NULL

#CO2_synop <- CO2_synop%>%drop_na(CO2_ppm)
#CO2_synop$CO2_ppm_new <- NA
CO2_synop$CO2_ppm_new[CO2_synop$VaisalaType=="old"] <- CO2_synop$CO2_ppm[CO2_synop$VaisalaType=="old"]
CO2_synop$CO2_ppm_new[CO2_synop$VaisalaType=="new"] <- CO2_synop$CO2_ppm[CO2_synop$VaisalaType=="new"]*2

fig1 <- plot_ly(CO2_synop#%>%drop_na(Point)
                ,
                x = ~DateTime, y = ~CO2_ppm_new, size = 1, color = ~VaisalaType)
fig1

###### continuous data
#read in continuous data to correct vaisala
ContinuousData <-  read.csv(here::here("/Synoptic/ContinuousData_forSynop_2022-01-27.csv"), skip=0, header = TRUE, sep = ",",
                            quote = "\"",dec = ".", fill = TRUE, comment.char = "")
ContinuousData$X <- NULL
ContinuousData$DateTime <- as.POSIXct(ContinuousData$DateTime,  format="%Y-%m-%d %H:%M:%S", tz = "UTC")

ContinuousData$Total_hPa <- ContinuousData$Baro_kpa * 10 + 1*2.4884




#adjust vaisala for temp and pressure
CO2_synop <- left_join(CO2_synop,ContinuousData,by="DateTime")

old <- CO2_synop[CO2_synop$VaisalaType == "old", ]
new <- CO2_synop[CO2_synop$VaisalaType == "new", ]


old$adjusted_ppm <- 
  old$CO2_ppm * (1 + (1013 - old$Total_hPa) * 0.0015) *
  (1 - (25 - old$GAVI_waterTempAve) * 0.003)

new$adjusted_ppm <- 
  new$CO2_ppm * (1 + (700 - new$Total_hPa) * 0.0015) * 
  (1 - (6.7 - new$GAVI_waterTempAve) * 0.005)

CO2_synop_2 <- rbind(old,new)

fig1 <- plot_ly(CO2_synop_2#%>%drop_na(point)
                ,
                x = ~DateTime, y = ~CO2_ppm, size = 1, color = ~VaisalaType)
fig1

fig2 <- plot_ly(CO2_synop_2#%>%drop_na(point)
                ,
                x = ~DateTime, y = ~adjusted_ppm, size = 1, color = ~VaisalaType)
fig2

