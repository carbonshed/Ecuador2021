##set working directory 
##just made this to take average of weather data stuff 
#since files are big 


#for preciptation 
data <- read.csv("Precipitation_2021-06-01_2021-09-20.csv", header = TRUE, sep = ",")
#data$fecha <- as.POSIXct(data$fecha, format="%m/%d/%Y %H:%M:%S %p", tz="UTC")
data[4993:11157, ]


