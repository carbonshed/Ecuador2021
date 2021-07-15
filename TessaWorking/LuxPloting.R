#Plotting Lux after binding
  #Run LuxBinding first (CarbonShed/Ecuador2021/TessaWorking)

#ggplot2
library(ggplot2)

#plotting individual lux sensors
ggplot(LUXabajo_01_Lux_data, aes(x=DateTime, y=Lux)) + 
    geom_point(shape=1)+
  ggtitle("LUXabajo_01")

ggplot(LUXabajo_02_Lux_data, aes(x=DateTime, y=Lux)) +
  geom_point(shape=1)+
  ggtitle("LUXabajo_02")

ggplot(LUXabajo_03_Lux_data, aes(x=DateTime, y=Lux)) +
  geom_point(shape=1)+
  ggtitle("LUXabajo_03")

ggplot(LUXabajo_04_Lux_data, aes(x=DateTime, y=Lux)) +
  geom_point(shape=1)+
  ggtitle("LUXabajo_04")

ggplot(LUXarriba_01_Lux_data, aes(x=DateTime, y=Lux)) +
  geom_point(shape=1) +
  ggtitle("LUXarriba_01") 

ggplot(LUXarriba_02_Lux_data, aes(x=DateTime, y=Lux)) +
  geom_point(shape=1) +
  ggtitle("LUXarriba_02") 

ggplot(LUXarriba_03_Lux_data, aes(x=DateTime, y=Lux)) +
  geom_point(shape=1)+
  ggtitle("LUXarriba_03") 

ggplot(LUXarriba_04_Lux_data, aes(x=DateTime, y=Lux)) +
  geom_point(shape=1)+
  ggtitle("LUXarriba_04") 

