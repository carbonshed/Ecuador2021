#Plotting Lux after binding
  #Run LuxBinding first (CarbonShed/Ecuador2021/TessaWorking)

#ggplot2
library(ggplot2)

#plotting individual lux sensors
ggplot(LUXabajo_04_Lux_data, aes(x=DateTime, y=Lux)) +
  geom_point(shape=1)  
