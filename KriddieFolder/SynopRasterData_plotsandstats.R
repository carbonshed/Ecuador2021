#read.csv
library(here)
library(dplyr) # for `rename`
library(tidyr) # for `gather`
library(ggplot2)
library(cowplot)
library(tidyverse)
library(rstatix)
library(ggpubr)
library(plotly)

##stories


#riparian to upland
df <- read.csv(here::here("/Synoptic/Synop_all_raster3.csv"))

##plot
df$Slope

#Factors: Slope, adjusted_ppm, FlowAccu, Flowlen, Elevation

#signifianct for everything adjusted_ppm, FlowAccu, Flowlen, Elevation
model <- lm(Flux_ave ~ Elevation, 
             data = df#%>%filter(FlowAccu>6000)
)
summary(model)

#significant for less than 1000 = adjusted_ppm, FlowAccu, Flowlen, Elevation
model1 <- lm(Flux_ave ~ adjusted_ppm + FlowAccu +Flowlen +Elevation, 
            data = df%>%filter(FlowAccu<1000)
            )
summary(model1)

#Factors: Slope, adjusted_ppm, FlowAccu, Flowlen, Elevation
#significant for more than 1000 =  FlowAccu, Flowlen, Elevation

model2 <- lm(Flux_ave ~ Elevation, 
            data = df%>%filter(FlowAccu>1000)
)
summary(model2)
#Factors: Slope, adjusted_ppm, FlowAccu, Flowlen, Elevation
#significant for more than 1000 =  FlowAccu, Flowlen, Elevation

model2 <- lm(Flux_ave ~ FlowAccu, 
             data = df%>%filter(FlowAccu>1000&FlowAccu<4000)%>%filter(Wetland=="GAVI-mainstem")
)
summary(model2)

#
model3 <- lm(adjusted_ppm ~ FlowAccu, 
             data = df%>%filter(FlowAccu>4000)%>%filter(Wetland=="COLM")
)
summary(model3)
# GAVI Flowlen, FlowAccu
model4 <- lm(adjusted_ppm ~ Slope, 
             data = df%>%filter(FlowAccu>4000)%>%filter(Wetland=="GAVI-mainstem")
)
summary(model4)



#significant FlowAccu>1000&FlowAccu<4000 =  FlowAccu, Flowlen, Elevation

model2 <- lm(Flux_ave ~ Elevation, 
             data = df%>%filter(FlowAccu>1000&FlowAccu<4000)
)
summary(model2)


#significant FlowAccu>9000 =   FlowAccu

model3 <- lm(Flux_ave ~ Elevation, 
             data = df%>%filter(FlowAccu>4000)
)
summary(model3)

#Factors: Slope, adjusted_ppm, FlowAccu, Flowlen, Elevation, CO2_ppm_ave


fig1 <- plot_ly(data = df%>%filter(FlowAccu>4000)#%>%filter(Wetland=="GAVI")
                , x = ~FlowAccu, y = ~Flux_ave, 
                color=~Wetland, size=1)

fig2 <- plot_ly(data = df%>%filter(FlowAccu>4000&FlowAccu<10000)#%>%filter(Wetland=="GAVI")
                , x = ~Slope, y = ~Flux_ave, 
                color=~Wetland, size=1)


fig3 <- plot_ly(data = df%>%filter(FlowAccu>1000)#%>%filter(Wetland=="GAVI")
                , x = ~Elevation, y = ~Flux_ave, 
                color=~Slope, size=1)

fig3 <- plot_ly(data = df#%>%filter(FlowAccu>10000)#%>%filter(Wetland=="GAVI")
                , x = ~Flowlen, y = ~Flux_ave, 
                color=~EOS_no, size=1)


df$EOS_no



fig2 <- plot_ly(data = df#%>%filter(FlowAccu>4000)#%>%filter(Wetland=="GAVI")
                , x = ~Elevation, y = ~Flux_ave, 
                color=~Wetland, size=1)



df$Flowlen
fig2 <- plot_ly(data = df%>%filter(FlowAccu>4000)#%>%filter(Wetland=="GAVI")
                , x = ~Slope*Flowlen, y = ~Flux_ave, 
                color=~Wetland, size=1)

fig2 <- plot_ly(data = df#%>%filter(FlowAccu>4000)#%>%filter(Wetland=="GAVI")
                , x = ~FlowAccu, y = ~Flowlen, 
                color=~Wetland, size=1)



df$Flowlen

fig2 <- plot_ly(data = df#%>%filter(K600_effective<40)
                , x = ~Flowlen, y = ~adjusted_ppm, 
                color=~Wetland, size=3)


fig3 <- plot_ly(data = df#%>%filter(Wetland=="GAVI")
                , x = ~slope, y = ~K600.effective, 
                color=~Wetland, size=3)

fig4 <- plot_ly(data = df#%>%filter(Wetland=="GAVI")
                , x = ~ante_slope, y = ~adjusted_ppm, 
                color=~Wetland, size=3)