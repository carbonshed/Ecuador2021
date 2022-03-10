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
library(rstatix)
##stories


#riparian to upland
df <- read.csv(here::here("/Synoptic/Synop_all_raster3.csv"))

##plot
df$Slope
###models####
#Factors: Slope, adjusted_ppm, FlowAccu, Flowlen, Elevation

#signifianct for everything adjusted_ppm, FlowAccu, Flowlen, Elevation
model <- lm(Flux_ave ~ Elevation, 
             data = df#%>%filter(FlowAccu>6000)
)
summary(model)

#significant for less than 1000 = adjusted_ppm, FlowAccu, Flowlen, Elevation
model1 <- lm(Flux_ave ~ adjusted_ppm +Elevation, #r2 = .72
            data = df%>%filter(FlowAccu<1000)
            )
summary(model1)
#flux = 7.220e+00 + 1.608e-04*adjusted_ppm + -1.661e-03*Elevation 

#significant for less than 4000 = adjusted_ppm, Elevation == r2=.5
model1 <- lm(Flux_ave ~ adjusted_ppm + Elevation, 
             data = df%>%filter(FlowAccu<4000)
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
###box and wisker####
ggplot(df, aes(x=factor(Wetland), y=Flux_ave,fill=factor(Wetland))) +
  geom_boxplot()+
  stat_summary(fun.y=mean, geom="point", shape=18, size=3, color="white")+
  geom_dotplot(binaxis='y', stackdir='center', dotsize=1,binwidth = .1)

ggplot(df, aes(x=Flux_ave,color=Wetland)) +
  geom_histogram(binwidth=.1,fill="white")+
  facet_wrap("Wetland")
###histograms###
flux_hist <- ggplot(df%>%drop_na(Flux_ave),
                    aes(x=Flux_ave, color=Wetland, fill=Wetland
                    )) +
  geom_histogram(bins = 10)+ 
  facet_grid(~factor(Wetland))

stat <- df%>%drop_na(Flux_ave)%>%filter(Wetland=="COLM"|Wetland=="GAVI-mainstem"|Wetland=="ANTE")
#stat$logFlux <- log10(stat$Flux_ave)
#stat <- stat%>%drop_na(Flux_ave)
stat <- df %>%
#  drop_na(Flux_ave)
  group_by(Wetland) %>%
  summarise(flux_mean = mean(Flux_ave,na.rm=TRUE),
            flux_median = median(Flux_ave,na.rm=TRUE),
            flux_n = n_distinct(Flux_ave,na.rm=TRUE))

a1 <- aov(stat$Wetland ~ stat$Flux_ave) 
summary(a1)
TukeyHSD(a1)

###figures####
#Factors: Slope, adjusted_ppm, FlowAccu, Flowlen, Elevation, CO2_ppm_ave
df$adjusted_ppm
fig1 <- plot_ly(data = df%>%filter(FlowAccu<1000)#%>%filter(Wetland=="GAVI")
                , x = ~7.220e+00 + 1.608e-04*adjusted_ppm + -1.661e-03*Elevation, y = ~Flux_ave, 
                color=~FlowAccu, size=1)

fig1 <- plot_ly(data = df%>%filter(FlowAccu>4000)#%>%filter(Wetland=="GAVI")
                , x = ~FlowAccu, y = ~Flux_ave, 
                color=~Wetland, size=1)

fig2 <- plot_ly(data = df%>%filter(FlowAccu>4000&FlowAccu<10000)#%>%filter(Wetland=="GAVI")
                , x = ~Slope, y = ~Flux_ave, 
                color=~Wetland, size=1)


fig3 <- plot_ly(data = df%>%filter(FlowAccu>1000)#%>%filter(Wetland=="GAVI")
                , x = ~Elevation, y = ~Flux_ave, 
                color=~Slope, size=1)

fig3 <- plot_ly(data = df%>%filter(FlowAccu<500)#
                %>%filter(Wetland=="ANTE")
                , x = ~FlowAccu, y = ~adjusted_ppm, 
                color=~EOS_no, size=1)

df$adjusted_ppm
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