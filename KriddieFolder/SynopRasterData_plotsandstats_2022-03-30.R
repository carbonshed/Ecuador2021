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


#2. Synop_all_raster3
model <- lm(Flux_ave ~ Elevation, 
            data = df#%>%filter(FlowAccu>6000)
)
summary(model)


test <- df%>%filter(FlowAccu>1000)
test$ppm_stnd <- test$adjusted_ppm/3432.5570
test$k600_stnd<- test$K600_effective/706.470986
test$FlowAccu_std<- test$FlowAccu/14598

### compare significance of 
model <- lm(Flux_ave ~ ppm_stnd + FlowAccu_std, 
            data = test%>%filter(FlowAccu>1000)
)
summary(model)

##
#less than 1000
test <- df%>%filter(FlowAccu<1000)
test$ppm_stnd <- test$adjusted_ppm/11174.6524
test$k600_stnd<- test$K600_effective/140.025195	
test$FlowAccu_std<- test$FlowAccu/752


model <- lm(Flux_ave ~ ppm_stnd + FlowAccu_std, 
            data = test%>%filter(FlowAccu<1000)
)
summary(model)

#####################
####RASTER STUFFS####
#####################

#riparian to upland
df <- read.csv(here::here("/Synoptic/Synop_all_raster3.csv"))
raster <- df[,c("Wetland","Wetland_1","Tract","Point","Date","EOS_no","VaisalaType","Flux_ave","AirTemp_c","WaterTemp_c","FlowAccu","Flowlen")]




#df <- read.csv(here::here("ProcessedData/ALL_synoptic_2022-03-24.csv"))
##plot
fig1 <- plot_ly(data = df%>%filter(FlowAccu<1000)#%>%filter(Wetland=="GAVI")
                , x = ~adjusted_ppm, y = ~Flux_ave, 
                color=~Wetland, size=1)


df$Wetland[which(df$Wetland == "GAVItrib3_2")] <- "GAVItrib3_1"


##


ggplot(df%>%filter(FlowAccu<1000),
       #%>%drop_na(adjusted_ppm) #%>% filter(dist<250)
        aes(x=log10(adjusted_ppm), y=Flux_ave)) +
  geom_point(aes(color=Wetland)) + 
  geom_smooth(method='lm') +
  xlab("CO2 ppm") + ylab("CO2 Flux (umol per m2 per s)") +labs(linetype="LEGEND", color="LEGEND") +
  scale_color_discrete(name="LEGEND",labels=c("Antenas", "Gavilan Tributary 1","Gavilan Tributary 2")) +
   scale_x_continuous(breaks=c(log10(300),log10(1000), log10(3000), log10(10000)), labels=c(300,1000,3000,10000)) +
#  scale_color_manual(name="LEGEND",labels=c("blah", "GAVItrib2","GAVItrib3_1","GAVItrib3_2")) +
   theme_bw() 


####

ggplot(df%>% filter(FlowAccu>1000),
       #%>%drop_na(adjusted_ppm) #%>% filter(dist<250)
       aes(x=log10(adjusted_ppm), y=Flux_ave)) +
  geom_point(aes(color=Wetland)) + 
#  geom_smooth(method='lm') +
  xlab("CO2 ppm") + ylab("CO2 Flux (umol per m2 per s)") +labs(linetype="LEGEND", color="LEGEND") +
  scale_color_discrete(name="LEGEND",labels=c("Colmillo", "Gavilan Mainstem","Gavilan Tributary")) +
  scale_x_continuous(breaks=c(log10(300),log10(1000), log10(3000), log10(10000)), labels=c(300,1000,3000,10000)) +
  #  scale_color_manual(name="LEGEND",labels=c("blah", "GAVItrib2","GAVItrib3_1","GAVItrib3_2")) +
  theme_bw() 

###box and wiskers of CO2 and flux -- by accumulation???
###box and wiskers

df$Wetland[which(df$Wetland == "GAVItrib3_1")] <- "Gavilan Tributary"
df$Wetland[which(df$Wetland == "GAVItrib3_2")] <- "Gavilan Tributary"
df$Wetland[which(df$Wetland == "GAVItrib2")] <- "Gavilan Tributary"
df$Wetland[which(df$Wetland == "GAVItrib1")] <- "Gavilan Tributary"

ggplot(df, aes(x = Wetland, y = Flux_ave, fill = Wetland)) + 
  geom_boxplot() +
  stat_summary(fun = "mean", geom = "point", shape = 8,
               size = 2, color = "white")+
  geom_jitter(shape=1,position=position_jitter(0.1)) +
  theme_bw() + theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank())+
    ylab("CO2 Flux (umol per m2 per s)") +
  scale_fill_discrete(name="LEGEND",labels=c("Antenas", "Colmillo","Gavilan mainstem","Gavilan Tributaries"))



ggplot(df, aes(x = Wetland, y = log10(adjusted_ppm), fill = Wetland)) + 
  geom_boxplot() +
  stat_summary(fun = "mean", geom = "point", shape = 8,
               size = 2, color = "white")+
  geom_jitter(shape=1,position=position_jitter(0.1)) +
  theme_bw() + theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank())+
  ylab("CO2 (ppm)") +
  scale_fill_discrete(name="LEGEND",labels=c("Antenas", "Colmillo","Gavilan mainstem","Gavilan Tributaries"))+
  scale_y_continuous(breaks=c(log10(300),log10(1000), log10(3000), log10(10000)), labels=c(300,1000,3000,10000)) 


ggplot(df#%>%filter(FlowAccu<4000)
       ,
       aes(x=FlowAccu*0.00125, y=Flux_ave)) +
  geom_point(aes(color=Wetland)) + 
  ylab("CO2 flux (umole per m2 per sec)") +
  xlab("Catchment size (hectares)") +
  #  geom_smooth(method='lm') +
  #  xlab("CO2 ppm") + ylab("CO2 Flux (umol per m2 per s") +labs(linetype="LEGEND", color="LEGEND") +
    scale_color_discrete(name="LEGEND",labels=c("Antenas", "Colmillo","Gavilan mainstem","Gavilan tributaries")) +
  #  scale_x_continuous(breaks=c(log10(300),log10(1000), log10(3000), log10(10000)), labels=c(300,1000,3000,1000)) +
  #  scale_color_manual(name="LEGEND",labels=c("blah", "GAVItrib2","GAVItrib3_1","GAVItrib3_2")) +
  theme_bw() 


###

###models####
#Factors: Slope, adjusted_ppm, FlowAccu, Flowlen, Elevation
df$adj
#signifianct for everything adjusted_ppm, FlowAccu, Flowlen, Elevation
model <- lm(Flux_ave ~ adjusted_ppm+FlowAccu, 
             data = df%>%filter(FlowAccu<1000)
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

fig1 <- plot_ly(data = df%>%filter(FlowAccu<1000)#%>%filter(Wetland=="GAVI")
                , x = ~adjusted_ppm, y = ~Flux_ave, 
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



fig2 <- plot_ly(data = df#%>%filter(K600_effective<40)
                , x = ~Flowlen, y = ~adjusted_ppm, 
                color=~Wetland, size=3)


fig3 <- plot_ly(data = df#%>%filter(Wetland=="GAVI")
                , x = ~slope, y = ~K600.effective, 
                color=~Wetland, size=3)

fig4 <- plot_ly(data = df#%>%filter(Wetland=="GAVI")
                , x = ~ante_slope, y = ~adjusted_ppm, 
                color=~Wetland, size=3)