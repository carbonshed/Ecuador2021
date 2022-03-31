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
#1. This much carbon is being emitted from this stream reach at this particular time
    ##can I take the average width? or is it more complex than that?
        #does width relate to distance? - it does for COLM, not for the others
          #but let's start with the average to begin

df <- read.csv(here::here("ProcessedData/ALL_synoptic_2022-02-18.csv"))
fluxCO2_summary<- df%>%group_by(Wetland)%>%
  summarise(flux_mean = mean(Flux_ave,na.rm = TRUE),
            flux_median = median(Flux_ave,na.rm = TRUE),
            flux_sd = sd(Flux_ave,na.rm = TRUE),
            CO2_mean = mean(adjusted_ppm,na.rm = TRUE),
            CO2_median = median(adjusted_ppm,na.rm = TRUE),
            CO2_sd = sd(adjusted_ppm,na.rm = TRUE))

df <- read.csv(here::here("ProcessedData/ALL_synoptic_2022-03-24.csv"))
summary <- df%>%group_by(Wetland)%>%
  summarise(max_dist = mean(dist))
#wetland: GAVI ANTE COLM
#mean width: 57.91489  44.20482 123.11972

#wetland      ANTE      COLM      GAVI      GAVItrib1     GAVItrib2
#mean flux: 0.3144167 1.1860937 0.6067742 1.1034615     1.5062500
#max dist : 242.62497 638.80104 563.32217 146.29565   79.23857

summary <- left_join(dist_summary,flux_summary,by="Wetland")
summary <- left_join(summary,width_summary,by="Wetland")
summary$co2_umolpers <- summary$mean_flux * summary$width_mean * 1
molTOmg <- 44*100
summary$CO2_mg_s <- summary$co2_umolpers * 1/100 * molTOmg

summary <- summary[,c("Wetland","CO2_mg_s")]

##
df <- read.csv(here::here("ProcessedData/ALL_synoptic_2022-03-10.csv"))
df <- df%>%filter(Wetland != "GAVItrib1")%>%filter(Wetland != "GAVItrib2")

####point 2####

flux_hist <- ggplot(df%>%drop_na(Flux_ave),
                    aes(x=Flux_ave, color=Wetland, fill=Wetland
                    )) +
  geom_histogram(bins = 30)+ 
  facet_grid(~factor(Wetland, levels=c("ANTE","GAVI","COLM"))) +
  geom_density(color="black",fill="black")+
  theme_classic()+
  geom_text(data    = dat_text,
            mapping = aes(x = x, y = y, label = label))+
  labs( x = "Flux")# +
#  theme(
#    strip.background = element_blank(),
#    strip.text.x = element_blank(),
#    legend.position = "none"  
#  ) 


CO2_hist <- 
  ggplot(df %>%drop_na(adjusted_ppm),
         aes(x=log10(adjusted_ppm),
             color=Wetland, fill=Wetland)) +
  geom_histogram(bins = 15)+ 
  geom_density(color="black",fill="black")+
  facet_grid(~factor(Wetland, levels=c("ANTE","GAVI","COLM"))) +
  theme_classic()+
  labs(x ="log10(CO2)") +
  geom_text(data    = dat_text,
            mapping = aes(x = x, y = y, label = label))#+
#  theme(
#    strip.background = element_blank(),
#    strip.text.x = element_blank(),
#    legend.position = "none"  
#  ) 

plot_grid(CO2_hist, flux_hist, nrow = 2
          #,
          #labels = c("CO2","Flux")#, rel_heights = c(.5, 1)
          )


######point 3 CO2 does not decrease with distance ####
df <- read.csv(here::here("ProcessedData/ALL_synoptic_2022-03-24.csv"))

#significance of co2 and distance
#ANTE: pvalue = .06099, r2 = 0.1361
#GAVI: pvalue < .001, r2 = 0.4349
#COLM: pvalue 0.7155, r2 = -0.03185 
#GaviTrib1 pvalue = 0.216, r2 = 0.05684 
#GaviTrib2 pvalue = 0.589, r2 = 0.148

model <- lm(adjusted_ppm ~ dist, 
            data = df%>%filter(Wetland=="ANTE")
)
summary(model)


fig3 <- plot_ly(data = df%>%filter(Wetland!="GAVItrib2")%>%filter(Wetland!="GAVItrib1")
                , x = ~dist, y = ~adjusted_ppm, 
                color=~Wetland, size=3)

fig3 <- ggplot(data = df%>%filter(Wetland!="GAVItrib2")%>%filter(Wetland!="GAVItrib1"),
               aes(dist,log10(adjusted_ppm), color=Wetland)) +
  geom_point(size=3) +
#  geom_smooth(method=lm, se=FALSE) + 
#  scale_color_discrete(name = "Wetland", labels = c("ANTE; p-value = .1; r2 = 0.1", "COLM; p-value < .001; r2 < .4", "GAVI; p-value < .001; r2= .3")) +
  My_Theme +
#  theme(ylab = "CO2 ppm") +
  theme_classic()
# + theme(legend.position = c(0.2, 0.9))

#####


flux <- ggplot(data=df ) +
  geom_line(aes(dist_ANTE, ele_fit), size = 2, alpha=.5) +
  geom_line(aes(dist_GAVI, ele_fit), size = 2, alpha=.5) +
  geom_line(aes(dist_COLM, ele_fit), size = 2,alpha=.5) +
  geom_point(data=df%>%drop_na(Flux_ave), aes(dist, ele_fit, color= Flux_ave),size=3)+
  scale_color_gradient(
    low = "blue", high = "red",
    space = "Lab",
    na.value = "grey50",
    guide = "colourbar",
    aesthetics = "colour",
    name = "Flux umol m^2 s^-1"
  )+  
  labs(y="elevation", x = "distance")  +
  #  facet_grid(~factor(Wetland, levels=c("ANTE","GAVI","COLM"))) +
  theme_classic() + My_Theme + theme(legend.position = "Top")#+ theme(legend.position = c(0.9, 0.9))

flux

dat_text <- data.frame(
  label = c("A", "A", "B"), color = "black",
  Wetland   = c("ANTE", "GAVI", "COLM"),
  x     = c(3, 3, 3),
  y     = c(12, 12, 12)
)



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

##plot
df$Slope
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