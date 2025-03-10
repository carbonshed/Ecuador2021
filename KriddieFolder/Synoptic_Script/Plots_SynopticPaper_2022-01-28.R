###Synoptic Plots
#Kriddie Whitmore
#2022-01-28

#this script is to produce plot for my synoptic paper.
#there is a catch, in wich I am uncertain about the method I am using to calibrate my 
#vaisalas. SO this is code for the plots, I might simply need to change the data source
library(here)
library(dplyr) # for `rename`
library(tidyr) # for `gather`
library(ggplot2)
library(cowplot)
library(tidyverse)
library(rstatix)
library(ggpubr)

#read in df
ANTE <- read.csv(here::here("/ProcessedData/ANTE_synoptic_2022-01-27.csv"))
GAVI <- read.csv(here::here("/ProcessedData/GAVI_synoptic_2022-01-27.csv"))
COLM <- read.csv(here::here("/ProcessedData/COLMILLO_synoptic_2022-01-27.csv"))

#MIGHT WANT TIME LATER
#ANTE <- ANTE[,c("lon_fit","lat_fit","ele_fit","dist","time","Date","EOS_no","Flux_ave","CO2_ppm_ave","adjusted_ppm")]
#COLM <- COLM[,c("lon_fit","lat_fit","ele_fit","dist","time","Date","EOS_no","Flux_ave","CO2_ppm_ave","adjusted_ppm")]
#GAVI <- GAVI[,c("lon_fit","lat_fit","ele_fit","dist","time","Date","EOS_no","Flux_ave","CO2_ppm_ave","adjusted_ppm")]

ANTE <- ANTE[,c("lon_fit","lat_fit","ele_fit","dist","Date","EOS_no","Flux_ave","CO2_ppm_ave","adjusted_ppm")]
COLM <- COLM[,c("lon_fit","lat_fit","ele_fit","dist","Date","EOS_no","Flux_ave","CO2_ppm_ave","adjusted_ppm")]
GAVI <- GAVI[,c("lon_fit","lat_fit","ele_fit","dist","Date","EOS_no","Flux_ave","CO2_ppm_ave","adjusted_ppm")]

#Remove unique
ANTE <- unique(ANTE)
GAVI <- unique(GAVI)
COLM <- unique(COLM)

GAVI$Wetland <- "GAVI"
ANTE$Wetland <- "ANTE"
COLM$Wetland <- "COLM"


df <- rbind(GAVI,ANTE,COLM)

##plot

##Flux

flux <- ggplot(data=df ) +
  geom_line(aes(dist, ele_fit), size = 2, #color="brown",
             alpha=.5) +
  geom_point(data=df%>%drop_na(Flux_ave), aes(dist, ele_fit, color= Flux_ave),size=3)+
  scale_color_gradient(
    low = "blue",
    high = "red",
    space = "Lab",
    na.value = "grey50",
    guide = "colourbar",
    aesthetics = "colour",
    name = "Flux umol m^2 s^-1"
  )+  
  labs(y="elevation", x = "distance")  +
  facet_grid(~factor(Wetland, levels=c("ANTE","GAVI","COLM"))) +
  theme_classic()

dat_text <- data.frame(
  label = c("A", "A", "B"), color = "black",
  Wetland   = c("ANTE", "GAVI", "COLM"),
  x     = c(3, 3, 3),
  y     = c(12, 12, 12)
)


flux_hist <- ggplot(df%>%drop_na(Flux_ave),
                    aes(x=Flux_ave, color=Wetland, fill=Wetland
                        )) +
  geom_histogram(bins = 30)+ 
  facet_grid(~factor(Wetland, levels=c("ANTE","GAVI","COLM"))) +
  geom_density(color="black",fill="black")+
  theme_classic()+
  geom_text(data    = dat_text,
    mapping = aes(x = x, y = y, label = label))+
  labs( x = "Flux") +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    legend.position = "none"  
  ) 


##stats
stat <- df%>%drop_na(Flux_ave)
stat$logFlux <- log10(stat$Flux_ave)
stat <- stat%>%drop_na(logFlux)%>%filter(logFlux != "-Inf")
a1 <- aov(stat$Wetland ~ stat$Flux_ave) 
summary(a1)
TukeyHSD(a1)

df %>%
  group_by(Wetland) %>%
  get_summary_stats(Flux_ave, type = "mean_sd")

res.aov <- df %>% anova_test(Flux_ave ~ Wetland)
res.aov


# Pairwise comparisons
pwc <- df %>%
  pairwise_t_test(Flux_ave ~ Wetland, p.adjust.method = "bonferroni")
pwc


##
top_row <-plot_grid(NULL,flux_hist, NULL, nrow = 1, labels = c("","",""),
                    rel_widths = c(.05,1, .35))

plot_grid(top_row, flux, nrow = 2,
          labels = c("A","B"), 
          rel_heights = c(.5, 1))

###CO2####

CO2 <- ggplot(data=df ) +
  geom_line(aes(dist, ele_fit), size = 2, #color="brown",
            alpha=.5) +
  geom_point(data=df%>%drop_na(adjusted_ppm), 
             aes(dist, ele_fit, 
                 color= log10(adjusted_ppm)),size=3)+
  scale_color_gradient(
    low = "blue",
    high = "red",
    space = "Lab",
    na.value = "grey50",
    guide = "colourbar",
    aesthetics = "colour",
    name = "log10(CO2 ppm)"
  )+  
  labs(y="elevation", x = "distance")  +
  facet_grid(~factor(Wetland, levels=c("ANTE","GAVI","COLM"))) +
  theme_classic()


dat_text <- data.frame(
  label = c("A", "A", "B"), color = "black",
  Wetland   = c("ANTE", "GAVI", "COLM"),
  x     = c(3.5, 3.5, 3.5),
  y     = c(20, 20, 20)
)

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
            mapping = aes(x = x, y = y, label = label))+
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    legend.position = "none"  
  ) 

##
top_row <-plot_grid(NULL,CO2_hist, NULL, nrow = 1, labels = c("","",""),
                    rel_widths = c(.04,1, 0.3))

plot_grid(top_row, CO2, nrow = 2,
          labels = c("A","B"), 
          rel_heights = c(.5, 1))

##stats
##stats
stat <- df%>%drop_na(Flux_ave)
stat$logCO2 <- log10(stat$adjusted_ppm)
a1 <- aov(stat$Wetland ~ stat$logCO2) 
summary(a1)
TukeyHSD(a1)

df %>%
  group_by(Wetland) %>%
  get_summary_stats(adjusted_ppm, type = "mean_sd")

res.aov <- df %>% anova_test(log10(adjusted_ppm) ~ Wetland)
res.aov


# Pairwise comparisons
pwc <- df %>%
  pairwise_t_test(adjusted_ppm ~ Wetland, p.adjust.method = "bonferroni")
pwc


  
 
 ####DOC###
 
###width###

GAVI_width <- read.csv(here::here("Geomorphology/Gavilan/GAVImainstem_Geomorph_2022-01-28.csv"))
ANTE_width <- read.csv(here::here("Geomorphology/Atenas/Antenas_Geomorph_2022-01-28.csv"))
COLM_width <- read.csv(here::here("Geomorphology/Colmillo/COLM_geomorph_2022-01-28.csv"))

COLM_width <- COLM_width[,1:3]
COLM_width$dist <- as.numeric(COLM_width$x)
COLM_width$width <- as.numeric(COLM_width$w)
COLM_width$depth <- as.numeric(COLM_width$d)
COLM_width$Wetland <- "COLM"
COLM_width <- COLM_width%>%select(Wetland, dist,width,depth)

GAVI_width <- GAVI_width[,1:4]
colnames(GAVI_width) <- c("Wetland","dist","width","depth")
GAVI_width$dist <- as.numeric(GAVI_width$dist)
GAVI_width$width <- as.numeric(GAVI_width$width)
GAVI_width$depth <- as.numeric(GAVI_width$depth)
GAVI_width <- GAVI_width%>%select(Wetland, dist,width,depth)

ANTE_width$dist <- as.numeric(ANTE_width$x)
ANTE_width$width <- as.numeric(ANTE_width$w)
ANTE_width$depth <- as.numeric(ANTE_width$d)
ANTE_width <- ANTE_width%>%select(Wetland, dist,width,depth)


width_df <- rbind(GAVI_width,ANTE_width,COLM_width)

##plot


Width_plot <- ggplot(data=width_df ) +
  geom_line(aes(dist, width), size = 2, #color="brown", alpha=.5
            ) +
  labs(y="width", x = "distance")  +
  facet_grid(~factor(Wetland, levels=c("ANTE","GAVI","COLM"))) +
  theme_classic()

##

dat_text <- data.frame(
  label = c("A", "B", "C"),
  Wetland   = c("ANTE", "GAVI", "COLM"),
  x     = c(2.5,2.5, 2.5),
  y     = c(30, 30, 30)
)

width_hist <- 
  ggplot(width_df %>%drop_na(width), 
         aes(x=log10(width), color=Wetland, fill=Wetland)) +
   geom_histogram(bins = 20)+ 
  geom_density(color="black",fill="black")+
  facet_grid(~factor(Wetland, levels=c("ANTE","GAVI","COLM"))) +
  theme_classic()+
  geom_text(data    = dat_text,
            mapping = aes(x = x, y = y, label = label)) +
  
  labs(x ="log10(width)") +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    legend.position = "none") 


##stats
width_df %>%
  group_by(Wetland) %>%
  get_summary_stats(width, type = "mean_sd")

res.aov <- width_df %>% anova_test(width ~ Wetland)
res.aov

width_df$logwidth <- log10(width_df$width)
a1 <- aov(width_df$logwidth ~ width_df$Wetland) 
summary(a1)
TukeyHSD(a1)

# Pairwise comparisons
pwc <- width_df %>%
  pairwise_t_test(width ~ Wetland, p.adjust.method = "bonferroni")
pwc


##
top_row <-plot_grid(NULL,width_hist, NULL, nrow = 1, labels = c("","",""),
                  rel_widths = c(.1,1, .5))

plot_grid(top_row, CO2, nrow = 2,
          labels = c("A","B"), 
          rel_heights = c(.5, 1)
)


##DOC
#missing waypoints here and there
WaterChem <- read.csv(here::here("WaterChem/WaterChem_synoptic_2022-01-28.csv"))
ANTE_waterchem <- read.csv(here::here("/ProcessedData/ANTE_WaterChem_synop_2022-01-29.csv"))

##plot

waterchem_plot <- ggplot(data=ANTE_waterchem ) +
  geom_point(aes(dist, ele_fit, color=DOC), size = 2, #color="brown", alpha=.5
  ) +
  labs(y="elevation", x = "distance")  +
#  facet_grid(~factor(Wetland, levels=c("ANTE","GAVI","COLM"))) +
  theme_classic()



WaterChem_hist <- ggplot(WaterChem %>%drop_na(DOC)%>%filter(Wetland != "GAVI")
                     , aes(x=DOC, color=Wetland, fill=Wetland)) +
    geom_histogram(bins = 15)+ 
  geom_density(color="black",fill="black")+
  facet_grid(~factor(Wetland, levels=c("ANTE","GAVI-m","COLM"))) +
  theme_classic()+
#  geom_text(data    = dat_text,
#            mapping = aes(x = x, y = y, label = label)) +
  
  labs(x ="DOC mg/L") + 
  theme(legend.position = "none")

#  theme(
#    strip.background = element_blank(),
#    strip.text.x = element_blank(),
#    legend.position = "none"  
#  ) 


##stats
WaterChem %>%filter(Wetland != "GAVI") %>%
  group_by(Wetland) %>%
  get_summary_stats(DOC, type = "mean_sd")

res.aov <- WaterChem %>% anova_test(DOC ~ Wetland)
res.aov


# Pairwise comparisons
pwc <- WaterChem%>%filter(Wetland != "GAVI") %>%
  pairwise_t_test(DOC ~ Wetland, p.adjust.method = "bonferroni")
pwc
##
top_row <-plot_grid(NULL,WaterChem, NULL, nrow = 1, labels = c("","",""),
                    rel_widths = c(.1,1, .5))

plot_grid(top_row, CO2, nrow = 2,
          labels = c("A","B"), 
          rel_heights = c(.5, 1)
)


WaterChem_hist <- ggplot(WaterChem %>%drop_na(TDN)%>%filter(Wetland != "GAVI")
                         , aes(x=log10(TDN), color=Wetland, fill=Wetland)) +
  geom_histogram(bins = 5)+ 
  geom_density(color="black",fill="black")+
  facet_grid(~factor(Wetland, levels=c("ANTE","GAVI-m","COLM"))) +
  theme_classic()+
  #  geom_text(data    = dat_text,
  #            mapping = aes(x = x, y = y, label = label)) +
  
  labs(x ="TDN mg/L")  + 
  theme(legend.title = element_blank())

#  theme(
#    strip.background = element_blank(),
#    strip.text.x = element_blank(),
#    legend.position = "none"  
#  ) 




##plot

plot_grid(WaterChem_hist, width_hist, nrow = 2,
          labels = c("A","B"), 
          rel_heights = c(1, 1)
)
