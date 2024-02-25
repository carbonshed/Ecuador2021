#this script is to run exploratory stats and figurs for my synop paper
#date: 2024-02-10


#read in data
df<- read_csv(here::here("ProcessedData/ALLSYNOPDATA_FINAL_2024-02-10.csv"))

#total flux for gavi tribs to add to bball plota
Trib1 <- read.csv(here::here("ProcessedData/GAVItrib1_synopticGeom_2022-08-30.csv"))
Trib2 <- read.csv(here::here("ProcessedData/GAVItrib2_synopticGeom_2022-08-30.csv"))

#tribstuff
Trib1$Wetland <- "Tributary 1"
Trib2$Wetland <- "Tributary 2"
Tribs <- rbind(Trib1,Trib2)

#test all
m2 <- lm(Flux_ave ~ scale(adjusted_ppm) + 
           scale(CatchmentSize_ha), 
         data = df %>%filter(flux_umolpers>-.1))
summary(m2)
#summary
df_summaryCO2 <- df%>%
  group_by(Wetland_4)%>%
  summarise(
    CO2_mean = mean(adjusted_ppm, na.rm = TRUE),
    CO2_stdev = sd(adjusted_ppm, na.rm = TRUE),
    CO2_median = median(adjusted_ppm, na.rm = TRUE),
    CO2_max = max(adjusted_ppm, na.rm = TRUE),
    CO2_min = min(adjusted_ppm, na.rm = TRUE),
    n()
  )

df_summaryFlux <- df%>%filter(Flux_ave>-.2)%>%
  group_by(Wetland_4)%>%
  summarise(
    Flux_mean = mean(Flux_ave, na.rm = TRUE),
    Flux_stdev = sd(Flux_ave, na.rm = TRUE),
    Flux_median = median(Flux_ave, na.rm = TRUE),
    Flux_max = max(Flux_ave, na.rm = TRUE),
    Flux_min = min(Flux_ave, na.rm = TRUE),
    n()
  )

df_summaryk600 <- df%>%filter(Flux_ave>-.2)%>%
  group_by(Wetland_4)%>%
  summarise(
    k600_mean = mean(K600.effective, na.rm = TRUE),
    k600_stdev = sd(K600.effective, na.rm = TRUE),
    k600_median = median(K600.effective, na.rm = TRUE),
    k600_max = max(K600.effective, na.rm = TRUE),
    k600_min = min(K600.effective, na.rm = TRUE),
    n()
  )

m1 <- lm(K600.effective ~ log(CatchmentSize_ha), 
         data = df %>%filter(Wetland_2=="Antenas"))
m1 <- lm(K600.effective~ CatchmentSize_ha, 
         data = df%>%filter(Wetland_4=="GAVItrib2"))
m1 <- lm(K600.effective~ CatchmentSize_ha, 
         data = df%>%filter(Wetland_3=="Gavilan River Network"))
m1 <- lm(K600.effective~ log(CatchmentSize_ha), 
         data = df)

summary(m1)

#tukey tests

anova.co2 <- aov(log(adjusted_ppm) ~ Wetland_5, data = df)
tukey.co2<-TukeyHSD(anova.co2)
tukey.co2

anova.flux <- aov(Flux_ave ~ Wetland_5, data = df%>%filter(Flux_ave>-.1))
summary(anova.flux)
tukey.flux<-TukeyHSD(anova.flux)
tukey.flux

anova.k600 <- aov(K600.effective ~ Wetland_5, data = df%>%filter(K600.effective>-30) )
summary(anova.k600)
tukey.k600<-TukeyHSD(anova.k600)
tukey.k600
###LOOK AT THIS IN COMBO WITH SLOPE!
ggplot(data=df%>%filter(K600.effective>-30),aes(x=log(CatchmentSize_ha),y=K600.effective, color=Wetland_4)) + geom_point(size=5)

### bins
df_bins <- df
df_bins$CatchmentSize_ha_log <- log(df_bins$CatchmentSize_ha)
df_bins <- df_bins%>%select(CatchmentSize_ha_log,K600.effective) %>%
  filter(K600.effective > -30) %>%
  mutate(Catchment_bin = cut(CatchmentSize_ha_log, breaks=5)) %>% 
  group_by(Catchment_bin) %>%
  summarise(
    k600_mean = mean(K600.effective, na.rm = TRUE),
    k600_stdev = sd(K600.effective, na.rm = TRUE),
    k600_median = median(K600.effective, na.rm = TRUE),
    k600_max = max(K600.effective, na.rm = TRUE),
    k600_min = min(K600.effective, na.rm = TRUE),
    n()
  )

p<-ggplot(data=df_bins, aes(x=Catchment_bin, y=k600_stdev)) +
  geom_bar(stat="identity", color="black",fill="grey", width=1)
p

p<-ggplot(data=df_bins, aes(x=Catchment_bin, y=k600_mean)) +
  geom_bar(stat="identity", color="black",fill="grey", width=1) +
  geom_point(aes(x=Catchment_bin,y=k600_median),shape=23,size=4,color="black",fill="black")
p



p <- ggplot(df_bins, aes(x=Catchment_bin, y=k600_mean)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=k600_mean-k600_stdev, ymax=k600_mean+k600_stdev), width=.2,
                position=position_dodge(.9))

p3 <- ggplot(df%>%filter(Flux_ave > -.3), aes(x=log(CatchmentSize_ha), y=K600.effective,color=Wetland)) + 
  geom_point(size=5)

#plot river network
rivernetwork <- ggplot(df %>%filter(Wetland_3=="Gavilan River Network"), 
                       aes(x = log(CatchmentSize_ha), y = log10(adjusted_ppm))) + 
  geom_point(size=3) +
  theme_bw() + 
  ylab(expression(paste(italic('p'),"CO"[2] ," [ppm]")))+  xlab("Catchment Size [Hectares]") +
  geom_smooth(method='lm') +
  MyTheme 
