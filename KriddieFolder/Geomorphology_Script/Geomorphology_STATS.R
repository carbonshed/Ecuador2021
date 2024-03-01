#descriptive stats
library(dplyr)

#synop <- read.csv(here::here("ProcessedData/ALL_synoptic_2022-06-08.csv"))
synop_1 <- read_csv(here::here("ProcessedData/ALLSYNOPDATA_FINAL_2024-02-10.csv"))

#total flux for gavi tribs to add to bball plota
Trib1 <- read.csv(here::here("ProcessedData/GAVItrib1_synopticGeom_2022-08-30.csv"))
Trib2 <- read.csv(here::here("ProcessedData/GAVItrib2_synopticGeom_2022-08-30.csv"))

#tribstuff
Trib1$Wetland <- "Tributary 1"
Trib2$Wetland <- "Tributary 2"
Tribs <- rbind(Trib1,Trib2)

synop_1 <- synop_1 %>% drop_na(Date)

synop_2 <- synop_1[order(synop_1$Wetland_5,synop_1$dist, decreasing = FALSE),]

#k600 <- read.csv(here::here("ProcessedData/ALL_synoptic_2022-08-26.csv"))

synop <- synop_2 %>%
  group_by(Wetland_5) %>%
#  arrange(date) %>%
  mutate(dist_diff = dist - lag(dist, default = first(dist)))

summary <- synop %>%
  filter(dist_diff>0)%>%
  filter(dist_diff<275)%>%
 # group_by(Wetland) %>%
  summarize(Mean = mean(dist_diff, na.rm=TRUE),
            Median = median(dist_diff,na.rm=TRUE),
            STDV = sd(dist_diff,na.rm = TRUE),
            Min = min(dist_diff,na.rm=TRUE),
            Max = max(dist_diff, na.rm=TRUE),
            n = n())

summary <- synop %>%
#  filter(dist_diff>0)%>%
  filter(dist_diff<275)%>%
#  group_by(Wetland) %>%
  summarize(Mean = mean(dist_diff, na.rm=TRUE),
            Median = median(dist_diff,na.rm=TRUE),
            STDV = sd(dist_diff,na.rm = TRUE),
            Min = min(dist_diff,na.rm=TRUE),
            Max = max(dist_diff, na.rm=TRUE),
            n = n(),
            Total_Distance = max(dist, na.rm=TRUE))

count <- k600%>%filter(K600.effective>-30) %>% 
  group_by(Wetland) %>%
  drop_na(K600.effective)%>%
  summarize(
            n = n(),
  )


summary_co2 <- synop %>%
  group_by(Wetland_4) %>%
  drop_na(adjusted_ppm)%>%
  summarize(pCO2_mean = mean(adjusted_ppm, na.rm=TRUE),
            pCO2_stdv = sd(adjusted_ppm,na.rm = TRUE),
            pCO2_median = median(adjusted_ppm,na.rm=TRUE),
            pCO2_min = min(adjusted_ppm,na.rm=TRUE),
            pCO2_max = max(adjusted_ppm, na.rm=TRUE),
            
            n = n(),
  )

summary_flux <- synop %>%
  group_by(Wetland_4) %>%
  filter(Flux_ave > -.6) %>%
  drop_na(Flux_ave)%>%
  summarize(flux_mean = mean(Flux_ave, na.rm=TRUE),
            flux_stdv = sd(Flux_ave,na.rm = TRUE),
            flux_median = median(Flux_ave,na.rm=TRUE),
            flux_min = min(Flux_ave,na.rm=TRUE),
            flux_max = max(Flux_ave, na.rm=TRUE),
            
            n = n(),
  )

summary_k600 <- synop %>%filter(K600.effective>-30)%>%
  drop_na(K600.effective)%>%
  group_by(Wetland_4) %>%
  summarize(k600_mean = mean(K600.effective, na.rm=TRUE),
            k600_stdv = sd(K600.effective,na.rm = TRUE),
            k600_median = median(K600.effective,na.rm=TRUE),
            k600_min = min(K600.effective,na.rm=TRUE),
            k600_max = max(K600.effective, na.rm=TRUE),
            
            n = n(),
  )
