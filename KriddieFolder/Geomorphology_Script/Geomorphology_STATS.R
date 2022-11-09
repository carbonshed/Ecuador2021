#descriptive stats
library(dplyr)

synop <- read.csv(here::here("ProcessedData/ALL_synoptic_2022-06-08.csv"))

synop <- synop %>% drop_na(Date)

k600 <- read.csv(here::here("ProcessedData/ALL_synoptic_2022-08-26.csv"))

synop <- synop %>%
  group_by(Wetland) %>%
#  arrange(date) %>%
  mutate(dist_diff = dist - lag(dist, default = first(dist)))

summary <- synop %>%
  filter(dist_diff>0)%>%
  filter(dist_diff<275)%>%
  group_by(Wetland) %>%
  summarize(Mean = mean(dist_diff, na.rm=TRUE),
            Median = median(dist_diff,na.rm=TRUE),
            STDV = sd(dist_diff,na.rm = TRUE),
            Min = min(dist_diff,na.rm=TRUE),
            Max = max(dist_diff, na.rm=TRUE),
            n = n()
            )

summary <- synop %>%
#  filter(dist_diff>0)%>%
  filter(dist_diff<275)%>%
  group_by(Wetland) %>%
  summarize(Mean = mean(dist_diff, na.rm=TRUE),
            Median = median(dist_diff,na.rm=TRUE),
            STDV = sd(dist_diff,na.rm = TRUE),
            Min = min(dist_diff,na.rm=TRUE),
            Max = max(dist_diff, na.rm=TRUE),
            n = n(),
            Total_Distance = max(dist, na.rm=TRUE)
  )

count <- k600%>%filter(K600.effective>-30) %>% 
  group_by(Wetland) %>%
  drop_na(K600.effective)%>%
  summarize(
            n = n(),
  )


summary <- synop %>%
  group_by(Wetland) %>%
  filter(Flux_ave > -.6) %>%
  summarize(pCO2_mean = mean(adjusted_ppm, na.rm=TRUE),
            pCO2_stdv = sd(adjusted_ppm,na.rm = TRUE),
            pCO2_median = median(adjusted_ppm,na.rm=TRUE),
            pCO2_min = min(adjusted_ppm,na.rm=TRUE),
            pCO2_max = max(adjusted_ppm, na.rm=TRUE),
            
            flux_mean = mean(Flux_ave, na.rm=TRUE),
            flux_stdv = sd(Flux_ave,na.rm = TRUE),
            flux_median = median(Flux_ave,na.rm=TRUE),
            flux_min = min(Flux_ave,na.rm=TRUE),
            flux_max = max(Flux_ave, na.rm=TRUE),
            
            n = n(),
  )


summary_k600 <- k600 %>%filter(K600.effective>-30)%>%
  group_by(Wetland) %>%
  summarize(k600_mean = mean(K600.effective, na.rm=TRUE),
            k600_stdv = sd(K600.effective,na.rm = TRUE),
            k600_median = median(K600.effective,na.rm=TRUE),
            k600_min = min(K600.effective,na.rm=TRUE),
            k600_max = max(K600.effective, na.rm=TRUE),
            
            n = n(),
  )
