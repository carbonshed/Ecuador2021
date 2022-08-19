#descriptive stats
library(dplyr)

synop <- read.csv(here::here("ProcessedData/ALL_synoptic_2022-06-08.csv"))
synop <- synop %>% drop_na(Date)

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
