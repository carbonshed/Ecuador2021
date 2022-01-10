#About:
#This script is used to compare trends seen in continuous data collected in 2019 with continuous data collected in 2021

library(here)
library(naniar)
library(plotly)
library(ggplot2)

Continuous2021 <- read.csv(here::here("ProcessedData/df_corrected_2022-07-01.csv"))
Continuous2021$DateTime <- as.POSIXct(Continuous2021$DateTime, format="%Y-%m-%d %H:%M:%S", tz = "UTC")
    
#stn2 data is kinda wierd. Lets remove some of it.


Continuous2021<-Continuous2021%>%mutate(adjusted_ppm = case_when(
  Station=="stn_02" & adjusted_ppm > 5000 ~ 0,
  TRUE~adjusted_ppm
))

Continuous2021[Continuous2021 == 0] <- NA


Continuous2019 <- 
  read.csv("C:/Users/whitm/OneDrive - University of North Carolina at Chapel Hill/Ecuador/data_4_analysis/All_Stream_Data.csv")
Continuous2019 <- Continuous2019%>%filter(Inj.x == "No")
Continuous2019 <- Continuous2019[,c("DateTime","V1","V2","V3","V4","V1_adjusted","V2_adjusted","V3_adjusted","V4_adjusted","lvl_421_m","lvl_436_m")]
colnames(Continuous2019) <- c("DateTime","V1","V2","V3","V4","V1_adjusted","V2_adjusted","V3_adjusted","V4_adjusted","lvl_stn3_m","lvl_stn1_m")

Continuous2019 <- Continuous2019 %>%                                         # Apply filter & is.na
  filter(!is.na(V1))
Continuous2019$DateTime <- as.POSIXct(Continuous2019$DateTime, format="%Y-%m-%d %H:%M:%S", tz = "UTC")





#lets plot stn1 for both 2019 and 2021 with water level

#plot stn2 for 2021 with stn3 2019
 

#plot stn1
stn1_fig2019 <- plot_ly(Continuous2019 %>%
          filter(DateTime > as.POSIXct("2019-07-12 12:00:00", tz = "UTC")&
                   DateTime < as.POSIXct("2019-08-14 12:00:00", tz = "UTC"))
          , x = ~DateTime, y= ~V1_adjusted, type = 'scatter', name = 'CO2', mode = 'markers')

stn1_fig2019 <- stn1_fig2019 %>% add_trace(y = ~lvl_stn1_m*10000, name = 'WaterLevel',mode = 'markers')
stn1_fig2019

stn1_fig2021 <- plot_ly(Continuous2021 %>% filter(Station=="stn_01") %>% 
                      filter(DateTime > as.POSIXct("2021-06-12 12:00:00", tz = "UTC")&
                               DateTime < as.POSIXct("2021-07-30 12:00:00", tz = "UTC"))
                    , x = ~DateTime, y= ~adjusted_ppm, type = 'scatter', name = 'CO2', mode = 'markers')

stn1_fig2021 <- stn1_fig2021 %>% add_trace(y = ~WL_m*10000, name = 'WaterLevel',mode = 'markers')
stn1_fig2021




#plot 2021

coeff <- 10000
stn1_fig2021 <- ggplot() + 
  geom_line(data=Continuous2021%>% filter(Station=="stn_01") %>% 
                          filter(DateTime > as.POSIXct("2021-06-12 12:00:00", tz = "UTC")&
                                   DateTime < as.POSIXct("2021-08-12 12:00:00", tz = "UTC")), 
            aes(x=DateTime,  y=adjusted_ppm, linetype = "1"), 
              alpha=1, 
              color="steelblue", size = 1 ) +
    ylim(0, 10000) +
  geom_line(data=Continuous2021%>% filter(Station=="stn_01") %>% 
               filter(DateTime > as.POSIXct("2021-06-12 12:00:00", tz = "UTC")&
                        DateTime < as.POSIXct("2021-08-12 12:00:00", tz = "UTC")), 
             aes(x= DateTime, y= WL_m*coeff, color="darksalmon"), size = 1, alpha=.6) +
   xlab("Date") +
  ylab("CO2 [ppm]") +
  theme(legend.position="none")


coeff <- 10000
stn2_fig2021 <- ggplot() + 
  geom_line(data=Continuous2021%>% filter(Station=="stn_02") %>% 
              filter(DateTime > as.POSIXct("2021-06-12 12:00:00", tz = "UTC")&
                       DateTime < as.POSIXct("2021-08-12 12:00:00", tz = "UTC")), 
            aes(x=DateTime,  y=adjusted_ppm, linetype = "1"), 
            alpha=1, color="steelblue", size = 1 ) +
  ylim(0, 10000) +
  geom_line(data=Continuous2021%>% filter(Station=="stn_01") %>% 
               filter(DateTime > as.POSIXct("2021-06-12 12:00:00", tz = "UTC")&
                        DateTime < as.POSIXct("2021-08-12 12:00:00", tz = "UTC")), 
             aes(x= DateTime, y= WL_m*coeff, color="darksalmon"), size = 1, alpha=.6) +
  xlab("Date") +
  ylab("CO2 [ppm]") +
  theme(legend.position="none")

#plot 2019


coeff <- 10000
stn1_fig2019 <- ggplot() + 
  geom_line(data=Continuous2019 %>% 
              filter(DateTime > as.POSIXct("2019-07-12 12:00:00", tz = "UTC")&
                       DateTime < as.POSIXct("2019-08-14 12:00:00", tz = "UTC")), 
            aes(x=DateTime,  y=V1_adjusted, linetype = "1"), 
            alpha=1, color="steelblue", size = 1 ) +
  ylim(0, 10000) +
  geom_line(data=Continuous2019 %>% 
               filter(DateTime > as.POSIXct("2019-07-12 12:00:00", tz = "UTC")&
                        DateTime < as.POSIXct("2019-08-14 12:00:00", tz = "UTC")), 
             aes(x= DateTime, y= lvl_stn1_m*coeff, color="darksalmon"), size = 1, alpha=.6) +
  xlab("Date") +
  ylab("CO2 [ppm]") +
  theme(legend.position="none")

stn3_fig2019 <- ggplot() + 
  geom_line(data=Continuous2019 %>% 
              filter(DateTime > as.POSIXct("2019-07-12 12:00:00", tz = "UTC")&
                       DateTime < as.POSIXct("2019-08-14 12:00:00", tz = "UTC")), 
            aes(x=DateTime,  y=V3_adjusted, linetype = "1"), 
            alpha=1, 
            color="steelblue", size = 1 ) +
  ylim(0, 10000) +
  geom_line(data=Continuous2019 %>% 
               filter(DateTime > as.POSIXct("2019-07-12 12:00:00", tz = "UTC")&
                        DateTime < as.POSIXct("2019-08-14 12:00:00", tz = "UTC")), 
             aes(x= DateTime, y= lvl_stn3_m*coeff, color="darksalmon"), size = 1, alpha=.6) +
  xlab("Date") +
  ylab("CO2 [ppm]") +
  theme(legend.position="none")


#All figs together

figure <- ggarrange(stn1_fig2019, stn1_fig2021, stn3_fig2019,stn2_fig2021,
                    labels = c("Stn1 2019", "Stn1 2021", "Stn3 2019","Stn2 2021"),
                    ncol = 2, nrow = 2)
figure

###continuous Data 2022 ploted

ggplot(data = Continuous2021%>%filter(Station!='stn_Well01')%>%filter(Station!='stn_Well02')
      # %>%filter(ppm < 15000)
       , aes(DateTime, ppm)) +
  geom_point(color = "steelblue",size=0.5) +
  labs(#title = "CO2  stations",
    y = "NOT ADJUSTED! CO2 ppm", x = "") + 
  facet_wrap(~ Station)

ggplot(data = Continuous2021%>%filter(Station!='stn_Well01')%>%filter(Station!='stn_Well02')%>% 
         filter(DateTime > as.POSIXct("2021-09-30 12:00:00", tz = "UTC")&
                  DateTime < as.POSIXct("2021-12-30 12:00:00", tz = "UTC"))
       # %>%filter(ppm < 15000)
       , aes(DateTime, ppm)) +
  geom_point(color = "steelblue",size=0.5) +
  labs(#title = "CO2  stations",
    y = "NOT ADJUSTED! CO2 ppm", x = "") + 
  facet_wrap(~ Station)

