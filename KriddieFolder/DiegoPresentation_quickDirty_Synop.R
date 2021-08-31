##this R script is going to be to make exploratory figs that Diego can use for his presentation
library(ggplot2)
library(ggforce)
#read it in baby


ANTE <-  read.csv(here::here("/synoptic/ANTE_edit_2021-08-27.csv"), skip=0, header = TRUE, sep = ",",
                                na.strings=c("","NA"), quote = "\"",dec = ".", fill = TRUE, comment.char = "")[,c(2:12)]


#####ORder by point
ANTE <- ANTE[order(ANTE[,"SynopID"], decreasing = FALSE),]

##RUN THAT LOOOOOOOOOOOOOOOOOOOOOOop

###this works. save it.

df <- data.frame(0,0,0)
colnames(df) <- c("SynopID","dist_dif","dist_total")

count = 0
for (i in 1:nrow(ANTE)){

  df[0,2] <- 0 

  df[i,"SynopID"] <- ANTE[i,"SynopID"]
  dist_dif <- distm(c(ANTE[i,"lon"], ANTE[i,"lat"]), c(ANTE[i+1,"lon"], ANTE[i+1,"lat"]), fun = distHaversine)/2
  df[i+1,"dist_dif"] <- dist_dif
  df[i+1,"dist_total"] <- dist_dif + df[i,"dist_total"]
}


#join synop data with distance canculations
ANTE <- full_join(ANTE,df, by = "SynopID")


##plot

ANTE_plot <-  ggplot(ANTE) + geom_point(aes(SynopID, ele, color = Flux_ave))
ANTE_plot <-  ggplot(ANTE) + geom_point(aes(dist_total, ele, color = CO2_ppm_ave))



##whatever, just plot it. skip everything
ANTE <-  read.csv(here::here("/synoptic/ANTE_edit_2021-08-27.csv"), skip=0, header = TRUE, sep = ",",
                  na.strings=c("","NA"), quote = "\"",dec = ".", fill = TRUE, comment.char = "")


ANTE_co2 <-  ggplot(ANTE) + geom_point(aes(dist_m, ele_estimate, color = CO2_ppm_ave))+
  scale_color_gradient(low="blue", high="red")
ANTE_flux <-  ggplot(ANTE) + geom_point(aes(dist_m, ele_estimate, color = log10(Flux_ave)))+
  scale_color_gradient(low="blue", high="red")
ANTE_eco <-  ggplot(ANTE) + geom_point(aes(dist_m, ele_estimate, color = notes))


ANTE_flux <-  ggplot(ANTE) +
  geom_ellipse(aes(x0 = 40, y0 = 4271, a = 20, b = 3, angle = pi / 30), fill = "Green") +
  geom_ellipse(aes(x0 = 225, y0 = 4300, a = 20, b = 2, angle = pi / 1), fill = "Green") +
  geom_ellipse(aes(x0 = 360, y0 = 4310, a = 65, b = 5, angle = pi / 30), fill = "Green") +
  geom_line(aes(dist_m, ele_estimate, color = log10(Flux_ave)), size=3) +
  scale_color_gradient(low="blue", high="red", breaks = c(0, -0.5, -1.0, -1.5, -2.0), labels = c("1.0", "0.32", "0.1","0.032","0.01")) +
  labs(color="CO2 Flux") + xlab("Distance") + ylab("Elevation") +
  theme_bw() +
  ggtitle("ATENAS - CO2 Evasion")


ANTE_co2 <-  ggplot(ANTE) +
  geom_ellipse(aes(x0 = 40, y0 = 4271, a = 20, b = 3, angle = pi / 30), fill = "Green") +
  geom_ellipse(aes(x0 = 225, y0 = 4300, a = 20, b = 2, angle = pi / 1), fill = "Green") +
  geom_ellipse(aes(x0 = 360, y0 = 4310, a = 65, b = 5, angle = pi / 30), fill = "Green") +
  geom_line(aes(dist_m, ele_estimate, color = log10(CO2_ppm_ave)), size=3) +
  scale_color_gradient(low="blue", high="red", breaks = c(3.5, 3.0, 2.5), labels = c("3162", "1000", "316")) +
  labs(color="pCO2") + xlab("Distance") + ylab("Elevation") +
#  geom_point(aes(dist_m, ele_estimate-5, shape = notes)) +
  theme_bw() +
  ggtitle("ATENAS - pCO2")


circles <- data.frame(
  x0 = rep(1:3, 3),
  y0 = rep(1:3, each = 3),
  r = seq(0.1, 1, length.out = 9)
)

ggplot() +
  geom_ellipse(aes(x0 = 0, y0 = 0, a = 10, b = 3, angle = pi / 4)) +
  coord_fixed()
ggplot() +
  geom_circle(aes(x0 = x0, y0 = y0, r = r, fill = r), data = circles)


####### GAVI Mainstem

GAVI <-  read.csv(here::here("/synoptic/GAVI_mainstem_2021-08-29Edit_ShrotScrpaayDONOTUSE.csv"), skip=0, header = TRUE, sep = ",",
                  na.strings=c("","NA"), quote = "\"",dec = ".", fill = TRUE, comment.char = "")


Flux_map <- qmplot(lon, lat, data = GAVI, zoom = 13,  maptype = "toner-background", color = CO2_ppm_ave)+
  scale_color_gradient(low="blue", high="red")

GAVI$Date.as.fact <- as.factor(GAVI$Date)
GAVI$Date <- as.Date(GAVI$Date, format = "%m/%d/%Y")
GAVI_sub <- subset(GAVI, Date > "2021-06-28")


Troubleshoot <- qmplot(Lon, Lat, data = subset(GAVI, Date == "2021-06-30"), zoom = 13,  maptype = "toner-background", 
                       color = CO2_ppm_ave, shape = Date.as.fact)+
  scale_color_gradient(low="blue", high="red")

Troubleshoot <- qmplot(Lon, Lat, data = GAVI_sub, zoom = 13,  maptype = "toner-background", 
                       color = CO2_ppm_ave, shape = Date.as.fact)+
  scale_color_gradient(low="blue", high="red")

GAVI_flux <-  ggplot(GAVI) +
  geom_ellipse(aes(x0 = 775, y0 = 4140, a = 40, b = 5, angle = pi / 1), fill = "Green") +
  geom_ellipse(aes(x0 = 580, y0 = 4105, a = 60, b = 8, angle = pi / 1), fill = "Green") +
  geom_ellipse(aes(x0 = 280, y0 = 4077, a = 50, b = 8, angle = pi / 1), fill = "Green") +
  geom_line(aes(dist_m, ele_estimate, color = Flux_ave), size=3) +
  scale_color_gradient(low="blue", high="red", #breaks = c(0, -1, -2, -3), labels = c("1.0", "0.1", "0.01","0.001")
                       ) +
  labs(color="CO2 Flux") + xlab("Distance") + ylab("Elevation") +
  theme_bw() +
  ggtitle("GAVILAN - CO2 Evasion")


GAVI_co2 <-  ggplot(GAVI) +
  geom_ellipse(aes(x0 = 775, y0 = 4140, a = 40, b = 5, angle = pi / 1), fill = "Green") +
  geom_ellipse(aes(x0 = 580, y0 = 4105, a = 60, b = 8, angle = pi / 1), fill = "Green") +
  geom_ellipse(aes(x0 = 280, y0 = 4077, a = 50, b = 8, angle = pi / 1), fill = "Green") +
  geom_line(aes(dist_m, ele_estimate, color = log10(CO2_ppm_ave)), size=3) +
  scale_color_gradient(low="blue", high="red", breaks = c(3.5, 3.0, 2.5), labels = c("3162", "1000", "316")) +
  labs(color="pCO2") + xlab("Distance (m)") + ylab("Elevation (m)") +
  #  geom_point(aes(dist_m, ele_estimate-5, shape = notes)) +
  theme_bw() +
  ggtitle("GAVILAN - pCO2") # for the main title



#### COLMILLO

COLMILLO <-  read.csv(here::here("/synoptic/COLMILLO_2021-08-30Edit_quickandDirty.csv"), skip=0, header = TRUE, sep = ",",
                  na.strings=c("","NA"), quote = "\"",dec = ".", fill = TRUE, comment.char = "")


Flux_map <- qmplot(lon, lat, data = COLMILLO, zoom = 13,  maptype = "toner-background", color = CO2_ppm_ave)+
  scale_color_gradient(low="blue", high="red")

COLMILLO$Date.as.fact <- as.factor(COLMILLO$Date)
COLMILLO$Date <- as.Date(COLMILLO$Date, format = "%m/%d/%Y")



Troubleshoot <- qmplot(lon, lat, data = COLMILLO, zoom = 13,  maptype = "toner-background", 
                       color = CO2_ppm_ave, shape = Date.as.fact)+
  scale_color_gradient(low="blue", high="red")

Troubleshoot <- qmplot(Lon, Lat, data = COLMILLO, zoom = 13,  maptype = "toner-background", 
                       color = CO2_ppm_ave, shape = Date.as.fact)+
  scale_color_gradient(low="blue", high="red")

COLMILLO_flux <-  ggplot(COLMILLO) +
  geom_ellipse(aes(x0 = 300, y0 = 3900, a = 100, b = 3, angle = pi / 240), fill = "Green") +
  geom_ellipse(aes(x0 = 735, y0 = 3924, a = 60, b = 2, angle = pi / 1), fill = "Green") +
  geom_ellipse(aes(x0 = 1036, y0 = 3932, a = 100, b = 3, angle = pi / 90), fill = "Green") +
  geom_line(aes(dist_m, ele_estimate, color = Flux_ave), size=3) +
  scale_color_gradient(low="blue", high="red", #breaks = c(0, -1, -2, -3), labels = c("1.0", "0.1", "0.01","0.001")
  ) +
  labs(color="CO2 Flux") + xlab("Distance") + ylab("Elevation") +
  theme_bw() +
  ggtitle("COLMILLO - CO2 Evasion")


COLMILLO_co2 <-  ggplot(COLMILLO) +
  geom_ellipse(aes(x0 = 300, y0 = 3900, a = 100, b = 3, angle = pi / 240), fill = "Green") +
  geom_ellipse(aes(x0 = 735, y0 = 3924, a = 60, b = 2, angle = pi / 1), fill = "Green") +
  geom_ellipse(aes(x0 = 1036, y0 = 3932, a = 100, b = 3, angle = pi / 90), fill = "Green") +
  geom_line(aes(dist_m, ele_estimate, color = CO2_ppm_ave), size=3) +
  scale_color_gradient(low="blue", high="red", #breaks = c(3.5, 3.0, 2.5), labels = c("3162", "1000", "316")
                       ) +
  labs(color="pCO2") + xlab("Distance (m)") + ylab("Elevation (m)") +
  #  geom_point(aes(dist_m, ele_estimate-5, shape = notes)) +
  theme_bw() +
  ggtitle("COLMILLO - pCO2") # for the main title

