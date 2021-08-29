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
  theme_bw()


ANTE_co2 <-  ggplot(ANTE) +
  geom_ellipse(aes(x0 = 40, y0 = 4271, a = 20, b = 3, angle = pi / 30), fill = "Green") +
  geom_ellipse(aes(x0 = 225, y0 = 4300, a = 20, b = 2, angle = pi / 1), fill = "Green") +
  geom_ellipse(aes(x0 = 360, y0 = 4310, a = 65, b = 5, angle = pi / 30), fill = "Green") +
  geom_line(aes(dist_m, ele_estimate, color = log10(CO2_ppm_ave)), size=3) +
  scale_color_gradient(low="blue", high="red", breaks = c(3.5, 3.0, 2.5), labels = c("3162", "1000", "316")) +
  labs(color="pCO2") + xlab("Distance") + ylab("Elevation") +
#  geom_point(aes(dist_m, ele_estimate-5, shape = notes)) +
  theme_bw()


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


####### COLMILLO

