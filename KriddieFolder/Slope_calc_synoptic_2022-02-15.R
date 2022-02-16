#Calculate Slope
#kriddie
#2/15/2022

#this script is used to calcualte slope for each stream profile,
#Ante,Gavi,Colm
#can be adjusted to change distance segment 


ANTE <- read.csv(here::here("/ProcessedData/ANTE_synoptic_2022-02-16.csv"))
ANTE$X <- NULL
##calculate slope please

save = ANTE[1,]
save$slope <- NA
loop <- ANTE%>%drop_na(ele_fit)
for(i in 1:nrow(loop)) {       # for-loop over rows
  x = loop$dist[i] - 10
  loop$ele_minus[i] <- loop[which.min(abs(loop$dist-x)),]$ele_fit
  loop$dist_minus[i] <- loop[which.min(abs(loop$dist-x)),]$dist
  
  y = loop$dist[i] + 3
  loop$ele_plus[i] <- loop[which.min(abs(loop$dist-y)),]$ele_fit
  loop$dist_plus[i] <- loop[which.min(abs(loop$dist-y)),]$dist
  
  loop$slope <- -1*(loop$ele_minus-loop$ele_plus)/(loop$dist_minus-loop$dist_plus)
  
}
loop$dist_minus <- NULL
loop$dist_plus <- NULL
loop$ele_minus <- NULL
loop$ele_plus <- NULL

ANTE <- rbind(save, loop)

#
df <- read.csv(here::here("ProcessedData/ALL_synoptic_2022-02-16.csv"))
test <- left_join(ANTE,df[,c("lon_fit","lat_fit","K600.effective")],by=c("lon_fit","lat_fit"))

ggplot(data=test,aes(slope,adjusted_ppm)) +
  geom_point(size=3) +
  geom_smooth(method=lm, se=FALSE) + 
  My_Theme + theme(legend.position = c(0.2, 0.9))


###GAVI

GAVI <- read.csv(here::here("/ProcessedData/GAVI_synoptic_2022-02-14.csv"))
GAVI$X <- NULL
##calculate slope please
#GAVI <- GAVI[,c("ele_fit","dist")]
save = GAVI[1,]
save$slope <- NA
loop <- GAVI%>%drop_na(ele_fit)
for(i in 1:nrow(loop)) {       # for-loop over rows
  x = loop$dist[i] - 3
  loop$ele_minus[i] <- loop[which.min(abs(loop$dist-x)),]$ele_fit
  loop$dist_minus[i] <- loop[which.min(abs(loop$dist-x)),]$dist
  
  y = loop$dist[i] + 10
  loop$ele_plus[i] <- loop[which.min(abs(loop$dist-y)),]$ele_fit
  loop$dist_plus[i] <- loop[which.min(abs(loop$dist-y)),]$dist
  
  loop$slope <- (loop$ele_minus-loop$ele_plus)/(loop$dist_minus-loop$dist_plus)
  
}
loop$dist_minus <- NULL
loop$dist_plus <- NULL
loop$ele_minus <- NULL
loop$ele_plus <- NULL

GAVI <- rbind(save, loop)

#

df <- read.csv(here::here("ProcessedData/ALL_synoptic_2022-02-15.csv"))
test <- left_join(GAVI,df[,c("lon_fit","lat_fit","K600.effective")],by=c("lon_fit","lat_fit"))


ggplot(data=test,aes(slope,adjusted_ppm)) +
  geom_point(size=3) +
  geom_smooth(method=lm, se=FALSE) + 
  My_Theme + theme(legend.position = c(0.2, 0.9))




###colm

COLM <- read.csv(here::here("/ProcessedData/COLMILLO_synoptic_2022-02-14.csv"))
COLM$X <- NULL
##calculate slope please

save = COLM[1,]
save$slope <- NA
loop <- COLM%>%drop_na(ele_fit)
for(i in 1:nrow(loop)) {       # for-loop over rows
  x = loop$dist[i] - 3
  loop$ele_minus[i] <- loop[which.min(abs(loop$dist-x)),]$ele_fit
  loop$dist_minus[i] <- loop[which.min(abs(loop$dist-x)),]$dist
  
  y = loop$dist[i] + 10
  loop$ele_plus[i] <- loop[which.min(abs(loop$dist-y)),]$ele_fit
  loop$dist_plus[i] <- loop[which.min(abs(loop$dist-y)),]$dist
  
  loop$slope <- (loop$ele_minus-loop$ele_plus)/(loop$dist_minus-loop$dist_plus)
  
}
loop$dist_minus <- NULL
loop$dist_plus <- NULL
loop$ele_minus <- NULL
loop$ele_plus <- NULL

COLM <- rbind(save, loop)

#

df <- read.csv(here::here("ProcessedData/ALL_synoptic_2022-02-15.csv"))
test <- left_join(COLM,df[,c("lon_fit","lat_fit","K600.effective")],by=c("lon_fit","lat_fit"))
test$adjusted_ppm
ggplot(data=test,aes(slope,adjusted_ppm)) +
  geom_point(size=3) +
  geom_smooth(method=lm, se=FALSE) + 
  My_Theme + theme(legend.position = c(0.2, 0.9))



##write out
write.csv(ANTE, here::here("ProcessedData/ANTE_synoptic_2022-02-15.csv"))
write.csv(GAVI, here::here("ProcessedData/GAVI_synoptic_2022-02-15.csv"))
write.csv(COLM, here::here("ProcessedData/COLM_synoptic_2022-02-15.csv"))


