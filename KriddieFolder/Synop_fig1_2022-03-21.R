## This will be the first plot of the synoptic paper
#1,2,3 go!!

#read in synoptic data 
library(here)
library(dplyr)

#read in the most recent synoptic data

df <- read.csv(here::here("ProcessedData/ALL_synoptic_2022-03-21.csv"))
df$X <- NULL
df <- unique(df)

##we want all the data to be going from upstream to downstream with increase distance.
#ANTE is already like this. you can use plot below to check all wetlands

ggplot(data=df%>%filter(Wetland=="GAVItrib2"), aes(x=dist, y=ele_fit, group=1)) +
  geom_line()+
  geom_point()

#you can't just reverse order the distance column because they are not collected at even intervals.
#loops below switch order of distance and calc total flux
#ANTE$dist <- rev(ANTE$dist)

ANTE <- df%>%filter(Wetland=="ANTE")
ANTE$Flux_ave[is.na(ANTE$Flux_ave)] <- 0
#ANTE$dist <- rev(ANTE$dist)
ANTE <- ANTE[order(ANTE$dist),]
ANTE$dist_diff <- NA
ANTE$Total_flux <- NA
ANTE$dist_new <- ANTE$dist

for(i in 1:nrow(ANTE)) {       # for-loop over rows
  if (i == 1) {
    ANTE$dist_diff[i] <- 0
    ANTE$Total_flux[i] <- ANTE$Flux_ave[1]
  } else {
    ANTE[i,"dist_diff"] <-   ANTE[i,"dist"] - ANTE[i-1,"dist"]
    ANTE[i,"Total_flux"] <- ANTE[i-1,"Total_flux"] + ANTE[i,"Flux_ave"]
  }
  }
  
  
##GAVI
GAVI <- df%>%filter(Wetland=="GAVI")
GAVI$Flux_ave[is.na(GAVI$Flux_ave)] <- 0
#ANTE$dist <- rev(ANTE$dist)
GAVI <- GAVI[order(GAVI$dist),]
GAVI$dist_diff <- NA
GAVI$Total_flux <- NA

for(i in 1:nrow(GAVI)) {       # for-loop over rows
  if (i == 1) {
    GAVI$dist_diff[i] <- 0
  } else {
    GAVI[i,"dist_diff"] <-   GAVI[i,"dist"] - GAVI[i-1,"dist"]
  }
}

GAVI <- GAVI[order(-GAVI$dist),]
GAVI$dist_new <- NA
for(i in 1:nrow(GAVI)) {       # for-loop over rows
  if (i == 1) {
    GAVI$Total_flux[i] <- GAVI$Flux_ave[1]
    GAVI$dist_new[i] <- 0
  } else {
    GAVI[i,"Total_flux"] <- GAVI[i-1,"Total_flux"] + GAVI[i,"Flux_ave"]
    GAVI[i,"dist_new"] <-   GAVI[i,"dist_diff"] + GAVI[i-1,"dist_new"]
  }
}

ggplot(data=GAVI%>%filter(Wetland=="GAVI"), aes(x=dist_new, y=ele_fit, group=1)) +
  geom_line()+
  geom_point()
ggplot(data=GAVI%>%filter(Wetland=="GAVI")%>%drop_na(adjusted_ppm), aes(x=dist_new, y=Total_flux, group=1)) +
  geom_line()+
  geom_point()
##

COLM <- df%>%filter(Wetland=="COLM")
COLM$Flux_ave[is.na(COLM$Flux_ave)] <- 0
COLM <- COLM[order(COLM$dist),]
COLM$dist_diff <- NA
COLM$Total_flux <- NA

for(i in 1:nrow(COLM)) {       # for-loop over rows
  if (i == 1) {
    COLM$dist_diff[i] <- 0
  } else {
    COLM[i,"dist_diff"] <-   COLM[i,"dist"] - COLM[i-1,"dist"]
  }
}

COLM <- COLM[order(-COLM$dist),]
COLM$dist_new <- NA
for(i in 1:nrow(COLM)) {       # for-loop over rows
  if (i == 1) {
    COLM$Total_flux[i] <- COLM$Flux_ave[1]
    COLM$dist_new[i] <- 0
  } else {
    COLM[i,"Total_flux"] <- COLM[i-1,"Total_flux"] + COLM[i,"Flux_ave"]
    COLM[i,"dist_new"] <-   COLM[i,"dist_diff"] + COLM[i-1,"dist_new"]
  }
}

ggplot(data=COLM%>%filter(Wetland=="COLM"), aes(x=dist_new, y=ele_fit, group=1)) +
  geom_line()+
  geom_point()

ggplot(data=COLM%>%filter(Wetland=="COLM")%>%drop_na(adjusted_ppm), aes(x=dist_new, y=Total_flux, group=1)) +
  geom_line()+
  geom_point()

##GAVItrib1
GAVItrib1 <- df%>%filter(Wetland=="GAVItrib1")
GAVItrib1$Flux_ave[is.na(GAVItrib1$Flux_ave)] <- 0
GAVItrib1 <- GAVItrib1[order(GAVItrib1$dist),]
GAVItrib1$dist_diff <- NA
GAVItrib1$Total_flux <- NA

for(i in 1:nrow(GAVItrib1)) {       # for-loop over rows
  if (i == 1) {
    GAVItrib1$dist_diff[i] <- 0
  } else {
    GAVItrib1[i,"dist_diff"] <-   GAVItrib1[i,"dist"] - GAVItrib1[i-1,"dist"]
  }
}

GAVItrib1 <- GAVItrib1[order(-GAVItrib1$dist),]
GAVItrib1$dist_new <- NA
for(i in 1:nrow(GAVItrib1)) {       # for-loop over rows
  if (i == 1) {
    GAVItrib1$Total_flux[i] <- GAVItrib1$Flux_ave[1]
    GAVItrib1$dist_new[i] <- 0
  } else {
    GAVItrib1[i,"Total_flux"] <- GAVItrib1[i-1,"Total_flux"] + GAVItrib1[i,"Flux_ave"]
    GAVItrib1[i,"dist_new"] <-   GAVItrib1[i,"dist_diff"] + GAVItrib1[i-1,"dist_new"]
  }
}

ggplot(data=GAVItrib1%>%filter(Wetland=="GAVItrib1"), aes(x=dist_new, y=ele_fit, group=1)) +
  geom_line()+
  geom_point()

ggplot(data=GAVItrib1%>%filter(Wetland=="GAVItrib1")%>%drop_na(adjusted_ppm), aes(x=dist_new, y=Total_flux, group=1)) +
  geom_line()+
  geom_point()


##GAVItrib2

GAVItrib2 <- df%>%filter(Wetland=="GAVItrib2")
GAVItrib2$Flux_ave[is.na(GAVItrib2$Flux_ave)] <- 0
GAVItrib2 <- GAVItrib2[order(GAVItrib2$dist),]
GAVItrib2$dist_diff <- NA
GAVItrib2$Total_flux <- NA

for(i in 1:nrow(GAVItrib2)) {       # for-loop over rows
  if (i == 1) {
    GAVItrib2$dist_diff[i] <- 0
  } else {
    GAVItrib2[i,"dist_diff"] <-   GAVItrib2[i,"dist"] - GAVItrib2[i-1,"dist"]
  }
}

GAVItrib2 <- GAVItrib2[order(-GAVItrib2$dist),]
GAVItrib2$dist_new <- NA
for(i in 1:nrow(GAVItrib2)) {       # for-loop over rows
  if (i == 1) {
    GAVItrib2$Total_flux[i] <- GAVItrib2$Flux_ave[1]
    GAVItrib2$dist_new[i] <- 0
  } else {
    GAVItrib2[i,"Total_flux"] <- GAVItrib2[i-1,"Total_flux"] + GAVItrib2[i,"Flux_ave"]
    GAVItrib2[i,"dist_new"] <-   GAVItrib2[i,"dist_diff"] + GAVItrib2[i-1,"dist_new"]
  }
}

ggplot(data=GAVItrib2%>%filter(Wetland=="GAVItrib2"), aes(x=dist_new, y=ele_fit, group=1)) +
  geom_line()+
  geom_point()

ggplot(data=GAVItrib2%>%filter(Wetland=="GAVItrib2")%>%drop_na(adjusted_ppm), aes(x=dist_new, y=Total_flux, group=1)) +
  geom_line()+
  geom_point()


####

#join
df2 <- rbind(ANTE,GAVI,COLM,GAVItrib2,GAVItrib1)

ggplot(df2%>%drop_na(adjusted_ppm) , aes(x=dist_new, y=Total_flux, group=Wetland)) +
  geom_line(aes(linetype=Wetland,
    #color=Wetland
    ),size=2)+
  geom_point(aes(color=K600.effective)) +
  

ggplot(df2 , aes(x=dist, y=ele_fit, group=Wetland)) +
  geom_line(aes(linetype=Wetland))#+

#  geom_point()
