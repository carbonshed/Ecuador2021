## This will be the first plot of the synoptic paper
#1,2,3 go!!

#read in synoptic data 

x$B <- rev(x$B)

df <- read.csv(here::here("ProcessedData/ALL_synoptic_2022-03-10.csv"))
df$dist_ANTE <- NULL
df$dist_COLM <- NULL
df$dist_GAVI <- NULL

ANTE <- df%>%filter(Wetland=="ANTE")
#ANTE$dist <- rev(ANTE$dist)
ANTE <- ANTE[order(ANTE$dist),]

GAVI <- df%>%filter(Wetland=="GAVI")
#GAVI$dist <- rev(GAVI$dist)
GAVI <- GAVI[order(GAVI$dist),]

COLM <- df%>%filter(Wetland=="COLM")
#COLM$dist <- rev(COLM$dist)
COLM <- COLM[order(COLM$dist),]

ANTE$dist_diff <- NA
ANTE$Total_flux <- NA
for(i in 1:nrow(ANTE)) {       # for-loop over rows
  if (i == 1) {
    ANTE$Total_flux[i] <- ANTE$Total_flux[1]
    ANTE$dist_diff[i] <- 0
  } else {
    ANTE[i,"Total_flux"] <- ANTE[i-1,"Total_flux"] + ANTE[i,"Flux_ave"]
    ANTE[i,"dist_diff"] <- ANTE[i,"dist"] - ANTE[i-1,"dist"]
  }
}

GAVI$Total_flux <- NA
for(i in 1:nrow(GAVI)) {       # for-loop over rows
  if (i == 1) {
    GAVI$Total_flux[i] <- 1
  } else {
    GAVI[i,"Total_flux"] <- GAVI[i-1,"Total_flux"] + GAVI[i,"Flux_ave"]
  }
}

COLM$Total_flux <- NA
for(i in 1:nrow(COLM)) {       # for-loop over rows
  if (i == 1) {
    COLM$Total_flux[i] <- 1
  } else {
    COLM[i,"Total_flux"] <- COLM[i-1,"Total_flux"] + COLM[i,"Flux_ave"]
  }
}


#join
df2 <- rbind(ANTE,GAVI,COLM)

ggplot(df2 , aes(x=dist, y=Total_flux, group=Wetland)) +
  geom_line(aes(linetype=Wetland,color=Wetland))#+
#  geom_point()
ggplot(df2 , aes(x=dist, y=ele_fit, group=Wetland)) +
  geom_line(aes(linetype=Wetland))#+

#  geom_point()
