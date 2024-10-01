#Load necessary packages
library(sf)
library(raster)
library(tidyverse)
library(tmap)
library(ggplot2)
library(dplyr)
library(tidyr)
library(geosphere)

dataFrame <- read.csv(here::here("ProcessedData/upscaling_datasets/point_Facc_Ele_gaviWS_TableToExcel.csv"))%>%rename(ele=Ele)%>%rename(flo_accu=FloAccu)

#dataFrame <- read.csv(here::here("Geomorphology/Geomorph_from_ArcPro/colm_rnetwork_point.csv"))
#dataFrame$NEAR_FID <- paste("seg",dataFrame$NEAR_FID,sep="_")
dataFrame$lat_save <- dataFrame$lat
dataFrame$lon_save <- dataFrame$lon
dataFrame$ID <- seq.int(nrow(dataFrame))
dataFrame <- dataFrame%>%filter(OBJECTID!=619)
  
my_sf <- st_as_sf(dataFrame, coords = c('lon', 'lat'),crs = 4326)
my_sf_proj <- st_transform(my_sf, 3857)


ggplot(my_sf_proj) + 
  geom_sf(aes(color=ele))

ggplot(my_sf_proj) + 
  geom_sf(aes(color=lon_save))

ggplot(my_sf_proj) + 
  geom_sf(aes(color=ele))
ggplot(my_sf_2) + 
  geom_sf(aes(color=log1p(F_mol_m2_d_eq1)))


#is crs correct?
i <- 619
my_sf_proj_1 <- my_sf_proj[i,]


rm(all_data)
rm(all_data_temp)



for(i in 1:nrow(my_sf_proj)) {
  # Step 4: Create a 10-meter buffer around the reference point
  
  buffer_min <- st_buffer(my_sf_proj[i,], dist = 7)
  buffer_max <- st_buffer(my_sf_proj[i,], dist = 12.5)
  
  my_sf_proj_1 <- my_sf_proj
  # intersection with buffer as a polygon
  my_sf_proj_1$min_dist <- 
    ifelse(sf::st_intersects(my_sf_proj_1, buffer_min, sparse = F), "Yes", "No")
  my_sf_proj_1$max_dist <- 
    ifelse(sf::st_intersects(my_sf_proj_1, buffer_max, sparse = F), "Yes", "No")
  
  #make table of all points that fall between the 2 buffer
  my_sf_proj_select <- my_sf_proj_1%>%filter(max_dist=="Yes")%>%filter(min_dist=="No")
 #add lat lon of original point so that we can calc difference in accumulation for each point
  my_sf_proj_select$geometry_center <- my_sf_proj[i,]$geometry
  my_sf_proj_select$floaccu_center <- my_sf_proj[i,]$flo_accu
  my_sf_proj_select$ele_center <- my_sf_proj[i,]$ele
  my_sf_proj_select$pointid <- my_sf_proj[i,]$pointid
  my_sf_proj_select$floaccu_diff <- my_sf_proj_select$floaccu_center - my_sf_proj_select$flo_accu

   #select upstream buy selecting point that positive and is the minimum closest
   my_sf_proj_select_upstream <-  my_sf_proj_select%>%filter(floaccu_diff > 0)
   my_sf_proj_select_upstream <- my_sf_proj_select_upstream[which.min(my_sf_proj_select_upstream$floaccu_diff), ]
   my_sf_proj_select_upstream <- my_sf_proj_select_upstream %>% st_drop_geometry()
   my_sf_proj_select_upstream <- my_sf_proj_select_upstream%>%dplyr::select(pointid,ele,flo_accu,lat_save,lon_save)%>%rename(ele_up10=ele)%>%rename(lat_up10=lat_save)%>%rename(lon_up10=lon_save)%>%rename(floaccu_up10=flo_accu)
   
   #select downstream by selecting the max difference in accumulation of all negative
   my_sf_proj_select_downstream <-  my_sf_proj_select%>%filter(floaccu_diff < 0)
   my_sf_proj_select_downstream <- my_sf_proj_select_downstream[which.max(my_sf_proj_select_downstream$floaccu_diff), ]
   my_sf_proj_select_downstream <- my_sf_proj_select_downstream %>% st_drop_geometry()
   my_sf_proj_select_downstream <- my_sf_proj_select_downstream%>%dplyr::select(pointid,ele,lat_save,lon_save,flo_accu)%>%rename(ele_down10=ele)%>%rename(lat_down10=lat_save)%>%rename(lon_down10=lon_save)%>%rename(floaccu_down10=flo_accu)
   
   my_sf_point <- my_sf_proj[i,]%>%dplyr::select(pointid,ele,flo_accu,lat_save,lon_save)%>%
     rename(lat_center=lat_save,lon_center=lon_save)
   my_sf_point <- my_sf_point %>% st_drop_geometry()
   
    #join 
   my_sf_point <- full_join(my_sf_point,my_sf_proj_select_upstream,by="pointid")
   my_sf_point <- full_join(my_sf_point,my_sf_proj_select_downstream,by="pointid")
   
   my_sf_point$dist_diff_mid_1 <- distm(c(my_sf_point$lon_up10, my_sf_point$lat_up10), 
                                    c(my_sf_point$lon_center, my_sf_point$lat_center), 
                                    fun = distHaversine)
   my_sf_point$dist_diff_mid_2 <- distm(c(my_sf_point$lon_center, my_sf_point$lat_center), 
                                      c(my_sf_point$lon_down10, my_sf_point$lat_down10), 
                                      fun = distHaversine)
  
      #now we can build our new dataframe
   if (!exists("all_data")){
     all_data <- my_sf_point
   }else{
     all_data_temp <- my_sf_point
     all_data <- rbind(all_data, all_data_temp)
     rm(all_data_temp)
     
   }
   #calc ele difference and slope mid
}

all_data$dist_diff_mid <-  all_data$dist_diff_mid_1 + all_data$dist_diff_mid_2
all_data$ele_diff_mid <- all_data$ele_up10 - all_data$ele_down10
all_data$slope_mid <- all_data$ele_diff_mid / all_data$dist_diff_mid
all_data_mid <-  all_data

#################
####slope up#####
#################
rm(all_data)
rm(all_data_temp)

for(i in 1:nrow(my_sf_proj)) {
  # Step 4: Create a 10-meter buffer around the reference point
  buffer_min20 <- st_buffer(my_sf_proj[i,], dist = 18*sqrt(2))
  buffer_max20 <- st_buffer(my_sf_proj[i,], dist = 20)
  
  my_sf_proj_1 <- my_sf_proj
  # intersection with buffer as a polygon
  my_sf_proj_1$min_dist20 <- 
    ifelse(sf::st_intersects(my_sf_proj_1, buffer_min, sparse = F), "Yes", "No")
  my_sf_proj_1$max_dist20 <- 
    ifelse(sf::st_intersects(my_sf_proj_1, buffer_max20, sparse = F), "Yes", "No")
  
  #make table of all points that fall between the 2 buffer
  my_sf_proj_select <- my_sf_proj_1%>%filter(max_dist20=="Yes")%>%filter(min_dist20=="No")
  #add lat lon of original point so that we can calc difference in accumulation for each point
  my_sf_proj_select$geometry_center <- my_sf_proj[i,]$geometry
  my_sf_proj_select$floaccu_center <- my_sf_proj[i,]$flo_accu
  my_sf_proj_select$ele_center <- my_sf_proj[i,]$ele
  my_sf_proj_select$pointid <- my_sf_proj[i,]$pointid
  my_sf_proj_select$floaccu_diff <- my_sf_proj_select$floaccu_center - my_sf_proj_select$flo_accu
  
  #select upstream buy selecting point that positive and is the minimum closest
  my_sf_proj_select_upstream <-  my_sf_proj_select%>%filter(floaccu_diff > 0)
  my_sf_proj_select_upstream <- my_sf_proj_select_upstream[which.min(my_sf_proj_select_upstream$floaccu_diff), ]
  my_sf_proj_select_upstream <- my_sf_proj_select_upstream %>% st_drop_geometry()
  my_sf_proj_select_upstream <- my_sf_proj_select_upstream%>%dplyr::select(pointid,ele,flo_accu,lat_save,lon_save)%>%rename(ele_up20=ele)%>%rename(lat_up20=lat_save)%>%rename(lon_up20=lon_save)%>%rename(floaccu_up20=flo_accu)
  
  my_sf_point <- my_sf_proj[i,]%>%dplyr::select(pointid,ele,flo_accu,lat_save,lon_save)%>%
    rename(lat_center=lat_save,lon_center=lon_save)
  my_sf_point <- my_sf_point %>% st_drop_geometry()
  
  #join 
  my_sf_point <- full_join(my_sf_point,my_sf_proj_select_upstream,by="pointid")
  my_sf_point <- full_join(my_sf_point,my_sf_proj_select_downstream,by="pointid")
  
  my_sf_point$dist_diff_up <- distm(c(my_sf_point$lon_up20, my_sf_point$lat_up20), 
                                       c(my_sf_point$lon_center, my_sf_point$lat_center), 
                                       fun = distHaversine)
  #now we can build our new dataframe
  if (!exists("all_data")){
    all_data <- my_sf_point
  }else{
    all_data_temp <- my_sf_point
    all_data <- rbind(all_data, all_data_temp)
    rm(all_data_temp)
    
  }
  #calc ele difference and slope mid
}

all_data$ele_diff_mid <- all_data$ele_up20 - all_data$ele_center
all_data$slope_mid <- all_data$ele_diff_mid / all_data$dist_diff_mid

all_data_up <-  all_data

#now join them together
all_data_final <- full_join(all_data_mid,all_data_up,by=c("pointid","ele","flo_accu","lat_center","lon_center"))   

#write out big ol dataframe
#write.csv(all_data_final,here::here("ProcessedData/slope_calculated_sept27.csv"))
#all_data_final <- read.csv(here::here("ProcessedData/slope_calculated_sept27.csv"))



ggplot() +
#  geom_sf(data = my_sf_proj_1, aes(color = max_dist)) +
  geom_sf(data = subset(my_sf_proj_1, max_dist == "Yes"), pch = 4, color = "blue") +
  geom_sf(data = buffer_max, fill = NA, color = "red") +
  geom_sf(data = subset(my_sf_proj_1, min_dist == "Yes"), pch = 4, color = "green") +
  geom_sf(data = buffer_min, fill = NA, color = "red") 


plot(st_geometry(buffer), col = "lightblue", main = "Points within 10 meters of Reference Point")
plot(st_geometry(my_sf_proj), add = TRUE, col = "black", pch = 16)
plot(st_geometry(my_sf_proj[i,]), add = TRUE, col = "red", pch = 16)

#Plot it:

ggplot(my_sf) + 
  geom_sf()


i <- 619
my_sf_2 <- st_as_sf(all_data, coords = c('lon_center', 'lat_center'))
my_sf_point <- my_sf_proj[i,] %>% st_drop_geometry()
my_sf_3 <- st_as_sf(my_sf_point, coords = c('lon_save', 'lat_save'))

ggplot() + 
  geom_sf(data=my_sf_2, aes(color=ele)) +
  geom_sf(data=my_sf_3 ,aes(color=flo_accu))
  
ggplot(my_sf_2) + 
  geom_sf(aes(color=log1p(F_mol_m2_d_eq1)))


#all_data_bind
my_sf_2 <- st_as_sf(all_data_bind, coords = c('lon', 'lat'))

ggplot(my_sf_2) + 
  geom_sf(aes(color=ele))

ggplot(my_sf_2) + 
  geom_sf(aes(color=co2))
ggplot(my_sf_2) + 
  geom_sf(aes(color=log1p(F_mol_m2_d_eq1)))


