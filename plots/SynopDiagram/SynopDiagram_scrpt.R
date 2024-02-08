#synop diagram
#2024-01-31


df <- read.csv(here::here("plots/SynopDiagram/ANTE_synoptic_forDiagram.csv"))
df$X <- NULL
df <- df%>%
  filter(lon_fit >= -78.19310)%>%
  filter(lon_fit < 	
           -78.19269)%>%filter(dist >  229.3445)

width_df <- df%>%drop_na(lat_wypt)
sampl_df <- df%>%drop_na(EOS_no)

x <- SpatialPoints(df[,c(1:2)])
y <- SpatialPoints(width_df[,c(1:2)]) 
z <- SpatialPoints(sampl_df[,c(1:2)])
plot(x, col = "lightblue",pch=19)
points(y, col = "black",pch=4)
points(z, col = "red", pch=17)

box(lty = 'solid', col = 'black')
