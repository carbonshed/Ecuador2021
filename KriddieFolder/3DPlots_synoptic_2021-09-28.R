library(plotly)
library(dplyr)
library(zoo)
library(plot3D)
library(splines)


Geomorph <- read.csv(here::here("/Geomorphology/Atenas/ANTENAS_GEOMORPH_2021-09-28.csv"))
Geomorph$X <- NULL
Geomorph$ID <- NULL
Geomorph$trksegID <- NULL


synop <- read.csv(here::here("Synoptic/ANTE_2021-08-27_withDOC.csv"))
synop$X <- NULL
synop$Date.as.fact <- NULL
colnames(synop) <- c("lon","lat","ele","Date","EOS_no","Flux_ave","Tract","Point","CO2_ppm_ave")
#there is a bad elevation data in synop, I am replacing it here, but need to work on figuring out what is up
synop$ele<-replace(synop$ele, synop$ele<4100,4302) 


df <- full_join(synop,Geomorph, by=c("lat","lon","ele")) 

fig <- plot_ly(df, x = ~lat, y = ~lon, z = ~ele, size = 1,
               marker = list(color = ~log10(CO2_ppm_ave), colorscale = c('#FFE1A1', '#683531'), showscale = TRUE)
            )
fig <- fig %>% add_markers()
#fig <- fig %>% add_trace(x = ~lat, y = ~lon, z = ~ele, #color = ~EOS_no#, colors = c('Blue','Yellow'),
#                         mode = 'markers')
fig <- fig %>% layout(scene = list(xaxis = list(title = 'lat'),
                                   yaxis = list(title = 'lon'),
                                   zaxis = list(title = 'elevation')))

fig



fig <- plot_ly(Geomorph, x = ~lat, y = ~lon, z = ~ele, size = 1
)
fig <- fig %>% add_markers()
#fig <- fig %>% add_trace(x = ~lat, y = ~lon, z = ~ele, #color = ~EOS_no#, colors = c('Blue','Yellow'),
#                         mode = 'markers')
fig <- fig %>% layout(scene = list(xaxis = list(title = 'lat'),
                                   yaxis = list(title = 'lon'),
                                   zaxis = list(title = 'elevation')))

Z = seq(0, 1, 0.01)
X = rnorm(length(Z), mean = 0, sd = 0.1)
Y = 2 * Z ^ 2 + rnorm(length(Z), mean = 0, sd = 0.1)

data = data.frame(X = X, Y = Y, Z= Z)

library(plot3D)
library(splines)
fit <- lm(cbind(X, Y) ~ poly(Z, 2))

install.packages("rgl")
library(rgl)
plot3d(X, Y, Z, col = "red")
lines3d(cbind(predict(fit), Z))


Z = seq(0, 1, 0.01)
X = rnorm(length(Z), mean = 0, sd = 0.1)
Y = 2 * Z ^ 2 + rnorm(length(Z), mean = 0, sd = 0.1)



Z = Geomorph$ele
X = Geomorph$lat
Y = Geomorph$lon

data = data.frame(X = X, Y = Y, Z= Z)

fit <- lm(cbind(X, Y) ~ poly(Z, 20))

plot3d(X, Y, Z, col = "red")
lines3d(cbind(predict(fit), Z))
















####
#OK I DON'T THINK LOESS WORKS FOR THIS##

loessMod10 <- loess(lat ~ ele, data=Geomorph, span=0.10) # 10% smoothing span
loessMod25 <- loess(lat ~ ele, data=Geomorph, span=0.25) # 25% smoothing span
loessMod50 <- loess(lat ~ ele, data=Geomorph, span=0.50) # 50% smoothing span

# get smoothed output
smoothed10 <- predict(loessMod10) 
smoothed25 <- predict(loessMod25) 
smoothed50 <- predict(loessMod50) 

plot(Geomorph$lat, x=Geomorph$ele, type="l", main="Loess Smoothing and Prediction", xlab="Date", ylab="Unemployment (Median)")
lines(smoothed10, x=Geomorph$ele, col="red")
lines(smoothed25, x=Geomorph$ele, col="green")
lines(smoothed50, x=Geomorph$ele, col="blue")

ele <- seq(4263, 4324, by=0.25) 
model_df <- as.data.frame(ele)
model_df$lat <- 

##### LON

loessMod10 <- loess(lon ~ ele, data=Geomorph, span=0.10) # 10% smoothing span
loessMod25 <- loess(lon ~ ele, data=Geomorph, span=0.25) # 25% smoothing span
loessMod50 <- loess(lon ~ ele, data=Geomorph, span=0.50) # 50% smoothing span

# get smoothed output
smoothed10 <- predict(loessMod10) 
smoothed25 <- predict(loessMod25) 
smoothed50 <- predict(loessMod50) 

plot(Geomorph$lon, x=Geomorph$ele, type="l", main="Loess Smoothing and Prediction", xlab="Date", ylab="Unemployment (Median)")
lines(smoothed10, x=Geomorph$ele, col="red")
lines(smoothed25, x=Geomorph$ele, col="green")
lines(smoothed50, x=Geomorph$ele, col="blue")

##make df from 4263.58 to 4324.63 
ele <- seq(4263, 4324, by=0.2) 
model_df <- as.data.frame(ele)
model_df$lat 



##########

fig
install.packages("akima")
install.packages("asbio")
library(asbio)
library(akima)

#### 3 d loess planes
Y <- Geomorph$ele
X <- data.matrix(Geomorph[1:2])



loess.surf(Y, X, span = 0.1, degree = 1, family = "gaussian", phi = 20, 
           theta = 50, xlab = "X", ylab = "Y", zlab = "Fit", line.col = 1, 
           line.type = 1, scale = TRUE, duplicate = "error", expand = 0.5)
