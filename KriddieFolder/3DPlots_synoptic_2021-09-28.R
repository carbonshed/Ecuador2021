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

fig


#######how do I do this line of best fit??? Try's below:

##how about interpSpline from splines package?###
#install.packages("splines")
library(splines)
x <- data.frame(long=Geomorph$lon,lat=Geomorph$lat)
spline <- interpSpline(x[,1],x[,2])
spline <- interpSpline(Geomorph$lat, Geomorph$lon)
plot(spline)
points(x,y)

###lets try doing this in chunks
Geomorph_sub <- Geomorph %>% filter(lon < -78.1941)

x <- data.frame(long=Geomorph_sub$lon,lat=Geomorph_sub$lat)
fit <- princurve::principal_curve(as.matrix(x))
sfit <- smooth.spline(x[,1] ~ x[,2], spar=.60)
#fit1<-smooth.spline(x[,1], x[,2],df=16)
#sfit <- smooth.spline(x[,1] ~ x[,2], spar=0.90)
summary(sfit)

plot(x[,1], x[,2], pch=19, main="Curve fit(s) of coordinates", 
     xlab="longitude", ylab="latitude", cex=0.50)
#lines(princurve::principal_curve(as.matrix(x)), lwd=1.25, col="red")
lines(sfit$y, sfit$x, lwd=1.25, col="blue")
#lines(sfit$y, sfit$x, lwd=1.25, col="green")

#legend("bottomright", legend=c("spline","princurve"),
#       lty=c(1,1), col=c("blue","red"))

#ok, now we make a wrapper function to interpolate

SSpline <- function(x, y, n = 48, ...) {
  ## fit the spline to x, and y
  mod <- smooth.spline(x, y, ...)
  ## predict from mod for n points over range of x
  pred.dat <- seq(from = min(x), to = max(x), length.out = n)
  ## predict
  preds <- predict(mod, x = pred.dat)
  ## return
  preds
}

time <- c(0,6,12,18,24,30,36,42)  

res <- SSpline(time, Geomorph_sub[1, 2:9])

###################
install.packages("princurve")
library(princurve)

x <- data.frame(long=Geomorph$lon,lat=Geomorph$lat)
fit <- princurve::principal_curve(as.matrix(x))
#sfit <- smooth.spline(x[,1] ~ x[,2], spar=0.80)
sfit <- smooth.spline(x[,1] ~ x[,2], spar=0.50)

smooth.spline(x = x[,1], 
              y =  x[,2], #w = NULL, 
              df= NULL, 
              spar = NULL, 
              lambda = NULL, 
              cv = FALSE,
              all.knots = FALSE, 
              nknots = .nknots.smspl,
              keep.data = TRUE, 
              df.offset = 0, 
              penalty = 1,
              control.spar = list(), 
              tol = 1e-6 * IQR(x), 
              keep.stuff = FALSE)

plot(x[,1], x[,2], pch=19, main="Curve fit(s) of coordinates", 
     xlab="longitude", ylab="latitude", cex=0.50)
lines(princurve::principal_curve(as.matrix(x)), lwd=1.25, col="red")
lines(sfit$y, sfit$x, lwd=1.25, col="blue")
legend("bottomright", legend=c("spline","princurve"),
       lty=c(1,1), col=c("blue","red"))



######


###


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



loess.surf(Y, X, span = 0.1, degree = 1, family = "gaussian", phi = 20, 
           theta = 50, xlab = "X", ylab = "Y", zlab = "Fit", line.col = 1, 
           line.type = 1, scale = TRUE, duplicate = "error", expand = 0.5)
