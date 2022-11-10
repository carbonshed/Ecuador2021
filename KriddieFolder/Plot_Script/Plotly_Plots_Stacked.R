library(plotly) 

fig3 <- plot_ly(WL_02%>%filter(DateTime>as.POSIXct("2022-07-25 00:00:00", tz="UTC")), x = ~DateTime, y = ~WLPres_kpa, type = 'scatter', mode = 'markers') 


#62.981-62.541
#64.347-63.041
#((64.347-63.041)+(62.981-62.541))/2

WL_02$test <- WL_02$WLPres_kpa
WL_02[which(WL_02$DateTime > as.POSIXct("2022-07-27 10:15:00", tz="UTC")&WL_02$DateTime < as.POSIXct("2022-10-12 13:45:00", tz="UTC")),]$test <- 
  WL_02[which(WL_02$DateTime > as.POSIXct("2022-07-27 10:15:00", tz="UTC")&WL_02$DateTime < as.POSIXct("2022-10-12 13:45:00", tz="UTC")),]$test + (64.347-63.041)

WL_02[which(WL_02$DateTime < as.POSIXct("2022-10-12 13:45:00", tz="UTC")),]$test <- 
  WL_02[which(WL_02$DateTime < as.POSIXct("2022-10-12 13:45:00", tz="UTC")),]$test - (62.981-62.541)
#WL_02 <- WL_02%>%filter(WLPres_kpa != 62.566)

fig4 <- plot_ly(WL_02, x = ~DateTime, y = ~test, type = 'scatter', mode = 'markers') 
fig4

fig1 <- plot_ly(WL_01%>%filter(DateTime>as.POSIXct("2022-01-25 00:00:00", tz="UTC")), x = ~DateTime, y = ~WLPres_kpa, type = 'scatter', mode = 'markers') 
fig2 <- plot_ly(WL_02%>%filter(DateTime>as.POSIXct("2022-01-25 00:00:00", tz="UTC")), x = ~DateTime, y = ~test, type = 'scatter', mode = 'markers') 

#fig1 <- plot_ly(x = c(3, 4, 5), y = c(1000, 1100, 1200), type = 'scatter', mode = 'lines+markers') 

#fig2 <- plot_ly(x = c(2, 3, 4), y = c(100, 110, 120), type = 'scatter', mode = 'lines+markers') 

#fig3 <- plot_ly(x = c(0, 1, 2), y = c(10, 11, 12), type = 'scatter', mode = 'lines+markers') 

fig <- subplot(fig1, fig2, nrows = 2) %>% 
  
  layout(title = list(text = "Stacked Subplots"),
         
         plot_bgcolor='#e5ecf6', 
         
         xaxis = list( 
           
           zerolinecolor = '#ffff', 
           
           zerolinewidth = 2, 
           
           gridcolor = 'ffff'), 
         
         yaxis = list( 
           
           zerolinecolor = '#ffff', 
           
           zerolinewidth = 2, 
           
           gridcolor = 'ffff')) 

fig
