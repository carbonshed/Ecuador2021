library(ggpubr)
library(ggforce)
#after running Ante geomorphology


Colm_flux <- ggplot(data=df_prediction ) +
  geom_line(aes(dist, ele_fit), size = 3, color="brown", alpha=.5) +
  geom_point(data=df_prediction%>%drop_na(Flux_ave), aes(dist, ele_fit, color= Flux_ave),size=4)+
  scale_color_gradient(
    low = "blue",
    high = "red",
    space = "Lab",
    na.value = "grey50",
    guide = "colourbar",
    aesthetics = "colour"
  )

Colm_co2 <- ggplot(data=df_prediction) +
  geom_line(aes(dist, ele_fit), size = 3, color="brown", alpha=.5) +
  geom_point(data=df_prediction%>%drop_na(Flux_ave), aes(dist, ele_fit, color= CO2_ppm_ave),size=4)+
  scale_color_gradient(
    low = "blue",
    high = "red",
    space = "Lab",
    na.value = "grey50",
    guide = "colourbar",
    aesthetics = "colour"
  )

figure <- ggarrange(Colm_flux, Colm_co2,
                    ncol = 2, nrow = 1)
figure

#after running GAVE geomorphology


GAVI_flux <- ggplot(data=df_prediction ) +
  geom_line(aes(dist, ele_fit), size = 3, color="brown", alpha=.5) +
  geom_point(data=df_prediction%>%drop_na(Flux_ave), aes(dist, ele_fit, color= Flux_ave),
             size=4) +
  scale_color_gradient(
    low = "blue",
    high = "red",
    space = "Lab",
    na.value = "grey50",
    guide = "colourbar",
    aesthetics = "colour"
  )

GAVI_co2 <- ggplot(data=df_prediction) +
  geom_line(aes(dist, ele_fit), size = 3, color="brown", alpha=.5) +
  geom_point(data=df_prediction%>%drop_na(Flux_ave), aes(dist, ele_fit, color= CO2_ppm_ave),
             size=4) +
  scale_color_gradient(
    low = "blue",
    high = "red",
    space = "Lab",
    na.value = "grey50",
    guide = "colourbar",
    aesthetics = "colour"
  )

figure <- ggarrange(GAVI_flux, GAVI_co2,
                    ncol = 2, nrow = 1)
figure

##after running ANTE geomoroph

ANTE_flux <- ggplot(data=df_prediction ) +
  geom_line(aes(dist, ele_fit), size = 4, color="brown", alpha=.5) +
  geom_point(data=df_prediction%>%drop_na(Flux_ave), aes(dist, ele_fit, color= Flux_ave),size=4)+
  scale_color_gradient(
    low = "blue",
    high = "red",
    space = "Lab",
    na.value = "grey50",
    guide = "colourbar",
    aesthetics = "colour"
  )

ANTE_co2 <- ggplot(data=df_prediction) +
  geom_line(aes(dist, ele_fit), size = 4, color="brown", alpha=.5) +
  geom_point(data=df_prediction%>%drop_na(Flux_ave), aes(dist, ele_fit, color= CO2_ppm_ave),size=4)+
  scale_color_gradient(
    low = "blue",
    high = "red",
    space = "Lab",
    na.value = "grey50",
    guide = "colourbar",
    aesthetics = "colour"
  )



figure <- ggarrange(ANTE_flux, ANTE_co2,
                    ncol = 2, nrow = 1)
figure

ANTE_DOC <- ggplot(data=df_prediction) +
  geom_line(aes(dist, ele_fit), size = 4, color="brown", alpha=.5) +
  geom_point(data=df_prediction%>%drop_na(Flux_ave), aes(dist, ele_fit, color= CO2_ppm_ave),size=4)+
  scale_color_gradient(
    low = "blue",
    high = "red",
    space = "Lab",
    na.value = "grey50",
    guide = "colourbar",
    aesthetics = "colour"
  )
