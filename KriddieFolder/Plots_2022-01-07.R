
#after running Ante geomorphology


Colm_flux <- ggplot(data=df_prediction ) +
  geom_line(aes(dist, ele_fit), size = 3, color="brown", alpha=.5) +
  geom_point(data=df_prediction%>%drop_na(Flux_ave), aes(dist, ele_fit, color= Flux_ave),size=5)

Colm_co2 <- ggplot(data=df_prediction) +
  geom_line(aes(dist, ele_fit), size = 3, color="brown", alpha=.5) +
  geom_point(data=df_prediction%>%drop_na(Flux_ave), aes(dist, ele_fit, color= CO2_ppm_ave),size=5)

figure <- ggarrange(Colm_flux, Colm_co2,
                    ncol = 2, nrow = 1)
figure
