##barplots
#plots for agu 2024

df <- read.csv(here::here("ProcessedData/upscaleFlux_allwatersheds_oct18.csv"))

df$cathment_name <- factor(df$cathment_name, levels = c("ante", "gavi", "colm"))

df_bin5 <- df%>%filter(catchment_ha>=1) %>% mutate(new_bin = cut(log10(catchment_ha), breaks=5))
df_bin5$cathment_name <- factor(df_bin5$cathment_name, levels = c("ante", "gavi", "colm"))


z_score_break <- 10

df_bin5$z_score <- (df_bin5$F_CO2_molperd_eq1-mean(df_bin5$F_CO2_molperd_eq1))/sd(df_bin5$F_CO2_molperd_eq1)
z_score_toremove <- df_bin5%>%filter(z_score>=z_score_break|z_score<=-z_score_break)


df_bin5$z_score <- (df_bin5$F_CO2_molperd_eq1-mean(df_bin5$F_CO2_molperd_eq1))/sd(df_bin5$F_CO2_molperd_eq1)

histogram <- df_bin5%>%filter(catchment_ha>=1)%>%
  ggplot( aes(x=catchment_ha, fill=cathment_name)) +
  geom_histogram( color="black", position = 'identity',bins = 10) +
  theme_classic(base_size = 16) +
  labs(fill="")+ xlab("Catchment area [ha]") + 
  scale_x_continuous(transform = "log",breaks=c(1,3, 10,30,100,300)) +
  scale_fill_manual(name = "Watershed",labels = c("Antenas", 'Gavil\u00e1n',"Colmillo"),values=c("#feb24c", "#a1d99b", "#bcbddc")) +
  facet_grid(rows=vars(cathment_name))+ 
  theme(strip.background = element_blank(),
        strip.text.y = element_blank()) + 
  guides(fill="none")



df_bin5_summary <- df_bin5%>%
  filter(z_score<z_score_break & z_score>-z_score_break)%>%
  group_by(new_bin,cathment_name)%>%filter(catchment_ha>=1)%>%
  summarise(
    co2_mean = mean(co2,na.rm =TRUE),
    co2_sd = sd(co2,na.rm =TRUE),
    slope_mid_mean = mean(slope_mid,na.rm =TRUE),
    slope_mid_sd = sd(slope_mid,na.rm =TRUE),
    k600_eq1_mean = mean(k600_eq1_final,na.rm =TRUE),
    k600_eq1_sd = sd(k600_eq1_final,na.rm =TRUE),
    F_mol_m2_d_eq1_mean = mean(F_mol_m2_d_eq1,na.rm =TRUE),
    F_mol_m2_d_eq1_sd = sd(F_mol_m2_d_eq1,na.rm =TRUE),
    F_CO2_molperd_eq1_mean = mean(F_CO2_molperd_eq1,na.rm =TRUE),
    F_CO2_molperd_eq1_sd = sd(F_CO2_molperd_eq1,na.rm =TRUE),
    F_CO2_molperd_eq1_sum = sum(F_CO2_molperd_eq1,na.rm =TRUE))

barplot <-ggplot(data=df_bin5_summary, aes(x=new_bin, y=F_CO2_molperd_eq1_sum,fill=cathment_name)) +
  geom_bar(stat="identity", color="black", position=position_dodge())+
  scale_x_discrete(name="Catchment area [ha]", labels = c('[9.0 - 16.6]','[16.6 - 40.0]','[40.0 - 57.5]','[57.5 - 107]','[107 - 199]')) +
  scale_fill_manual(name = "Watershed", labels = c("Antenas", 'Gavil\u00e1n',"Colmillo"),values=c("#feb24c", "#a1d99b", "#bcbddc")) +
  #   scale_fill_discrete(name = "", labels = c("Antenas", 'Gavil\u00e1n',"Colmillo")) +
  ylab(expression(paste("Total CO"[2]," evasion [mol ", day^-1,"]"))) +
  theme_minimal(base_size = 18)+ theme(axis.text.x = element_text(angle = 25, vjust = 1, hjust=1)) + 
  
  theme(legend.position="top")


df$code_names <- NA
df[df$F_CO2_molperd_eq1 <= 0,]$code_names <- "< 0"
df[df$F_CO2_molperd_eq1 > 1.12,]$code_names <- "> 1.12"




full <- ggarrange(histogram,NULL,barplot,ncol = 3,widths=c(1,.2,1.75),common.legend = TRUE,legend = "bottom")

