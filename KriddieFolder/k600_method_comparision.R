#chamber vs. empirical formula

#what is elevation diff for gavi down?
test <- df_ulseth%>%filter(site=="Gavilan Outlet")
slope_gavi <- (max(test$ele_arcpro)-min(test$ele_arcpro))/(max(test$x_pred)-min(test$x_pred))

df_chamber <- read_csv(here::here("Whitmore_et_al_2024/Data.csv"))
df_chamber$site <- ifelse(df_chamber$Wetland_5=="ANTE","Ante",
                          ifelse(df_chamber$Wetland_5=="COLM","Colm",df_chamber$Wetland_5))

df_all_methods <- read.csv(here::here("ProcessedData/AllSynoptic_k600_Feb24.csv"))
df_all_methods$site <- ifelse(df_all_methods$site=="GaviTrib1","GavilanTribs",
                               ifelse(df_all_methods$site=="GaviTrib2","GavilanTribs",
                                      ifelse(df_all_methods$site=="GaviTrib3","GavilanTribs", 
                                             ifelse(df_all_methods$site=="GaviUp","Gavilan Inlet",
                                                    ifelse(df_all_methods$site=="GaviDown","Gavilan Outlet",
                                                           df_all_methods$site)))))
df_all_methods_melt <- df_all_methods %>%select(site,K600_chamber,k600_eq1_final,k600_raymond_eq1,k600_raymond_eq2,k600_raymond_eq3,k600_raymond_eq4,k600_raymond_eq5,k600_raymond_eq6,k600_natchimuthu_model1,k600_natchimuthu_model2,eD,slope_up20,x_pred)%>%rename(
  dist=x_pred,K600_ulseth=k600_eq1_final)%>%
  pivot_longer(cols = -c(site, dist,slope_up20,eD), names_to = c("method"))%>%rename(K600=value)


df_all_methods_2019 <- read.csv(here::here("ProcessedData/AllSynoptic_2019_Feb24.csv"))
df_all_methods_2019_melt <- df_all_methods_2019 %>%select(K600_chamber,k600_eq1_final,k600_raymond_eq1,k600_raymond_eq2,k600_raymond_eq3,k600_raymond_eq4,k600_raymond_eq5,k600_raymond_eq6,k600_natchimuthu_model1,k600_natchimuthu_model2,slope_mid,dist.m.AVE)%>%rename(
  dist=dist.m.AVE,K600_ulseth=k600_eq1_final)%>%
  pivot_longer(cols = -c(dist,slope_mid), names_to = c("method"))%>%rename(K600=value)




df_ulseth <- read.csv(here::here("ProcessedData/AllSynoptic_raymondk600_Oct8.csv"))%>%drop_na(k600_eq1_ulsethcorrection)
df_ulseth$site <- ifelse(df_ulseth$site=="GaviTrib1","GavilanTribs",
                          ifelse(df_ulseth$site=="GaviTrib2","GavilanTribs",
                                 ifelse(df_ulseth$site=="GaviTrib3","GavilanTribs", 
                                        ifelse(df_ulseth$site=="GaviUp","Gavilan Inlet",
                                               ifelse(df_ulseth$site=="GaviDown","Gavilan Outlet",
                                                      df_ulseth$site)))))


df_chamber$method <- "Chamber"
df_ulseth$method <- "Ulseth"
df_chamber$slope_up20 <- 0
df_chamber$eD <- 20

#df_ulseth$k600_eq1_final

df_chamber_select <- df_chamber%>%select(site,method,K600,eD,dist,slope_up20)
df_ulseth_select <- df_ulseth %>%
  select(site,method,k600_eq1_final,eD,slope_up20,x_pred)%>%rename(
    K600=k600_eq1_final,dist=x_pred)


df_new <- rbind(df_chamber_select,df_ulseth_select)

tribslope_ave <- mean(c(0.117,0.085,0.067))
slope_data <- data.frame(
  site = c("Ante", "Colm", "Gavilan Inlet","Gavilan Outlet","GavilanTribs"),
  slope_m.m   = c(0.123,.043,0.166,0.093,0.08966667),
  slope_perc = c(12.3,4.3,16.6,9.3,9.0))

df_new <- full_join(df_new,slope_data,by="site")
df_new <- df_new%>% arrange(desc(slope_perc))
df_all_methods_melt <- full_join(df_all_methods_melt,slope_data,by="site")
#df_new <- order(df_new$slope_perc)

options(scipen = 999)

Fig3a_ulseth <- ggplot() + 
  geom_boxplot(data=df_new, aes(x = as.factor(slope_perc), y = K600, fill = method)) + 
  geom_point(data=df_new, aes(x = as.factor(slope_perc), y = K600, fill = method),
             pch = 21, position = position_jitterdodge(0.15)) +
  xlab("Reach slope (%)") +  
  ylab(expression(paste(italic('k')[600]," (m ", d^-1,")"))) +
  scale_y_log10() +
  scale_fill_manual(name="Method",labels=c("\nChamber Method\n", "\nUlseth et al. (2019)"),
                    values = c("#636363","#998ec3")) +
  theme_bw(base_size = 18)

Fig3a_2019 <- ggplot() + 
  geom_point(data=df_all_methods_2019_melt%>%filter(dist<20) , aes(x = method, y = K600, fill = method),
             pch = 21, size=4,position = position_jitterdodge(0.15)) +
  xlab("") +  
  ylab(expression(paste(italic('k')[600]," (m ", d^-1,")"))) +
  scale_fill_manual(name="Method",
                    labels=c("Chamber Method", "Natchimuthu et al. (2017) model 1","Natchimuthu et al. (2017) model 2",
                             "Raymond et al. (2012) eq. 1","Raymond et al. (2012) eq. 2",
                             "Raymond et al. (2012) eq. 3","Raymond et al. (2012) eq. 4",
                             "Raymond et al. (2012) eq. 5","Raymond et al. (2012) eq. 6",
                             "Ulseth et al. (2019)"),
                    values = c("#636363","#fff7bc","#fec44f",
                               '#edf8fb','#ccece6','#99d8c9','#66c2a4','#2ca25f','#006d2c',
                               '#998ec3'
                               )) +
  scale_y_log10() + theme_bw(base_size = 18) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

Fig3a_ulseth <- ggplot() + 
  geom_boxplot(data=df_new, aes(x = as.factor(slope_perc), y = K600, fill = method)) + 
  geom_point(data=df_new, aes(x = as.factor(slope_perc), y = K600, fill = method),
             pch = 21, position = position_jitterdodge(0.15)) +
  xlab("Reach slope (%)") +  
  ylab(expression(paste(italic('k')[600]," (m ", d^-1,")"))) +
  scale_y_log10() +
  scale_fill_manual(name="Method",labels=c("\nChamber Method\n", "\nUlseth et al. (2019)"),
                    values = c("#636363","#998ec3")) +
  theme_bw(base_size = 18)


Fig3a_ray_eq1 <- ggplot() + 
  geom_boxplot(data=df_all_methods_melt%>%filter(method=="K600_chamber"|method=="k600_raymond_eq1") , 
               aes(x = as.factor(slope_perc), y = K600, fill = method)) + 
  #geom_point(data=df_new, aes(x = as.factor(slope_perc), y = K600, fill = method),
  #           pch = 21, position = position_jitterdodge(0.15)) +
  xlab("Reach slope (%)") +  
  ylab(expression(paste(italic('k')[600]," (m ", d^-1,")"))) +
  scale_y_log10() +
  scale_fill_manual(name="Method",labels=c("\nChamber Method\n", "\nRaymond et al. (2012) eq. 1\n"),
                    values = c("#d73027","#edf8fb")) +
  theme_bw(base_size = 18)

Fig3a_all <- ggplot() + 
  geom_boxplot(data=df_all_methods_melt#%>%filter(method=="K600_chamber"|method=="k600_raymond_eq2") 
               , 
               aes(x = as.factor(slope_perc), y = K600, fill = method)) + 
  xlab("Reach slope (%)") +  
  ylab(expression(paste(italic('k')[600]," (m ", d^-1,")"))) +
  scale_y_log10() +
  scale_fill_manual(name="Method",
                    labels=c("Chamber Method", "Natchimuthu et al. (2017) model 1","Natchimuthu et al. (2017) model 2",
                             "Raymond et al. (2012) eq. 1","Raymond et al. (2012) eq. 2",
                             "Raymond et al. (2012) eq. 3","Raymond et al. (2012) eq. 4",
                             "Raymond et al. (2012) eq. 5","Raymond et al. (2012) eq. 6",
                             "Ulseth et al. (2019)"),
                    values = c("#636363","#fff7bc","#fec44f",
                               '#edf8fb','#ccece6','#99d8c9','#66c2a4','#2ca25f','#006d2c',
                               '#998ec3'
                    )) +
  theme_bw(base_size = 16)





Fig3a_all <- ggplot() + 
  geom_point(data=df_all_methods_2019 %>%filter(dist.m.AVE<20) , aes(x = K600_chamber, y = k600_eq1_final)) + 
  ylab(expression(paste(italic('k')[600]," (m ", d^-1,")"))) +
  scale_y_log10() +
  theme_bw(base_size = 18)

Fig3a_all <- ggplot() + 
  geom_point(data=df_all_methods_2019 %>%filter(dist.m.AVE<20) , aes(x = K600_chamber, y = k600_raymond_eq3)) + 
  ylab(expression(paste(italic('k')[600]," (m ", d^-1,")"))) +
  scale_y_log10() +
  theme_bw(base_size = 18)



Fig3b_all <- ggplot() + 
  geom_boxplot(data=df_all_methods_melt%>%filter(eD < .02) , aes(x = site, y = K600, fill = method)) + 
  # geom_point(data=df_new, aes(x = site, y = K600, fill = method),
  #             pch = 21, position = position_jitterdodge(0.15)) +
  xlab("Reach slope (%)") +  
  ylab(expression(paste(italic('k')[600]," (m ", d^-1,")"))) +
  scale_y_log10() +
  # scale_fill_manual(name="Method",labels=c("\nChamber Method\n", "\nUlseth et al. (2019)"),
  #                    values = c("#F6BE00FF","#6D8325FF")) +
  theme_bw(base_size = 18)



Fig3c_all <- ggplot() + 
  geom_point(data=df_all_methods_melt%>%filter(site == "Gavilan Outlet")%>%filter(dist<50) , aes(x = method, y = K600, fill = method)) + 
  # geom_point(data=df_new, aes(x = site, y = K600, fill = method),
  #             pch = 21, position = position_jitterdodge(0.15)) +
  xlab("Reach slope (%)") +  
  ylab(expression(paste(italic('k')[600]," (m ", d^-1,")"))) +
  scale_y_log10() +
  # scale_fill_manual(name="Method",labels=c("\nChamber Method\n", "\nUlseth et al. (2019)"),
  #                    values = c("#F6BE00FF","#6D8325FF")) +
  theme_bw(base_size = 18)



Fig3c <- ggplot(df_new%>%filter(site == "Gavilan Outlet")%>%filter(dist<50), aes(x = as.factor(slope_perc), y = K600, fill = method)) + 
  geom_boxplot() +
  geom_point(data=df_new%>%filter(site =="Gavilan Outlet")%>%filter(dist<50), aes(x = as.factor(slope_perc), y = K600, fill = method),
             pch = 21, position = position_jitterdodge(0.15)) +
  xlab("Reach slope (%)") +   
  ylab(expression(paste(italic('k')[600]," [m ", d^-1,"]"))) +
  scale_fill_manual(name="Method",labels=c("\nChamber Method\n", "\nUlseth et al. (2019)\n(subreach slope < 5%)"),
                    values = c("#F6BE00FF","#6D8325FF")) +
  scale_y_log10() +
  theme_bw(base_size = 18)




Fig3 <- ggarrange(Fig3a + rremove("x.axis") , Fig3b,
                  ncol = 1, nrow = 2, legend = "right",
                  align = "hv", 
                  labels = c("a.","b."),
                  font.label = list(size = 20, color = "black", face = "bold", family = NULL, position = "top"))

Fig3
