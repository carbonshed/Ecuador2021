#boxplots
#plot data
library(ggplot2)
library(dplyr)
library(ggpubr)
library(ggbreak)
#read in data 

library(MASS) # to access Animals data sets
library(scales) # to access break formatting functions


synop_allsites$site_2 <- synop_allsites$site
synop_allsites[synop_allsites$site=="GaviTrib1", "site_2"] <- "GaviTribs"
synop_allsites[synop_allsites$site=="GaviTrib2", "site_2"] <- "GaviTribs"
synop_allsites[synop_allsites$site=="GaviTrib3", "site_2"] <- "GaviTribs"

synop_allsites_sub <- synop_allsites%>%dplyr::select(site_2,adjusted_ppm)%>%rename(value=adjusted_ppm)%>%drop_na(value)
synop_allsites_sub$variable <- "co2"

k600_df$site_2 <- k600_df$site
k600_df[k600_df$site=="GaviTrib1", "site_2"] <- "GaviTribs"
k600_df[k600_df$site=="GaviTrib2", "site_2"] <- "GaviTribs"
k600_df[k600_df$site=="GaviTrib3", "site_2"] <- "GaviTribs"

k600_df_sub1 <- k600_df%>%dplyr::select(site_2,k600_eq1_final)%>%rename(value=k600_eq1_final)%>%drop_na(value)
k600_df_sub1$variable <- "k600_eq1_final"
k600_df_sub2 <- k600_df%>%dplyr::select(site_2,F_mol_m2_d_eq1)%>%rename(value=F_mol_m2_d_eq1)%>%drop_na(value)
k600_df_sub2$variable <- "F_mol_m2_d_eq1"

df_sub <- rbind(synop_allsites_sub,k600_df_sub1,k600_df_sub2)


#creat labels

#specify labels for plot
df_sub$site_2 <- factor(df_sub$site_2,levels = c("GaviTribs","Ante", "GaviUp", "GaviDown", "Colm"))
df_sub$site_2 <- factor(df_sub$site_2,levels = c("GaviTribs","Ante", "GaviUp", "GaviDown", "Colm"))

my_labels <- c('Tribs', 'Antenas', 'Gavil\u00e1n Inlet', 'Gavil\u00e1n Outlet','Colmillo')

# Basic scatter plot


f_names <- list(co2 = expression(paste(italic('p'),"CO"[2]," [ppm]")), 
                k600_eq1_final = expression(paste(italic('k')[600]," [m ", d^-1,"]")), 
                F_mol_m2_d_eq1 = expression(paste(" Evasion [ mol ", m^-2, d^-1,"]"))
                )
f_labeller <- function(variable, value){return(f_names[value])}

#annotate for significance

ann_text <- read.csv(here::here("KriddieFolder/annotation_boxplots_letters.csv"))
ann_text$site_2 <- as.factor(ann_text$site_2)
ann_text$variable <- as.factor(ann_text$variable)
ann_text$Label <- as.factor(ann_text$Label)
ann_text$value <- as.numeric(ann_text$value)


h_line <- data.frame(
  yintercept = c(air_ppm, NA,NA),
  variable = c(4, 4)
)

##plots

p <- ggplot(df_sub,
            aes(x=site_2, y=value)) 

air_ppm <- 416.45
p1 <- p +  facet_grid(rows = vars(variable),labeller = f_labeller,scales = "free")  +
#  geom_hline(yintercept=1.0, linetype="dotdash", color = "black", size=1) +
  geom_boxplot(outlier.shape = NA,fill="grey") +
  geom_jitter(shape=18,position=position_jitter(0.1)) +
  geom_hline(data = df_sub %>% filter(variable == "co2"),
             aes(yintercept = air_ppm), col = "red", linetype = 2, linewidth=1) +
  theme_bw(base_size = 16) +
  stat_summary(fun = "mean", geom = "point", shape = 23, size = 3,fill="red") +
  scale_y_continuous(name="",
                     transform="log1p", 
                     breaks=c(1,3,10,30,100,300,1000,3000,10000), 
                     labels=c("1","3","10","30","100","300","1,000","3,000","10,000"))+
  scale_x_discrete(labels=my_labels) + xlab("")+ theme(axis.text.x = element_text(angle = 25, vjust = 1, hjust=1))


p2 <- p1 + 
  geom_text(aes(x=site_2, y=value, label=Label, group=NULL),data=ann_text)  
  
  
