# This code pulls the data files from CO2 folder, combines them, and plots them
# Note: the naming pattern for the files must be well-established
library(dplyr)
library(ggplot2)
theme_set(theme_bw()) #just a preference for the plots
library(readxl)

#set working directory
setwd("~/Ecuador2021/Ecuador2021/CO2")

setwd(here::here("Ecuador2021/Ecuador2021/CO2"))


#makes a list of data frames (aka the excel files)
file.list <- list.files(pattern='*.xlsx')
df.list <- lapply(file.list, read_excel)

#makes one giant data frame with all the data 
df <- bind_rows(df.list, .id = "station")

#clean up the data to make it able to be plotted 

data <- read.csv("NewOrganizedGrowthRates.csv")
data <- data %>% mutate(Block = factor(Block), Treatment = factor(Treatment), 
                        Side = factor(Side), Group = factor(Group))

data <- data %>% mutate(side_block = paste(Block, Side, sep="_"))
data <- data %>% mutate(treat_block = paste(Block, Side, Treatment, sep = "_"))

pd<- position_dodge(0.5)
ggplot(data = data, aes(x=Group,y=Growth_rate,color=Treatment, shape = Treatment))+ 
  facet_grid(~Side,labeller="label_both") +
  scale_color_viridis_d(end=.85)+ 
  labs(x="Group",y="Growth Rate (1/day)" ,color="Treatment")+ 
  geom_point(aes(y=Growth_rate),position= pd)+
  geom_linerange(aes(y = Growth_rate, ymin = Growth_rate - Stdev, ymax = Growth_rate + Stdev), position = pd)

