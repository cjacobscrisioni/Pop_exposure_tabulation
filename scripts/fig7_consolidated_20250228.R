library(ggplot2)
library(ggrepel)
library(scales)
library(tidyr)

setwd("Z:/20_Werk/2023_VU_WorldBank/data/oct_24")

fds<-read.csv("flood_depth_change.csv", sep=";", fileEncoding="UTF-8-BOM")

plotdata<-fds %>% pivot_longer(
  cols = !SSP & !Flood.depth, 
  names_to = "Region", 
  values_to = "Pop"
)

plotdata$series<-paste(plotdata$SSP, " ", gsub('\\.', " ", plotdata$Region))

ggplot(plotdata, aes(fill=Flood.depth, y=Pop, x=SSP)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_manual(values=c("navy","lightblue")) +
  xlab("Results per SSP") + 
  ylab("Change in exposed population by 2100") +
  labs(fill='Flood depth') +
  theme_light() +
  scale_y_continuous(labels = scales::percent) + 
  facet_wrap(~gsub('\\.', ' ', Region))

ggsave("Z:/20_Werk/2023_VU_WorldBank/pics/241023/flood_exposure.png", width=8, height=8)
