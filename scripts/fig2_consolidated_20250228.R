library(ggplot2)
library(ggrepel)

setwd("Z:/20_Werk/2023_VU_WorldBank/data/aug_24")

mdata<-read.csv("tot_pop_2up.csv", sep=";")

seldata<-subset(mdata, ZoneId=="China" | ZoneId=="India" | ZoneId=="Indonesia" | ZoneId=="United States" | ZoneId=="Pakistan")
attach(seldata)

seldata$grp<-paste(seldata$ZoneId, "_", seldata$SSP)
seldata$cntr_year<-paste(seldata$ZoneId, "_", seldata$Year)
seldata$at_floodrisk<-tot_fl_1cmto1m + tot_fl_1mto2_5m+tot_fl_2_5mto5m+tot_fl_5mto10m+tot_fl_10mplus
seldata$at_landsliderisk<-tot_ls_high

seldata$at_floodrisk_M<-seldata$at_floodrisk / (1000 * 1000 * 100)
seldata$at_landsliderisk_M<-seldata$at_landsliderisk / (1000 * 1000 * 100)

agg_min<-aggregate(list(min_at_floodrisk=seldata$at_floodrisk_M, min_at_landsliderisk=seldata$at_landsliderisk_M), list(seldata$cntr_year), FUN=min)
names(agg_min)[names(agg_min)=="Group.1"]<-"cntr_year"
seldata<-merge(agg_min, seldata, by='cntr_year')

agg_max<-aggregate(list(max_at_floodrisk=seldata$at_floodrisk_M, max_at_landsliderisk=seldata$at_landsliderisk_M), list(seldata$cntr_year), FUN=max)
names(agg_max)[names(agg_max)=="Group.1"]<-"cntr_year"
seldata<-merge(agg_max, seldata, by='cntr_year')

ggplot(data=seldata, aes(x=Year, y=at_floodrisk_M, group=grp, color=ZoneId)) + 
  geom_ribbon(aes(ymin = min_at_floodrisk, ymax = max_at_floodrisk, fill=ZoneId), alpha = 0.05) +
  geom_line(linewidth=1.5, alpha=0.5) +
  xlab("Year") + ylab("Population at risk of flooding (x 100M)") +
  theme_light() +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=14)) +
  theme(legend.title = element_blank()) +
  theme(legend.text=element_text(size=12)) +
  theme(legend.position=c(0.15,0.825)) +
  theme(legend.text=element_text(size=12)) +
  #theme(panel.border = element_rect(colour = "black", fill=NA)) + 
  theme(legend.background = element_blank(), legend.box.background = element_rect(colour = "black"))

  #theme(legend.background = element_rect()) #, linewidth = 0.5)) #, colour = "black")
  #theme(legend.position='none')
  #theme(legend.title = element_blank()) + theme(legend.text =  element_text(size = 14))

ggsave("Z:/20_Werk/2023_VU_WorldBank/pics/241023/floodrisk_exposure_big5_trajectory.png", width=8, height=8)


ggplot(data=seldata, aes(x=Year, y=at_landsliderisk_M, group=grp, color=ZoneId)) + 
  geom_ribbon(aes(ymin = min_at_landsliderisk, ymax = max_at_landsliderisk, fill=ZoneId), alpha = 0.05) +
  geom_line(linewidth=1.5, alpha=0.5) +
  xlab("Year") + ylab("Population at high landslide probability (x 100M)") +
  theme_light() +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=14)) +
  theme(legend.position='none')
  # theme(legend.title = element_blank()) + theme(legend.text =  element_text(size = 14))

ggsave("Z:/20_Werk/2023_VU_WorldBank/pics/241023/landslide_exposure_big5_trajectory.png", width=8, height=8)
