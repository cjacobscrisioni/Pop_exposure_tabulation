library(ggplot2)
library(ggrepel)
library(scales)
library(tidyr)

setwd("Z:/20_Werk/2023_VU_WorldBank/data/oct_24")

pop_crit_10m<-10000000
pop_crit_5m <-5000000
pop_crit_1m <-1000000

countrydata<-read.csv("countries_pop_fig2.csv", sep=";", fileEncoding="UTF-8-BOM")

#flcdata<-subset(countrydata, Country_size > pop_crit_5m & (Pop_flood_risk_2010 / Country_size)>0.05)
flcdata<-subset(countrydata, Country_size > pop_crit_5m & (Pop_ls_risk_2010 / Country_size) > 0.02)
flcdata$pop2100<-(flcdata$Pop_ls_safe_2100 + flcdata$Pop_ls_risk_2100)
flcdata$perc_dpop_2100<-(flcdata$pop2100 - flcdata$Country_size) / flcdata$Country_size
flcdata$perc_dlsexp_2100<-(flcdata$Pop_ls_risk_2100 - flcdata$Pop_ls_risk_2010) / flcdata$Pop_ls_risk_2010
flcdata$plotsize<-log(flcdata$Country_size / pop_crit_5m)+1
plot(flcdata$perc_dpop_2100, flcdata$perc_dlsexp_2100)

#flcdata$crit_intens_label<-flcdata$perc_dflexp_2100 < -0.33 | flcdata$perc_dflexp_2100> 0.66 | flcdata$perc_dpop_2100 > 1 | flcdata$Country_size > 250000000 | (flcdata$perc_dpop_2100>0 & flcdata$perc_dflexp_2100 < 0) | (flcdata$perc_dpop_2100<0 & flcdata$perc_dflexp_2100 > 0)
flcdata$crit_intens_label<-flcdata$Country_size > 100000000 | (flcdata$perc_dpop_2100>0 & flcdata$perc_dlsexp_2100 < 0) | (flcdata$perc_dpop_2100<0 & flcdata$perc_dlsexp_2100 > 0)
flcdata$crit_all_label<-flcdata$perc_dlsexp_2100 > 1.25 | flcdata$perc_dpop_2100 > 1.5 | flcdata$Country_size > 250000000
#flcdata$crit_all_label<-flcdata$perc_dlsexp_2100 > 2 | flcdata$perc_dpop_2100 > 2.5 | flcdata$Pop_ls_risk_2010 > 10000000 
flcdata$all_label<-""
flcdata$all_label[flcdata$crit_all_label]<-flcdata$Country[flcdata$crit_all_label]
flcdata$intens_label<-""
flcdata$intens_label[flcdata$crit_intens_label]<-flcdata$Country[flcdata$crit_intens_label]

fl_all<-ggplot(flcdata, 
                aes(y=perc_dlsexp_2100, x=perc_dpop_2100, color=WB_region, label=all_label)
                #aes(x=growth.in.ls.safe, y=growth.in.ls.unsafe, color=coltest)
)
fl_all=fl_all+
  coord_cartesian(xlim=c(-1,4.78), ylim=c(-1, 4.75))+
  xlab("Relative population change") + 
  ylab("Change in landslide exposure") +
  geom_point(aes(size = log(flcdata$Country_size / pop_crit_5m)+1)) + 
  geom_point(shape = 1, aes(size = log(flcdata$Country_size / pop_crit_5m)+1), color = "black") +
  scale_size_continuous(breaks = c(1.30, 2.30, 3.30), labels = c("10M", "100M", "1,000M")) +
  #geom_point(shape = 1, size = log(flcdata$Pop_ls_risk_2010 / 100000)+1, color = "black") +
  scale_colour_manual(values = c(
    "Europe and Central Asia" = "red",
    "East Asia and Pacific" = "lightblue",
    "South Asia" = "darkblue",
    "Middle East and North Africa" = "yellow",
    "Sub-Saharan Africa" = "orange",
    "North America" = "purple",
    "Latin America and Caribbean" = "green"
  )) +
  #geom_abline(intercept = 0, slope = 1, size = 0.5, linetype="dashed") +
  theme_light() +
  geom_vline(xintercept = 0, linetype = "dashed", color="grey") +
  geom_abline(intercept = 0, slope = 0, size = 0.5, linetype="dashed", color="grey") +
  #geom_abline(intercept = 0, slope = 1, size = 0.5, linetype="dashed") +
  scale_x_continuous(labels = scales::percent) + 
  scale_y_continuous(labels = scales::percent) + 
  #geom_rect(aes(xmin = 1, xmax = 5, ymin = 0, ymax = 1500), color = "black", alpha = 0) +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=14)) +
  geom_text_repel(size=5, box.padding = 0.75, max.overlaps = Inf, color="black")+
  #theme(legend.position='none')
  theme(legend.title = element_blank()) +
  guides(color = guide_legend(override.aes = list(size=6))) + 
  theme(legend.text=element_text(size=13)) +
  theme(legend.position=c(0.15,0.75))
  

#ggsave("Z:/20_Werk/2023_VU_WorldBank/pics/241023/landslide_exposure_countries_all.png", width=8, height=8)


fl_zoom<-ggplot(flcdata, 
                    aes(y=perc_dlsexp_2100, x=perc_dpop_2100, color=WB_region, label=intens_label)
                    #aes(x=growth.in.ls.safe, y=growth.in.ls.unsafe, color=coltest)
)
fl_zoom=fl_zoom+
  coord_cartesian(xlim=c(-0.75,1), ylim=c(-0.75, 1))+
  geom_point(size = log(flcdata$Country_size / pop_crit_5m)+1) + 
  xlab("Relative population change") + 
  ylab("Change in landslide exposure") +
  geom_point(shape = 1, aes(size = log(flcdata$Country_size / pop_crit_5m)+1, color = "black")) +
  scale_colour_manual(values = c(
    "Europe and Central Asia" = "red",
    "East Asia and Pacific" = "lightblue",
    "South Asia" = "darkblue",
    "Middle East and North Africa" = "yellow",
    "Sub-Saharan Africa" = "orange",
    "North America" = "purple",
    "Latin America and Caribbean" = "green"
  )) +
  scale_size_continuous(breaks = c(1.30, 2.30, 3.30), labels = c("10M", "100M", "1,000M")) +
  #geom_abline(intercept = 0, slope = 1, size = 0.5, linetype="dashed") +
  theme_light() +
  geom_vline(xintercept = 0, linetype = "dashed", color="grey") +
  geom_abline(intercept = 0, slope = 0, size = 0.5, linetype="dashed", color="grey") +
  #geom_abline(intercept = 0, slope = 1, size = 0.5, linetype="dashed") +
  scale_x_continuous(labels = scales::percent) + 
  scale_y_continuous(labels = scales::percent) + 
  theme(axis.text=element_text(size=14), axis.title=element_text(size=14)) +
  geom_text_repel(size=5, box.padding = 0.75, max.overlaps = Inf, color="black")+
  theme(legend.position='none')
  #theme(legend.title = element_blank()) +
  #theme(legend.text=element_text(size=12)) +
  #theme(legend.position=c(0.2,0.75))

#ggsave("Z:/20_Werk/2023_VU_WorldBank/pics/241023/landslide_exposure_countries_zoom.png", width=8, height=8)

fl_all + 
  annotation_custom(ggplotGrob(fl_zoom), xmin = 2.2, xmax = 5, ymin = -1.25, ymax = 1.5) +
  geom_rect(aes(xmin = -0.85, xmax = 1.1, ymin = -0.85, ymax = 1.1), color='black', linetype='dotted', alpha=0) +
  geom_rect(aes(xmin = 2.2, xmax = 5, ymin = -1.25, ymax = 1.5), color='black', linetype='dotted', alpha=0) +
  geom_segment(aes(x = 1.1, y = -0.85, xend = 2.2, yend = -1.25), color="black",linetype='dotted') +
  geom_segment(aes(x = 1.1, y = 1.1, xend = 2.2, yend = 1.5), color="black",linetype='dotted')

  #geom_path(aes(x,y), data=data.frame(x = c(1,2), y=c(-0.75,-1.25)), color="black",linetype='solid')

ggsave("Z:/20_Werk/2023_VU_WorldBank/pics/241023/landslide_exposure_countries_exp.png", width=15.8, height=10)

