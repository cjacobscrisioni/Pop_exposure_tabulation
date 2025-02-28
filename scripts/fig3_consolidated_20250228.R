library(ggplot2)
library(ggrepel)
library(scales)
library(tidyr)

setwd("Z:/20_Werk/2023_VU_WorldBank/data/may_24")

regdata<-read.csv("wb_regions_pop_fig2.csv", sep=";")
regdata$pop2100<-(regdata$Pop_ls_safe_2100 + regdata$Pop_ls_risk_2100)
regdata$pop2010<-(regdata$Pop_ls_safe_2010 + regdata$Pop_ls_risk_2010)
regdata$perc_dpop_2100<-(regdata$pop2100 - regdata$pop2010) / regdata$pop2010
regdata$perc_dlsexp_2100<-(regdata$Pop_ls_risk_2100 - regdata$Pop_ls_risk_2010) / regdata$Pop_ls_risk_2010
regdata$perc_flsexp_2100<-(regdata$Pop_flood_risk_2100 - regdata$Pop_flood_risk_2010) / regdata$Pop_flood_risk_2010
regdata$plotsize<-log(regdata$pop2010 / pop_crit_5m)+1
plot(regdata$perc_dpop_2100, regdata$perc_dlsexp_2100)

regdata$ls_perc_growth_risk<-(regdata$Pop_ls_risk_2100 - regdata$Pop_ls_risk_2010) / (regdata$pop2100 - regdata$pop2010)
regdata$fl_perc_growth_risk<-(regdata$Pop_flood_risk_2100 - regdata$Pop_flood_risk_2010) / (regdata$pop2100 - regdata$pop2010)

fl_all<-ggplot(regdata, 
                 aes(y=perc_flsexp_2100, x=perc_dpop_2100, color=WB.region, label=WB.region)
                 #aes(x=growth.in.ls.safe, y=growth.in.ls.unsafe, color=coltest)
)
fl_all+
  coord_cartesian(xlim=c(-0.5,2), ylim=c(-0.5, 2))+
  geom_point(shape = 1, aes(size = regdata$plotsize), color = "black") +
  geom_point(aes(size = regdata$plotsize)) +
  xlab("Relative population change") + 
  ylab("Change in flood exposure") +
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
  scale_size_continuous(range=c(3,7), breaks = c(5.60, 6.29, 6.99), labels = c("500M", "1,000M", "2,000M")) +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=14)) +
  geom_text_repel(size=5, box.padding = 0.75, max.overlaps = Inf, color="black")+
  guides(color = guide_legend(override.aes = list(size=5.5))) + 
  theme(legend.title = element_blank()) +
  theme(legend.text=element_text(size=12)) +
  theme(legend.position=c(0.8,0.25))

ggsave("Z:/20_Werk/2023_VU_WorldBank/pics/241023/regional_flood_exposure_change.png", width=8, height=8)


ls_all<-ggplot(regdata, 
               aes(y=perc_dlsexp_2100, x=perc_dpop_2100, color=WB.region, label=WB.region)
               #aes(x=growth.in.ls.safe, y=growth.in.ls.unsafe, color=coltest)
)
ls_all+
  coord_cartesian(xlim=c(-0.5,2), ylim=c(-0.5, 2))+
  geom_point(shape = 1, aes(size = regdata$plotsize), color = "black") +
  geom_point(aes(size = regdata$plotsize)) +
  xlab("Relative population change") + 
  ylab("Change in landslide exposure") +
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
  #geom_abline(intercept = 0, slope = 1, size = 0.5, linetype="dashed", color="lightgrey") +
  scale_x_continuous(labels = scales::percent) + 
  scale_y_continuous(labels = scales::percent) + 
  scale_size_continuous(range=c(3,7), breaks = c(5.60, 6.29, 6.99), labels = c("500M", "1,000M", "2,000M")) +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=14)) +
  geom_text_repel(size=5, box.padding = 0.75, max.overlaps = Inf, color="black")+
  theme(legend.position='none')
  

ggsave("Z:/20_Werk/2023_VU_WorldBank/pics/241023/regional_landslide_exposure_change.png", width=8, height=8)

# theme(legend.title = element_blank()) + theme(legend.text =  element_text(size = 14))
