library(ggplot2)
library(ggrepel)
library(scales)
library(tidyr)

setwd("Z:/20_Werk/2023_VU_WorldBank/data/may_24")

# country-based
pop_crit_10m<-10000000
pop_crit_5m <-5000000
pop_crit_1m <-1000000

countrydata<-read.csv("countries_urb_fig2.csv", sep=";", fileEncoding="UTF-8-BOM")
countrydata$urb2010<-(countrydata$urb_ls_safe_2010 + countrydata$urb_ls_risk_2010)
countrydata$urb2100<-(countrydata$urb_ls_safe_2100 + countrydata$urb_ls_risk_2100)
countrydata$perc_durb_2100<-(countrydata$urb2100 - countrydata$urb2010) / countrydata$urb2010
countrydata$perc_dflexp_2100<-(countrydata$urb_fl_risk_2100 - countrydata$urb_fl_risk_2010) / countrydata$urb_fl_risk_2010
countrydata$perc_dlsexp_2100<-(countrydata$urb_ls_risk_2100 - countrydata$urb_ls_risk_2010) / countrydata$urb_ls_risk_2010

countrydata$plotsize<-log(countrydata$Country_size / pop_crit_5m)+1
plot(countrydata$perc_durb_2100, countrydata$perc_dflexp_2100)


flcdata<-subset(countrydata, Country_size > pop_crit_5m & fl_intensity > 0.02)

flcdata$perc_growth_risk<-(flcdata$urb_fl_risk_2100 - flcdata$urb_fl_risk_2010) / (flcdata$urb2100 - flcdata$urb2010)
flcdata$intens_crit_label<-flcdata$perc_growth_risk> 0.125 | flcdata$fl_intensity > 0.2 | flcdata$Country_size > 250000000
flcdata$intens_label<-""
flcdata$intens_label[flcdata$intens_crit_label]<-flcdata$Country[flcdata$intens_crit_label]


flintensctr<-ggplot(flcdata, 
                    aes(y=perc_growth_risk, x=fl_intensity, color=WB.Region, label=intens_label)
                    #aes(y=perc_growth_risk, x=perc_durb_2100, color=WB.Region, label=intens_label)
                    #aes(x=growth.in.ls.safe, y=growth.in.ls.unsafe, color=coltest)
)
flintensctr+
  #coord_cartesian(xlim = c(0, 0.45), ylim=c(0, 0.6))+
  geom_point(aes(size = log(flcdata$Country_size / pop_crit_5m)+1)) + 
  geom_point(shape = 1, aes(size = log(flcdata$Country_size / pop_crit_5m)+1, color = "black")) +
  xlab("Share of land within flood-prone area") + ylab("Share of urban expansion within flood-prone area") +
  scale_colour_manual(values = c(
    "Europe and Central Asia" = "red",
    "East Asia and Pacific" = "lightblue",
    "South Asia" = "darkblue",
    "Middle East and North Africa" = "yellow",
    "Sub-Saharan Africa" = "orange",
    "North America" = "purple",
    "Latin America and Caribbean" = "green"
  )) +
  geom_vline(xintercept = 0, linetype = "dashed", color="grey") +
  #geom_abline(intercept = 0, slope = 1, size = 0.5, linetype="dashed") +
  geom_abline(intercept = 0, slope = 0, size = 0.5, linetype="longdash", color="grey") +
  theme_light() +
  scale_x_continuous(labels = scales::percent) + 
  scale_y_continuous(labels = scales::percent) + 
  scale_size_continuous(breaks = c(1.30, 2.30, 3.30), labels = c("10M", "100M", "1,000M")) +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=14)) +
  geom_text_repel(size=5, box.padding = 0.75, max.overlaps = Inf, color="black")+
  #theme(legend.position='none')
  guides(color = guide_legend(override.aes = list(size=6))) + 
  theme(legend.title = element_blank()) +
  theme(legend.text=element_text(size=12)) +
  theme(legend.position=c(0.8,0.75))

ggsave("Z:/20_Werk/2023_VU_WorldBank/pics/241023/floodprone_land_vs_perc_built_in_floodprone.png", width=8, height=8)

#flcdata<-subset(countrydata, Country_size > pop_crit_5m & (Pop_ls_risk_2010 / Country_size) > 0.02)
flcdata<-subset(countrydata, Country_size > pop_crit_5m & ls_intensity > 0.05)

flcdata$perc_growth_risk<-(flcdata$urb_ls_risk_2100 - flcdata$urb_ls_risk_2010) / (flcdata$urb2100 - flcdata$urb2010)
flcdata$intens_crit_label<-flcdata$perc_growth_risk> 0.125 | flcdata$ls_intensity > 0.2 | flcdata$Country_size > 250000000
flcdata$intens_label<-""
flcdata$intens_label[flcdata$intens_crit_label]<-flcdata$Country[flcdata$intens_crit_label]


lsintensctr<-ggplot(flcdata, 
                    aes(y=perc_growth_risk, x=ls_intensity, color=WB.Region, label=intens_label)
                    #aes(y=perc_growth_risk, x=perc_durb_2100, color=WB.Region, label=intens_label)
                    #aes(x=growth.in.ls.safe, y=growth.in.ls.unsafe, color=coltest)
)
lsintensctr+
  #coord_cartesian(xlim = c(0, 0.45), ylim=c(0, 0.6))+
  geom_point(aes(size = log(flcdata$Country_size / pop_crit_5m)+1)) + 
  geom_point(shape = 1, aes(size = log(flcdata$Country_size / pop_crit_5m)+1), color = "black") +
  xlab("Share of land within landslide-prone area") + ylab("Share of urban expansion within landslide-prone area") +
  scale_colour_manual(values = c(
    "Europe and Central Asia" = "red",
    "East Asia and Pacific" = "lightblue",
    "South Asia" = "darkblue",
    "Middle East and North Africa" = "yellow",
    "Sub-Saharan Africa" = "orange",
    "North America" = "purple",
    "Latin America and Caribbean" = "green"
  )) +
  geom_vline(xintercept = 0, linetype = "dashed", color="grey") +
  #geom_abline(intercept = 0, slope = 1, size = 0.5, linetype="dashed") +
  geom_abline(intercept = 0, slope = 0, size = 0.5, linetype="longdash", color="grey") +
  theme_light() +
  scale_x_continuous(labels = scales::percent) + 
  scale_y_continuous(labels = scales::percent) + 
  scale_size_continuous(breaks = c(1.30, 2.30, 3.30), labels = c("10M", "100M", "1,000M")) +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=14)) +
  geom_text_repel(size=5, box.padding = 0.75, max.overlaps = Inf, color="black")+
  theme(legend.position='none')
  
#theme(legend.title = element_blank()) + 
#theme(legend.text =  element_text(size = 14)) 
#+ theme(legend.position="top")

ggsave("Z:/20_Werk/2023_VU_WorldBank/pics/241023/landslideprone_land_vs_perc_built_in_landslideprone.png", width=8, height=8)

