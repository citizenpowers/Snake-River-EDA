library(tidyverse)
library(dplyr)
library(readr)
library(ggplot2)
library(lubridate)




# Import Data -------------------------------------------------------------


SK02_WQ_Data <- read_csv("//ad.sfwmd.gov/dfsroot/userdata/mpowers/Desktop/Snake-River-EDA/Data/SK02. WQ Data.csv")
SK09_WQ_Data <- read_csv("//ad.sfwmd.gov/dfsroot/userdata/mpowers/Desktop/Snake-River-EDA/Data/SK09 WQ.csv")



study_analytes <- c("FECAL COLIFORM, MPN","	PHOSPHATE, TOTAL AS P","PHOSPHATE, ORTHO AS P","AMMONIA-N","NITRATE+NITRITE-N")


# Tidy Data ---------------------------------------------------------------

Snake_Creek_WQ <- bind_rows(SK02_WQ_Data ,SK09_WQ_Data) %>%
filter(`Test Name` %in% study_analytes ) %>%
mutate(Date=dmy_hs(`Collection_Date`))
  
str(Snake_Creek_WQ)



# Figures -----------------------------------------------------------------


ggplot(Snake_Creek_WQ,aes(`Collection_Date`,Value,fill=`Station ID`,color=`Station ID`))+
facet_wrap(~`Test Name`)+
geom_point()+geom_smooth()


#DCS Depth vs TP wet season vs dry season STA34
ggplot(filter(Wet_vs_dry,Position=="Downstream",Ecotope!="Naiad",STA=="STA-3/4 Cell 2B"),aes(`DCS (Field Data)`,`TPO4`*1000,fill=Ecotope,color=Ecotope))+
  facet_grid(Season~Ecotope,labeller = labeller(.rows = label_value, .cols = label_parsed))+scale_y_continuous(breaks=seq(0,80,10))+Presentation_theme2+coord_cartesian(ylim=c(0,50))+
  geom_rect(aes(xmin = 45.72, ymin = -Inf, xmax = 76.2, ymax = 80),alpha=.5,fill="#e0ecf4",color="#e0ecf4")+
  geom_point(shape=21,size=2.5,color="grey70")+geom_smooth(span=10)+
  geom_hline(yintercept = 13,linetype="longdash",color="#785d37")+
  ylab(expression(TP~(mu~g~L^-1)))+xlab("Depth to Consolidated Substrate (cm)")+guides(fill="none",color="none")

ggsave(plot = last_plot(),filename="./Figures/TPO4 vs Water Depth wet vs dry season STA34- 2024 SFER.jpeg",width =8, height =6, units = "in")
