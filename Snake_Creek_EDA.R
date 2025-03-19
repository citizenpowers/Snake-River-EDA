library(tidyverse)
library(dplyr)
library(readr)
library(ggplot2)
library(lubridate)
library(RColorBrewer)
library(colorspace)
library(readxl)

library(Rccp)

# Import Data -------------------------------------------------------------

#work links
SK02_WQ_Data <- read_csv("//ad.sfwmd.gov/dfsroot/userdata/mpowers/Desktop/Snake-River-EDA/Data/SK02. WQ Data.csv")
SK09_WQ_Data <- read_csv("//ad.sfwmd.gov/dfsroot/userdata/mpowers/Desktop/Snake-River-EDA/Data/SK09 WQ.csv")
S29_Flow <- read_csv("//ad.sfwmd.gov/dfsroot/userdata/mpowers/Desktop/Snake-River-EDA/Data/S29 Flow .csv" , skip = 3)
S30C_Flow <- read_csv("//ad.sfwmd.gov/dfsroot/userdata/mpowers/Desktop/Snake-River-EDA/Data/S30c Flow.csv",  skip = 3)

#home links 
SK02_WQ_Data <- read_csv("Data/SK02. WQ Data.csv")
SK09_WQ_Data <- read_csv("Data/SK09 WQ.csv")
S29_Flow <- read_csv("Data/S29 Flow .csv", skip = 3)
S30C_Flow <- read_csv("Data/S30C Flow.csv", skip = 3)

study_analytes <- c("FECAL COLIFORM, MPN","PHOSPHATE, TOTAL AS P","PHOSPHATE, ORTHO AS P","AMMONIA-N","NITRATE+NITRITE-N")


# Tidy Data ---------------------------------------------------------------

Snake_Creek_WQ <- bind_rows(SK02_WQ_Data ,SK09_WQ_Data) %>%
filter(`Test Name` %in% study_analytes ) %>%
filter() %>%  
mutate(Date=parse_date_time(`Collection_Date`,"%d%b%y %H%S")) %>%
mutate(`Join Date`=as.Date(Date)) %>%  
mutate(Value=ifelse(`Test Name`=="PHOSPHATE, ORTHO AS P" & `Remark Code`=="U",0.002, Value)) %>%  #substituting 0.002 for undetected values
mutate(Year=year(Date),Month=month(Date),Day=day(Date),Hour=hour(Date),Minute=minute(Date),`Label Date`=ISOdatetime(2050,Month, Day, Hour, Minute, 0, tz = "UTC"))
  

Snake_Creek_Flow <- S29_Flow %>%
bind_rows(S30C_Flow) %>%  
mutate(Date=parse_date_time(`Daily Date`,"%d%b%y")) %>%
mutate(`Join Date`=as.Date(Date)) %>%  
mutate(Flow=if_else(is.na(`Data Value`),0,`Data Value`)) %>%  #converted NA to 0 flow
mutate(Year=year(Date),Month=month(Date),Day=day(Date),Hour=hour(Date),Minute=minute(Date),`Label Date`=ISOdatetime(2050,Month, Day, Hour, Minute, 0, tz = "UTC"))

  
Snake_Creek_WQ_Flow <- Snake_Creek_WQ %>%
left_join(filter(Snake_Creek_Flow,Station=="S30_C" ),by="Join Date")


# Summary Tables  ---------------------------------------------------------

Snake_Creek_WQ_summary <- Snake_Creek_WQ %>%
group_by(`Station ID`,`Test Name`)  %>%
summarise(records=n(),`Earliest Date`=min(`Join Date`),`Latest Date`=max(`Join Date`),Mean=round(mean(Value,na.rm=T),3),`Standard Deviation`=round(sd(Value,na.rm=T),3),Min=min(Value,na.rm=T),Max=prettyNum(max(Value,na.rm=T),big.mark=",",scientific=FALSE),Units=min(Units,na.rm=T))

write_csv(Snake_Creek_WQ_summary,"./Data/Snake_Creek_WQ_summary.csv")


Snake_Creek_Flow_Summary <- Snake_Creek_Flow %>%
group_by(Station) %>%
summarise(records=n(),`Earliest Date`=min(`Join Date`),`Latest Date`=max(`Join Date`),Mean=prettyNum(mean(Flow,na.rm=T),big.mark=",",scientific=FALSE),`Standard Deviation`=prettyNum(sd(Flow,na.rm=T),big.mark=",",scientific=FALSE),Min=min(Flow,na.rm=T),Max=max(Flow,na.rm=T))

write_csv(Snake_Creek_Flow_Summary,"./Data/Snake_Creek_Flow_Summary.csv")


# Figures Time Series -----------------------------------------------------------------

#Flow time series plot
ggplot(Snake_Creek_Flow,aes(`Label Date`,Flow,fill=`Station`,color=`Station`))+
scale_x_datetime(date_labels = "%b",date_breaks = "1 month")+facet_wrap(~Station,scales="free")+
scale_colour_manual(values = c("#e9a3c9", "#a1d76a"))+
scale_fill_manual(values = c("#e9a3c9", "#a1d76a"))+xlab("Month")+
ylab(expression(Flow~(cfs)))+labs(title="Flow Measured in Snake Creek at at S30C and S29S")+
geom_point(shape=21,color="black")+geom_smooth(color="black",fill="grey70")+theme_bw()

ggsave(plot = last_plot(),filename="./Figures/Seasonal Flow.jpeg",width =16, height =9, units = "in")


#TP time series plot
ggplot(filter(Snake_Creek_WQ,`Test Name`=="PHOSPHATE, TOTAL AS P"),aes(`Label Date`,Value,fill=`Station ID`,color=`Station ID`))+
scale_x_datetime(date_labels = "%b",date_breaks = "1 month")+
scale_colour_manual(values = c("#e9a3c9", "#a1d76a"))+
scale_fill_manual(values = c("#e9a3c9", "#a1d76a"))+xlab("Month")+
ylab(expression(TP~(m~g/L)))+coord_cartesian(ylim=c(0,0.04))+labs(title="PHOSPHATE, TOTAL AS P")+
geom_point(shape=21,color="black")+geom_smooth()+theme_bw()

ggsave(plot = last_plot(),filename="./Figures/Seasonal TP.jpeg",width =8, height =9, units = "in")

#OPO4 time series plot
ggplot(filter(Snake_Creek_WQ,`Test Name`=="PHOSPHATE, ORTHO AS P"),aes(`Label Date`,Value,fill=`Station ID`,color=`Station ID`))+
scale_x_datetime(date_labels = "%b",date_breaks = "1 month")+
scale_colour_manual(values = c("#e9a3c9", "#a1d76a"))+
scale_fill_manual(values = c("#e9a3c9", "#a1d76a"))+labs(title="PHOSPHATE, ORTHO AS P")+
ylab(expression(OPO4~(m~g/L)))+xlab("Month")+coord_cartesian(ylim=c(0,0.02))+
geom_point(shape=21,color="black")+geom_smooth(method="loess")+theme_bw()

ggsave(plot = last_plot(),filename="./Figures/Seasonal OPO4.jpeg",width =8, height =9, units = "in")

#AMMONIA-N time series plot
ggplot(filter(Snake_Creek_WQ,`Test Name`=="AMMONIA-N"),aes(`Label Date`,Value,fill=`Station ID`,color=`Station ID`))+
scale_x_datetime(date_labels = "%b",date_breaks = "1 month")+
scale_colour_manual(values = c("#e9a3c9", "#a1d76a"))+
scale_fill_manual(values = c("#e9a3c9", "#a1d76a"))+
ylab(expression(AMMONIA-N~(m~g/L)))+xlab("Month")+labs(title="AMMONIA-N")+
geom_point(shape=21,color="black")+geom_smooth(method="loess")+theme_bw()

ggsave(plot = last_plot(),filename="./Figures/Seasonal Ammonia.jpeg",width =8, height =9, units = "in")

#NITRATE+NITRITE-N time series plot
ggplot(filter(Snake_Creek_WQ,`Test Name`=="NITRATE+NITRITE-N"),aes(`Label Date`,Value,fill=`Station ID`,color=`Station ID`))+
scale_x_datetime(date_labels = "%b",date_breaks = "1 month")+
scale_colour_manual(values = c("#e9a3c9", "#a1d76a"))+
scale_fill_manual(values = c("#e9a3c9", "#a1d76a"))+
ylab(expression(NITRATE+NITRITE-N~(m~g/L)))+xlab("Month")+labs(title="NITRATE+NITRITE-N")+
geom_point(shape=21,color="black")+geom_smooth(method="loess")+theme_bw()

ggsave(plot = last_plot(),filename="./Figures/Seasonal NOx.jpeg",width =8, height =9, units = "in")

#FECAL COLIFORM, time series  MPN plot
ggplot(filter(Snake_Creek_WQ,`Test Name`=="FECAL COLIFORM, MPN"),aes(`Label Date`,Value,fill=`Station ID`,color=`Station ID`))+
scale_x_datetime(date_labels = "%b",date_breaks = "1 month")+
scale_colour_manual(values = c("#e9a3c9", "#a1d76a"))+
scale_fill_manual(values = c("#e9a3c9", "#a1d76a"))+
ylab(expression(FECAL~COLIFORM~(MPN/100)))+xlab("Month")+labs(title="FECAL COLIFORM, MPN")+
geom_point(shape=21,color="black")+geom_smooth(method="loess")+theme_bw()

ggsave(plot = last_plot(),filename="./Figures/Seasonal Fecal Coliform.jpeg",width =8, height =9, units = "in")


# Figures Flow vs Analytes ------------------------------------------------

#TP vs flow plot
ggplot(filter(Snake_Creek_WQ_Flow,`Test Name`=="PHOSPHATE, TOTAL AS P"),aes(Flow,Value,fill=`Station ID`,color=`Station ID`))+
scale_colour_manual(values = c("#e9a3c9", "#a1d76a"))+
scale_fill_manual(values = c("#e9a3c9", "#a1d76a"))+
ylab(expression(TP~(m~g/L)))+coord_cartesian(ylim=c(0,0.04))+labs(title="TP vs Flow")+xlab("Flow (cfs)")+
geom_point(shape=21,color="black")+geom_smooth(method="lm")+theme_bw()

ggsave(plot = last_plot(),filename="./Figures/TP vs Flow.jpeg",width =8, height =9, units = "in")

#OPO4 vs flow plot
ggplot(filter(Snake_Creek_WQ_Flow,`Test Name`=="PHOSPHATE, ORTHO AS P"),aes(Flow,Value,fill=`Station ID`,color=`Station ID`))+
scale_colour_manual(values = c("#e9a3c9", "#a1d76a"))+
scale_fill_manual(values = c("#e9a3c9", "#a1d76a"))+
ylab(expression(OPO4~(m~g/L)))+coord_cartesian(ylim=c(0,0.04))+labs(title="PHOSPHATE, ORTHO AS P")+xlab("Flow (cfs)")+
geom_point(shape=21,color="black")+geom_smooth()+theme_bw()

ggsave(plot = last_plot(),filename="./Figures/OPO4 vs Flow.jpeg",width =8, height =9, units = "in")

#AMMONIA-N vs flow plot
ggplot(filter(Snake_Creek_WQ_Flow,`Test Name`=="AMMONIA-N"),aes(Flow,Value,fill=`Station ID`,color=`Station ID`))+
scale_colour_manual(values = c("#e9a3c9", "#a1d76a"))+
scale_fill_manual(values = c("#e9a3c9", "#a1d76a"))+
ylab(expression(AMMONIA-N~(m~g/L)))+labs(title="AMMONIA-N")+xlab("Flow (cfs)")+
geom_point(shape=21,color="black")+geom_smooth(method="lm")+theme_bw()

ggsave(plot = last_plot(),filename="./Figures/AMMONIA-N vs Flow.jpeg",width =8, height =9, units = "in")

#NITRATE+NITRITE-N vs flow plot
ggplot(filter(Snake_Creek_WQ_Flow,`Test Name`=="NITRATE+NITRITE-N"),aes(Flow,Value,fill=`Station ID`,color=`Station ID`))+
scale_colour_manual(values = c("#e9a3c9", "#a1d76a"))+
scale_fill_manual(values = c("#e9a3c9", "#a1d76a"))+
ylab(expression(NITRATE+NITRITE-N~(m~g/L)))+labs(title="NITRATE+NITRITE-N")+xlab("Flow (cfs)")+
geom_point(shape=21,color="black")+geom_smooth(method="lm")+theme_bw()

ggsave(plot = last_plot(),filename="./Figures/NITRATE+NITRITE-N vs Flow.jpeg",width =8, height =9, units = "in")

#FECAL COLIFORM, MPN vs flow plot
ggplot(filter(Snake_Creek_WQ_Flow,`Test Name`=="FECAL COLIFORM, MPN"),aes(Flow,Value,fill=`Station ID`,color=`Station ID`))+
scale_colour_manual(values = c("#e9a3c9", "#a1d76a"))+
scale_fill_manual(values = c("#e9a3c9", "#a1d76a"))+
ylab(expression(FECAL~COLIFORM~(MPN/100)))+labs(title="FECAL COLIFORM, MPN")+xlab("Flow (cfs)")+
geom_point(shape=21,color="black")+geom_smooth()+theme_bw()

ggsave(plot = last_plot(),filename="./Figures/FECAL COLIFORM vs Flow.jpeg",width =8, height =9, units = "in")


