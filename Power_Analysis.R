#goal of this script is to make sample size estimates required to detect 2ppb diffenrce in Snake Creek


library(tidyverse)
library(dplyr)
library(readr)
library(ggplot2)
library(lubridate)
library(pwr)





# import data -------------------------------------------------------------

Karina_C9 <- read_excel("//ad.sfwmd.gov/dfsroot/userdata/mpowers/Desktop/Snake-River-EDA/Data/Karina_C9.xlsx")





# Tidy data ---------------------------------------------------------------

#find the paired differnces in TP between adjacent stations
Karina_C9_tidy_TP <- Karina_C9 %>%
filter(`Param Name`=="Phosphorus, Total (TP)") %>%
mutate(Date=as.Date(`Date And Time` )) %>%  
select(`Station Unique ID`,`Param Name`,Value,Date)  %>%
pivot_wider(names_from="Station Unique ID",values_from="Value")  %>%
mutate(`Diff 1-2`=SK01-SK02,`Diff 1-5`=SK01-SK05,`Diff 1-9`=SK01-SK09,`Diff 1-10`=SK01-SK10,
       `Diff 2-5`=SK02-SK05,`Diff 2-9`=SK02-SK09,`Diff 2-10`=SK02-SK10,
       `Diff 5-9`=SK05-SK09,`Diff 5-10`=SK05-SK10,
       `Diff 9-10`=SK09-SK10)

Karina_C9_tidy_TN <- Karina_C9 %>%
filter(`Param Name`=="Total Nitrogen") %>%
mutate(Date=as.Date(`Date And Time` )) %>%  
select(`Station Unique ID`,`Param Name`,Value,Date)  %>%
pivot_wider(names_from="Station Unique ID",values_from="Value")  %>%
mutate(`Diff 1-2`=SK01-SK02,`Diff 1-5`=SK01-SK05,`Diff 1-9`=SK01-SK09,`Diff 1-10`=SK01-SK10,
         `Diff 2-5`=SK02-SK05,`Diff 2-9`=SK02-SK09,`Diff 2-10`=SK02-SK10,
         `Diff 5-9`=SK05-SK09,`Diff 5-10`=SK05-SK10,
         `Diff 9-10`=SK09-SK10)




# Summary stats -----------------------------------------------------------

Karina_C9_summary_TP <- Karina_C9_tidy_TP %>%
select(8:17) %>%
pivot_longer(names_to = "Difference",values_to = "Value",1:10)  %>%
group_by(Difference) %>%
summarise(n(),obs=sum(!is.na(Value)),mean=mean(Value,na.rm=T), StDev=sd(Value,na.rm=T),`Cohen's D`=abs(mean/StDev),`Cohen's D 2ppb`=abs(0.002/StDev),
`sample size`=pwr.t.test(n = NULL, d = `Cohen's D 2ppb`, sig.level = 0.05, power = 0.5, type = c("paired"),alternative = c("two.sided"))$n)


Karina_C9_summary_TN <- Karina_C9_tidy_TN %>%
  select(8:17) %>%
  pivot_longer(names_to = "Difference",values_to = "Value",1:10)  %>%
  group_by(Difference) %>%
  summarise(n(),obs=sum(!is.na(Value)),mean=mean(Value,na.rm=T), StDev=sd(Value,na.rm=T),`Cohen's D`=abs(mean/StDev),`Cohen's D 50ppb`=abs(0.05/StDev),
            `sample size`=pwr.t.test(n = NULL, d = `Cohen's D 50ppb`, sig.level = 0.05, power = 0.5, type = c("paired"),alternative = c("two.sided"))$n)



