rm(list = setdiff(ls(),lsf.str()))
library(lubridate)
library(tidyverse)
library(data.table)

amzn <- fread("/Users/luzhang/Desktop/data/AMZN.csv")
amzn <- data.frame(amzn)
amzn$year <- format(amzn$time, "%Y")

amzn_table <- amzn %>% 
  group_by(year) %>%
  summarise(avg_price = mean(open), std_price = sqrt(var(open)),
            avg_volume = mean(volume))

amzn_table <- data.frame(amzn_table)
amzn_table
