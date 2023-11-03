rm(list = setdiff(ls(), lsf.str()))
library(tidyverse)
library(ggplot2)


# set work directory
setwd("/Users/luzhang/Desktop/data/AAPL")

aapl <- list.files(pattern = ".rds") %>%
  map(readRDS) %>% 
  bind_rows()

aapl$time<-as.POSIXct(aapl$time,taz="EST")

options(digits.secs=3)
Sys.setenv(TZ='EST')

# truncate data from 2008-01-012to 2018-05-31, 9:30 AM to 16:00 PM

aapl$time<-as.POSIXct(aapl$time,format="%H:%M:%OS", taz="EST")
aapl<- subset(aapl, lubridate::hour(aapl$time)*60
              +lubridate::minute(aapl$time) >= 9*60+30)
aapl <- subset(aapl, lubridate::hour(aapl$time)*60
               +lubridate::minute(aapl$time) <= 16*60)
head(aapl)
tail(aapl)
length(aapl$time)

aapl$year <-  format(aapl$time, "%Y")
aapl_table <- aapl %>% 
  group_by(year) %>%
  summarise(avg_price = mean(open), std = sqrt(var(open)),
            avg_volume = mean(volume))

aapl_table <- data.frame(aapl_table)
aapl_table

