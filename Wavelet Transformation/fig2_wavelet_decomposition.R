library(tidyverse)

# set work directory
setwd("/Users/luzhang/Desktop/data/AAPL")


# import multiple rds files
aapl <- list.files(pattern = ".rds") %>%
  map(readRDS) %>% 
  bind_rows()

#Length(aapl$time)
#head(aapl)
#tail(aapl)

# data preparing
aapl$time<-as.POSIXct(aapl$time,taz="EST")

options(digits.secs=3)
Sys.setenv(TZ='EST')

# truncate data from 2008-01-02 to 2018-05-31, 9:30 AM to 16:00 PM

aapl$time<-as.POSIXct(aapl$time,format="%H:%M:%OS", taz="EST")
aapl<- subset(aapl, lubridate::hour(aapl$time)*60
              +lubridate::minute(aapl$time) >= 9*60+30)
aapl <- subset(aapl, lubridate::hour(aapl$time)*60
               +lubridate::minute(aapl$time) <= 16*60)
head(aapl)
tail(aapl)
length(aapl$time)

#import packages
library(wmtsa)
#library(astsa)
library(highfrequency)
library(xts)
library(TTR)

day_index<-endpoints(prices, on = "days", k = 1)

#2008-01-02
start<-day_index[1]+1
end<-day_index[1+1]-30

raw_signal<- aapl[start:end,]$close
time <- aapl[start:end,]$time
fitered_signal <- wavShrink(raw_signal, wavelet="d4",
                            n.level=1, 
                            shrink.fun="soft", thresh.fun="adaptive")

#save as a data frame

fig2<-data.frame(time, raw_signal,fitered_signal)

# save as csv file
write.csv(fig2,"/Users/luzhang/Documents/GitHub/WrappingUp_Exploring_Intraday_Momentum_python/Data/fig2.2_raw_filtered_signal.csv")


