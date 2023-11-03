rm(list = setdiff(ls(),lsf.str()))
library(lubridate)
library(tidyverse)
library(data.table)
library(ggplot2)

SPY <- fread("/Users/luzhang/Desktop/data/SPY.csv")
SPY <- data.frame(SPY)
SPY$year <- format(SPY$time, "%Y")

SPY_table <- SPY %>% 
  group_by(year) %>%
 summarise(avg_open = mean(open), avg_high = mean(high),avg_low = mean(low),
           avg_close = mean(close), avg_volume = mean(volume))

SPY_table <- data.frame(SPY_table)
SPY_table

ratio <- max(SPY_table$avg_volume)/max(SPY_table$avg_low)
ggplot(SPY_table) +
  geom_bar(aes(x=year, y=avg_volume),stat="identity", fill = "steelblue") +
  geom_line(aes(x=year, y=avg_open*ratio),stat="identity", group = 1, color = "orange") +
  geom_point(aes(x=year, y=avg_open*ratio)) +
  geom_errorbar(aes(x=year, ymin=avg_high*ratio, ymax=avg_low*ratio), width=.1, colour="orange", 
                position = position_dodge(0.05)) +
  scale_y_continuous("Volume", sec.axis = sec_axis(~ . / ratio, name = "Price"))+
  labs(x="", title = "SPY")+
  theme_bw()



