rm(list = setdiff(ls(), lsf.str()))
library(data.table)
library(PerformanceAnalytics)


dat <- fread("/Users/luzhang/Documents/GitHub/WrappingUp_Exploring_Intraday_Momentum_2rd_try/Data/AMZN_nscoreReturns.csv")
dat <- data.frame(dat)
head(dat)

chart.Correlation(dat,histogram = FALSE)
