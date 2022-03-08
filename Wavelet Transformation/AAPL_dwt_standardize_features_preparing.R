##################################################
################## import APPL ###################

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

library(WaveletComp)
library(remotes)
library(wmtsa)
library(astsa)
library(ggplot2)
library(highfrequency)
library(xts)
library(TTR)

Close<-aapl$close
High<-aapl$high
Low<-aapl$low
Open<-aapl$open
Volume<-aapl$volume

prices<-xts(Close,aapl$time)
day_index<-endpoints(prices, on = "days", k = 1)

# last min prices for each trading day
lmP<-prices[day_index,]
#tail(lmP)

# average prices from 1 to 360 mins
n<-length(day_index)-1
avg_360<-rep(0,n)
for (i in 1:n){
  start<-day_index[i]+1
  end<-day_index[i+1]-30
  avg_360[i]<-mean(prices[start:end])
}

#create y label
n<-length(lmP)
y_label<-rep(0,n)
for (i in 1:n ){
  if(avg_360[i]<lmP[i]){
    y_label[i]=1
  }else{
    y_label[i]=0
  }
}

table(y_label)
#0    1 
#1318 1304

n<-length(day_index)-1
start<-rep(0,n)
end<-rep(0,n)
for (i in 1:n){
  start[i]<-day_index[i]+1
  end[i]<-day_index[i+1]-30
}

#ADX done

dwt_ADX<-rep(0,n)
for (i in 1:n){
  dwtH<-wavShrink(High[start[i]:end[i]], wavelet="d4",
                  n.level=1, 
                  shrink.fun="soft", thresh.fun="adaptive")
  dwtL<-wavShrink(Low[start[i]:end[i]], wavelet="d4",
                  n.level=1, 
                  shrink.fun="soft", thresh.fun="adaptive")
  dwtC<-wavShrink(Close[start[i]:end[i]], wavelet="d4",
                  n.level=1, 
                  shrink.fun="soft", thresh.fun="adaptive")
  dwt_HLC<-as.matrix(cbind(dwtH,dwtL,dwtC))
  std_HLC<-scale(dwt_HLC)
  ADX<-ADX(std_HLC,n=30)[,4]
  dwt_ADX[i]<-mean(ADX,na.rm = TRUE)
}

#aroon done
dwt_aroon<-rep(0,n)
for (i in 1:n){
  dwtH<-wavShrink(High[start[i]:end[i]], wavelet="d4",
                  n.level=1, 
                  shrink.fun="soft", thresh.fun="adaptive")
  dwtL<-wavShrink(Low[start[i]:end[i]], wavelet="d4",
                  n.level=1, 
                  shrink.fun="soft", thresh.fun="adaptive")
  dwt_HL<-as.matrix(cbind(dwtH,dwtL))
  std_HL<-scale(dwt_HL)
  aroon<-aroon(std_HL,n=30)[,3]# aroon oscillator
  dwt_aroon[i]<-mean(aroon,na.rm = TRUE)
}

#ATR done
dwt_trueHigh<-rep(0,n)
dwt_trueLow<-rep(0,n)
for (i in 1:n){
  dwtH<-wavShrink(High[start[i]:end[i]], wavelet="d4",
                  n.level=1, 
                  shrink.fun="soft", thresh.fun="adaptive")
  dwtL<-wavShrink(Low[start[i]:end[i]], wavelet="d4",
                  n.level=1, 
                  shrink.fun="soft", thresh.fun="adaptive")
  dwtC<-wavShrink(Close[start[i]:end[i]], wavelet="d4",
                  n.level=1, 
                  shrink.fun="soft", thresh.fun="adaptive")
  dwt_HLC<-as.matrix(cbind(dwtH,dwtL,dwtC))
  std_HLC<-scale(dwt_HLC)
  L<-ATR(std_HLC,n=30)[,3]
  H<-ATR(std_HLC,n=30)[,4]
  dwt_trueHigh[i]<-mean(H,na.rm = TRUE)
  dwt_trueLow[i]<-mean(L,na.rm = TRUE)
}

#BBands done
dwt_BBandsdn<-rep(0,n)
dwt_BBandsmavg<-rep(0,n)
dwt_BBandsup<-rep(0,n)

for (i in 1:n){
  dwtH<-wavShrink(High[start[i]:end[i]], wavelet="d4",
                  n.level=1, 
                  shrink.fun="soft", thresh.fun="adaptive")
  dwtL<-wavShrink(Low[start[i]:end[i]], wavelet="d4",
                  n.level=1, 
                  shrink.fun="soft", thresh.fun="adaptive")
  dwtC<-wavShrink(Close[start[i]:end[i]], wavelet="d4",
                  n.level=1, 
                  shrink.fun="soft", thresh.fun="adaptive")
  dwt_HLC<-as.matrix(cbind(dwtH,dwtL,dwtC))
  std_HLC<-scale(dwt_HLC)
  BBands<-BBands(std_HLC,n=30)
  dwt_BBandsdn[i]<-mean(BBands[,1],na.rm = TRUE)
  dwt_BBandsmavg[i]<-mean(BBands[,2],na.rm = TRUE)
  dwt_BBandsup[i]<-mean(BBands[,3],na.rm = TRUE)
}

#CCI done
dwt_CCI<-rep(0,n)
for (i in 1:n){
  dwtH<-wavShrink(High[start[i]:end[i]], wavelet="d4",
                  n.level=1, 
                  shrink.fun="soft", thresh.fun="adaptive")
  dwtL<-wavShrink(Low[start[i]:end[i]], wavelet="d4",
                  n.level=1, 
                  shrink.fun="soft", thresh.fun="adaptive")
  dwtC<-wavShrink(Close[start[i]:end[i]], wavelet="d4",
                  n.level=1, 
                  shrink.fun="soft", thresh.fun="adaptive")
  dwt_HLC<-as.matrix(cbind(dwtH,dwtL,dwtC))
  std_HLC<-scale(dwt_HLC)
  CCI<-CCI(std_HLC,n=30)
  dwt_CCI[i]<-mean(CCI,na.rm = TRUE)
}

#chaikinVolatility done
dwt_chaikinVolatility<-rep(0,n)
for (i in 1:n){
  dwtH<-wavShrink(High[start[i]:end[i]], wavelet="d4",
                  n.level=1, 
                  shrink.fun="soft", thresh.fun="adaptive")
  dwtL<-wavShrink(Low[start[i]:end[i]], wavelet="d4",
                  n.level=1, 
                  shrink.fun="soft", thresh.fun="adaptive")
  dwtC<-wavShrink(Close[start[i]:end[i]], wavelet="d4",
                  n.level=1, 
                  shrink.fun="soft", thresh.fun="adaptive")
  dwt_HLC<-as.matrix(cbind(dwtH,dwtL,dwtC))
  std_HLC<-scale(dwt_HLC)
  chaikinVolatility<-chaikinVolatility(std_HLC,n=30)
  dwt_chaikinVolatility[i]<-mean(chaikinVolatility,na.rm = TRUE)
}


#CLV done
dwt_CLV<-rep(0,n)
for (i in 1:n){
  dwtH<-wavShrink(High[start[i]:end[i]], wavelet="d4",
                  n.level=1, 
                  shrink.fun="soft", thresh.fun="adaptive")
  dwtL<-wavShrink(Low[start[i]:end[i]], wavelet="d4",
                  n.level=1, 
                  shrink.fun="soft", thresh.fun="adaptive")
  dwtC<-wavShrink(Close[start[i]:end[i]], wavelet="d4",
                  n.level=1, 
                  shrink.fun="soft", thresh.fun="adaptive")
  dwt_HLC<-as.matrix(cbind(dwtH,dwtL,dwtC))
  std_HLC<-scale(dwt_HLC)
  CLV<-CLV(std_HLC)
  dwt_CLV[i]<-mean(CLV,na.rm = TRUE)
}

#CMOClose done
dwt_CMOClose<-rep(0,n)
for (i in 1:n){
  dwtC<-wavShrink(Close[start[i]:end[i]], wavelet="d4",
                  n.level=1, 
                  shrink.fun="soft", thresh.fun="adaptive")
  stdC<-scale(dwtC)
  CMOClose<-CMO(stdC,n=30)
  dwt_CMOClose[i]<-mean(CMOClose,na.rm = TRUE)
}
#CTI done
dwt_CTI<-rep(0,n)
for (i in 1:n){
  dwtC<-wavShrink(Close[start[i]:end[i]], wavelet="d4",
                  n.level=1, 
                  shrink.fun="soft", thresh.fun="adaptive")
  stdC<-scale(dwtC)
  CTI<-CTI(stdC,n=30)
  dwt_CTI[i]<-mean(CTI,na.rm = TRUE)
}

#DonchianChannel done
dwt_DonchianChannelH<-rep(0,n)
dwt_DonchianChannelM<-rep(0,n)
dwt_DonchianChannelL<-rep(0,n)
DonchianChannel<-DonchianChannel(dwt_HL)
for (i in 1:n){
  dwtH<-wavShrink(High[start[i]:end[i]], wavelet="d4",
                  n.level=1, 
                  shrink.fun="soft", thresh.fun="adaptive")
  dwtL<-wavShrink(Low[start[i]:end[i]], wavelet="d4",
                  n.level=1, 
                  shrink.fun="soft", thresh.fun="adaptive")
  dwt_HL<-as.matrix(cbind(dwtH,dwtL))
  std_HL<-scale(dwt_HL)
  DonchianChannel<-DonchianChannel(std_HL,n=30)
  dwt_DonchianChannelH[i]<-mean(DonchianChannel[,1],na.rm = TRUE)
  dwt_DonchianChannelM[i]<-mean(DonchianChannel[,2],na.rm = TRUE)
  dwt_DonchianChannelL[i]<-mean(DonchianChannel[,3],na.rm = TRUE)
}

#DPOClose done
dwt_DPOClose<-rep(0,n)
for (i in 1:n){
  dwtC<-wavShrink(Close[start[i]:end[i]], wavelet="d4",
                  n.level=1, 
                  shrink.fun="soft", thresh.fun="adaptive")
  stdC<-scale(dwtC)
  DPO<-DPO(stdC,n=30)
  dwt_DPOClose[i]<-mean(DPO,na.rm = TRUE)
}

#DVI done
dwt_DVIClose<-rep(0,n)
for (i in 1:n){
  dwtC<-wavShrink(Close[start[i]:end[i]], wavelet="d4",
                  n.level=1, 
                  shrink.fun="soft", thresh.fun="adaptive")
  stdC<-scale(dwtC)
  DVI<-DVI(stdC,n=30)
  dwt_DVIClose[i]<-mean(DVI,na.rm = TRUE)
}

#GMMAClose done
dwt_GMMAClose<-rep(0,n)
for (i in 1:n){
  dwtC<-wavShrink(Close[start[i]:end[i]], wavelet="d4",
                  n.level=1, 
                  shrink.fun="soft", thresh.fun="adaptive")
  stdC<-scale(dwtC)
  GMMA<-GMMA(stdC)
  dwt_GMMAClose[i]<-mean(GMMA,na.rm = TRUE)
}

#KSTClose done
dwt_KSTClose<-rep(0,n)
for (i in 1:n){
  dwtC<-wavShrink(Close[start[i]:end[i]], wavelet="d4",
                  n.level=1, 
                  shrink.fun="soft", thresh.fun="adaptive")
  #stdC<-scale(dwtC)
  KST<-KST(dwtC,n=30)
  dwt_KSTClose[i]<-mean(KST,na.rm = TRUE)
}

#lags done
dwt_lags<-rep(0,n)
for (i in 1:n){
  dwtC<-wavShrink(Close[start[i]:end[i]], wavelet="d4",
                  n.level=1, 
                  shrink.fun="soft", thresh.fun="adaptive")
  stdC<-scale(dwtC)
  lags<-lags(stdC,n=30)
  dwt_lags[i]<-mean(lags,na.rm = TRUE)
}

#MACD done
dwt_MACD<-rep(0,n) #The price oscillator.

for (i in 1:n){
  dwtC<-wavShrink(Close[start[i]:end[i]], wavelet="d4",
                  n.level=1, 
                  shrink.fun="soft", thresh.fun="adaptive")
  stdC<-scale(dwtC)
  MACD<-MACD(stdC)[,1]
  dwt_MACD[i]<-mean(MACD,na.rm = TRUE)
}


#PBands done
dwt_PBandsdn<-rep(0,n)
dwt_PBandscenter<-rep(0,n)
dwt_PBandsup<-rep(0,n)

for (i in 1:n){
  dwtC<-wavShrink(Close[start[i]:end[i]], wavelet="d4",
                  n.level=1, 
                  shrink.fun="soft", thresh.fun="adaptive")
  dwtC<-xts(dwtC,aapl$time[start[i]:end[i]])
  stdC<-scale(dwtC)
  PBands<-PBands(stdC,n=30)
  dwt_PBandsdn[i]<-mean(PBands[,1],na.rm = TRUE)
  dwt_PBandscenter[i]<-mean(PBands[,2],na.rm = TRUE)
  dwt_PBandsup[i]<-mean(PBands[,3],na.rm = TRUE)
}

#ROCclose done
dwt_ROCClose<-rep(0,n)
for (i in 1:n){
  dwtC<-wavShrink(Close[start[i]:end[i]], wavelet="d4",
                  n.level=1, 
                  shrink.fun="soft", thresh.fun="adaptive")
  stdC<-scale(dwtC)
  ROC<-ROC(stdC,n=30)
  dwt_ROCClose[i]<-mean(ROC,na.rm = TRUE)
}

#momentum done
dwt_momentumClose<-rep(0,n)
for (i in 1:n){
  dwtC<-wavShrink(Close[start[i]:end[i]], wavelet="d4",
                  n.level=1, 
                  shrink.fun="soft", thresh.fun="adaptive")
  stdC<-scale(dwtC)
  momentum<-momentum(stdC,n=30)
  dwt_momentumClose[i]<-mean(momentum,na.rm = TRUE)
}

#RSI done
dwt_RSIClose<-rep(0,n)
for (i in 1:n){
  dwtC<-wavShrink(Close[start[i]:end[i]], wavelet="d4",
                  n.level=1, 
                  shrink.fun="soft", thresh.fun="adaptive")
  stdC<-scale(dwtC)
  RSI<-RSI(stdC,n=30)
  dwt_RSIClose[i]<-mean(RSI,na.rm = TRUE)
}

#runSum done
dwt_runSum<-rep(0,n)

for (i in 1:n){
  dwtC<-wavShrink(Close[start[i]:end[i]], wavelet="d4",
                  n.level=1, 
                  shrink.fun="soft", thresh.fun="adaptive")
  stdC<-scale(dwtC)
  runSum<-runSum(stdC,n=30)
  dwt_runSum[i]<-mean(runSum,na.rm = TRUE)
}

#runMin  done
dwt_runMin<-rep(0,n)
for (i in 1:n){
  dwtC<-wavShrink(Close[start[i]:end[i]], wavelet="d4",
                  n.level=1, 
                  shrink.fun="soft", thresh.fun="adaptive")
  stdC<-scale(dwtC)
  runMin<-runMin(stdC,n=30)
  dwt_runMin[i]<-mean(runMin,na.rm = TRUE)
}

#runMax done
dwt_runMax<-rep(0,n)
for (i in 1:n){
  dwtC<-wavShrink(Close[start[i]:end[i]], wavelet="d4",
                  n.level=1, 
                  shrink.fun="soft", thresh.fun="adaptive")
  stdC<-scale(dwtC)
  runMax<-runMax(stdC,n=30)
  dwt_runMax[i]<-mean(runMax,na.rm = TRUE)
}

#runMedian done
dwt_runMedian<-rep(0,n)
for (i in 1:n){
  dwtC<-wavShrink(Close[start[i]:end[i]], wavelet="d4",
                  n.level=1, 
                  shrink.fun="soft", thresh.fun="adaptive")
  stdC<-scale(dwtC)
  runMedian<-runMedian(stdC,n=30)
  dwt_runMedian[i]<-mean(runMedian,na.rm = TRUE)
}

#SAR done
dwt_SAR<-rep(0,n)
for (i in 1:n){
  dwtH<-wavShrink(High[start[i]:end[i]], wavelet="d4",
                  n.level=1, 
                  shrink.fun="soft", thresh.fun="adaptive")
  dwtL<-wavShrink(Low[start[i]:end[i]], wavelet="d4",
                  n.level=1, 
                  shrink.fun="soft", thresh.fun="adaptive")
  dwt_HL<-as.matrix(cbind(dwtH,dwtL))
  std_HL<-scale(dwt_HL)
  SAR<-SAR(std_HL)
  dwt_SAR[i]<-mean(SAR,na.rm = TRUE)
}

#SMA done
dwt_SMAClose<-rep(0,n)

for (i in 1:n){
  dwtC<-wavShrink(Close[start[i]:end[i]], wavelet="d4",
                  n.level=1, 
                  shrink.fun="soft", thresh.fun="adaptive")
  stdC<-scale(dwtC)
  SMA<-SMA(stdC,n=30)
  dwt_SMAClose[i]<-mean(SMA,na.rm = TRUE)
}

#EMA done
dwt_EMAClose<-rep(0,n)

for (i in 1:n){
  dwtC<-wavShrink(Close[start[i]:end[i]], wavelet="d4",
                  n.level=1, 
                  shrink.fun="soft", thresh.fun="adaptive")
  stdC<-scale(dwtC)
  EMA<-EMA(stdC,n=30)
  dwt_EMAClose[i]<-mean(EMA, na.rm = TRUE)
}

#DEMA done
dwt_DEMAClose<-rep(0,n)

for (i in 1:n){
  dwtC<-wavShrink(Close[start[i]:end[i]], wavelet="d4",
                  n.level=1, 
                  shrink.fun="soft", thresh.fun="adaptive")
  stdC<-scale(dwtC)
  DEMA<-DEMA(stdC,n=30)
  dwt_DEMAClose[i]<-mean(DEMA,na.rm = TRUE)
}

#SNR done
dwt_SNR<-rep(0,n)

for (i in 1:n){
  dwtH<-wavShrink(High[start[i]:end[i]], wavelet="d4",
                  n.level=1, 
                  shrink.fun="soft", thresh.fun="adaptive")
  dwtL<-wavShrink(Low[start[i]:end[i]], wavelet="d4",
                  n.level=1, 
                  shrink.fun="soft", thresh.fun="adaptive")
  dwtC<-wavShrink(Close[start[i]:end[i]], wavelet="d4",
                  n.level=1, 
                  shrink.fun="soft", thresh.fun="adaptive")
  dwt_HLC<-as.matrix(cbind(dwtH,dwtL,dwtC))
  std_HLC<-scale(dwt_HLC)
  SNR<-SNR(std_HLC, n=30)
  dwt_SNR[i]<-mean(SNR,na.rm = TRUE)
}


#SMI done
dwt_SMI<-rep(0,n)

for (i in 1:n){
  dwtH<-wavShrink(High[start[i]:end[i]], wavelet="d4",
                  n.level=1, 
                  shrink.fun="soft", thresh.fun="adaptive")
  dwtL<-wavShrink(Low[start[i]:end[i]], wavelet="d4",
                  n.level=1, 
                  shrink.fun="soft", thresh.fun="adaptive")
  dwtC<-wavShrink(Close[start[i]:end[i]], wavelet="d4",
                  n.level=1, 
                  shrink.fun="soft", thresh.fun="adaptive")
  dwt_HLC<-as.matrix(cbind(dwtH,dwtL,dwtC))
  std_HLC<-scale(dwt_HLC)
  SMI<-SMI(std_HLC,n=30)[,1]
  dwt_SMI[i]<-mean(SMI,na.rm = TRUE)
}

#TDI done
dwt_TDI<-rep(0,n)

for (i in 1:n){
  dwtC<-wavShrink(Close[start[i]:end[i]], wavelet="d4",
                  n.level=1, 
                  shrink.fun="soft", thresh.fun="adaptive")
  stdC<-scale(dwtC)
  TDI<-TDI(stdC,n=30)[,1]
  dwt_TDI[i]<-mean(TDI,na.rm = TRUE)
}

#TRIX done
dwt_TRIX<-rep(0,n)

for (i in 1:n){
  dwtC<-wavShrink(Close[start[i]:end[i]], wavelet="d4",
                  n.level=1, 
                  shrink.fun="soft", thresh.fun="adaptive")
  stdC<-scale(dwtC)
  TRIX<-TRIX(stdC,n=30)[,1]
  dwt_TRIX[i]<-mean(TRIX,na.rm = TRUE)
}

#VHF done
dwt_VHF<-rep(0,n)

for (i in 1:n){
  dwtC<-wavShrink(Close[start[i]:end[i]], wavelet="d4",
                  n.level=1, 
                  shrink.fun="soft", thresh.fun="adaptive")
  stdC<-scale(dwtC)
  VHF<-VHF(stdC,n=30)
  dwt_VHF[i]<-mean(VHF,na.rm = TRUE)
}

#Volatility done
dwt_volatility<-rep(0,n)

for (i in 1:n){
  dwtO<-wavShrink(Open[start[i]:end[i]], wavelet="d4",
                  n.level=1, 
                  shrink.fun="soft", thresh.fun="adaptive")
  dwtH<-wavShrink(High[start[i]:end[i]], wavelet="d4",
                  n.level=1, 
                  shrink.fun="soft", thresh.fun="adaptive")
  dwtL<-wavShrink(Low[start[i]:end[i]], wavelet="d4",
                  n.level=1, 
                  shrink.fun="soft", thresh.fun="adaptive")
  dwtC<-wavShrink(Close[start[i]:end[i]], wavelet="d4",
                  n.level=1, 
                  shrink.fun="soft", thresh.fun="adaptive")
  dwt_OHLC<-as.matrix(cbind(dwtO,dwtH,dwtL,dwtC))
  volatility<-volatility(dwt_OHLC,n=30)
  dwt_volatility[i]<-mean(volatility,na.rm = TRUE)
}

#williamsAD done
dwt_williamsAD<-rep(0,n)

for (i in 1:n){
  dwtH<-wavShrink(High[start[i]:end[i]], wavelet="d4",
                  n.level=1, 
                  shrink.fun="soft", thresh.fun="adaptive")
  dwtL<-wavShrink(Low[start[i]:end[i]], wavelet="d4",
                  n.level=1, 
                  shrink.fun="soft", thresh.fun="adaptive")
  dwtC<-wavShrink(Close[start[i]:end[i]], wavelet="d4",
                  n.level=1, 
                  shrink.fun="soft", thresh.fun="adaptive")
  dwt_HLC<-as.matrix(cbind(dwtH,dwtL,dwtC))
  std_HLC<-scale(dwt_HLC)
  williamsAD<-williamsAD(std_HLC)
  dwt_williamsAD[i]<-mean(williamsAD,na.rm = TRUE)
}


#ZigZag 
dwt_ZigZag<-rep(0,n)

for (i in 1:n){
  dwtH<-wavShrink(High[start[i]:end[i]], wavelet="d4",
                  n.level=1, 
                  shrink.fun="soft", thresh.fun="adaptive")
  dwtL<-wavShrink(Low[start[i]:end[i]], wavelet="d4",
                  n.level=1, 
                  shrink.fun="soft", thresh.fun="adaptive")
  dwt_HL<-as.matrix(cbind(dwtH,dwtL))
  std_HL<-scale(dwt_HL)
  ZigZag<-ZigZag(std_HL)
  dwt_ZigZag[i]<-mean(ZigZag,na.rm = TRUE)
}

#adding features related to volume

# chaikinAD done
dwt_chaikinAD<-rep(0,n)

for (i in 1:n){
  dwtH<-wavShrink(High[start[i]:end[i]], wavelet="d4",
                  n.level=1, 
                  shrink.fun="soft", thresh.fun="adaptive")
  dwtL<-wavShrink(Low[start[i]:end[i]], wavelet="d4",
                  n.level=1, 
                  shrink.fun="soft", thresh.fun="adaptive")
  dwtC<-wavShrink(Close[start[i]:end[i]], wavelet="d4",
                  n.level=1, 
                  shrink.fun="soft", thresh.fun="adaptive")
  dwtV<-wavShrink(Volume[start[i]:end[i]], wavelet="d4",
                  n.level=1, 
                  shrink.fun="soft", thresh.fun="adaptive")
  dwt_HLC<-as.matrix(cbind(dwtH,dwtL,dwtC))
  std_HLC<-scale(dwt_HLC)
  stdV<-scale(dwtV)
  chaikinAD<-chaikinAD(std_HLC,stdV)
  dwt_chaikinAD[i]<-mean(chaikinAD,na.rm = TRUE)
}

# CMF done
dwt_CMF<-rep(0,n)

for (i in 1:n){
  dwtH<-wavShrink(High[start[i]:end[i]], wavelet="d4",
                  n.level=1, 
                  shrink.fun="soft", thresh.fun="adaptive")
  dwtL<-wavShrink(Low[start[i]:end[i]], wavelet="d4",
                  n.level=1, 
                  shrink.fun="soft", thresh.fun="adaptive")
  dwtC<-wavShrink(Close[start[i]:end[i]], wavelet="d4",
                  n.level=1, 
                  shrink.fun="soft", thresh.fun="adaptive")
  dwtV<-wavShrink(Volume[start[i]:end[i]], wavelet="d4",
                  n.level=1, 
                  shrink.fun="soft", thresh.fun="adaptive")
  dwt_HLC<-as.matrix(cbind(dwtH,dwtL,dwtC))
  std_HLC<-scale(dwt_HLC)
  stdV<-scale(dwtV)
  CMF<-CMF(std_HLC,stdV,n=30)
  dwt_CMF[i]<-mean(CMF,na.rm = TRUE)
}

# CMO volume done
dwt_CMOvolume<-rep(0,n)
for (i in 1:n){
  dwtV<-wavShrink(Volume[start[i]:end[i]], wavelet="d4",
                  n.level=1, 
                  shrink.fun="soft", thresh.fun="adaptive")
  stdV<-scale(dwtV)
  CMO<-CMO(stdV,n=30)
  dwt_CMOvolume[i]<-mean(CMO,na.rm = TRUE)
}

# DPO volume done
dwt_DPOvolume<-rep(0,n)
for (i in 1:n){
  dwtV<-wavShrink(Volume[start[i]:end[i]], wavelet="d4",
                  n.level=1, 
                  shrink.fun="soft", thresh.fun="adaptive")
  stdV<-scale(dwtV)
  DPO<-DPO(stdV,n=30)
  dwt_DPOvolume[i]<-mean(DPO,na.rm = TRUE)
}

#EMV done
dwt_EMV<-rep(0,n)

for (i in 1:n){
  dwtH<-wavShrink(High[start[i]:end[i]], wavelet="d4",
                  n.level=1, 
                  shrink.fun="soft", thresh.fun="adaptive")
  dwtL<-wavShrink(Low[start[i]:end[i]], wavelet="d4",
                  n.level=1, 
                  shrink.fun="soft", thresh.fun="adaptive")
  dwtV<-wavShrink(Volume[start[i]:end[i]], wavelet="d4",
                  n.level=1, 
                  shrink.fun="soft", thresh.fun="adaptive")
  dwt_HL<-as.matrix(cbind(dwtH,dwtL))
  std_HL<-scale(dwt_HL)
  stdV<-scale(dwtV)
  EMV<-EMV(std_HL,stdV,n=30)
  dwt_EMV[i]<-mean(EMV,na.rm = TRUE)
}

# GMMA volume done
dwt_GMMAvolume<-rep(0,n)
for (i in 1:n){
  dwtV<-wavShrink(Volume[start[i]:end[i]], wavelet="d4",
                  n.level=1, 
                  shrink.fun="soft", thresh.fun="adaptive")
  stdV<-scale(dwtV)
  GMMA<-GMMA(stdV)
  dwt_GMMAvolume[i]<-mean(GMMA,na.rm = TRUE)
}

# MACD volume done
dwt_MACDvolume<-rep(0,n)
for (i in 1:n){
  dwtV<-wavShrink(Volume[start[i]:end[i]], wavelet="d4",
                  n.level=1, 
                  shrink.fun="soft", thresh.fun="adaptive")
  stdV<-scale(dwtV)
  MACD<-MACD(stdV)
  dwt_MACDvolume[i]<-mean(MACD,na.rm = TRUE)
}

#MFI done
dwt_MFI<-rep(0,n)

for (i in 1:n){
  dwtH<-wavShrink(High[start[i]:end[i]], wavelet="d4",
                  n.level=1, 
                  shrink.fun="soft", thresh.fun="adaptive")
  dwtL<-wavShrink(Low[start[i]:end[i]], wavelet="d4",
                  n.level=1, 
                  shrink.fun="soft", thresh.fun="adaptive")
  dwtC<-wavShrink(Close[start[i]:end[i]], wavelet="d4",
                  n.level=1, 
                  shrink.fun="soft", thresh.fun="adaptive")
  dwtV<-wavShrink(Volume[start[i]:end[i]], wavelet="d4",
                  n.level=1, 
                  shrink.fun="soft", thresh.fun="adaptive")
  dwt_HLC<-as.matrix(cbind(dwtH,dwtL,dwtC))
  std_HLC<-scale(dwt_HLC)
  stdV<-scale(dwtV)
  MFI<-MFI(std_HLC,stdV,n=30)
  dwt_MFI[i]<-mean(MFI,na.rm = TRUE)
}

#OBV done
dwt_OBV<-rep(0,n)
for (i in 1:n){
  dwtC<-wavShrink(Close[start[i]:end[i]], wavelet="d4",
                  n.level=1, 
                  shrink.fun="soft", thresh.fun="adaptive")
  stdC<-scale(dwtC)
  dwtV<-wavShrink(Volume[start[i]:end[i]], wavelet="d4",
                  n.level=1, 
                  shrink.fun="soft", thresh.fun="adaptive")
  stdV<-scale(dwtV)
  OBV<-OBV(stdC,stdV)
  dwt_OBV[i]<-mean(OBV,na.rm = TRUE)
}

# ROC volume
dwt_ROCvolume<-rep(0,n)
for (i in 1:n){
  dwtV<-wavShrink(Volume[start[i]:end[i]], wavelet="d4",
                  n.level=1, 
                  shrink.fun="soft", thresh.fun="adaptive")
  stdV<-scale(dwtV)
  ROC<-ROC(stdV,n=30)
  dwt_ROCvolume[i]<-mean(ROC,na.rm = TRUE)
}


#runPercentRank done
dwt_runPR<-rep(0,n)
for (i in 1:n){
  dwtC<-wavShrink(Close[start[i]:end[i]], wavelet="d4",
                  n.level=1, 
                  shrink.fun="soft", thresh.fun="adaptive")
  dwtV<-wavShrink(Volume[start[i]:end[i]], wavelet="d4",
                  n.level=1, 
                  shrink.fun="soft", thresh.fun="adaptive")
  stdV<-scale(dwtV)
  cv<-as.matrix(cbind(dwtC,stdV))
  stdcv<-scale(cv)
  runPR<-runPercentRank(stdcv,n=30)
  dwt_runPR[i]<-mean(runPR,na.rm = TRUE)
}

#SMA volume done
dwt_SMAvolume<-rep(0,n)
for (i in 1:n){
  dwtV<-wavShrink(Volume[start[i]:end[i]], wavelet="d4",
                  n.level=1, 
                  shrink.fun="soft", thresh.fun="adaptive")
  stdV<-scale(dwtV)
  SMA<-SMA(stdV,n=30)
  dwt_SMAvolume[i]<-mean(SMA,na.rm = TRUE)
}


csv<-data.frame(dwt_ADX, dwt_aroon,dwt_trueHigh,dwt_trueLow,
                dwt_BBandsdn,dwt_BBandsmavg,dwt_BBandsup,
                dwt_CCI, dwt_chaikinVolatility,dwt_CLV,
                dwt_CMOClose,dwt_CTI,dwt_DonchianChannelH,dwt_DonchianChannelM,dwt_DonchianChannelL,
                dwt_DPOClose,dwt_DVIClose,
                dwt_GMMAClose,dwt_KSTClose,dwt_lags,dwt_MACD,dwt_PBandsdn,dwt_PBandscenter,dwt_PBandsup,
                dwt_ROCClose,dwt_momentumClose,dwt_RSIClose,dwt_runSum,dwt_runMin,
                dwt_runMax,dwt_runMedian,dwt_SAR,dwt_SMAClose,dwt_EMAClose,dwt_DEMAClose,
                dwt_SNR,dwt_SMI,dwt_TDI,dwt_TRIX,dwt_VHF,
                dwt_volatility,dwt_williamsAD,dwt_ZigZag,
                dwt_chaikinAD,dwt_CMF,dwt_CMOvolume,dwt_DPOvolume,dwt_EMV,dwt_GMMAvolume,
                dwt_MACDvolume,dwt_MFI,dwt_OBV,dwt_ROCvolume,dwt_runPR,dwt_SMAvolume)

write.csv(csv, "/Users/luzhang/Desktop/AAPL_features/features_dwt_standardize.csv")

##################################################
################# Save features and label ########

features <- read.csv("/Users/luzhang/Desktop/AAPL_features/features_dwt_standardize.csv", header = T)
head(features)
dim(features)
#2662 56
features$X<-NULL

allSet<-data.frame(Y=as.factor(y_label),features)
head(allSet)

write.csv(allSet,"/Users/luzhang/Desktop/AAPL_features/allSet_dwt_standardize.csv" )

##################################################
################# random forest #################

rm(list = setdiff(ls(), lsf.str()))
library(fmlr)
library(quantmod)
library(TTR) # for various indicators
library(randomForestFML)
library(ROCR)
library(caret)

features <- read.csv("/Users/luzhang/Desktop/AAPL_features/allSet_dwt_standardize.csv", header = T)
head(features)
dim(features)
#2662 57
features$X<-NULL


allSet<-data.frame(features)
head(allSet)

#exclude NA at the begining of the indicators
idx_NA <- apply(allSet,1,function(x){sum(is.na(x))>0})
allSet <- subset(allSet, !idx_NA)
allSet$Y<-as.factor(allSet$Y)
dim(allSet)
# 2662 56
table(allSet$Y)
#0 1
#1318 1304
nx <- nrow(allSet)
trainSet <- allSet[1:floor(nx*2/3),]
testSet <- allSet[(floor(nx*2/3)+1):nx,]
dim(allSet); dim(trainSet); dim(testSet)
#[1] 2622   56
#[1] 1748   56
#[1] 874  56

table(trainSet$Y)
#0   1 
#876 872 
table(testSet$Y)
#0   1 
#442 432 

#### original  random forest 
set.seed(1)
model_rf <- caret::train(Y ~ .,
                         data = trainSet,
                         method = "rf",
                         preProcess = c("scale", "center"),
                         trControl = trainControl(method = "repeatedcv", 
                                                  number = 10, 
                                                  repeats = 10, 
                                                  verboseIter = FALSE))
final <- data.frame(actual = testSet$Y,
                    predict(model_rf, newdata = testSet, type = "prob"))
final$predict <- ifelse(final$X0 > 0.5, 0, 1)
cm_original <- confusionMatrix(as.factor(final$predict), testSet$Y)

#Confusion Matrix and Statistics

#Reference
#Prediction   0   1
#0 345  79
#1  97 353

#Accuracy : 0.7986          
#95% CI : (0.7705, 0.8247)
#No Information Rate : 0.5057          
#P-Value [Acc > NIR] : <2e-16          

#Kappa : 0.5974          

#Mcnemar's Test P-Value : 0.2             
                                          
#            Sensitivity : 0.7805          
#           Specificity : 0.8171          
#         Pos Pred Value : 0.8137          
 #        Neg Pred Value : 0.7844          
#             Prevalence : 0.5057          
#         Detection Rate : 0.3947          
#   Detection Prevalence : 0.4851          
#      Balanced Accuracy : 0.7988          
                                          
 #      'Positive' Class : 0               
          
#################   random forest _balanced data set   #####

################    under-sampling  #######################
ctrl <- trainControl(method = "repeatedcv", 
                     number = 10, 
                     repeats = 10, 
                     verboseIter = FALSE,
                     sampling = "down")
set.seed(1)
model_rf_under <- caret::train( Y~ .,
                                data = trainSet,
                                method = "rf",
                                preProcess = c("scale", "center"),
                                trControl = ctrl)
final_under <- data.frame(actual = testSet$Y,
                          predict(model_rf_under, newdata = testSet, type = "prob"))
final_under$predict <- ifelse(final_under$X0 > 0.5, 0, 1)
cm_under <- confusionMatrix(as.factor(final_under$predict), testSet$Y)

#Confusion Matrix and Statistics

#Reference
#Prediction   0   1
#0 349  82
#1  93 350

#Accuracy : 0.7998          
#95% CI : (0.7717, 0.8258)
#No Information Rate : 0.5057          
#P-Value [Acc > NIR] : <2e-16          

#Kappa : 0.5996          

#Mcnemar's Test P-Value : 0.4497          
                                          
#            Sensitivity : 0.7896          
##            Specificity : 0.8102          
#         Pos Pred Value : 0.8097          
#         Neg Pred Value : 0.7901          
#             Prevalence : 0.5057          
#         Detection Rate : 0.3993          
#   Detection Prevalence : 0.4931          
#      Balanced Accuracy : 0.7999          
                                          
#       'Positive' Class : 0               
                   
################## Over Sampling ##################

ctrl <- trainControl(method = "repeatedcv", 
                     number = 10, 
                     repeats = 10, 
                     verboseIter = FALSE,
                     sampling = "up")
set.seed(1)
model_rf_over <- caret::train( Y~ .,
                               data = trainSet,
                               method = "rf",
                               preProcess = c("scale", "center"),
                               trControl = ctrl)
final_over <- data.frame(actual = testSet$Y,
                         predict(model_rf_over, newdata = testSet, type = "prob"))
final_over$predict <- ifelse(final_over$X0 > 0.5, 0, 1)
cm_over <- confusionMatrix(as.factor(final_over$predict), testSet$Y)

#Confusion Matrix and Statistics

#Reference
#Prediction   0   1
#0 347  79
#1  95 353

#Accuracy : 0.8009          
#95% CI : (0.7729, 0.8269)
#No Information Rate : 0.5057          
#P-Value [Acc > NIR] : <2e-16          

#Kappa : 0.6019          

#Mcnemar's Test P-Value : 0.2555          
                                          
#            Sensitivity : 0.7851          
 #           Specificity : 0.8171          
#         Pos Pred Value : 0.8146          
#         Neg Pred Value : 0.7879          
#             Prevalence : 0.5057          
#         Detection Rate : 0.3970          
#   Detection Prevalence : 0.4874          
#      Balanced Accuracy : 0.8011          
                                          
 #      'Positive' Class : 0              

################## Rose ##################

ctrl <- trainControl(method = "repeatedcv", 
                     number = 10, 
                     repeats = 10, 
                     verboseIter = FALSE,
                     sampling = "rose")
set.seed(1)
model_rf_rose <- caret::train( Y~ .,
                               data = trainSet,
                               method = "rf",
                               preProcess = c("scale", "center"),
                               trControl = ctrl)
final_rose <- data.frame(actual = testSet$Y,
                         predict(model_rf_rose, newdata = testSet, type = "prob"))
final_rose$predict <- ifelse(final_rose$X0 > 0.5, 0, 1)
cm_rose <- confusionMatrix(as.factor(final_rose$predict), testSet$Y)

#Confusion Matrix and Statistics

#Reference
#Prediction   0   1
#0 339  96
#1 103 336

#Accuracy : 0.7723         
#95% CI : (0.743, 0.7997)
#No Information Rate : 0.5057         
#P-Value [Acc > NIR] : <2e-16         

#Kappa : 0.5446         

#Mcnemar's Test P-Value : 0.6706         
                                         
#            Sensitivity : 0.7670         
#            Specificity : 0.7778         
#         Pos Pred Value : 0.7793         
#         Neg Pred Value : 0.7654         
#             Prevalence : 0.5057         
#         Detection Rate : 0.3879         
#   Detection Prevalence : 0.4977         
#      Balanced Accuracy : 0.7724         
                                         
#       'Positive' Class : 0                  

################## smote ##################

ctrl <- trainControl(method = "repeatedcv", 
                     number = 10, 
                     repeats = 10, 
                     verboseIter = FALSE,
                     sampling = "smote")
set.seed(1)
model_rf_smote <- caret::train( Y~ .,
                                data = trainSet,
                                method = "rf",
                                preProcess = c("scale", "center"),
                                trControl = ctrl)
final_smote <- data.frame(actual = testSet$Y,
                          predict(model_rf_smote, newdata = testSet, type = "prob"))
final_smote$predict <- ifelse(final_smote$X0 > 0.5, 0, 1)
cm_smote <- confusionMatrix(as.factor(final_smote$predict), testSet$Y)

#Confusion Matrix and Statistics

#Reference
#Prediction   0   1
#0 314  51
#1 128 381

#Accuracy : 0.7952          
#95% CI : (0.7669, 0.8215)
#No Information Rate : 0.5057          
#P-Value [Acc > NIR] : < 2.2e-16       

#Kappa : 0.5912          

#Mcnemar's Test P-Value : 1.343e-08       
                                          
#            Sensitivity : 0.7104          
#            Specificity : 0.8819          
#         Pos Pred Value : 0.8603          
#         Neg Pred Value : 0.7485          
#             Prevalence : 0.5057          
 #        Detection Rate : 0.3593          
 #  Detection Prevalence : 0.4176          
#      Balanced Accuracy : 0.7962          
                                          
 #      'Positive' Class : 0           

########### compare predictions ########
models <- list(original = model_rf,
               under = model_rf_under,
               over = model_rf_over,
               smote = model_rf_smote,
               rose = model_rf_rose)
resampling <- resamples(models)
bwplot(resampling)

library(dplyr)
comparison <- data.frame(model = names(models),
                         Specificity = rep(NA, length(models)),
                         Precision = rep(NA, length(models)),
                         Recall = rep(NA, length(models)),
                         F1 = rep(NA, length(models)))


for (name in names(models)) {
  model <- get(paste0("cm_", name))
  class<-model$byClass
  comparison[comparison$model == name, ] <- filter(comparison, model == name) %>%
    mutate(
      Specificity = class["Specificity"],
      Precision = class["Precision"],
      Recall = class["Recall"],
      F1 = class["F1"])
}



write.csv(comparison, "/Users/luzhang/Desktop/AAPL_features/comparsion_dwt_standardized.csv")

library(tidyr)
comparison %>%
  gather(x, y, Specificity:F1) %>%
  ggplot(aes(x = x, y = y, color = model)) +
  geom_jitter(width = 0.2, alpha = 0.5, size = 3)

