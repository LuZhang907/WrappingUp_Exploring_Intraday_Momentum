##################################################
################## import APPL ###################
rm(list = setdiff(ls(), lsf.str()))
library(tidyverse)
library(remotes)
library(astsa)
library(ggplot2)
library(highfrequency)
library(xts)
library(wmtsa)
library(TTR)

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

# truncate data from 2008-01-012to 2018-05-31, 9:30 AM to 16:00 PM

aapl$time<-as.POSIXct(aapl$time,format="%H:%M:%OS", taz="EST")
aapl<- subset(aapl, lubridate::hour(aapl$time)*60
              +lubridate::minute(aapl$time) >= 9*60+30)
aapl <- subset(aapl, lubridate::hour(aapl$time)*60
               +lubridate::minute(aapl$time) <= 16*60)
head(aapl)
tail(aapl)
length(aapl$time)

############  standardized raw features calculation ##########

Close<-aapl$close
High<-aapl$high
Low<-aapl$low
Open<-aapl$open
Volume<-aapl$volume

prices<-xts(Close,aapl$time)
day_index<-endpoints(prices, on = "days", k = 1)

install.packages("/Users/luzhang/Downloads/wmtsa_2.0-3.tar.gz",repos=NULL, type='source')

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
#y_label
#0    1 
#1318 1304


##indicators calculation


n<-length(day_index)-1
start<-rep(0,n)
end<-rep(0,n)
for (i in 1:n){
  start[i]<-day_index[i]+1
  end[i]<-day_index[i+1]-30
}


raw_HLC<-as.matrix(cbind(High,Low,Close))
raw_HL<-as.matrix(cbind(High,Low))
raw_OHLC<-as.matrix(cbind(Open,High, Low, Close))
raw_CV<-as.matrix(cbind(Close,Volume))

# ADX done
raw_ADX<-rep(0,n)
for (i in 1:n){
  HLC<-raw_HLC[start[i]:end[i],]
  stdHLC<-scale(HLC)
  ADX<-ADX(stdHLC,n=30)[,4]
  raw_ADX[i]<-mean(ADX,na.rm = TRUE)
}

#aroon done
raw_aroon<-rep(0,n)
for (i in 1:n){
  HL<-raw_HL[start[i]:end[i],]
  stdHL<-scale(HL)
  aroon<-aroon(stdHL,n=30)[,3]# aroon oscillator
  raw_aroon[i]<-mean(aroon,na.rm = TRUE)
}
#ATR done
raw_trueHigh<-rep(0,n)
raw_trueLow<-rep(0,n)
for (i in 1:n){
  HLC<-raw_HLC[start[i]:end[i],]
  stdHLC<-scale(HLC)
  L<-ATR(stdHLC,n=30)[,3]
  H<-ATR(stdHLC,n=30)[,4]
  raw_trueHigh[i]<-mean(H,na.rm = TRUE)
  raw_trueLow[i]<-mean(L,na.rm = TRUE)
}

#BBands done
raw_BBandsdn<-rep(0,n)
raw_BBandsmavg<-rep(0,n)
raw_BBandsup<-rep(0,n)

for (i in 1:n){
  HLC<-raw_HLC[start[i]:end[i],]
  stdHLC<-scale(HLC)
  BBands<-BBands(stdHLC,n=30)
  raw_BBandsdn[i]<-mean(BBands[,1],na.rm = TRUE)
  raw_BBandsmavg[i]<-mean(BBands[,2],na.rm = TRUE)
  raw_BBandsup[i]<-mean(BBands[,3],na.rm = TRUE)
}

#CCI done
raw_CCI<-rep(0,n)
for (i in 1:n){
  HLC<-raw_HLC[start[i]:end[i],]
  stdHLC<-scale(HLC)
  CCI<-CCI(stdHLC,n=30)
  raw_CCI[i]<-mean(CCI,na.rm = TRUE)
}

# ChaikinAD done, new feature on AAPL
raw_chaikinAD<-rep(0,n)
for (i in 1:n){
  HLC<-raw_HLC[start[i]:end[i],]
  stdHLC<-scale(HLC)
  volume<-Volume[start[i]:end[i]]
  chaikinAD<-chaikinAD(stdHLC,volume)
  raw_chaikinAD[i]<-mean(chaikinAD,na.rm = TRUE)
}

#chaikinVolatility done
raw_chaikinVolatility<-rep(0,n)
for (i in 1:n){
  HLC<-raw_HLC[start[i]:end[i],]
  stdHLC<-scale(HLC)
  chaikinVolatility<-chaikinVolatility(stdHLC,n=30)
  raw_chaikinVolatility[i]<-mean(chaikinVolatility,na.rm = TRUE)
}

#CLV done
raw_CLV<-rep(0,n)
for (i in 1:n){
  HLC<-raw_HLC[start[i]:end[i],]
  stdHLC<-scale(HLC)
  CLV<-CLV(stdHLC)
  raw_CLV[i]<-mean(CLV,na.rm = TRUE)
}

# CMF, new feature, done
raw_CMF<-rep(0,n)
for (i in 1:n){
  HLC<-raw_HLC[start[i]:end[i],]
  stdHLC<-scale(HLC)
  volume<-Volume[start[i]:end[i]]
  CMF<-CMF(stdHLC,volume,n=30)
  raw_CMF[i]<-mean(CMF,na.rm = TRUE)
}

#CMOClose done
raw_CMOClose<-rep(0,n)
for (i in 1:n){
  C<-Close[start[i]:end[i]]
  stdC<-scale(C)
  CMOClose<-CMO(stdC,n=30)
  raw_CMOClose[i]<-mean(CMOClose,na.rm = TRUE)
}

#CMOvolumn done
raw_CMOvolumn<-rep(0,n)
for (i in 1:n){
  v<-Volume[start[i]:end[i]]
  CMOv<-CMO(v,n=30)
  raw_CMOvolumn[i]<-mean(CMOv,na.rm = TRUE)
}
#CTI done
raw_CTI<-rep(0,n)
for (i in 1:n){
  C<-Close[start[i]:end[i]]
  stdC<-scale(C)
  CTI<-CTI(stdC,n=30)
  raw_CTI[i]<-mean(CTI,na.rm = TRUE)
}

#DonchianChannel 
raw_DonchianChannelH<-rep(0,n)
raw_DonchianChannelM<-rep(0,n)
raw_DonchianChannelL<-rep(0,n)
for (i in 1:n){
  HL<-raw_HL[start[i]:end[i],]
  stdHL<-scale(HL)
  DonchianChannel<-DonchianChannel(stdHL,n=30)
  raw_DonchianChannelH[i]<-mean(DonchianChannel[,1],na.rm = TRUE)
  raw_DonchianChannelM[i]<-mean(DonchianChannel[,2],na.rm = TRUE)
  raw_DonchianChannelL[i]<-mean(DonchianChannel[,3],na.rm = TRUE)
}

#DPOClose done
raw_DPOClose<-rep(0,n)
for (i in 1:n){
  C<-Close[start[i]:end[i]]
  stdC<-scale(C)
  DPO<-DPO(stdC,n=30)
  raw_DPOClose[i]<-mean(DPO,na.rm = TRUE)
}

#DPOvolumn done
raw_DPOvolumn<-rep(0,n)
for (i in 1:n){
  V<-Volume[start[i]:end[i]]
  stdV<-scale(V)
  DPO<-DPO(stdV,n=30)
  raw_DPOvolumn[i]<-mean(DPO,na.rm = TRUE)
}

#DVI done
raw_DVIClose<-rep(0,n)
for (i in 1:n){
  C<-Close[start[i]:end[i]]
  stdC<-scale(C)
  DVI<-DVI(stdC,n=30)
  raw_DVIClose[i]<-mean(DVI,na.rm = TRUE)
}

# EMV, new feature,done
raw_EMV<-rep(0,n)
for (i in 1:n){
  HL<-raw_HL[start[i]:end[i],]
  stdHL<-scale(HL)
  volume<-Volume[start[i]:end[i]]
  stdV<-scale(volume)
  EMV<-EMV(stdHL,stdV,n=30)[,1]
  raw_EMV[i]<-mean(EMV,na.rm = TRUE)
}

#GMMAClose done
raw_GMMAClose<-rep(0,n)
for (i in 1:n){
  C<-Close[start[i]:end[i]]
  stdC<-scale(C)
  GMMA<-GMMA(stdC)
  raw_GMMAClose[i]<-mean(GMMA,na.rm = TRUE)
}

#GMMAvolumn done
raw_GMMAvolumn<-rep(0,n)
for (i in 1:n){
  v<-Volume[start[i]:end[i]]
  stdV <-scale(v)
  GMMA<-GMMA(stdV)
  raw_GMMAvolumn[i]<-mean(GMMA,na.rm = TRUE)
}

#KSTClose done
raw_KSTClose<-rep(0,n)
for (i in 1:n){
  C<-Close[start[i]:end[i]]
  #C<-scale(C) #warnings produce, so using the original dataset
  KST<-KST(C)
  raw_KSTClose[i]<-mean(KST,na.rm = TRUE)
}

#lags done
raw_lags<-rep(0,n)
for (i in 1:n){
  C<-Close[start[i]:end[i]]
  stdC<-scale(C)
  lags<-lags(stdC,n=30)
  raw_lags[i]<-mean(lags,na.rm = TRUE)
}

#MACDclose done
raw_MACDclose<-rep(0,n) #The price oscillator.

for (i in 1:n){
  C<-Close[start[i]:end[i]]
  stdC<-scale(C)
  MACD<-MACD(stdC)[,1]
  raw_MACDclose[i]<-mean(MACD,na.rm = TRUE)
}

#MACDvolumn done 
raw_MACDvolumn<-rep(0,n) #The price oscillator.

for (i in 1:n){
  v<-Volume[start[i]:end[i]]
  stdV<-scale(v)
  MACD<-MACD(stdV)[,1]
  raw_MACDvolumn[i]<-mean(MACD,na.rm = TRUE)
}

#MFI done
raw_MFI<-rep(0,n)
for (i in 1:n){
  HLC<-raw_HLC[start[i]:end[i],]
  stdHLC<-scale(HLC)
  volumn<-Volume[start[i]:end[i]]
  stdV<-scale(volumn)
  MFI<-MFI(stdHLC,stdV,n=30)
  raw_MFI[i]<-mean(MFI,na.rm = TRUE)
}

#OBV done
raw_OBV<-rep(0,n)
for (i in 1:n){
  close<-Close[start[i]:end[i]]
  stdC<-scale(close)
  volumn<-Volume[start[i]:end[i]]
  stdV <- scale(volumn)
  obv<-OBV(stdC,volumn)
  raw_OBV[i]<-mean(obv,na.rm = TRUE)
}

#PBands done
raw_PBandsdn<-rep(0,n)
raw_PBandscenter<-rep(0,n)
raw_PBandsup<-rep(0,n)

for (i in 1:n){
  price<-prices[start[i]:end[i]]
  stdp<-scale(price)
  PBands<-PBands(stdp,n=30)
  raw_PBandsdn[i]<-mean(PBands[,1],na.rm = TRUE)
  raw_PBandscenter[i]<-mean(PBands[,2],na.rm = TRUE)
  raw_PBandsup[i]<-mean(PBands[,3],na.rm = TRUE)
}

#ROCclose done
raw_ROCClose<-rep(0,n)
for (i in 1:n){
  C<-Close[start[i]:end[i]]
  stdC<-scale(C)
  ROC<-ROC(stdC,n=30)
  raw_ROCClose[i]<-mean(ROC,na.rm = TRUE)
}

#ROCvolumn done
raw_ROCvolumn<-rep(0,n)
for (i in 1:n){
  v<-Volume[start[i]:end[i]]
  stdV<-scale(v)
  ROC<-ROC(v,n=30)
  raw_ROCvolumn[i]<-mean(ROC,na.rm = TRUE)
}

# momentum close done
raw_momentumClose<-rep(0,n)
for (i in 1:n){
  C<-Close[start[i]:end[i]]
  stdC<-scale(C)
  momentum<-momentum(stdC,n=30)
  raw_momentumClose[i]<-mean(momentum,na.rm = TRUE)
}

# momentum volumn done
raw_momentumV<-rep(0,n)
for (i in 1:n){
  v<-Volume[start[i]:end[i]]
  stdV<-scale(v)
  momentum<-momentum(stdV,n=30)
  raw_momentumV[i]<-mean(momentum,na.rm = TRUE)
}

#RSI done
raw_RSIClose<-rep(0,n)
for (i in 1:n){
  C<-Close[start[i]:end[i]]
  stdC<-scale(C)
  RSI<-RSI(stdC,n=30)
  raw_RSIClose[i]<-mean(RSI,na.rm = TRUE)
}

#runPercentRank done
raw_runPrank<-rep(0,n)
for (i in 1:n){
  #v<-Volumn[start[i]:end[i]]
  cv<-raw_CV[start[i]:end[i],]
  stdcv<-scale(cv)
  runPR<-runPercentRank(stdcv,n=30)
  raw_runPrank[i]<-mean(runPR,na.rm = TRUE)
}

#runSum close done
raw_runSum<-rep(0,n)

for (i in 1:n){
  C<-Close[start[i]:end[i]]
  stdC<-scale(C)
  runSum<-runSum(stdC,n=30)
  raw_runSum[i]<-mean(runSum,na.rm = TRUE)
}


#runMin  done
raw_runMin<-rep(0,n)
for (i in 1:n){
  C<-Close[start[i]:end[i]]
  stdC<-scale(C)
  runMin<-runMin(stdC,n=30)
  raw_runMin[i]<-mean(runMin,na.rm = TRUE)
}

#runMax done
raw_runMax<-rep(0,n)
for (i in 1:n){
  C<-Close[start[i]:end[i]]
  stdC<-scale(C)
  runMax<-runMax(stdC,n=30)
  raw_runMax[i]<-mean(runMax,na.rm = TRUE)
}

#runMedian done
raw_runMedian<-rep(0,n)
for (i in 1:n){
  C<-Close[start[i]:end[i]]
  stdC<-scale(C)
  runMedian<-runMedian(stdC,n=30)
  raw_runMedian[i]<-mean(runMedian,na.rm = TRUE)
}

#SAR done
raw_SAR<-rep(0,n)
for (i in 1:n){
  HL<-raw_HL[start[i]:end[i],]
  stdHL<-scale(HL)
  SAR<-SAR(stdHL)
  raw_SAR[i]<-mean(SAR,na.rm = TRUE)
}

#SMA close done
raw_SMAClose<-rep(0,n)

for (i in 1:n){
  C<-Close[start[i]:end[i]]
  stdC<-scale(C)
  SMA<-SMA(stdC,n=30)
  raw_SMAClose[i]<-mean(SMA,na.rm = TRUE)
}

#SMA volumn done
raw_SMAv<-rep(0,n)

for (i in 1:n){
  v<-Volume[start[i]:end[i]]
  stdV<-scale(v)
  SMA<-SMA(stdV,n=30)
  raw_SMAv[i]<-mean(SMA,na.rm = TRUE)
}

#EMA done
raw_EMAClose<-rep(0,n)

for (i in 1:n){
  C<-Close[start[i]:end[i]]
  stdC<-scale(C)
  EMA<-EMA(stdC,n=30)
  raw_EMAClose[i]<-mean(EMA, na.rm = TRUE)
}

#DEMA done
raw_DEMAClose<-rep(0,n)

for (i in 1:n){
  C<-Close[start[i]:end[i]]
  stdC<-scale(C)
  DEMA<-DEMA(stdC,n=30)
  raw_DEMAClose[i]<-mean(DEMA,na.rm = TRUE)
}

#SNR done
raw_SNR<-rep(0,n)

for (i in 1:n){
  HLC<-raw_HLC[start[i]:end[i],]
  stdHLC<-scale(HLC)
  SNR<-SNR(stdHLC, n=30)
  raw_SNR[i]<-mean(SNR,na.rm = TRUE)
}


#SMI done
raw_SMI<-rep(0,n)

for (i in 1:n){
  HLC<-raw_HLC[start[i]:end[i],]
  stdHLC<-scale(HLC)
  SMI<-SMI(stdHLC)[,1]
  raw_SMI[i]<-mean(SMI,na.rm = TRUE)
}

#TDI done
raw_TDI<-rep(0,n)

for (i in 1:n){
  C<-Close[start[i]:end[i]]
  stdC<-scale(C)
  TDI<-TDI(stdC,n=30)[,1]
  raw_TDI[i]<-mean(TDI,na.rm = TRUE)
}

#TRIX done
raw_TRIX<-rep(0,n)

for (i in 1:n){
  C<-Close[start[i]:end[i]]
  stdC<-scale(C)
  TRIX<-TRIX(stdC,n=30)[,1]
  raw_TRIX[i]<-mean(TRIX,na.rm = TRUE)
}

#VHF done
raw_VHF<-rep(0,n)

for (i in 1:n){
  C<-Close[start[i]:end[i]]
  stdC<-scale(C)
  VHF<-VHF(stdC,n=30)
  raw_VHF[i]<-mean(VHF,na.rm = TRUE)
}

#Volatility done
raw_volatility<-rep(0,n)

for (i in 1:n){
  OHLC<-raw_OHLC[start[i]:end[i],]
  #stdOHLC<-scale(OHLC) # errors, non-leading NAs
  volatility<-volatility(OHLC,n=30)
  raw_volatility[i]<-mean(volatility,na.rm = TRUE)
}

#williamsAD done
raw_williamsAD<-rep(0,n)

for (i in 1:n){
  HLC<-raw_HLC[start[i]:end[i],]
  stdHLC<-scale(HLC)
  williamsAD<-williamsAD(stdHLC)
  raw_williamsAD[i]<-mean(williamsAD,na.rm = TRUE)
}


#ZigZag 
raw_ZigZag<-rep(0,n)

for (i in 1:n){
  HL<-raw_HL[start[i]:end[i],]
  stdHL<-scale(HL)
  ZigZag<-ZigZag(stdHL)
  raw_ZigZag[i]<-mean(ZigZag,na.rm = TRUE)
}

csv<-data.frame(raw_ADX, raw_aroon,raw_trueHigh,raw_trueLow,
                raw_BBandsdn,raw_BBandsmavg,raw_BBandsup,
                raw_CCI, raw_chaikinVolatility,raw_CLV,
                raw_CMOClose,raw_CTI,raw_DonchianChannelH,raw_DonchianChannelM,raw_DonchianChannelL,
                raw_DPOClose,raw_DVIClose,
                raw_GMMAClose,raw_KSTClose,raw_lags,raw_MACDclose,raw_PBandsdn,raw_PBandscenter,raw_PBandsup,
                raw_ROCClose,raw_momentumClose,raw_RSIClose,raw_runSum,raw_runMin,
                raw_runMax,raw_runMedian,raw_SAR,raw_SMAClose,raw_EMAClose,raw_DEMAClose,
                raw_SNR,raw_SMI,raw_TDI,raw_TRIX,raw_VHF,
                raw_volatility,raw_williamsAD,raw_ZigZag,raw_chaikinAD,raw_CMF,raw_CMOvolumn,raw_DPOvolumn,
                raw_EMV, raw_GMMAvolumn,raw_MACDvolumn,raw_MFI,raw_OBV,raw_ROCvolumn,raw_runPrank,raw_SMAv)

write.csv(csv, "/Users/luzhang/Documents/GitHub/WrappingUp_Exploring_Intraday_Momentum/Data/AAPL_features_raw_standardize.csv")


##################################################
################# Save features and label ########

features <- read.csv("/Users/luzhang/Documents/GitHub/WrappingUp_Exploring_Intraday_Momentum/Data/AAPL_features_raw_standardize.csv", header = T)
head(features)
dim(features)
#2622 56
features$X<-NULL

allSet<-data.frame(Y=as.factor(y_label),features)
head(allSet)

write.csv(allSet,"/Users/luzhang/Documents/GitHub/WrappingUp_Exploring_Intraday_Momentum/Data/AAPL_allSet_raw_standardize.csv" )

