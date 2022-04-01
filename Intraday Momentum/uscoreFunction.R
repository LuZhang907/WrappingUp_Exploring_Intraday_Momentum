uscore <- function(data, aunif = -0.5)
{
  if (is.vector(data))
  {
    nr <- length(data)
    us <- ((1:nr) + aunif)/(nr + 1 + 2 * aunif)
    jj <- rank(data)
    out <- us[jj]
  } else
  {
    nc <- ncol(data)
    nr <- nrow(data)
    out <- matrix(0, nr, nc)
    us <- ((1:nr) + aunif)/(nr + 1 + 2 * aunif)
    for (j in 1:nc)
    {
      jj <- rank(data[, j])
      tem <- us[jj]
      out[, j] <- tem
    }
  }
  out
}

# u-score returns (AAPL)
returns<-read.csv("/Users/luzhang/Documents/GitHub/WrappingUp_Exploring_Intraday_Momentum/Data/AAPL_Returns.csv")
head(returns)
returns$datetime<-NULL
returns$X<-NULL
head(returns)

summary(returns)

uscoreReturns<-uscore(returns)
head(uscoreReturns)

uscoreReturns<-data.frame(uscoreReturns)
head(uscoreReturns)

colnames(uscoreReturns)<-names(returns)
head(uscoreReturns)

write.csv(uscoreReturns,"/Users/luzhang/Documents/GitHub/WrappingUp_Exploring_Intraday_Momentum/Data/AAPL_uscoreReturns.csv")

# u-score features (AAPL)
features<-read.csv("/Users/luzhang/Documents/GitHub/AAPL_python/DATA/features.csv")

uscoreFeatures = features
uscoreFeatures$date<-NULL

summary(uscoreFeatures)

uscoreFeatures<-uscore(uscoreFeatures)
head(uscoreFeatures)

uscoreFeatures<-data.frame(uscoreFeatures)

featureNames <- names(features)

colnames(uscoreFeatures)<-featureNames[2:33]
head(uscoreFeatures)
# add date to data frame
uscoreFeatures$date <- features$date

write.csv(uscoreFeatures,"/Users/luzhang/Documents/GitHub/AAPL_python/DATA/uscoredFeatures.csv")

# u-score returns (SPY) 
returns<-read.csv("/Users/luzhang/Documents/GitHub/WrappingUp_Exploring_Intraday_Momentum/Data/SPY_Returns.csv")
head(returns)
returns$datetime<-NULL
head(returns)

uscoreReturns<-uscore(returns)
head(uscoreReturns)

uscoreReturns<-data.frame(uscoreReturns)
head(uscoreReturns)

colnames(uscoreReturns)<-names(returns)
head(uscoreReturns)

write.csv(uscoreReturns,"/Users/luzhang/Documents/GitHub/WrappingUp_Exploring_Intraday_Momentum/Data/SPY_uscoreReturns.csv",index=TRUE)

#u-score features (SPY)
features<-read.csv("/Users/luzhang/Documents/GitHub/AAPL_python/DATA/featuresSPY.csv")

uscoreFeatures = features
uscoreFeatures$date<-NULL

summary(uscoreFeatures)

uscoreFeatures<-uscore(uscoreFeatures)
head(uscoreFeatures)

uscoreFeatures<-data.frame(uscoreFeatures)

featureNames <- names(features)

colnames(uscoreFeatures)<-featureNames[2:33]
head(uscoreFeatures)
# add date to data frame
uscoreFeatures$date <- features$date

#write.csv(uscoreFeatures,"/Users/luzhang/Documents/GitHub/AAPL_python/DATA/uscoredFeaturesSPY.csv")

# u-score returns (QQQ) 
returns<-read.csv("/Users/luzhang/Documents/GitHub/WrappingUp_Exploring_Intraday_Momentum/Data/QQQ_Returns.csv")
head(returns)
returns$datetime<-NULL
head(returns)

uscoreReturns<-uscore(returns)
head(uscoreReturns)

uscoreReturns<-data.frame(uscoreReturns)
head(uscoreReturns)

colnames(uscoreReturns)<-names(returns)
head(uscoreReturns)

write.csv(uscoreReturns,"/Users/luzhang/Documents/GitHub/WrappingUp_Exploring_Intraday_Momentum/Data/QQQ_uscoreReturns.csv")

#u-score features (SPY)
features<-read.csv("/Users/luzhang/Documents/GitHub/AAPL_python/DATA/featuresQQQ.csv")

uscoreFeatures = features
uscoreFeatures$date<-NULL

summary(uscoreFeatures)

uscoreFeatures<-uscore(uscoreFeatures)
head(uscoreFeatures)

uscoreFeatures<-data.frame(uscoreFeatures)

featureNames <- names(features)

colnames(uscoreFeatures)<-featureNames[2:33]
head(uscoreFeatures)
# add date to data frame
uscoreFeatures$date <- features$date

write.csv(uscoreFeatures,"/Users/luzhang/Documents/GitHub/AAPL_python/DATA/uscoredFeaturesQQQ.csv")
