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

# u-score features (AAPL)
data<-read.csv("/Users/luzhang/Documents/GitHub/WrappingUp_Exploring_Intraday_Momentum_2rd_try/Data/NVDA_Returns.csv")
head(data)
features = data
features$datetime<-NULL
head(features)

#summary(features)

uscoreFeatures<-uscore(features)
#head(uscoreFeatures)

uscoreFeatures<-data.frame(uscoreFeatures)
#head(uscoreFeatures)

colnames(uscoreFeatures)<-names(features)
#uscoreFeatures$date <- data$date
head(uscoreFeatures)

write.csv(uscoreFeatures,"/Users/luzhang/Documents/GitHub/WrappingUp_Exploring_Intraday_Momentum_2rd_try/Data/NVDA_uscoreReturns.csv")

