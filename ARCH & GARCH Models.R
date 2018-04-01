#----------------------------------#
#         ARCH & GARCH Models      #
#                                  #
#           Dr Aric LaBarr         #
#----------------------------------#

# Needed Libraries for Analysis #
library(graphics)
library(haven)
library(ks)
library(scales)
library(FinTS)
library(ccgarch)
library(fGarch)
library(rugarch)

# Load Stock Data From SAS #
stocks <- read_sas('E:/Courses/Financial Analytics/Data/stocks.sas7bdat')

# Drilling Down into Stocks Data Set for Easy Variable Naming #
attach(stocks)
names(stocks)

# Plot Price Data #
plot(x = Date, y = msft_p, col="black", main="MSFT Stock Price", xlab="", ylab="Price", lwd=2, type="l")

# Plot Returns Data #
plot(x = Date, y = msft_r, col="black", main="MSFT Stock Return (Logarithmic)", xlab="", ylab="Daily Returns", lwd=2, type="l")

hist(msft_r, breaks=50, main='MSFT Return Distribution', xlab='Daily Returns')

# Test for GARCH Effects and Normality #
ARCHTEST.P <- rep(0,30)
for(i in 1:30){
  ARCHTEST.P[i] <- ArchTest(stocks[,i], lags=3)$p.value
}
ARCHTEST.P
order(ARCHTEST.P)

jb.test(msft_r[2:2517]) # Function cannot handle NA values #

# Estimate Different GARCH Models #
GARCH.N <- garchFit(formula= ~ garch(1,1), data=msft_r[2:2517], cond.dist="norm")
summary(GARCH.N)

GARCH.t <- garchFit(formula= ~ garch(1,1), data=msft_r[2:2517], cond.dist="std")
summary(GARCH.t)

EGARCH <- ugarchfit(data = msft_r[2:2517], spec = ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1,1))))
EGARCH

Skew.GARCH.N <- garchFit(formula= ~ garch(1,1), data=msft_r[2:2517], cond.dist="snorm")
summary(Skew.GARCH.N)

Skew.GARCH.t <- garchFit(formula= ~ garch(1,1), data=msft_r[2:2517], cond.dist="sstd")
summary(Skew.GARCH.t)

GARCH.M <- ugarchfit(data = msft_r[2:2517], spec = ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(1,1)), mean.model=list(armaOrder=c(0,0))))
GARCH.M

EWMA <- ugarchfit(data = msft_r[2:2517], spec = ugarchspec(variance.model=list(model="iGARCH", garchOrder=c(1,1), variance.targeting=0), mean.model=list(armaOrder=c(0,0))))
EWMA
