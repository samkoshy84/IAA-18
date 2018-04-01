library(fGarch)
library(dplyr)
library(aTSA)

# Directory
setwd("C:\\Users\\Sam Koshy\\Desktop\\MSA - Spring\\finproject")

# Reading data and posturing
stocks <- read.csv("Stocks.csv", header = TRUE)
stocks <- stocks %>% select(ends_with("r"))
stocks <- stocks[2:1035,]

# Repeat for lags 1 to 10
#ARCHTEST.P <- rep(0,30)
#for(i in 1:30){
#  ARCHTEST.P[i] <- ArchTest(stocks[,i], lags=1)$p.value
#}
#ARCHTEST.P
#order(ARCHTEST.P)
# GE has decaying significance at higher lags

# ARMA(0,0) Estimates and Portmonteau-test for ARCH effects
lagrange <- data.frame('company' = names(stocks), 'p.value' = rep(NA, times = 30))
for(i in 1:30)
{
  model <- arima(stocks[,i], order = c(0,0,0))
  test.value <- arch.test(model, output = F)
  test.value <- as.data.frame(test.value)
  lagrange[i,2] <- test.value[1,3]
}
lagrange2 <- lagrange[order(lagrange[,2], decreasing = FALSE),]

# 5 stocks with lowest p-values are CVX, XOM, HD, and JNJ
best6.companies <- stocks[, c("CVX_r", "XOM_r", "HD_r", "JNJ_r", "PFE_r", "GE_r")]

#GARCH MODELING
garch_compare <- function(firm)
{
  GARCH.N <- garchFit(formula = ~ garch(1,1), data = best6.companies[,firm], cond.dist = "norm", trace = F)
  summary(GARCH.N)
  
  GARCH.t <- garchFit(formula = ~ garch(1,1), data = best6.companies[,firm], cond.dist = "std", trace = F)
  summary(GARCH.t)
  
  Skew.GARCH.N <- garchFit(formula = ~ garch(1,1), data = best6.companies[,firm], cond.dist = "snorm", trace = F)
  summary(Skew.GARCH.N)
  
  Skew.GARCH.t <- garchFit(formula = ~ garch(1,1), data = best6.companies[,firm], cond.dist = "sstd", trace = F)
  summary(Skew.GARCH.t)
  
}

garch_compare("CVX_r")
# For CVX we have t-GARCH model with lowest AIC -6.02477, alpha=9.8733e-02, beta=8.9043e-01, shape=5.9556e+00
garch_compare("XOM_r")
# For XOM we have t-GARCH model with lowest AIC -6.35974, alpha=8.2843e-02, beta=9.0510e-01, shape=5.2200e+00
#very slow (slowest) decay of market shock effect beta>.9
garch_compare("HD_r")
# For HD we have skewed t-GARCH model with lowest AIC -6.31167, alpha=1.3286e-01, beta=8.1547e-01, skew=8.9523e-01, shape=5.2791e+00
#very sensitive to market shocks alpha>.1
garch_compare("JNJ_r")
# For JNJ we have t-GARCH mode with AIC -6.70704, alpha=1.1688e-01, beta=8.0934e-01, shape=4.6209e+00
#very sensitive to market shocks alpha>.1
garch_compare("PFE_r")
# For PFE we have t-GARCH mode with AIC -6.4422, alpha=1.8393e-01, beta=7.7068e-01, skew=4.4279e+00
#very sensitive (most) to market shocks alpha>.1,
garch_compare("GE_r")
# For GE we have t-GARCH mode with AIC -6.239636, alpha=2.4419e-01, beta=7.0176e-01, shape=4.3450e+00

#alpha ranking: PFE, HD, JNJ, CVX, XOM
#beta ranking: XOM, CVX, HD, JNJ, PFE
#alpha+beta for CVX and XOM <.99 but very close to .99, hence t-garch models for these two stock are barely stable

# Fitting appropriate GARCH models
fit.CVX <- garchFit(formula = ~ garch(1,1), data = best5.companies[,"CVX_r"], cond.dist = "std", trace = F)
fit.XOM <- garchFit(formula = ~ garch(1,1), data = best5.companies[,"XOM_r"], cond.dist = "std", trace = F)
fit.HD <- garchFit(formula = ~ garch(1,1), data = best5.companies[,"HD_r"], cond.dist = "sstd", trace = F)
fit.JNJ <- garchFit(formula = ~ garch(1,1), data = best5.companies[,"JNJ_r"], cond.dist = "std", trace = F)
fit.PFE <- garchFit(formula = ~ garch(1,1), data = best5.companies[,"PFE_r"], cond.dist = "std", trace = F)

# The forecasting 5 days ahead
pred.CVX <- predict(fit.CVX, 5)
pred.XOM <- predict(fit.XOM, 5)
pred.HD <- predict(fit.HD, 5)
pred.JNJ <- predict(fit.JNJ, 5)
pred.PFE <- predict(fit.PFE, 5)

Pred <- cbind(pred.CVX,pred.XOM,pred.HD,pred.JNJ,pred.PFE)
Corr<- cor(best5.companies)
write.csv(Pred,file="C:\\Users\\Sam Koshy\\Desktop\\MSA - Spring\\finproject\\Pred.csv")
write.csv(Corr,file="C:\\Users\\Sam Koshy\\Desktop\\MSA - Spring\\finproject\\Corr.csv")
