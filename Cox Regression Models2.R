#############################
#                           #
#     MSA Class of 2018     #
#                           #
#    Survival Analysis:     #
#   Cox Regression Models   #
#                           #
#      Matthew Austin       #
#                           #
#############################

# need these packages
library(survival)
library(survminer)

# load data
data_dir <- "/Users/matt/Downloads/survival17/data/"
input_file <- "recid.csv"
recid <- read.csv(paste(data_dir, input_file, sep = ""), header = TRUE)

# fit proportional hazards model using coxph()
# same structure as everything else
fit <- coxph(Surv(week, arrest == 1) ~ fin + age + race + wexp +
               mar + paro + prio, data = recid)
summary(fit)

# plot survival curve
ggcoxadjustedcurves(fit)
# who is this reference population?
fit$means

# create a new dataset for some comparison of interest
newdata <- data.frame(fin = c(1,0),
                      age = c(30,30),
                      race = c(0,0),
                      wexp = c(1,0),
                      mar = c(0,0),
                      paro = c(0,0),
                      prio = c(0,0))
# plot adjusted
ggcoxadjustedcurves(fit, data = newdata, variable = newdata$fin)

# concordance
survConcordance(Surv(week, arrest == 1) ~ predict(fit), data = recid)

# shrinkage coefficient
df.model <- length(fit$coefficients) # number of coefficients in model
LRchisq <- 2*diff(fit$loglik) # 2*(model log-likelihood - null log-likelihood)
(shrink <- 1 - (df.model/LRchisq))
lp.shrink <- shrink*fit$linear

# use smaller model to illustrate diagnostic plots
fit <- coxph(Surv(week, arrest == 1) ~ fin + age + prio, data = recid)

# dfbetas
ggcoxdiagnostics(fit, type = "dfbetas")

# martingale residuals vs. time
m.res <- residuals(fit, type = "martingale")
plot(recid$week, m.res, pch = 19, cex = 0.5, xlab = "week", 
     ylab = "martingale residuals", main = "martingale residuals vs. time")
cens <- which(recid$arrest == 0)
uncens <- which(recid$arrest == 1)
points(recid$week[cens], m.res[cens], pch = 19, cex = 0.5, col = "blue")
points(recid$week[uncens], m.res[uncens], pch = 19, cex = 0.5, col = "red")
legend("bottomleft", inset = 0.05, c("censored", "arrested"), col = c("blue", "red"),
       pch = 19)
abline(h = 0)
# deviance residuals vs. time
d.res <- residuals(fit, type = "deviance")
plot(recid$week, d.res, pch = 19, cex = 0.5, xlab = "week", 
     ylab = "deviance residuals", main = "deviance residuals vs. time")
cens <- which(recid$arrest == 0)
uncens <- which(recid$arrest == 1)
points(recid$week[cens], d.res[cens], pch = 19, cex = 0.5, col = "blue")
points(recid$week[uncens], d.res[uncens], pch = 19, cex = 0.5, col = "red")
legend("bottomleft", inset = 0.05, c("censored", "arrested"), col = c("blue", "red"),
       pch = 19)
abline(h = 0)

# checking functional form
# martingale residuals
plot(recid$age, m.res, pch = 19, cex = 0.5, xlab = "age",
     ylab = "martingale residuals", main = "functional form for age")
lines(lowess(recid$age, m.res), col = "red")
plot(recid$prio, m.res, pch = 19, cex = 0.5, xlab = "#prior convictions",
     ylab = "martingale residuals", main = "functional form for prior convictions")
lines(lowess(recid$prio, m.res), col = "red")
# deviance residuals
plot(recid$age, d.res, pch = 19, cex = 0.5, xlab = "age",
     ylab = "deviance residuals", main = "functional form for age")
lines(lowess(recid$age, d.res), col = "red")
plot(recid$prio, d.res, pch = 19, cex = 0.5, xlab = "#prior convictions",
     ylab = "deviance residuals", main = "functional form for prior convictions")
lines(lowess(recid$prio, d.res), col = "red")

# checking PH
# testing correlation of residuals with time
cox.zph(fit)
# zph plots
plot(cox.zph(fit))

# time-varying coefficients
fit <- coxph(Surv(week, arrest == 1) ~ fin + age + race + wexp +
               mar + paro + prio, data = recid)

# let's fit b(t) = log(time) using tt() within coxph()
# here you can define this time transform as whatever function you want
fit2 <- coxph(Surv(week, arrest == 1) ~ fin + age + race + wexp +
                mar + paro + prio + tt(age), tt = function(x, t, ...){x*log(t)},
              data = recid)

# time-varying predictors
data_dir <- "/Users/matt/Downloads/survival17/data/"
input_file <- "recid2.csv"
recid2 <- read.csv(paste(data_dir, input_file, sep = ""), header = TRUE)

fit.emp <- coxph(Surv(start, stop, arrest.time == 1) ~ fin + age + race + wexp +
                   mar + paro + prio + employed, data = recid2)
summary(fit.emp)

# use previous week instead
data_dir <- "/Users/matt/Downloads/survival17/data/"
input_file <- "recidlag.csv"
recid.lag <- read.csv(paste(data_dir, input_file, sep = ""), header = TRUE)

fit.lag <- coxph(Surv(start, stop, arrest.time == 1) ~ fin + age + race + wexp +
                   mar + paro + prio + employed, data = recid.lag)
summary(fit.lag)

# "wide" to "long" format
library(tidyverse)
recid2 <- recid %>%
  # create id variable
  mutate(subj = row_number()) %>%
  # weekly employment for each person
  gather(key = "weekly.emp", value = "employed", emp1:emp52) %>%
  # delete rows where employed is missing since that means they're no longer
  # at risk
  filter(!is.na(employed)) %>%
  # create weekly start/stop times for each person (start, stop]
  group_by(subj) %>%
  mutate(start = row_number()-1,
         stop = row_number(),
         week = last(stop),
         # at each person's last recorded stop time, were they arrested?
         arrest.time = ifelse(arrest == 1 & stop == week, 1, 0)) %>%
  # get rid of all the columns i'm not using
  select(subj, start, stop, arrest.time, fin:prio, employed) %>%
  # arrange by id, stop
  arrange(subj, stop) %>%
  ungroup()
# did this actually work?
nrow(recid2) == sum(recid$week)

# lagged employment
recid.lag <- recid2 %>%
  group_by(subj) %>%
  # use employment status from previous week
  mutate(employed = lag(employed, n = 1)) %>%
  # since there's no "previous" week for employment prior to 1,
  # remove each person's record for week 1
  slice(-1) %>%
  ungroup()
# did this work too?
nrow(recid.lag) == (sum(recid$week) - nrow(recid))