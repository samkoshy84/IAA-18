##############################
#                            #
#     MSA Class of 2018      #
#                            #
#     Survival Analysis:     #
#    Accelerated Failure     #
#        Time Models         #
#                            #
#       Matthew Austin       #
#                            #
##############################

# need these packages
library(survival)

# load data
data_dir <- "/Users/matt/Downloads/survival17/data/"
input_file <- "recid.csv"
recid <- read.csv(paste(data_dir, input_file, sep = ""), header = TRUE)

# fit AFT models using survreg()
# like glm(), there's a "dist" argument (default is weibull) that we'll get to
# in a bit, but everything else works the same as you've seen before
fit <- survreg(Surv(week, arrest == 1) ~ fin + age + race + wexp + mar + paro +
                 prio, data = recid, dist = "weibull")
# and you can call a summary, which gives coefficient estimates, SEs, etc.
# notice the test for log(scale), which is testing whether log(scale) = 0,
# meaning testing if exponential is ok
summary(fit)

# let's look at some different distributions
# actually we'll quickly switch to a different package for the purpose
# of plotting
library(flexsurv)
# the syntax in flexsurvreg() is the same as survreg()
# weibull distribution
fit.weibull <- flexsurvreg(Surv(week, arrest == 1) ~ fin + age + race + wexp +
                             mar + paro + prio, data = recid, dist = "weibull")
# plot cumulative hazard along with KM estimates
# you hope the curve and CI are pretty close to the KM estimates
plot(fit.weibull, type = "cumhaz", ci = TRUE, conf.int = FALSE, 
     xlab = "week", ylab = "cumulative hazard", main = "weibull distribution")
# exponential
fit.exp <- flexsurvreg(Surv(week, arrest == 1) ~ fin + age + race + wexp +
                             mar + paro + prio, data = recid,
                       dist = "exponential")
plot(fit.exp, type = "cumhaz", ci = TRUE, conf.int = FALSE, 
     xlab = "week", ylab = "cumulative hazard", main = "exponential distribution")

# some other distributions
fit.lnorm <- flexsurvreg(Surv(week, arrest == 1) ~ fin + age + race + wexp +
                         mar + paro + prio, data = recid,
                       dist = "lognormal")
plot(fit.lnorm, type = "cumhaz", ci = TRUE, conf.int = FALSE, 
     xlab = "week", ylab = "cumulative hazard", main = "lognormal distribution")

fit.llogis <- flexsurvreg(Surv(week, arrest == 1) ~ fin + age + race + wexp +
                           mar + paro + prio, data = recid,
                         dist = "llogis")
plot(fit.llogis, type = "cumhaz", ci = TRUE, conf.int = FALSE, 
     xlab = "week", ylab = "cumulative hazard",
     main = "log-logistic distribution")

# predicted quantiles
# predict() returns a list with the type (fit) [[1]] and se (se.fit) [[2]]
pct <- c(0.25, 0.5, 0.75) # 25th, 50th (median) and 75th quantiles
pred.quantile <- predict(fit, type = "quantile", se.fit = TRUE,
                       p = pct)
# note that these are the quantiles for EVENT times, not survival times
# so 0.25 means this is the time where events have happened to 25% of people,
# so S(t) = 0.75 at whatever this time is
head(pred.quantile$fit)

# or you can predict actual time to recidivism
pred.time <- predict(fit, type = "response", se.fit = TRUE)
head(pred.time$fit)

# to see actual survival probabilities at the time where we actually observed
# the event, use psurvreg()
# the first argument is the observed times
# next is the "mean" (but isn't technically the mean, it's just t(x) %*% beta)
# and scale is the scale from the model, and last we specify the distribution
surv.prob <- 1 - psurvreg(recid$week, mean = fit$linear,
                          scale = fit$scale, distribution = fit$dist)
head(surv.prob)

# or you can predict survival probabilities at some time of interest
# let's look at the 10-week survival probabilities
# I'll still use psurvreg(), but my "time" for everyone is just 10
pred.10week <- 1 - psurvreg(10, mean = fit$linear, 
                            scale = fit$scale, distribution = fit$dist)
head(pred.10week)

# get predicted time of recidivism if the people who didn't get financial aid
# did get it (assuming same survival prob)
# first, get the values of t(x) %*% beta for people who were rearrested and
# didn't receive financial aid
old.lp <- fit$linear[recid$arrest == 1 & recid$fin == 0]
# and take that subset of the original data and survival probs
recid.nofin <- recid[recid$arrest == 1 & recid$fin == 0,]
same.surv.prob <- surv.prob[recid$arrest == 1 & recid$fin == 0]
# now give them all financial aid
recid.nofin$fin <- 1
# now get the new values of t(x) %*% beta
new.lp <- predict(fit, newdata = recid.nofin,
                        type = "lp", se.fit = TRUE)
# now, keeping the same survival probability, get the time 
# corresponding to the new linear predictor values
new.time <- qsurvreg(1 - same.surv.prob, mean = new.lp$fit,
                     scale = fit$scale, distribution = fit$dist)
time.diff <- new.time - recid.nofin$week
df <- data.frame(survprob = same.surv.prob, old.time = recid.nofin$week,
                 new.time = new.time, diff = time.diff)
head(df)