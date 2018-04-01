##############################
#                            #
#     MSA Class of 2018      #
#                            #
#     Survival Analysis:     #
#     Survival & Hazards     #
#                            #
#       Matthew Austin       #
#                            #
##############################

# need these packages
library(haven)
library(survival)

# simple example
lcs17 <- data.frame(team = c("CLE", "HOU", "BOS", "NYY", "MIN",
                             "LAD", "WAS", "CHC", "ARI", "COL"),
                    day = c(5, 4, 4, 6, 1,
                            3, 5, 5, 4, 1),
                    eliminated = c(1, 0, 1, 0, 1,
                                   0, 1, 0, 1, 1))
# you can see here I'm creating a dataset with a column for time (day)
# and status (eliminated)
lcs17

# in R, one of the most common ways (but certainly not the only way) to do
# survival analysis is with the survival package (shocking, I know).
# to get started with survival analysis, we need to create a Surv object
# using those time and status columns
# Surv() takes both of these as the arguments "time" and "event" respectively
lcs.surv <- with(lcs17, Surv(time = day, event = eliminated))
# this object now has the survival time for each observation,
# and a "+" to indicate censored observations
lcs.surv

# to create survival curves, survfit() takes a formula just like usual
# ~ 1 to fit model with no predictors
lcs.fit <- survfit(lcs.surv ~ 1)
# calling this gives the sample size, #events (elimination), median, and CI
lcs.fit
# and the summary gives us the time, number at risk, number of events,
# and S(t), which matches what we did in the slides
summary(lcs.fit)

# ok, moving on to the recidivism study
data_dir <- "/Users/matt/Downloads/survival17/data/"
input_file <- "recid.sas7bdat"

recid <- read_sas(paste(data_dir, input_file, sep = ""))

# create survival object
# "arrest==1" specifies that a value of 1 for this variable is an event!
# default in Surv() is to assume 0/1 where 1 is the event.
# here we don't need to specify this, but it never hurts to do so and we'll
# need it later in the course
recid.surv <- with(recid, Surv(time = week, event = arrest==1))

# survival curves
recid.fit <- survfit(recid.surv ~ 1)
# can you figure out why it doesn't give you the median here?
recid.fit
# you can plot the curve like so
plot(recid.fit, mark.time = TRUE, 
     xlab = "week", ylab = "survival probability",
     main = "recidivism Kaplan-Meier curve")

# the default R plot is pretty bland, but there are some other packages with
# nicer looking plots for survival data
library(survminer)
ggsurvplot(recid.fit, data = recid, conf.int = TRUE)

library(GGally)
ggsurv(recid.fit, back.white = TRUE)
# look different than the others? pay attention to the axis values

# stratify by work experience and plot
recid.fit2 <- with(recid, survfit(recid.surv ~ wexp))
ggsurvplot(recid.fit2, data = recid, conf.int = TRUE)
# plot(recid.fit2, mark.time = TRUE, conf.int = TRUE, col = c("blue", "red"),
#      xlab = "week", ylab = "survival probability",
#      main = "recidivism curve by work experience")
# legend("bottomleft", inset = 0.05, c("none", "work"), col = c("blue", "red"), 
#        lty = 1)
# ggsurv(recid.fit2, CI = TRUE, back.white = TRUE)

# log-rank test
# use the survdiff() function
# this also takes the formula & data inputs as usual, but need a new argument:
# rho = 0 is the normal log-rank test, rho = 1 is the wilcoxon test
survdiff(recid.surv ~ wexp, rho = 0, data = recid)
survdiff(recid.surv ~ wexp, rho = 1, data = recid)

# plot hazard: #events/#at risk
hazard <- with(recid.fit, n.event/n.risk)
plot(recid.fit$time, hazard, type = "l", xlab = "week", ylab = "hazard",
     main = "estimated hazard")
