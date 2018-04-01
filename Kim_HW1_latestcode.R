#read in dataset

cost <- read.csv('C:/Users/kmkei/Google Drive/MSA/Fall 3/Simulation and Risk Analysis/Cost_2.csv', header=TRUE, skip=2)
proj <- read.csv('C:/Users/kmkei/Google Drive/MSA/Fall 3/Simulation and Risk Analysis/Projections.csv', header=TRUE, skip=2)

library(graphics)
library(ks)

#subset cost to 1990-2006

cost.s <- cost[31:47, ]

#histogram of average geometric change

#cost.s$Average.Geometric.Change.n <- as.numeric(levels(cost.s$Average.Geometric.Change)[cost.s$Average.Geometric.Change])
hist(cost.s$Average.Geometric.Change, breaks = 20)

#histograms of all other geometric changes

cost.s$Geometric.Return...Crude.Oil.n <- as.numeric(levels(cost.s$Geometric.Return...Crude.Oil)[cost.s$Geometric.Return...Crude.Oil])
cost.s$Geometric.Return...Natural.Gas.n <- as.numeric(levels(cost.s$Geometric.Return...Natural.Gas)[cost.s$Geometric.Return...Natural.Gas])
cost.s$Geometric.Return...Dry.Well.n <- as.numeric(levels(cost.s$Geometric.Return...Dry.Well)[cost.s$Geometric.Return...Dry.Well])

hist(cost.s$Geometric.Return...Crude.Oil.n, breaks = 20)
hist(cost.s$Geometric.Return...Natural.Gas.n, breaks = 20)
hist(cost.s$Geometric.Return...Dry.Well.n, breaks = 20)

#Q-Q plots of the geometric changes

qqnorm(cost.s$Geometric.Return...Crude.Oil.n)
qqnorm(cost.s$Geometric.Return...Natural.Gas.n)
qqnorm(cost.s$Geometric.Return...Dry.Well.n)
qqnorm(cost.s$Average.Geometric.Change)

#kolmogorov-smirnov test for normality

ks.test(x = cost.s$Average.Geometric.Change, y = pnorm)
ks.test(x = cost.s$Geometric.Return...Crude.Oil.n, y = pnorm)
ks.test(x = cost.s$Geometric.Return...Natural.Gas.n, y = pnorm)
ks.test(x = cost.s$Geometric.Return...Dry.Well.n, y = pnorm)

library(nortest)

#anderson-darling test for normality

ad.test(cost.s$Average.Geometric.Change)
ad.test(cost.s$Geometric.Return...Crude.Oil.n)
ad.test(cost.s$Geometric.Return...Natural.Gas.n)
ad.test(cost.s$Geometric.Return...Dry.Well.n)

#all variables are normal

#crude oil drilling simulation

mean(cost.s$Geometric.Return...Crude.Oil.n)
sd(cost.s$Geometric.Return...Crude.Oil.n)

c <- rnorm(n=10000, mean=0.1218, sd=0.1698)

crude.den <- density(c, bw='SJ-ste')
crude.den
est.crude <- rkde(fhat=kde(P1, h=0.02877), n=10000)
hist(est.crude, breaks=50, main='Estimated One Year Value Distribution', xlab='Final Value')

#natural gas drilling simulation

mean(cost.s$Geometric.Return...Natural.Gas.n)
sd(cost.s$Geometric.Return...Natural.Gas.n)

n <- rnorm(n=10000, mean=0.0848, sd=0.1489)
natural.den <- density(n, bw='SJ-ste')
natural.den
est.natural <- rkde(fhat=kde(n, h=0.02445), n=10000)
hist(est.natural, breaks=50)

#dry well drilling simulation

mean(cost.s$Geometric.Return...Dry.Well.n)
sd(cost.s$Geometric.Return...Dry.Well.n)

d <- rnorm(n=10000, mean=0.1185, sd=0.1567)
drill.den <- density(d, bw='SJ-ste')
drill.den
est.drill <- rkde(fhat=kde(d, h=0.02645), n=10000)
hist(est.drill, breaks=50)

#average geometric changes

mean(cost.s$Average.Geometric.Change)
sd(cost.s$Average.Geometric.Change)

a <- rnorm(n=10000, mean=0.1084, sd=0.1141)
hist(a, breaks=50) #under normal distribution
avg.den <- density(a, bw='SJ-ste')
avg.den
est.avg <- rkde(fhat=kde(a, h=0.01881), n=10000)
hist(est.avg, breaks=50) #using kernel density estimator

#######doing it the correct way

set.seed(12345)
new.cost <- read.csv('C:/Users/kmkei/Google Drive/MSA/Fall 3/Simulation and Risk Analysis/New_Cost.csv', header=TRUE, skip=2)
new.cost2 <- new.cost[32:47, ]

hist(new.cost2$Average.Geometric.Change.2, breaks = 20)

#Testing Normality

qqnorm(new.cost2$Average.Geometric.Change.2)
ad.test(new.cost2$Average.Geometric.Change.2)

#Under Assumption of Normality

mean(new.cost2$Average.Geometric.Change.2)
sd(new.cost2$Average.Geometric.Change.2)
new.c <- rnorm(n=10000, mean=0.1108602, sd=0.1145411)
hist(new.c, breaks=50)

r.value=matrix((1+rnorm(12*10000,0.1108602,0.1145411)),nrow=30)
mat.mult=1000*apply(r.value,2,prod)
hist(mat.mult, breaks=50)
max(mat.mult)
min(mat.mult)
mean(mat.mult) #expected amount of return
quantile(mat.mult, 0.05)

#KDE

Density.P12 <- density(new.c, bw="SJ-ste") #density estimator; send the actual variable (P1); highly 
#recommend to use the methodology as SJ-ste
Density.P12

Est.P12 <- rkde(fhat=kde(new.c, h=0.01982), n=10000)
hist(Est.P12, breaks=50)

#######New Ideas#########



c12 <- rep(0,10000)
for(i in 1:10000){
  c0 <- 386.87
  r <- rnorm(n=10000, mean=0.1108602, sd=0.1145411)
  
  ct <- c0*exp(r)
  
  for(j in 1:12){
    r <- rnorm(n=1, mean=0.1108602, sd=0.1145411)
    ct <- ct*exp(r)
  }
  c12[i] <- ct
}

#########SAM'S CODE#########

library(xlsx)
PriceProj <- read.csv('C:/Users/kmkei/Google Drive/MSA/Fall 3/Simulation and Risk Analysis/Proj.csv', header=TRUE, skip=2)
DrillingCost <- read.csv('C:/Users/kmkei/Google Drive/MSA/Fall 3/Simulation and Risk Analysis/Drill.csv', header=TRUE, skip=2)

cost <- DrillingCost[2:49, 8]

library(graphics)
library(ks)

hist(cost, breaks = 20)
mean(cost)
sd(cost)

library(nortest)
qqnorm(cost)
ad.test(cost)

estAvgGeo <- rnorm(n=10000, mean=0.1127039, sd=0.1758857)
hist(estAvgGeo, breaks = 10)

# This is the average cost from 2006 #
y = 2279.8

P12 <- rep(0,10000)
for(i in 1:10000){
  P0 <- 2279.8
  estAvgGeo <- rnorm(n=10000, mean=0.1127039, sd=0.1758857)
  Pt <- P0*(1 + estAvgGeo)
  for(j in 1:12){
    estAvgGeo <- rnorm(n=10000, mean=0.1127039, sd=0.1758857)
    Pt <- Pt*(1+estAvgGeo)
  }
  P12[i] <- Pt
}

mean(P12)
sd(P12)

hist(P12, breaks=50, main='12 Year Value Distribution', xlab='Final Value')


###Now use the kernel density estimator

k <- rnorm(n=10000, mean=0.1127039, sd=0.1758857) #creating rate of return of distribution
#rnorm says to draw random samples that are normal
P0 <- 2279.8
P1 <- P0*(1+k)

Density.P1 <- density(P1, bw="SJ-ste") #density estimator; send the actual variable (P1); highly 
#recommend to use the methodology as SJ-ste
Density.P1

Est.P1 <- rkde(fhat=kde(P1, h=69.12), n=10000)
hist(Est.P1, breaks=50, main='Estimated One Year Value Distribution', xlab='Final Value')

#Now looping for 12 years

P12 <- rep(0,10000)
for(i in 1:10000){
  P0 <- 2279.8
  estAvgGeo <- rnorm(n=10000, mean=0.1127039, sd=0.1758857)
  Pt <- P0*(1 + estAvgGeo)
  Est.P1 <- rkde(fhat=kde(Pt, h=69.12), n=10000)
  for(j in 1:12){
    estAvgGeo <- rnorm(n=10000, mean=0.1127039, sd=0.1758857)
    Pt <- Pt*(1+estAvgGeo)
  }
  P12[i] <- Pt
}

r.value=matrix((1+rnorm(12*10000,0.1127039,0.1758857)),nrow=12)
mat.mult=2279.8*apply(r.value,2,prod)


k <- rnorm(n=10000, mean=0.1127039, sd=0.1758857)
P0 <- 2279.8
P1 <- P0*(1+k)

Density.P1 <- density(P1, bw="SJ-ste")

Est.P1 <- rkde(fhat=kde(P1, h=69.12), n=10000)

for (i in 1:10000){
  P0 <- 2279.8
  k <- rnorm(n=10000, mean=0.1127039, sd=0.1758857)
  Pt <- P0*(1 + k)
  for(j in 1:12){
    Pt <- rkde(fhat=kde(Pt, h=69.12), n=10000)
  }
  P12[i] <- Pt
}



#trying it the matrix way
k <- rnorm(n=10000, mean=0.1127039, sd=0.1758857)
P0 <- 2279.8
P1 <- P0*(1+k)
d <- matrix(rkde(fhat=kde(P1, h=69.12), n=12*10000), nrow=12)
e <- 2279.8*apply(d, 2, prod)
hist(e, breaks=50)

#trying it manually

#2007
m <- rnorm(n=10000, mean=0.1127039, sd=0.1758857)
initial <- 2279.8
nexts <- initial*(1 + m)
den1 <- density(nexts, bw="SJ-ste")
den1
est1 <- rkde(fhat=kde(nexts, h=64.56), n=10000)
hist(est1, breaks=20)

#second year
nexts2 <- nexts*(1+m)
est2 <- rkde(fhat=kde(nexts2, h=64.56), n=10000)
hist(est2, breaks=20)

#third year
nexts3 <- nexts2*(1+m)
est3 <- rkde(fhat=kde(nexts3, h=64.56), n=10000)
hist(est3, breaks=20)

#fourth year
nexts4 <- nexts3*(1+m)
est4 <- rkde(fhat=kde(nexts4, h=64.56), n=10000)
hist(est4, breaks=20)

#fifth year
nexts5 <- nexts4*(1+m)
est5 <- rkde(fhat=kde(nexts5, h=64.56), n=10000)
hist(est5, breaks=20)

#sixth year
nexts6 <- nexts5*(1+m)
est6 <- rkde(fhat=kde(nexts6, h=64.56), n=10000)
hist(est6, breaks=20)

#doing some loop stuff

m <- rnorm(n=10000, mean=0.1127039, sd=0.1758857)
initial <- 2279.8
p <- initial*(1 + m)
den1 <- density(p, bw="SJ-ste")

for (i in 1:12){
  kerden <- rkde(fhat=kde(p, h=64.56), n=10000)
  p <- p*(1 + m)
  i = i + 1
}
hist(kerden, breaks=50)

#latest idea for how to do this

rate.value=matrix((1+rnorm(12*10000,0.1127039,0.1758857)),nrow=12)
t=2279.8*apply(rate.value,2,prod)
den.t <- density(t, bw="SJ-ste")
den.t
est.t <- rkde(fhat=kde(t, h=466), n=10000)
hist(est.t, breaks=50)







