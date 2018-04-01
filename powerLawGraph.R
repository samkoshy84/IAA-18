###########################################################
#  GENERATING RANDOM GRAPHS FROM SPECIFIED DEGREE DIST.   #
###########################################################

install.packages("igraph")
library('igraph')

# PowerLaw Graph

x=1:30
y=floor(20*x^(-1.1))
barplot(y)
prob = y/sum(y)
#set.seed(7515)
# Sample integers according to a power law distribution
DegreePL = sample(30,30,prob,replace=T)
#' The next line of code insures that the total sum of
#' degrees in the graph is even. It would be impossible
#' to draw a graph if this weren't the case
if (!(sum(DegreePL)%%2==0)) {DegreePL[1]=DegreePL[1]+as.integer(1)}
# Look at the Degree Distribution that we just generated
hist(DegreePL,col = c("lightblue"), main="Power Law, alpha=0.95", cex.lab=2,cex.main=2.5,cex.axis=1.2)
# Draw a random graph that has that degree distribution
randomg <- degree.sequence.game(DegreePL,method="vl")
plot(randomg, vertex.label=NA, vertex.size = 8, vertex.color = "skyblue", edge.color = "black", main ="Power Law Graph")

# Poisson Graph

DegreeP=rpois(40, lambda = 3)
if (!(sum(DegreeP)%%2==0)) {DegreeP[1]=DegreeP[1]+as.integer(1)}
# Look at the Degree Distribution that we just generated
hist(DegreeP, col = c("lightblue"), main="Poisson, lambda = 1.8", cex.lab=2,cex.main=2.5,cex.axis=1.2)
# Draw a random graph that has that degree distribution
randomg <- degree.sequence.game(DegreeP,method="simple")
plot(randomg, vertex.label=NA, vertex.size = 8, vertex.color = "skyblue", edge.color = "black", main ="Poisson Graph")

