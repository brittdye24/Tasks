setwd('C:\\Users\\britt\\Evolution\\Tasks\\task_10')
library(phytools)
library(ape)
library(maps)
#Questions 1-3
trees <- vector(mode="list", length = 1)
births <- c()
Fractions <- c()
abl <- c()
?pbtree
for(x in 1:100) {
  births[x] <- runif(1)
  Fractions[x] <- runif(1)
  trees[[x]] <- pbtree(b=births[x], d=births[x]*Fractions[x], n=100)
}
plot(trees[[x]])
#Question 4
tiplog <- log(sapply(trees, Ntip))
birthrate <- births
deathrate <- births * Fractions
netdiv <- birthrate - deathrate
plot(netdiv, tiplog, xlab="net diversification rate", ylab="tips")
abline(lm(tiplog ~ netdiv))
cor(netdiv, tiplog)
#There is a positive correlation of 0.2924643 between net diversification rate and log of total number of tips.
#Questions 5-6
final <- unlist(abl)
plot(birthrate, abl, xlab="speciation rate", ylab="average branch length")
abline(lm(abl ~ birthrate))
cor(birthrate, abl)
#Negative correlation
#Question 7
tips <- sapply(trees, Ntip)
which.max(tips)
largetree <- trees[[which.max(tips)]]
plot(largetree, type="radial")
rates <- c()
meantraits <- c()
vartraits <- c()
traits <- vector(mode="list", length=1)
for(x in 1:100) {
  rates[x] <- runif(1)
  traits[[x]] <- fastBM(tree = largetree, sig2=rates[x])
  meantraits[[x]] <- mean(traits[[x]])
  vartraits[[x]] <- var(traits[[x]])
}
#Question 8
meantraits <- unlist(meantraits)
plot(meantraits, rates, xlab = "mean of traits", ylab="rates")
abline(lm(rates ~ meantraits))
cor(meantraits, rates)
#The correlation is 0.1749397
#Question 9
vartraits <- unlist(meantraits)
plot(vartraits, rates, xlab="variance of traits", ylab="rates")
abline(lm(rates ~ vartraits))
cor(vartraits, rates)
#Correlation is 0.1749397
#Question 10
cor(traits[[1]], traits[[2]])
#Correlation is 0.1592321
plot(traits[[1]], traits[[2]], xlab="first element of traits", ylab="second element of traits")
traitmat <- cbind(traits[[1]], traits[[2]])
traitmat
#ExtraCredit
?phylomorphospace
phylomorphospace(largetree, traitmat, xlab="first element of traits", ylab="second element of traits")
