setwd('C:\\Users\\britt\\Evolution\\Tasks\\task_09')
library(phytools)
library(ape)
library(maps)
tree <- read.tree("https://jonsmitchell.com/data/anolis.tre")
plot(tree, type="fan")
tree$tip.label
tree$edge.length
tree
#Question #1: There are 82 tips and 163 branch lengths. 
data <- read.csv("https://jonsmitchell.com/data/svl.csv", stringsAsFactors=F
    , row.names=1)
data
#Question #2: The object "data" gives different species of lizards and their snout-vent length.
svl <- setNames(data$svl, row.names(data))
Ancestors <- fastAnc(tree, svl, vars=TRUE, CI=TRUE)
Ancestors
#Question #3: The estimated values stored are ancestral characters, variances, and the CI95 is the 95% confidence interval.
#Question #4: 
par(mar=c(0.1,0.1,0.1,0.1))
plot(tree,type="fan", lwd=2, show.tip.label=F)
tiplabels(pch=16, cex=0.25*svl[tree$tip.label])
nodelabels(pch=16, cex=0.25*Ancestors$ace)
obj <- contMap(tree, svl, plot=F)
plot(obj, type="fan", legend=0.7*max(nodeHeights(tree)), sig=2, fsize=c(0.7, 0.9))
fossilData <- data.frame(svl=log(c(25.4, 23.2, 17.7, 19.7, 24, 31)), tip1=c(
  "Anolis_aliniger", "Anolis_aliniger", "Anolis_occultus", "Anolis_ricordii
  ", "Anolis_cristatellus", "Anolis_occultus"), tip2=c("Anolis_chlorocyanus
  ", "Anolis_coelestinus", "Anolis_hendersoni", "Anolis_cybotes", "Anolis_
  angusticeps", "Anolis_angusticeps"))
fossilNodes <- c()
nodeN <- c()
fossilNodes
nodeN
  for(i in 1:6) {
Node <- fastMRCA(tree, fossilData[i, "tip1"], fossilData[i, "tip2"])
fossilNodes <- c()
nodeN[i] <- Node
  }
names(fossilNodes) <- nodeN
Ancestors_withFossils <- fastAnc(tree, svl, anc.states=fossilNodes, CI=TRUE, var=TRUE)
Ancestors_withFossils
Ancestors_withoutFossils <- fastAnc(tree, svl, var=TRUE, CI=TRUE)
Anccestors_withoutFossils
plot(Ancestors_withFossils$ace, Ancestors_withoutFossils$ace, xlab='with fossils', ylab='without fossils')
library(phytools)
library(ape)
library(maps)
install.packages("geiger")
library(geiger)
fitContinuous(tree, svl, model="BM")
#AIC= -6.512
#Frequency of best fit: 1
fitContinuous(tree, svl, model="EB")
#AIC: -7.23
#Frequency of best fit: 0.30
fitContinuous(tree, svl, model="rate_trend")
#AIC: -6.98
#Frequency of best fit: 0.04
fitContinuous(tree, svl, model="delta")
#AIC= -6.11
#Frequency of best fit: 0.24
#2: EB, smallest AIC
?fastAnc
#3: using fitContinuous