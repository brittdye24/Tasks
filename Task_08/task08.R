library("phytools")
library("ape")
text.string <- 
    "(((((((cow, pig),whale),(bat,(lemur,human))),(robin,iguana)),coelacanth
        ),(gold_fish, trout)),shark);"
vert.tree <- read.tree(text=text.string)
plot(vert.tree, edge.width=2)
nodelabels(frame="circle", bg='white', cex=1)
str(vert.tree)
tree <- read.tree(text="(((A,B),(C,D)),E);")
plotTree(tree, offset=1)
tiplabels(frame="circle", bg='lightblue', cex=1)
nodelabels(frame="circle", bg='white', cex=1)
tree$tip.label
tree$edge
AnolisTree <- force.ultrametric(read.tree("https://jonsmitchell.com/data/anolis.tre"))
par(las=1)
hist(AnolisTree$edge.length, col='black', border='white', main="", xlab="
    edge lengths for the Anolis tree", ylim=c(0, 50), xlim=c(0, 6))
tipEdges <- which(AnolisTree$edge[,2] <= Ntip(AnolisTree))
Lengths <- AnolisTree$edge.length
names(Lengths) <- AnolisTree$tip.label
names(Lengths)[which(Lengths == min(Lengths))]
plot(AnolisTree, cex=0.25)
Labs <- sapply(AnolisTree$edge.length, round, digits=2)
edgelabels(text=Labs, cex=0.25)
?plot.phylo
tree1 <- (AnolisTree)
tree1
plot(tree1, show.tip.label = FALSE)
tree$tip.label
plot(tree1, type="fan")
plot(tree1, type="fan", tip.col='red')
?which()
tree2 <- drop.tip(AnolisTree, 'Anolis_occultus')
plot(tree2, cex = 0.25)
ltt(AnolisTree)
abline(0, 1, lwd=2, col='red', lty=2)
?fit.bd()
fit.bd(tree2, b=NULL, d=NULL, rho=0.2)
#Question 1: Based on the numbers, humans but by distance the goldfish. 
#Question 2: No
#Question 3: plot(tree1, show.tip.label = FALSE)
#Question 4: plot(tree1, type = "fan")
#Question 5: plot(tree1, type = "fan", tip.col='red')
#Question 6: Anolis occultus has shortest edge. 
#Question 7: tree2 <- drop.tip(AnolisTree, 'Anolis_occultus)
#Question 8: plot(tree2, cex = 0.25)
#Question 9: The red line has a slope of 1 so it is not changing. The black line has a different pattern. The red one does not go down
  #because of the slope. 
#Question 10: fit.bd(tree2, b=NULL, d=NULL, rho=0.2) 
