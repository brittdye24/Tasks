setwd('C:\\Users\\britt\\Evolution\\Tasks\\Task_09')
library(phytools)
text.string <- 
    "(((((((cow, pig), whale), (bat,(lemur,human))),(robin, iguana)), coelacanth
      ),(gold_fish, trout)),shark):"
vert.tree <- read.tree(text=text.string)
plot(tree, type="fan")
data <- read.csv("https://jonsmitchell.com/data/svl.csv", stringsAsFactors=F, row.names=1)
svl <- setNames(data$svl, rownames(data))
rownames(data)
Ancestors <- fastAnc(tree, svl, vars=TRUE, CI=TRUE)
setwd('C:\\Users\\britt\\Evolution\\Tasks\\Task_08')
Ancestors <- fastAnc(tree, svl, vars=TRUE, CI=TRUE)
