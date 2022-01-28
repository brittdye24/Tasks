trueMean1 <- 5
trueSD1 <- 5
population1 <- rnorm(1e6, trueMean1, trueSD1)
trueMean2 <- 4
trueSD2 <- 5
population2 <- rnorm(1e6, trueMean2, trueSD2)
Size <- 50
Sample1 <- sample(population1, Size)
Sample2 <- sample(population2, Size)
boxplot(Sample1, Sample2)
#Populations are equal with the same median and quartile values. The minimum and maximum values of the population are different. 
source("http://jonsmitchell.com/code/simFxn04.R")
MatGrandma <- makeFounder("grandma_mom")
head(MatGrandma)
nrow(MatGrandma)
MatGrandpa <- makeFounder("grandpa_mom")
head(MatGrandpa)
nrow(MatGrandpa)
PatGrandma <- makeFounder("grandma_da")
head(PatGrandma)
nrow(PatGrandma)
PatGrandpa <- makeFounder("grandpa_da")
Alan <- makeBaby(PatGrandma, PatGrandpa)
head(Alan)
nrow(Alan)
Brenda <- makeBaby(MatGrandma, MatGrandpa)
head(Brenda)
Focus <- makeBaby(Brenda, Alan)
ToMom <- length( grep("mom", Focus ) ) / length( Focus )
ToMomMom <- length( grep( "grandma_mom", Focus ) ) / length( Focus )
ToMomDad <- length( grep( "grandpa_mom", Focus ) ) / length( Focus )
ToMomMom
ToMomDad
#Focus is not equally related to each maternal grandparent. 
ToDad <- length( grep( "dad", Focus ) ) / length( Focus)
ToDadMom
ToDad
ToMom
Sibling_01 <- makeBaby(Brenda, Alan)
ToSib <- length( intersect( Focus, Sibling_01 ) ) / length( Focus )
ToSib
Focus
ManySiblings <- replicate( 1e3, length( intersect( Focus, makeBaby(Brenda, Alan) ) ) / length( Focus ) )
ManySiblings
quantile(ManySiblings)
mean(ManySiblings)
plot(density(ManySiblings), main="", xlab="proportion shared genes")
HWE <- function(p) {
    aa <- p^2
    ab <- 2 * p * (1-p)
    bb <- (1-p)^2
    return(c(aa=aa, ab=ab, bb=bb))
}
HWE(0.5)
plot(1, 1, type="n", xlim=c(0,1), ylim=c(0,1), xlab="freq. allele a", ylab="geno. freq")
p <- seq(from = 0, to = 1, by = 0.01)
GenoFreq <- t(sapply(p, HWE))
lines(p, GenoFreq[,"aa"], lwd=2, col="red")
#Both increase together. There is no time or space on this graph, only the frequency of the allele and genotype frequencies. 
lines(p, GenoFreq[,"ab"], lwd=2, col="purple")
lines(p, GenoFreq[, "bb"], lwd=2, col="blue")
legend("top", legend=c("aa", "ab", "bb"), col=c("red", "purple", "blue"), lty=1, lwd=2, bty="n")
Pop <- simPop(500)
points(Pop[, "freqa"], Pop[,"Genotypes.aa"]/500, pch=21, bg="red")
#The points follow the expectation. 
Pop <- simPop(50)
points(Pop[,"freqa"], Pop[,"Genotypes.aa"]/50, pch=22, bg="red")
#The points are more frequent in the allele and in the genotype. The smaller sample size. 
install.packages("learnPopGen")
library(learnPopGen)
x <- genetic.drift(Ne=1000, nrep=5, pause=0.01)
PopSizes <- 5:50
Samples <- rep(PopSizes, 5)
tExt <- sapply(Samples, function(x) nrow(simPop(x, 500)))
Line <- lm(tExt ~ Samples)
summary(Line)
Line$coef
plot(Samples, tExt)
abline(Line)
Line2 <- lm( tExt~Samples + 0)
Line2
Line2$coef
plot(Samples, tExt)
abline(Line)
abline(Line2)
Line
Line2
#Line 2 does not give the intercept value along with the sample. Value of samples from 1 and 2 are very similar. 