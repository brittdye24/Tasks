setwd('C:\\Users\\britt\\Evolution\\Tasks\\Task_04')
results <- read.csv("http://jonsmitchell.com/data/biol112labresults.csv", stringsAsFactors=F)
counts <- results[,c("yellow", "red", "green", "blue", "black", "tan")]
backgrounds <- c("White", "Red", "Yellow", "Green", "Blue", "Black")
backgroundCol <- c("white", "#d53e4f", "#fee08b", "#abdda4", "#3288bd", "black")
calcChi(counts[1,])
Chisqs <- apply(counts, 1, calcChi)
plotChis(counts)
plotChis(counts)
plotChis(counts)
plotChis(counts)
plotChis(counts)
#As the value of chi-sqaured gets higher, there are fewer bars that show greater numerical value. When it is 300, there is only 
  #one bar that is not 0. When it is 40.8, the bars all have numerical value but are very uneven. The lower the chi-squared, the more
  #even the bars are. plotChis() shows many different possibilities that can occur because the range is 1 to n. 
Avg <- mean(Chisqs)
Avg
#The average here is much greater than the critical value. Here is the average is 60.99081 and the critical value from the packet is 11.70.
backgroundAvgs <- tapply(Chisqs, results[,3], mean)
propSig <- length( which( Chisqs > 11.70) ) / length(Chisqs)
percSig <- round(100 * propSig)
propSig
percSig
#Having 92 significant trials does seem surprising to me. I do not think that is all due to natural selection. There could be error or
  #too small of a sampling size. 
par(las = 1, mar = c(4, 4, 1, 1), mgp = c(2, 0.5, 0), tck = -0.01, cex.axis=1)
hist(Chisqs, main="", xlab="chi-squared values", ylab="frequency")
par(las = 1, mar = c(4, 4, 1, 1), mgp =c(2, 0.5, 0), tck = -0.01, cex.axis=1)
plot(1, 1, xlim=c(0, 400), ylim=c(1, 8.5), xlab="", ylab="", type="n", yaxt="n")
axis(2, at = 1:length(backgrounds), labels = backgrounds)
mtext(side=1, expression(chi^2), cex=1.75, line=2.5)
counter <- 1
for (i in backgrounds) {
  Data <- Chisqs[which(results[,3] == i)]
  addHist(Y=counter, Dat=Data, Color=backgroundCol[counter])
  counter <- counter + 1
}
abline( v = 11.70, lty=2, lwd = 2, col='black')
#Some backgrounds have more to show on the right of the line but I am not sure what it is signifying or how to read it. 
Simulation <- simDraws(10000)
addHist(Y=7, Dat=Simulation, Color="lightgray")
mtext(side=2, at=7, line=0, "simulated")
abline(v = 11.70, lty=2, lwd=2)
Fit <- c(1, 1, 1, 1, 1, 1)
names(Fit) <- 1:6
Simulation2 <- simDraws(1e4, w = Fit)
addHist(Y=8, Dat=Simulation2, Color=rgb(0,0,0,0.25))
Fit <- c(0.1, 1, 1, 1, 1, 1)
names(Fit) <- 1:6
Simulation3 <- simDraws(1e4, w = Fit)
addHist(Y=8, Dat=Simulation3, Color=rgb(0,0,0,0.25))
Fit <- c(0.5, 0.6, 0.7, 1, 1, 1)
names(Fit) <- 1:6
Simulation4 <- simDraws(1e4, w = Fit)
addHist(Y=8, Dat=Simulation4, Color=rgb(0,0,0,0.25))
Fit <- c(0.1, 0.2, 0.3, 0.4, 0.5, 1)
names(Fit) <- 1:6
Simulation5 <- simDraws(1e4, w = Fit)
addHist(Y=8, Dat=Simulation5, Color=rgb(0,0,0,0.25))
Fit <- c(0.1, 0.1, 0.1, 0.1, 0.1, 1)
names(Fit) <- 1:6
Simulation6 <- simDraws(1e4, w = Fit)
addHist(Y=8, Dat=Simulation6, Color=rgb(0,0,0,0.25))
mtext(side=2, at=8, line=0, "sel. sim.")
Simulation7 <- c(Simulation2, Simulation3, Simulation4, Simulation5, Simulation6)
addHist(Y=8, Dat=Simulation7, Color=rgb(0,0,1,0.25))
simPop(Popsize=100, nGenerations=100, h=1, s=0, initial_p=0.5, mu = 0, twoway = TRUE, w = NULL)
simDraws
function(nruns, ncols=6, nstart=10, nrounds=3, w=NULL)	{
  Chiout <- c()
  for (j in 1:nruns)	{
    Start <- rep(1:ncols, nstart)
    Pop <- Start
    for (i in 1:nrounds)	{
      if (is.null(w))	{
        Draws <- sample(Pop, 20, replace = F)
      }
      else if (!is.null(w))	{
        if (length(setdiff(unique(Pop), names(w))) == 0)	{
          Draws <- sample(Pop, 20, replace=F, prob=w[Pop])
        }
        else if (length(setdiff(unique(Pop), names(w))) != 0)	{
          cat("Not enough fitness values! ", setdiff(unique(Pop), names(w)))
        }
      }
      Pop <- sort(c(Draws,Draws,Draws))
    }
    Summary <- c()
    for (k in 1:ncols)	{
      Summary[k] <- length(which(Pop == k))
    }
    
    Chiout[j] <- sum(((Summary - nstart)^2) / nstart)
  }
  return(Chiout)
}
#The mixture compared to the student generated data is much different but the generated data was under the assumption things were done perfectly. 
 #Selection was done in the lab and is also done by humans. 
  #The simulations to the left of the line are very similar in those done by the students, however the top one done as if things were 
  #done perfectly does show some  differences when compared to the others. 
#There is not strong selection shown in that data. Comparing the student numbers to the critical value does not give information on
  #the selection. Comparing them against the simulated numbers tells more about the processes. Adding the mutation would have to change
  #the x^2 values in some way because changing the possibilities would have to see what the chances are of getting the mutation due to
  #random chance. 