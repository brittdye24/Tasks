setwd('C:\\Users\\britt\\Evolution\\Tasks\\Task_11')
?rnorm
x <- rnorm(100, mean=5, sd=2)
summary(x)
x
y <- ((x*5)+2)+runif(100, 0, 0.1)
plot(x,y)
abline(lm(y~x))
coef(lm(y~x))
#The y-intercept is 2.035808. The slope is 5.002274. 2 and 5 were initially used
#when I formed the x object. 
z <- c()
for(i in 1:100) {
  z[i] <- runif(1, 0, 1)
  y <- ((x*z[i])+2)+runif(100, 0, 0.1)
  mb <- coef(lm(y~z[1:100]))
}
mb  
plot(z, z[1:100])
#The trend of the line is the same for both graphs just on different scales. 
#2
n <- 10000
prize <- sample(c("A","B", "C"), size=n, replace=TRUE)
open <- ifelse(prize=="A", sample(c("B", "C"), size=n, replace=TRUE), ifelse(prize=="B", "C", "B"))
notopen <- ifelse(open=="B", "C", "B")
same <- sum(prize=="A")/n
notsame <- sum(prize==notopen)/n
chanceWIN <- c(same, notsame)
barplot(chanceWIN, names.arg = c("no switch", "switch"), ylab="winning chance", main="Monty Hall Chance of Winning Fabululous Prize", col="hotpink")
