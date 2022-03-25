setwd('C:\\Users\\britt\\OneDrive\\BIOL 461')
MIAMI <- read.csv("C:/Users/britt/OneDrive/BIOL 461/miami data.csv")
HAWAII <- read.csv("C:/Users/britt/OneDrive/BIOL 461/hawaii data.csv")
hawaii01 <- read.csv("C:/Users/britt/OneDrive/BIOL 461/hawaii data no")

par(mfrow=c(1,2), mar=c(4,4,1,1), las=1)
plot(HAWAII$Monarch, HAWAII$Longevity, pch=16, xlab="lineage", ylab="longevity")
plot(MIAMI$Monarch, MIAMI$Longevity, pch=16, xlab="lineage", ylab="longevity")
title(main="Miami")
