library(smoof)
library(ggplot2)

# ALGORYTM PRS
#-----------------------------------------------------------------
makeRnd1 <- function(n){
    return(c(runif(n, -32.768, 32.768)))
}

makeRnd2 <- function(n){
    return(c(runif(n, -5.12, 5.12)))
}

rndFunction1 <- function(n, num, flag) {
    if(flag == TRUE){
        func <- makeAckleyFunction(n)
        rndFunc <- makeRnd1
    }
    else{
        func <- makeRastriginFunction(n)
        rndFunc <- makeRnd2
    }
    
    currMin <- Inf

    for (i in 1:num){
        pointValue <- func(rndFunc(n))

        if (pointValue < currMin){
            currMin <- pointValue
        }
    }
    return(c(currMin))
}

# ALGORYTM MS
# --------------------------------------------------------

msFun <- function(n, lowerLeft, upperRight, flag) {
    if(flag == TRUE) fun <- makeAckleyFunction(n)
    else fun <- makeRastriginFunction(n)

    start_points <- matrix(runif(100*n, min = lowerLeft, max = upperRight), ncol = n)
    bounds <- rbind(rep(lowerLeft, n), rep(upperRight, n))
    numProduce <- 0
    currMin <- Inf

    for(i in 1:100){
        result <- optim(start_points[i, ], fun, method = "L-BFGS-B", lower = bounds[1,], upper = bounds[2,])
        if(result$value[1] < currMin){
            currMin <- result$value[1]
        }
        numProduce <- numProduce + result$counts[1]
    }
    return(c(numProduce[[1]],currMin))
}


ackleyMs2 <- replicate(50, msFun(2, -32.768, 32.768, TRUE))
ackleyMs10 <- replicate(50, msFun(10, -32.768, 32.768, TRUE))
ackleyMs20 <- replicate(50, msFun(20, -32.768, 32.768, TRUE))

rastringMs2 <- replicate(50, msFun(2, -5.12, 5.12, FALSE))
rastringMs10 <- replicate(50, msFun(10, -5.12, 5.12, FALSE))
rastringMs20 <- replicate(50, msFun(20, -5.12, 5.12, FALSE))

ackley2 <- replicate(50, rndFunction1(2, mean(ackleyMs2[1,]), TRUE))
ackley10 <- replicate(50, rndFunction1(10, mean(ackleyMs10[1,]), TRUE))
ackley20 <- replicate(50, rndFunction1(20, mean(ackleyMs20[1,]), TRUE))

rastring2 <- replicate(50, rndFunction1(2, mean(rastringMs2[1, ]), FALSE))
rastring10 <- replicate(50, rndFunction1(10, mean(rastringMs10[1, ]), FALSE))
rastring20 <- replicate(50, rndFunction1(20, mean(rastringMs20[1, ]), FALSE))



hist(ackley2, breaks = 50, main = "Funkcja Ackley'a w 2 wymiarach - PRS", xlab = "Wartość minimum", ylab = "Ilość", axes = FALSE)
axis(1,at=seq(-1000, 1000,by=0.1))
axis(2,at=seq(0, 50,by=1))

hist(ackley10, breaks = 50, main = "Funkcja Ackley'a w 10 wymiarach - PRS", xlab = "Wartość minimum", ylab = "Ilość", axes = FALSE)
axis(1,at=seq(-1000, 1000,by=0.1))
axis(2,at=seq(0, 50,by=1))

hist(ackley20, breaks = 50, main = "Funkcja Ackley'a w 20 wymiarach - PRS", xlab = "Wartość minimum", ylab = "Ilość", axes = FALSE)
axis(1,at=seq(-1000, 1000,by=0.1))
axis(2,at=seq(0, 50,by=1))



hist(ackleyMs2[2,], breaks = 50, main = "Funkcja Ackley'a w 2 wymiarach - MS", xlab = "Wartość minimum", ylab = "Ilość")
axis(1,at=seq(-10000, 10000,by=100))
axis(2,at=seq(-50, 50,by=5))

hist(ackleyMs10[2,], breaks = 50, main = "Funkcja Ackley'a w 10 wymiarach - MS", xlab = "Wartość minimum", ylab = "Ilość")
axis(1,at=seq(-10000, 10000,by=100))
axis(2,at=seq(0, 50,by=5))

hist(ackleyMs20[2,], breaks = 50, main = "Funkcja Ackley'a w 20 wymiarach - MS", xlab = "Wartość minimum", ylab = "Ilość")
axis(1,at=seq(-10000, 10000,by=100))
axis(2,at=seq(0, 50,by=5))




boxplot(ackley2, main = "Wykres pudełkowy dla Ackley'a w 2 wymiarach - PRS", axes = TRUE)
# axis(2,at=seq(2, 50,by=0.5))

boxplot(ackleyMs2[2,], main = "Wykres pudełkowy dla Ackley'a w 2 wymiarach - MS", axes = TRUE)
# axis(2,at=seq(2, 50,by=0.5))
boxplot(ackleyMs10[2,], main = "Wykres pudełkowy dla Ackley'a w 10 wymiarach - MS", axes = TRUE)
# axis(2,at=seq(2, 50,by=0.5))



hist(restring2, breaks = 50, main = "Funkcja Restring'a w 2 wymiarach - PRS", xlab = "Wartość minimum", ylab = "Ilość")

hist(restring10, breaks = 50, main = "Funkcja Restring'a w 10 wymiarach - PRS", xlab = "Wartość minimum", ylab = "Ilość", axes = FALSE)
axis(1,at=seq(-10000, 10000,by=1))
axis(2,at=seq(0, 50,by=1))

hist(restring20, breaks = 50, main = "Funkcja Restring'a w 20 wymiarach - PRS", xlab = "Wartość minimum", ylab = "Ilość")
axis(1,at=seq(-10000, 10000,by=5))
axis(2,at=seq(0, 50,by=1))



hist(restringMs2[2,], breaks = 50, main = "Funkcja Restring'a w 2 wymiarach - MS", xlab = "Wartość minimum", ylab = "Ilość", axes = FALSE)
axis(1,at=seq(-10000, 10000,by=50))
axis(2,at=seq(0, 50,by=1))

hist(restringMs10[2,], breaks = 50, main = "Funkcja Restring'a w 10 wymiarach - MS", xlab = "Wartość minimum", ylab = "Ilość", axes = FALSE)
axis(1,at=seq(-10000, 10000,by=50))
axis(2,at=seq(0, 50,by=1))

hist(restringMs20[2,], breaks = 50, main = "Funkcja Restring'a w 20 wymiarach - MS", xlab = "Wartość minimum", ylab = "Ilość", axes = FALSE)
axis(1,at=seq(-10000, 10000,by=50))
axis(2,at=seq(0, 50,by=1))

boxplot(restring2, main = "Wykres pudełkowy dla Restring'a w 2 wymiarach", axes = TRUE)
boxplot(restringMs2[2,], main = "Wykres pudełkowy dla Restring'a w 2 wymiarach - MS", axes = TRUE)
boxplot(restringMs10[2,], main = "Wykres pudełkowy dla Restring'a w 10 wymiarach - MS", axes = TRUE)



hist(restring2, breaks = 50)
hist(restring10, breaks = 50)
hist(restring20, breaks = 50)
boxplot(restring2)


hist(restringMs20, breaks = 50)

# MS_resultsAMs2 <- unlist(ackleyMs2[1:length(ackleyMs2)])
MS_resultsAMs2 <- ackleyMs2[2,]
PRS_resultsA2 <- ackley2

MS_resultsAMs10 <- ackleyMs10[2,]
PRS_resultsA10 <- ackley10

MS_resultsAMs20 <- ackleyMs20[2,]
PRS_resultsA20 <- ackley20



MS_resultsRMs2 <- rastringMs2[2,]
PRS_resultsR2 <- rastring2

MS_resultsRMs10 <- rastringMs10[2,]
PRS_resultsR10 <- rastring10

MS_resultsRMs20 <- rastringMs20[2,]
PRS_resultsR20 <- rastring20

mean(MS_resultsAMs2)
mean(PRS_resultsA2)

mean(MS_resultsAMs10)
mean(PRS_resultsA10)

mean(MS_resultsAMs20)
mean(PRS_resultsA20)

mean(MS_resultsRMs2)
mean(PRS_resultsR2)

mean(MS_resultsRMs10)
mean(PRS_resultsR10)

mean(MS_resultsRMs20)
mean(PRS_resultsR20)

t.test(MS_resultsMs2, PRS_resultsA2)
t.test(MS_resultsAMs10, PRS_resultsA10)
t.test(MS_resultsAMs20, PRS_resultsA20)

t.test(MS_resultsRMs2, PRS_resultsR2)
t.test(MS_resultsRMs10, PRS_resultsR10)
t.test(MS_resultsRMs20, PRS_resultsR20)

# Analiza wykresów
# Wykres 1
sd(PRS_resultsA2)
sd(PRS_resultsA10)
sd(PRS_resultsA20)
var(PRS_resultsA20)
summary(PRS_resultsA2)
summary(PRS_resultsA10)
summary(PRS_resultsA20)

sd(MS_resultsAMs2)
sd(MS_resultsAMs10)
sd(MS_resultsAMs20)
summary(MS_resultsAMs2)
summary(MS_resultsAMs10)
summary(MS_resultsAMs20)






