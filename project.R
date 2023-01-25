library(openssl)
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

rndFunction1 <- function(n, num, flag) { # numbers of demenstions
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

msFun2 <- function(n, lowerLeft, upperRight) {
    fun <- makeAckleyFunction(n)
    start_points <- matrix(runif(100*n, min = lowerLeft, max = upperRight), ncol = n)
    bounds <- rbind(rep(lowerLeft, n), rep(upperRight, n))
    finalList <- list()
    numProduce <- 0
    currMin <- Inf
    for(i in 1:100){
        result <- optim(start_points[i, ], fun, method = "L-BFGS-B", lower = bounds[1,], upper = bounds[2,])
        if(result$value[1] < currMin){
            currMin <- result$value[1]
        }
        numProduce <- numProduce + result$counts[1]
        # finalList[[i]] <- result$value
    }
    # return(c(numProduce[[1]],finalList))
    return(c(numProduce[[1]],currMin))
}


ackleyMs2 <- replicate(50, msFun2(2, -32.768, 32.768))
ackleyMs10 <- replicate(50, msFun2(10, -32.768, 32.768))
ackleyMs20 <- replicate(50, msFun2(20, -32.768, 32.768))

restringMs2 <- replicate(50, msFun2(2, -5.12, 5.12))
restringMs10 <- replicate(50, msFun2(10, -5.12, 5.12))
restringMs20 <- replicate(50, msFun2(20, -5.12, 5.12))

ackley2 <- replicate(50, rndFunction1(2, mean(ackleyMs2[1,]), TRUE))
ackley10 <- replicate(50, rndFunction1(10, mean(ackleyMs10[1,]), TRUE))
ackley20 <- replicate(50, rndFunction1(20, mean(ackleyMs20[1,]), TRUE))

hist(ackley2, breaks = 50)
hist(ackley10, breaks = 50)
hist(ackley20, breaks = 50)
boxplot(ackley2)

restring2 <- replicate(50, rndFunction1(2, mean(restringMs2[1, ]), FALSE))
restring10 <- replicate(50, rndFunction1(10, mean(restringMs10[1, ]), FALSE))
restring20 <- replicate(50, rndFunction1(20, mean(restringMs20[1, ]), FALSE))

hist(restring2, breaks = 50)
hist(restring10, breaks = 50)
hist(restring20, breaks = 50)
boxplot(restring2)


# MS_resultsAMs2 <- unlist(ackleyMs2[1:length(ackleyMs2)])
MS_resultsAMs2 <- ackleyMs2[2,]
PRS_resultsA2 <- ackley2

MS_resultsAMs10 <- ackleyMs10[2,]
PRS_resultsA10 <- ackley10

MS_resultsAMs20 <- ackleyMs20[2,]
PRS_resultsA20 <- ackley20



MS_resultsRMs2 <- restringMs2[2,]
PRS_resultsR2 <- restring2

MS_resultsRMs10 <- restringMs10[2,]
PRS_resultsR10 <- restring10

MS_resultsRMs20 <- restringMs20[2,]
PRS_resultsR20 <- restring20


t.test(MS_resultsMs2, PRS_resultsA2)
t.test(MS_resultsAMs10, PRS_resultsA10)
t.test(MS_resultsAMs20, PRS_resultsA20)

t.test(MS_resultsRMs2, PRS_resultsR2)
t.test(MS_resultsRMs10, PRS_resultsR10)
t.test(MS_resultsRMs20, PRS_resultsR20)



