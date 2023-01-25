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
# recFun <- function(f1, f2, n, val = Inf, i = 1){
#     if(i > 1000) {
#         return(val)
#     }

#     pointValue <- f1(f2(n))

#     if(pointValue < val){
#         val <- pointValue
#     }

#     i <- i + 1
#     return(recFun(f1, f2, n, val, i))    
# }



# 1000 wywołań
# rndFunction1 <- function(n) { # numbers of demenstions
#     func <- makeAckleyFunction(n)
#     return(c(recFun(func, makeRnd1, n)))
# } 

rndFunction1 <- function(n) { # numbers of demenstions
    func <- makeAckleyFunction(n)
    currMin <- Inf

    for (i in 1:1000){
        pointValue <- func(makeRnd1(n))

        if (pointValue < currMin){
            currMin <- pointValue
        }
    }
    return(c(currMin))
} 


rndFunction2 <- function(n) { # numbers of demenstions
    func <- makeRastriginFunction(n)
    currMin <- Inf

    for (i in 1:1000){
        pointValue <- func(makeRnd2(n))
        if (pointValue < currMin){
            currMin <- pointValue
        }
    }
    return(c(currMin))
} 


test <- makeRnd(20)
ackley2 <- replicate(50, func50, rndFunction1(20))

hist(ackley2, breaks = 50)
hist(ackley10, breaks = 50)
hist(ackley20, breaks = 50)
boxplot(ackley2)

restring2 <- replicate(50, rndFunction2(2))
restring10 <- replicate(50, rndFunction2(10))
restring20 <- replicate(50, rndFunction2(20))

hist(restring2, breaks = 50)
hist(restring10, breaks = 50)
hist(restring20, breaks = 50)
boxplot(restring2)


# ALGORYTM MS
# --------------------------------------------------------

msFun1 <- function(n) {
    func <- makeAckleyFunction(n)
    start_points <- makeRnd1(100)
    bounds <- c(-32.768, 32.768)
    output_list <- list()
    
    for(i in 1:100){
        result <- optim(start_points[i], func, method = "L-BFGS-B",
                        lower = bounds[[1]], upper = bounds[[2]])
        output_list[[i]] <- result
    }
    return(output_list)
}

output_list <- msFun1(2)
best_result <- output_list[[which.min(sapply(output_list, function(x) x$value))]]
print(best_result)