#Data Collected for each simulation
columns <- c("iterationNum","SDR","Mean Difference","Median Difference",
             "t Statistic","Observed t p-val",
             "W Statistic", "Observed W p-val",
             "AB Statistic", "Observed AB p-val",
             "LP p-val")

# Method to plot t-values
plotAlphaDist <- function(sim) {
  par(mfrow=c(2,2))
  hist(sim$`Observed t p-val`)
  hist(sim$`Observed W p-val`)
  plot(density(sim$`Observed t p-val`,kernel="epanechnikov"))
  plot(density(sim$`Observed W p-val`,kernel="epanechnikov"))
}
# Method to plot t and W statistics
plottWstat <- function(sim) {
  par(mfrow=c(2,2))
  hist(sim$`t Statistic`)
  hist(sim$`W Statistic`)
  plot(density(sim$`t Statistic`,kernel="epanechnikov"))
  plot(density(sim$`W Statistic`,kernel="epanechnikov"))
}
# Simulations
simulateUniform <- function(n,SDR,cShift,n1,n2) {
  time1 <- Sys.time()
  alt <- "two.sided"
  results <- data.frame(matrix(nrow=0,ncol=length(columns)))
  colnames(results) <- columns
  for(i in 1:n) {
    set.seed(i)
    x <- runif(n1,-0.5,0.5)
    y <- runif(n2,-0.5,0.5)
    
    # Enforces Given SDR ratio and Shift in Center
    
    y <- (y-median(y))*SDR+median(y)+cShift*(1/sqrt(12))
    tTest <- t.test(x,y,alternative=alt)
    wTest <- wilcox.test(x,y,alternative=alt,exact=FALSE)
    abTest <- ansari.test(x,y,exact=FALSE)
    lpTest <- pLepage(x,y,method="Asymptotic")
    results[nrow(results)+1,] <- list(i, sd(y)/sd(x), mean(y)-mean(x), median(y)-median(x), 
                                      tTest$statistic, tTest$p.value, 
                                      wTest$statistic, wTest$p.value, 
                                      abTest$statistic, abTest$p.value, pLepage(x,y,method="Asymptotic")$p.val)
    #print(i)
  }
  print(paste("Start: ",time1,"  End: ",Sys.time()))
  return(results)
}
simulateNormal <- function(n,SDR,cShift,n1,n2) {
  time1 <- Sys.time()
  alt <- "two.sided"
  results <- data.frame(matrix(nrow=0,ncol=length(columns)))
  colnames(results) <- columns
  for(i in 1:n) {
    set.seed(i)
    x <- rnorm(n1,0,1)
    y <- rnorm(n2,0,1)
    # Enforces Given SDR ratio and Shift in Center
    y <- (y-median(y))*SDR+median(y)+cShift
    tTest <- t.test(x,y,alternative=alt)
    wTest <- wilcox.test(x,y,alternative=alt,exact=FALSE)
    abTest <- ansari.test(x,y,exact=FALSE)
    lpTest <- pLepage(x,y,method="Asymptotic")
    results[nrow(results)+1,] <- list(i, sd(y)/sd(x), mean(y)-mean(x), median(y)-median(x), 
                                      tTest$statistic, tTest$p.value, 
                                      wTest$statistic, wTest$p.value, 
                                      abTest$statistic, abTest$p.value, pLepage(x,y,method="Asymptotic")$p.val)
    #print(i)
  }
  print(paste("Start: ",time1,"  End: ",Sys.time()))
  return(results)
}

simulateLaplace <- function(n,SDR,cShift,n1,n2) {
  time1 <- Sys.time()
  alt <- "two.sided"
  results <- data.frame(matrix(nrow=0,ncol=length(columns)))
  colnames(results) <- columns
  for(i in 1:n) {
    set.seed(i)
    x <- rLaplace(n1,0,1)
    y <- rLaplace(n2,0,1)
    # Enforces Given SDR ratio and Shift in Center
    y <- (y-median(y))*SDR+median(y)+cShift
    tTest <- t.test(x,y,alternative=alt)
    wTest <- wilcox.test(x,y,alternative=alt,exact=FALSE)
    abTest <- ansari.test(x,y,exact=FALSE)
    lpTest <- pLepage(x,y,method="Asymptotic")
    results[nrow(results)+1,] <- list(i, sd(y)/sd(x), mean(y)-mean(x), median(y)-median(x), 
                                      tTest$statistic, tTest$p.value, 
                                      wTest$statistic, wTest$p.value, 
                                      abTest$statistic, abTest$p.value, pLepage(x,y,method="Asymptotic")$p.val)
    #print(i)
  }
  print(paste("Start: ",time1,"  End: ",Sys.time()))
  return(results)
}

simulateExponential <- function(n,SDR,cShift,n1,n2) {
  time1 <- Sys.time()
  alt <- "two.sided"
  results <- data.frame(matrix(nrow=0,ncol=length(columns)))
  colnames(results) <- columns
  for(i in 1:n) {
    set.seed(i)
    x <- rexp(n1,1)
    y <- rexp(n2,1)
    # Enforces Given SDR ratio and Shift in Center
    y <- (y-median(y))*SDR+median(y)+cShift
    tTest <- t.test(x,y,alternative=alt)
    wTest <- wilcox.test(x,y,alternative=alt,exact=FALSE)
    abTest <- ansari.test(x,y,exact=FALSE)
    lpTest <- pLepage(x,y,method="Asymptotic")
    results[nrow(results)+1,] <- list(i, sd(y)/sd(x), mean(y)-mean(x), median(y)-median(x), 
                                      tTest$statistic, tTest$p.value, 
                                      wTest$statistic, wTest$p.value, 
                                      abTest$statistic, abTest$p.value, pLepage(x,y,method="Asymptotic")$p.val)
    #print(i)
  }
  print(paste("Start: ",time1,"  End: ",Sys.time()))
  return(results)
}

simulateChiSquare <- function(n,SDR,cShift,n1,n2) {
  time1 <- Sys.time()
  alt <- "two.sided"
  results <- data.frame(matrix(nrow=0,ncol=length(columns)))
  colnames(results) <- columns
  for(i in 1:n) {
    set.seed(i)
    x <- rchisq(n1,1)
    y <- rchisq(n2,1)
    # Enforces Given SDR ratio and Shift in Center
    y <- (y-median(y))*SDR+median(y)+cShift
    tTest <- t.test(x,y,alternative=alt)
    wTest <- wilcox.test(x,y,alternative=alt,exact=FALSE)
    abTest <- ansari.test(x,y,exact=FALSE)
    lpTest <- pLepage(x,y,method="Asymptotic")
    results[nrow(results)+1,] <- list(i, sd(y)/sd(x), mean(y)-mean(x), median(y)-median(x), 
                                      tTest$statistic, tTest$p.value, 
                                      wTest$statistic, wTest$p.value, 
                                      abTest$statistic, abTest$p.value, pLepage(x,y,method="Asymptotic")$p.val)
    #print(i)
  }
  print(paste("Start: ",time1,"  End: ",Sys.time()))
  return(results)
}

simulateLogNormal <- function(n,SDR,cShift,n1,n2) {
  time1 <- Sys.time()
  alt <- "two.sided"
  results <- data.frame(matrix(nrow=0,ncol=length(columns)))
  colnames(results) <- columns
  for(i in 1:n) {
    set.seed(i)
    x <- rlnorm(n1,1)
    y <- rlnorm(n2,1)
    # Enforces Given SDR ratio and Shift in Center
    y <- (y-median(y))*SDR+median(y)+cShift
    tTest <- t.test(x,y,alternative=alt)
    wTest <- wilcox.test(x,y,alternative=alt,exact=FALSE)
    abTest <- ansari.test(x,y,exact=FALSE)
    lpTest <- pLepage(x,y,method="Asymptotic")
    results[nrow(results)+1,] <- list(i, sd(y)/sd(x), mean(y)-mean(x), median(y)-median(x), 
                                      tTest$statistic, tTest$p.value, 
                                      wTest$statistic, wTest$p.value, 
                                      abTest$statistic, abTest$p.value, pLepage(x,y,method="Asymptotic")$p.val)
    #print(i)
  }
  print(paste("Start: ",time1,"  End: ",Sys.time()))
  return(results)
}

simulateDistributionAtRandom <- function(n,SDR,cShift,n1,n2) {
  time1 <- Sys.time()
  alt <- "two.sided"
  results <- data.frame(matrix(nrow=0,ncol=length(columns)))
  colnames(results) <- columns
  for(i in 1:n) {
    set.seed(i)
    xNum <- sample(1:6,1)
    yNum <- sample(1:6,1)
    x <- returnRandomDistribution(xNum,n1)
    x <- x-median(x)
    y <- returnRandomDistribution(yNum,n2)
    y <- (y-median(y))*(SDR*sd(x)/sd(y))+cShift
    median(y)-median(x)
    sd(y)/sd(x)
    
    # Enforces Given SDR ratio and Shift in Center
    y <- (y-median(y))*SDR+median(y)+cShift
    tTest <- t.test(x,y,alternative=alt)
    wTest <- wilcox.test(x,y,alternative=alt,exact=FALSE)
    abTest <- ansari.test(x,y,exact=FALSE)
    lpTest <- pLepage(x,y,method="Asymptotic")
    results[nrow(results)+1,] <- list(i, sd(y)/sd(x), mean(y)-mean(x), median(y)-median(x), 
                                      tTest$statistic, tTest$p.value, 
                                      wTest$statistic, wTest$p.value, 
                                      abTest$statistic, abTest$p.value, pLepage(x,y,method="Asymptotic")$p.val)
    #print(i)
  }
  print(paste("Start: ",time1,"  End: ",Sys.time()))
  return(results)
}
returnRandomDistribution <- function(num,n) {
  switch(num, chisq={x<-rchisq(n,1)},
         exp={x<-rexp(n,1)},
         Laplace={x<-rLaplace(n,0,1)},
         lnorm={x<-rlnorm(n,0,1)},
         norm={x<-rnorm(n,0,1)},
         unif={x<-runif(n,-0.5,0.5)})
  return(x)
  
}
sd(rlnorm(100000,0,1))
x <- rlnorm(10000000,0,1)
x <- rlnorm(10000000,0,0.4812)
sd(x)
sd(log(x))
sd(rlnorm(10000000,0,0.6931))
