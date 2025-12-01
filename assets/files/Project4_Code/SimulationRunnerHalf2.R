library(NSM3)
library(ExtDist)
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
         Laplace={x<-rLaplace(n)},
         lnorm={x<-rlnorm(n)},
         norm={x<-rnorm(n)},
         unif={x<-runif(n,-0.5,0.5)})
  return(x)
  
}
#
# Uniform Distribution Simulation
#
#Vary Balance in Sample Sizes At Each Level of c and sdr
#Vary SDR level at each level of c
print(paste("Start Job ",Sys.time()))
unifSimEqualS1C0 <- simulateUniform(30000,1,0,30,30)
unifSimN1S1C0 <- simulateUniform(30000,1,0,20,40)
unifSimN2S1C0 <- simulateUniform(30000,1,0,40,20)

unifSimEqualS1.25C0 <- simulateUniform(30000,1.25,0,30,30)
unifSimN1S1.25C0 <- simulateUniform(30000,1.25,0,20,40)
unifSimN2S1.25C0 <- simulateUniform(30000,1.25,0,40,20)

unifSimEqualS1.5C0 <- simulateUniform(30000,1.5,0,30,30)
unifSimN1S1.5C0 <- simulateUniform(30000,1.5,0,20,40)
unifSimN2S1.5C0 <- simulateUniform(30000,1.5,0,40,20)

unifSimEqualS2C0 <- simulateUniform(30000,2,0,30,30)
unifSimN1S2C0 <- simulateUniform(30000,2,0,20,40)
unifSimN2S2C0 <- simulateUniform(30000,2,0,40,20)

# Vary each level of c
# Center Shift is 0.5 base SD
unifSimEqualS1C0.5 <- simulateUniform(30000,1,0.5,30,30)
unifSimN1S1C0.5 <- simulateUniform(30000,1,0.5,20,40)
unifSimN2S1C0.5 <- simulateUniform(30000,1,0.5,40,20)

unifSimEqualS1.25C0.5 <- simulateUniform(30000,1.25,0.5,30,30)
unifSimN1S1.25C0.5 <- simulateUniform(30000,1.25,0.5,20,40)
unifSimN2S1.25C0.5 <- simulateUniform(30000,1.25,0.5,40,20)

unifSimEqualS1.5C0.5 <- simulateUniform(30000,1.5,0.5,30,30)
unifSimN1S1.5C0.5 <- simulateUniform(30000,1.5,0.5,20,40)
unifSimN2S1.5C0.5 <- simulateUniform(30000,1.5,0.5,40,20)

unifSimEqualS2C0.5 <- simulateUniform(30000,2,0.5,30,30)
unifSimN1S2C0.5 <- simulateUniform(30000,2,0.5,20,40)
unifSimN2S2C0.5 <- simulateUniform(30000,2,0.5,40,20)

# Center Shift is 1 base SD
unifSimEqualS1C1 <- simulateUniform(30000,1,1,30,30)
unifSimN1S1C1 <- simulateUniform(30000,1,1,20,40)
unifSimN2S1C1 <- simulateUniform(30000,1,1,40,20)

unifSimEqualS1.25C1 <- simulateUniform(30000,1.25,1,30,30)
unifSimN1S1.25C1 <- simulateUniform(30000,1.25,1,20,40)
unifSimN2S1.25C1 <- simulateUniform(30000,1.25,1,40,20)

unifSimEqualS1.5C1 <- simulateUniform(30000,1.5,1,30,30)
unifSimN1S1.5C1 <- simulateUniform(30000,1.5,1,20,40)
unifSimN2S1.5C1 <- simulateUniform(30000,1.5,1,40,20)

unifSimEqualS2C1 <- simulateUniform(30000,2,1,30,30)
unifSimN1S2C1 <- simulateUniform(30000,2,1,20,40)
unifSimN2S2C1 <- simulateUniform(30000,2,1,40,20)

#
# Normal Distribution Simulation
#
#Vary Balance in Sample Sizes At Each Level of c and sdr
#Vary SDR level at each level of c
normSimEqualS1C0 <- simulateNormal(30000,1,0,30,30)
normSimN1S1C0 <- simulateNormal(30000,1,0,20,40)
normSimN2S1C0 <- simulateNormal(30000,1,0,40,20)

normSimEqualS1.25C0 <- simulateNormal(30000,1.25,0,30,30)
normSimN1S1.25C0 <- simulateNormal(30000,1.25,0,20,40)
normSimN2S1.25C0 <- simulateNormal(30000,1.25,0,40,20)

normSimEqualS1.5C0 <- simulateNormal(30000,1.5,0,30,30)
normSimN1S1.5C0 <- simulateNormal(30000,1.5,0,20,40)
normSimN2S1.5C0 <- simulateNormal(30000,1.5,0,40,20)

normSimEqualS2C0 <- simulateNormal(30000,2,0,30,30)
normSimN1S2C0 <- simulateNormal(30000,2,0,20,40)
normSimN2S2C0 <- simulateNormal(30000,2,0,40,20)

# Vary each level of c
# Center Shift is 0.5 base SD
normSimEqualS1C0.5 <- simulateNormal(30000,1,0.5,30,30)
normSimN1S1C0.5 <- simulateNormal(30000,1,0.5,20,40)
normSimN2S1C0.5 <- simulateNormal(30000,1,0.5,40,20)

normSimEqualS1.25C0.5 <- simulateNormal(30000,1.25,0.5,30,30)
normSimN1S1.25C0.5 <- simulateNormal(30000,1.25,0.5,20,40)
normSimN2S1.25C0.5 <- simulateNormal(30000,1.25,0.5,40,20)

normSimEqualS1.5C0.5 <- simulateNormal(30000,1.5,0.5,30,30)
normSimN1S1.5C0.5 <- simulateNormal(30000,1.5,0.5,20,40)
normSimN2S1.5C0.5 <- simulateNormal(30000,1.5,0.5,40,20)

normSimEqualS2C0.5 <- simulateNormal(30000,2,0.5,30,30)
normSimN1S2C0.5 <- simulateNormal(30000,2,0.5,20,40)
normSimN2S2C0.5 <- simulateNormal(30000,2,0.5,40,20)

# Center Shift is 1 base SD
normSimEqualS1C1 <- simulateNormal(30000,1,1,30,30)
normSimN1S1C1 <- simulateNormal(30000,1,1,20,40)
normSimN2S1C1 <- simulateNormal(30000,1,1,40,20)

normSimEqualS1.25C1 <- simulateNormal(30000,1.25,1,30,30)
normSimN1S1.25C1 <- simulateNormal(30000,1.25,1,20,40)
normSimN2S1.25C1 <- simulateNormal(30000,1.25,1,40,20)

normSimEqualS1.5C1 <- simulateNormal(30000,1.5,1,30,30)
normSimN1S1.5C1 <- simulateNormal(30000,1.5,1,20,40)
normSimN2S1.5C1 <- simulateNormal(30000,1.5,1,40,20)

normSimEqualS2C1 <- simulateNormal(30000,2,1,30,30)
normSimN1S2C1 <- simulateNormal(30000,2,1,20,40)
normSimN2S2C1 <- simulateNormal(30000,2,1,40,20)
#
# Laplace Distribution Simulation
#
lpSimEqualS1C0 <- simulateLaplace(30000,1,0,30,30)
lpSimN1S1C0 <- simulateLaplace(30000,1,0,20,40)
lpSimN2S1C0 <- simulateLaplace(30000,1,0,40,20)

lpSimEqualS1.25C0 <- simulateLaplace(30000,1.25,0,30,30)
lpSimN1S1.25C0 <- simulateLaplace(30000,1.25,0,20,40)
lpSimN2S1.25C0 <- simulateLaplace(30000,1.25,0,40,20)

lpSimEqualS1.5C0 <- simulateLaplace(30000,1.5,0,30,30)
lpSimN1S1.5C0 <- simulateLaplace(30000,1.5,0,20,40)
lpSimN2S1.5C0 <- simulateLaplace(30000,1.5,0,40,20)

lpSimEqualS2C0 <- simulateLaplace(30000,2,0,30,30)
lpSimN1S2C0 <- simulateLaplace(30000,2,0,20,40)
lpSimN2S2C0 <- simulateLaplace(30000,2,0,40,20)

# Vary each level of c
# Center Shift is 0.5 base SD
lpSimEqualS1C0.5 <- simulateLaplace(30000,1,0.5,30,30)
lpSimN1S1C0.5 <- simulateLaplace(30000,1,0.5,20,40)
lpSimN2S1C0.5 <- simulateLaplace(30000,1,0.5,40,20)

lpSimEqualS1.25C0.5 <- simulateLaplace(30000,1.25,0.5,30,30)
lpSimN1S1.25C0.5 <- simulateLaplace(30000,1.25,0.5,20,40)
lpSimN2S1.25C0.5 <- simulateLaplace(30000,1.25,0.5,40,20)

lpSimEqualS1.5C0.5 <- simulateLaplace(30000,1.5,0.5,30,30)
lpSimN1S1.5C0.5 <- simulateLaplace(30000,1.5,0.5,20,40)
lpSimN2S1.5C0.5 <- simulateLaplace(30000,1.5,0.5,40,20)

lpSimEqualS2C0.5 <- simulateLaplace(30000,2,0.5,30,30)
lpSimN1S2C0.5 <- simulateLaplace(30000,2,0.5,20,40)
lpSimN2S2C0.5 <- simulateLaplace(30000,2,0.5,40,20)

# Center Shift is 1 base SD
lpSimEqualS1C1 <- simulateLaplace(30000,1,1,30,30)
lpSimN1S1C1 <- simulateLaplace(30000,1,1,20,40)
lpSimN2S1C1 <- simulateLaplace(30000,1,1,40,20)

lpSimEqualS1.25C1 <- simulateLaplace(30000,1.25,1,30,30)
lpSimN1S1.25C1 <- simulateLaplace(30000,1.25,1,20,40)
lpSimN2S1.25C1 <- simulateLaplace(30000,1.25,1,40,20)

lpSimEqualS1.5C1 <- simulateLaplace(30000,1.5,1,30,30)
lpSimN1S1.5C1 <- simulateLaplace(30000,1.5,1,20,40)
lpSimN2S1.5C1 <- simulateLaplace(30000,1.5,1,40,20)

lpSimEqualS2C1 <- simulateLaplace(30000,2,1,30,30)
lpSimN1S2C1 <- simulateLaplace(30000,2,1,20,40)
lpSimN2S2C1 <- simulateLaplace(30000,2,1,40,20)