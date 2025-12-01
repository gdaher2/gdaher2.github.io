#Plot Alpha Distributions
plotAlphaDist <- function(sim) {
  par(mfrow=c(2,2))
  hist(sim$Observed.t.p.val)
  hist(sim$Observed.W.p.val)
  plot(density(sim$Observed.t.p.val,kernel="epanechnikov"))
  plot(density(sim$Observed.W.p.val,kernel="epanechnikov"))
}
plotAlphaDist(unifSimN1S1C0)

# Method to plot t and W statistics
plottWstat <- function(sim) {
  par(mfrow=c(2,2))
  hist(sim$t.Statistic)
  hist(sim$W.Statistic)
  plot(density(sim$t.Statistic,kernel="epanechnikov"))
  plot(density(sim$W.Statistic,kernel="epanechnikov"))
}

plottWAlpha <-function(dString,s,c) {
  print(paste0(dString,"Sim","Equal","S",s,"C",c))
  plot
}
plottWAlpha("norm",1,0)


distUsed <- c("norm","unif","lp","chisq","exp","lnorm")

plotWp <- function(stringD,c) {
  stringDist <- paste0(stringD,"Sim")
  plot( density( get(paste0(stringDist,"Equal","S",1,"C",c))$Observed.W.p.val ),main=paste0("SDR=",1," and Sample Size 1:1") )
  plot( density( get(paste0(stringDist,"N1"   ,"S",1,"C",c))$Observed.W.p.val ),main=paste0("SDR=",1," and Sample Size 1:2") )
  plot( density( get(paste0(stringDist,"N2"   ,"S",1,"C",c))$Observed.W.p.val ),main=paste0("SDR=",1," and Sample Size 2:1") )
  
  plot( density( get(paste0(stringDist,"Equal","S",1.25,"C",c))$Observed.W.p.val ),main=paste0("SDR=",1.25," and Sample Size 1:1") )
  plot( density( get(paste0(stringDist,"N1"   ,"S",1.25,"C",c))$Observed.W.p.val ),main=paste0("SDR=",1.25," and Sample Size 1:2") )
  plot( density( get(paste0(stringDist,"N2"   ,"S",1.25,"C",c))$Observed.W.p.val ),main=paste0("SDR=",1.25," and Sample Size 2:1") )
  
  plot( density( get(paste0(stringDist,"Equal","S",1.5,"C",c))$Observed.W.p.val ),main=paste0("SDR=",1.5," and Sample Size 1:1") )
  plot( density( get(paste0(stringDist,"N1"   ,"S",1.5,"C",c))$Observed.W.p.val ),main=paste0("SDR=",1.5," and Sample Size 1:2") )
  plot( density( get(paste0(stringDist,"N2"   ,"S",1.5,"C",c))$Observed.W.p.val ),main=paste0("SDR=",1.5," and Sample Size 2:1") )

  plot( density( get(paste0(stringDist,"Equals","S",2,"C",c))$Observed.W.p.val ),main=paste0("SDR=",2," and Sample Size 1:1") )
  plot( density( get(paste0(stringDist,"N1"   ,"S",2,"C",c))$Observed.W.p.val ),main=paste0("SDR=",2," and Sample Size 1:2") )
  plot( density( get(paste0(stringDist,"N2"   ,"S",2,"C",c))$Observed.W.p.val ),main=paste0("SDR=",2," and Sample Size 2:1") )
  }
par(mfrow=c(4,3))
for(i in distUsed) {
  plotWp(i,0)
}
for(i in distUsed) {
  plotWp(i,0.5)
}
for(i in distUsed) {
  plotWp(i,1)
}

plottp <- function(stringD,c) {
  stringDist <- paste0(stringD,"Sim")
  plot( density( get(paste0(stringDist,"Equal","S",1,"C",c))$Observed.t.p.val ),main=paste0("SDR=",1," and Sample Size 1:1") )
  plot( density( get(paste0(stringDist,"N1"   ,"S",1,"C",c))$Observed.t.p.val ),main=paste0("SDR=",1," and Sample Size 1:2") )
  plot( density( get(paste0(stringDist,"N2"   ,"S",1,"C",c))$Observed.t.p.val ),main=paste0("SDR=",1," and Sample Size 2:1") )
  
  plot( density( get(paste0(stringDist,"Equal","S",1.25,"C",c))$Observed.t.p.val ),main=paste0("SDR=",1.25," and Sample Size 1:1") )
  plot( density( get(paste0(stringDist,"N1"   ,"S",1.25,"C",c))$Observed.t.p.val ),main=paste0("SDR=",1.25," and Sample Size 1:2") )
  plot( density( get(paste0(stringDist,"N2"   ,"S",1.25,"C",c))$Observed.t.p.val ),main=paste0("SDR=",1.25," and Sample Size 2:1") )
  
  plot( density( get(paste0(stringDist,"Equal","S",1.5,"C",c))$Observed.t.p.val ),main=paste0("SDR=",1.5," and Sample Size 1:1") )
  plot( density( get(paste0(stringDist,"N1"   ,"S",1.5,"C",c))$Observed.t.p.val ),main=paste0("SDR=",1.5," and Sample Size 1:2") )
  plot( density( get(paste0(stringDist,"N2"   ,"S",1.5,"C",c))$Observed.t.p.val ),main=paste0("SDR=",1.5," and Sample Size 2:1") )
  
  plot( density( get(paste0(stringDist,"Equals","S",2,"C",c))$Observed.t.p.val ),main=paste0("SDR=",2," and Sample Size 1:1") )
  plot( density( get(paste0(stringDist,"N1"   ,"S",2,"C",c))$Observed.t.p.val ),main=paste0("SDR=",2," and Sample Size 1:2") )
  plot( density( get(paste0(stringDist,"N2"   ,"S",2,"C",c))$Observed.t.p.val ),main=paste0("SDR=",2," and Sample Size 2:1") )
}
par(mfrow=c(4,3))
for(i in distUsed) {
  plottp(i,0)
}
for(i in distUsed) {
  plottp(i,0.5)
}
for(i in distUsed) {
  plottp(i,1)
}
powerW0.5 <- data.frame(matrix(nrow=0,ncol=4))
names(powerW0.5) <- c("Distribution and SDR","Sample Size 1:1","Sample Size 1:2","Sample Size 2:1")
alpha <- 0.05
powerW0.5 <- data.frame(matrix(nrow=0,ncol=4))
names(powerW0.5) <- c("Distribution and SDR","Sample Size 1:1","Sample Size 1:2","Sample Size 2:1")
for(i in distUsed) {
  for(j in c("1","1.25","1.5")) {
    v1 <- get(paste0(i,"Sim","Equal","S",j,"C",0.5))$Observed.W.p.val
    q1 <- sum(v1 <= alpha)/30000
    v2 <- get(paste0(i,"Sim","N1","S",j,"C",0.5))$Observed.W.p.val
    q2 <- sum(v2 <= alpha)/30000
    v3 <- get(paste0(i,"Sim","N2","S",j,"C",0.5))$Observed.W.p.val
    q3 <- sum(v3 <= alpha)/30000
    powerW0.5[nrow(powerW0.5)+1,] <- list(paste0(i, " SDR=",j),q1,q2,q3)
  }
  v1 <- get(paste0(i,"Sim","Equals","S",2,"C",0.5))$Observed.W.p.val
  q1 <- sum(v1 <= alpha)/30000
  v2 <- get(paste0(i,"Sim","N1","S",2,"C",0.5))$Observed.W.p.val
  q2 <- sum(v2 <= alpha)/30000
  v3 <- get(paste0(i,"Sim","N2","S",2,"C",0.5))$Observed.W.p.val
  q3 <- sum(v3 <= alpha)/30000
  powerW0.5[nrow(powerW0.5)+1,] <- list(paste0(i, " SDR=",2),q1,q2,q3)
}

powerW1 <- data.frame(matrix(nrow=0,ncol=4))
names(powerW1) <- c("Distribution and SDR","Sample Size 1:1","Sample Size 1:2","Sample Size 2:1")
for(i in distUsed) {
  for(j in c("1","1.25","1.5")) {
    v1 <- get(paste0(i,"Sim","Equal","S",j,"C",1))$Observed.W.p.val
    q1 <- sum(v1 <= alpha)/30000
    v2 <- get(paste0(i,"Sim","N1","S",j,"C",1))$Observed.W.p.val
    q2 <- sum(v2 <= alpha)/30000
    v3 <- get(paste0(i,"Sim","N2","S",j,"C",1))$Observed.W.p.val
    q3 <- sum(v3 <= alpha)/30000
    powerW1[nrow(powerW1)+1,] <- list(paste0(i, " SDR=",j),q1,q2,q3)
  }
  v1 <- get(paste0(i,"Sim","Equals","S",2,"C",1))$Observed.W.p.val
  q1 <- sum(v1 <= alpha)/30000
  v2 <- get(paste0(i,"Sim","N1","S",2,"C",1))$Observed.W.p.val
  q2 <- sum(v2 <= alpha)/30000
  v3 <- get(paste0(i,"Sim","N2","S",2,"C",1))$Observed.W.p.val
  q3 <- sum(v3 <= alpha)/30000
  powerW1[nrow(powerW1)+1,] <- list(paste0(i, " SDR=",2),q1,q2,q3)
}


par(mfrow=c(4,2))
for(i in distUsed) {
  plot( density( get(paste0(i,"Sim","Equal","S",1,"C", 0))$Observed.W.p.val ),main=paste0("Wilcoxon P-Values SDR = 1 ",i," Dist") )
  plot( density( get(paste0(i,"Sim","Equal","S",1,"C",0))$Observed.t.p.val ),main=paste0("t-test P-Values SDR = 1 ",i," Dist") )

  plot( density( get(paste0(i,"Sim","Equal","S",1.25,"C",0))$Observed.W.p.val ),main=paste0("Wilcoxon P-Values SDR = 1.25 ",i," Dist") )
  plot( density( get(paste0(i,"Sim","Equal","S",1.25,"C",0))$Observed.t.p.val ),main=paste0("t-test P-Values SDR = 1.25 ",i," Dist") )
  
  plot( density( get(paste0(i,"Sim","Equal","S",1.5,"C",0))$Observed.W.p.val ),main=paste0("Wilcoxon P-Values SDR = 1.5 ",i," Dist") )
  plot( density( get(paste0(i,"Sim","Equal","S",1.5,"C",0))$Observed.t.p.val ),main=paste0("t-test P-Values SDR = 1.5 ",i," Dist") )
  
  plot( density( get(paste0(i,"Sim","Equals","S",2,"C",0))$Observed.W.p.val ),main=paste0("Wilcoxon P-Values SDR = 2 ",i," Dist") )
  plot( density( get(paste0(i,"Sim","Equals","S",2,"C",0))$Observed.t.p.val ),main=paste0("t-test P-Values SDR = 2 ",i," Dist") )
}


powerAB1 <- data.frame(matrix(nrow=0,ncol=4))
names(powerAB1) <- c("Distribution and SDR","Sample Size 1:1","Sample Size 1:2","Sample Size 2:1")
for(i in distUsed) {
  for(j in c("1","1.25","1.5")) {
    v1 <- get(paste0(i,"Sim","Equal","S",j,"C",1))$Observed.AB.p.val
    q1 <- sum(v1 <= alpha)/30000
    v2 <- get(paste0(i,"Sim","N1","S",j,"C",1))$Observed.AB.p.val
    q2 <- sum(v2 <= alpha)/30000
    v3 <- get(paste0(i,"Sim","N2","S",j,"C",1))$Observed.AB.p.val
    q3 <- sum(v3 <= alpha)/30000
    powerAB1[nrow(powerAB1)+1,] <- list(paste0(i, " SDR=",j),q1,q2,q3)
  }
  v1 <- get(paste0(i,"Sim","Equals","S",2,"C",1))$Observed.AB.p.val
  q1 <- sum(v1 <= alpha)/30000
  v2 <- get(paste0(i,"Sim","N1","S",2,"C",1))$Observed.W.AB.val
  q2 <- sum(v2 <= alpha)/30000
  v3 <- get(paste0(i,"Sim","N2","S",2,"C",1))$Observed.W.AB.val
  q3 <- sum(v3 <= alpha)/30000
  powerAB1[nrow(powerAB1)+1,] <- list(paste0(i, " SDR=",2),q1,q2,q3)
}
powerAB0.5 <- data.frame(matrix(nrow=0,ncol=4))
names(powerAB0.5) <- c("Distribution and SDR","Sample Size 1:1","Sample Size 1:2","Sample Size 2:1")
for(i in distUsed) {
  for(j in c("1","1.25","1.5")) {
    v1 <- get(paste0(i,"Sim","Equal","S",j,"C",0.5))$Observed.AB.p.val
    q1 <- sum(v1 <= alpha)/30000
    v2 <- get(paste0(i,"Sim","N1","S",j,"C",0.5))$Observed.AB.p.val
    q2 <- sum(v2 <= alpha)/30000
    v3 <- get(paste0(i,"Sim","N2","S",j,"C",0.5))$Observed.AB.p.val
    q3 <- sum(v3 <= alpha)/30000
    powerAB1[nrow(powerAB1)+1,] <- list(paste0(i, " SDR=",j),q1,q2,q3)
  }
  v1 <- get(paste0(i,"Sim","Equals","S",2,"C",0.5))$Observed.AB.p.val
  q1 <- sum(v1 <= alpha)/30000
  v2 <- get(paste0(i,"Sim","N1","S",2,"C",0.5))$Observed.W.AB.val
  q2 <- sum(v2 <= alpha)/30000
  v3 <- get(paste0(i,"Sim","N2","S",2,"C",0.5))$Observed.W.AB.val
  q3 <- sum(v3 <= alpha)/30000
  powerAB1[nrow(powerAB1)+1,] <- list(paste0(i, " SDR=",2),q1,q2,q3)
}
powerComparison1 <- data.frame(matrix(nrow=24,ncol=4) )
names(powerComparison1) <- c("Distribution and SDR","W Power","W & AB Power","Lepage Power")
for(i in 1:24) {
  powerComparison1[i,1] <- powerW1[i,1]
  powerComparison1[i,2] <- powerW1[i,2]
  powerComparison1[i,3] <- powerWAB1[i,2]
  powerComparison1[i,4] <- powerLP1[i,2]
}
powerComparison0.5 <- data.frame(matrix(nrow=24,ncol=4) )
names(powerComparison0.5) <- c("Distribution and SDR","W Power","W & AB Power","Lepage Power")
for(i in 1:24) {
  powerComparison0.5[i,1] <- powerW0.5[i,1]
  powerComparison0.5[i,2] <- powerW0.5[i,2]
  powerComparison0.5[i,3] <- powerWAB0.5[i,2]
  powerComparison0.5[i,4] <- powerLP0.5[i,2]
}
  
  
}