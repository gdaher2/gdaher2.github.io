#Testing Simulation of Each type
test1 <- simulateChiSquare(100,1,0,30,30)
test2 <- simulateDistributionAtRandom(100,1,0,30,30)
test3 <- simulateExponential(100,1,0,30,30)
test4 <- simulateLaplace(100,1,0,30,30)
test5 <- simulateLogNormal(100,1,0,30,30)
test6 <- simulateNormal(100,1,0,30,30)
test7 <- simulateUniform(100,1,0,30,30)
summary(test1$SDR)

# Testing Enforcmenet of SDR and center shift
x <- rexp(100000,1)
sd(x)
mean(x)
median(x)
print("GAP")
y <- (x-median(x))*SDR+median(x)+cShift
sd(y)
mean(y)
median(y)
sd(y)/sd(x)
median(y)-median(x)
A_x <- median(y)

x <- rlnorm(10000,0,1)
y <- (x-median(x))*(1/sd(x))
par(mfrow=c(2,2))
hist(x)
plot(density(x))
hist(y)
plot(density(y))

y2 <- (x-min(x))*(1/sd(x))
y2 <- y2-median(y2)+median(x)
par(mfrow=c(2,2))
hist(x)
plot(density(x))
hist(y)
plot(density(y))

#Unif Write
write.csv(unifSimEqualS1C0,"E:/OneDrive/Documents/College/STAT 219/FinalProject/SimsDone/unifSimEqualS1C0.csv")
write.csv(unifSimN1S1C0,"E:/OneDrive/Documents/College/STAT 219/FinalProject/SimsDone/unifSimN1S1C0.csv")
write.csv(unifSimN2S1C0,"E:/OneDrive/Documents/College/STAT 219/FinalProject/SimsDone/unifSimN2S1C0.csv")

write.csv(unifSimEqualS1.25C0,"E:/OneDrive/Documents/College/STAT 219/FinalProject/SimsDone/unifSimEqualS1.25C0.csv")
write.csv(unifSimN1S1.5C0,"E:/OneDrive/Documents/College/STAT 219/FinalProject/SimsDone/unifSimN1S1.25C0.csv")
write.csv(unifSimN2S1.25C0,"E:/OneDrive/Documents/College/STAT 219/FinalProject/SimsDone/unifSimN2S1.25C0.csv")

write.csv(unifSimEqualS1.5C0,"E:/OneDrive/Documents/College/STAT 219/FinalProject/SimsDone/unifSimEqualS1.5C0.csv")
write.csv(unifSimN1S1.5C0,"E:/OneDrive/Documents/College/STAT 219/FinalProject/SimsDone/unifSimN1S1.5C0.csv")
write.csv(unifSimN2S1.5C0,"E:/OneDrive/Documents/College/STAT 219/FinalProject/SimsDone/unifSimN2S1.5C0.csv")

write.csv(unifSimEqualS2C0,"E:/OneDrive/Documents/College/STAT 219/FinalProject/SimsDone/unifSimEqualsS2C0.csv")
write.csv(unifSimN1S2C0,"E:/OneDrive/Documents/College/STAT 219/FinalProject/SimsDone/unifSimN1S2C0.csv")
write.csv(unifSimN2S2C0,"E:/OneDrive/Documents/College/STAT 219/FinalProject/SimsDone/unifSimN2S2C0.csv")

# Norm Write
write.csv(normSimEqualS1C0,"E:/OneDrive/Documents/College/STAT 219/FinalProject/SimsDone/normSimEqualS1C0.csv")
write.csv(normSimN1S1C0,"E:/OneDrive/Documents/College/STAT 219/FinalProject/SimsDone/normSimN1S1C0.csv")
write.csv(normSimN2S1C0,"E:/OneDrive/Documents/College/STAT 219/FinalProject/SimsDone/normSimN2S1C0.csv")

write.csv(normSimEqualS1.25C0,"E:/OneDrive/Documents/College/STAT 219/FinalProject/SimsDone/normSimEqualS1.25C0.csv")
write.csv(normSimN1S1.5C0,"E:/OneDrive/Documents/College/STAT 219/FinalProject/SimsDone/normSimN1S1.25C0.csv")
write.csv(normSimN2S1.25C0,"E:/OneDrive/Documents/College/STAT 219/FinalProject/SimsDone/normSimN2S1.25C0.csv")

write.csv(normSimEqualS1.5C0,"E:/OneDrive/Documents/College/STAT 219/FinalProject/SimsDone/normSimEqualS1.5C0.csv")
write.csv(normSimN1S1.5C0,"E:/OneDrive/Documents/College/STAT 219/FinalProject/SimsDone/normSimN1S1.5C0.csv")
write.csv(normSimN2S1.5C0,"E:/OneDrive/Documents/College/STAT 219/FinalProject/SimsDone/normSimN2S1.5C0.csv")

write.csv(normSimEqualS2C0,"E:/OneDrive/Documents/College/STAT 219/FinalProject/SimsDone/normSimEqualsS2C0.csv")
write.csv(normSimN1S2C0,"E:/OneDrive/Documents/College/STAT 219/FinalProject/SimsDone/normSimN1S2C0.csv")
write.csv(normSimN2S2C0,"E:/OneDrive/Documents/College/STAT 219/FinalProject/SimsDone/normSimN2S2C0.csv")

#LP Write
write.csv(lpSimEqualS1C0,"E:/OneDrive/Documents/College/STAT 219/FinalProject/SimsDone/lpSimEqualS1C0.csv")
write.csv(lpSimN1S1C0,"E:/OneDrive/Documents/College/STAT 219/FinalProject/SimsDone/lpSimN1S1C0.csv")
write.csv(lpSimN2S1C0,"E:/OneDrive/Documents/College/STAT 219/FinalProject/SimsDone/lpSimN2S1C0.csv")

write.csv(lpSimEqualS1.25C0,"E:/OneDrive/Documents/College/STAT 219/FinalProject/SimsDone/lpSimEqualS1.25C0.csv")
write.csv(lpSimN1S1.5C0,"E:/OneDrive/Documents/College/STAT 219/FinalProject/SimsDone/lpSimN1S1.25C0.csv")
write.csv(lpSimN2S1.25C0,"E:/OneDrive/Documents/College/STAT 219/FinalProject/SimsDone/lpSimN2S1.25C0.csv")

write.csv(lpSimEqualS1.5C0,"E:/OneDrive/Documents/College/STAT 219/FinalProject/SimsDone/lpSimEqualS1.5C0.csv")
write.csv(lpSimN1S1.5C0,"E:/OneDrive/Documents/College/STAT 219/FinalProject/SimsDone/lpSimN1S1.5C0.csv")
write.csv(lpSimN2S1.5C0,"E:/OneDrive/Documents/College/STAT 219/FinalProject/SimsDone/lpSimN2S1.5C0.csv")

write.csv(lpSimEqualS2C0,"E:/OneDrive/Documents/College/STAT 219/FinalProject/SimsDone/lpSimEqualsS2C0.csv")
write.csv(lpSimN1S2C0,"E:/OneDrive/Documents/College/STAT 219/FinalProject/SimsDone/lpSimN1S2C0.csv")
write.csv(lpSimN2S2C0,"E:/OneDrive/Documents/College/STAT 219/FinalProject/SimsDone/lpSimN2S2C0.csv")

plotAlphaDist(unifSimEqualS1.25C0)
plotAlphaDist(unifSimEqualS1.25C1)
plotAlphaDist(unifSimEqualS1.25C0.5)

setwd("E:/OneDrive/Documents/College/STAT 219/FinalProject/SimsDone")
fnames <- list.files()
csv <- lapply(fnames, read.csv)
result <- do.call(rbind, csv)

read_plus <- function(flnm) {
  read_csv(flnm) %>%
    mutate(filename = flnm)
}

tbl_with_sources <-
  list.files(pattern = "\\.csv$",
             full.names = T) %>%
  map_df(~read_plus(.))

library(stringr)
### READS ALL CSVS
setwd("E:/OneDrive/Documents/College/STAT 219/FinalProject/SimsDone")
##Reads all Simulations from Files
filenames <- list.files()

##Create list of data frame names without the ".csv" part 
names <-str_replace_all(filenames,".csv","")

###Load all files
for(i in names){
  filepath <- file.path("E:/OneDrive/Documents/College/STAT 219/FinalProject/SimsDone/",paste(i,".csv",sep=""))
  assign(i, read.csv(filepath))
}