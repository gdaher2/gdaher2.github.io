library(NSM3)
library(ExtDist)
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

#
# Exponential Distribution Simulation
#
expSimEqualS1C0 <- simulateExponential(30000,1,0,30,30)
expSimN1S1C0 <- simulateExponential(30000,1,0,20,40)
expSimN2S1C0 <- simulateExponential(30000,1,0,40,20)

expSimEqualS1.25C0 <- simulateExponential(30000,1.25,0,30,30)
expSimN1S1.25C0 <- simulateExponential(30000,1.25,0,20,40)
expSimN2S1.25C0 <- simulateExponential(30000,1.25,0,40,20)

expSimEqualS1.5C0 <- simulateExponential(30000,1.5,0,30,30)
expSimN1S1.5C0 <- simulateExponential(30000,1.5,0,20,40)
expSimN2S1.5C0 <- simulateExponential(30000,1.5,0,40,20)

expSimEqualS2C0 <- simulateExponential(30000,2,0,30,30)
expSimN1S2C0 <- simulateExponential(30000,2,0,20,40)
expSimN2S2C0 <- simulateExponential(30000,2,0,40,20)

# Vary each level of c
# Center Shift is 0.5 base SD
expSimEqualS1C0.5 <- simulateExponential(30000,1,0.5,30,30)
expSimN1S1C0.5 <- simulateExponential(30000,1,0.5,20,40)
expSimN2S1C0.5 <- simulateExponential(30000,1,0.5,40,20)

expSimEqualS1.25C0.5 <- simulateExponential(30000,1.25,0.5,30,30)
expSimN1S1.25C0.5 <- simulateExponential(30000,1.25,0.5,20,40)
expSimN2S1.25C0.5 <- simulateExponential(30000,1.25,0.5,40,20)

expSimEqualS1.5C0.5 <- simulateExponential(30000,1.5,0.5,30,30)
expSimN1S1.5C0.5 <- simulateExponential(30000,1.5,0.5,20,40)
expSimN2S1.5C0.5 <- simulateExponential(30000,1.5,0.5,40,20)

expSimEqualS2C0.5 <- simulateExponential(30000,2,0.5,30,30)
expSimN1S2C0.5 <- simulateExponential(30000,2,0.5,20,40)
expSimN2S2C0.5 <- simulateExponential(30000,2,0.5,40,20)

# Center Shift is 1 base SD
expSimEqualS1C1 <- simulateExponential(30000,1,1,30,30)
expSimN1S1C1 <- simulateExponential(30000,1,1,20,40)
expSimN2S1C1 <- simulateExponential(30000,1,1,40,20)

expSimEqualS1.25C1 <- simulateExponential(30000,1.25,1,30,30)
expSimN1S1.25C1 <- simulateExponential(30000,1.25,1,20,40)
expSimN2S1.25C1 <- simulateExponential(30000,1.25,1,40,20)

expSimEqualS1.5C1 <- simulateExponential(30000,1.5,1,30,30)
expSimN1S1.5C1 <- simulateExponential(30000,1.5,1,20,40)
expSimN2S1.5C1 <- simulateExponential(30000,1.5,1,40,20)

expSimEqualS2C1 <- simulateExponential(30000,2,1,30,30)
expSimN1S2C1 <- simulateExponential(30000,2,1,20,40)
expSimN2S2C1 <- simulateExponential(30000,2,1,40,20)

#
# Lognormal Distribution Simulation
#
lnormSimEqualS1C0 <- simulateLogNormal(30000,1,0,30,30)
lnormSimN1S1C0 <- simulateLogNormal(30000,1,0,20,40)
lnormSimN2S1C0 <- simulateLogNormal(30000,1,0,40,20)

lnormSimEqualS1.25C0 <- simulateLogNormal(30000,1.25,0,30,30)
lnormSimN1S1.25C0 <- simulateLogNormal(30000,1.25,0,20,40)
lnormSimN2S1.25C0 <- simulateLogNormal(30000,1.25,0,40,20)

lnormSimEqualS1.5C0 <- simulateLogNormal(30000,1.5,0,30,30)
lnormSimN1S1.5C0 <- simulateLogNormal(30000,1.5,0,20,40)
lnormSimN2S1.5C0 <- simulateLogNormal(30000,1.5,0,40,20)

lnormSimEqualS2C0 <- simulateLogNormal(30000,2,0,30,30)
lnormSimN1S2C0 <- simulateLogNormal(30000,2,0,20,40)
lnormSimN2S2C0 <- simulateLogNormal(30000,2,0,40,20)

# Vary each level of c
# Center Shift is 0.5 base SD
lnormSimEqualS1C0.5 <- simulateLogNormal(30000,1,0.5,30,30)
lnormSimN1S1C0.5 <- simulateLogNormal(30000,1,0.5,20,40)
lnormSimN2S1C0.5 <- simulateLogNormal(30000,1,0.5,40,20)

lnormSimEqualS1.25C0.5 <- simulateLogNormal(30000,1.25,0.5,30,30)
lnormSimN1S1.25C0.5 <- simulateLogNormal(30000,1.25,0.5,20,40)
lnormSimN2S1.25C0.5 <- simulateLogNormal(30000,1.25,0.5,40,20)

lnormSimEqualS1.5C0.5 <- simulateLogNormal(30000,1.5,0.5,30,30)
lnormSimN1S1.5C0.5 <- simulateLogNormal(30000,1.5,0.5,20,40)
lnormSimN2S1.5C0.5 <- simulateLogNormal(30000,1.5,0.5,40,20)

lnormSimEqualS2C0.5 <- simulateLogNormal(30000,2,0.5,30,30)
lnormSimN1S2C0.5 <- simulateLogNormal(30000,2,0.5,20,40)
lnormSimN2S2C0.5 <- simulateLogNormal(30000,2,0.5,40,20)

# Center Shift is 1 base SD
lnormSimEqualS1C1 <- simulateLogNormal(30000,1,1,30,30)
lnormSimN1S1C1 <- simulateLogNormal(30000,1,1,20,40)
lnormSimN2S1C1 <- simulateLogNormal(30000,1,1,40,20)

lnormSimEqualS1.25C1 <- simulateLogNormal(30000,1.25,1,30,30)
lnormSimN1S1.25C1 <- simulateLogNormal(30000,1.25,1,20,40)
lnormSimN2S1.25C1 <- simulateLogNormal(30000,1.25,1,40,20)

lnormSimEqualS1.5C1 <- simulateLogNormal(30000,1.5,1,30,30)
lnormSimN1S1.5C1 <- simulateLogNormal(30000,1.5,1,20,40)
lnormSimN2S1.5C1 <- simulateLogNormal(30000,1.5,1,40,20)

lnormSimEqualS2C1 <- simulateLogNormal(30000,2,1,30,30)
lnormSimN1S2C1 <- simulateLogNormal(30000,2,1,20,40)
lnormSimN2S2C1 <- simulateLogNormal(30000,2,1,40,20)

# 
# Chisquare Distribution Simulation
#
chisqSimEqualS1C0 <- simulateChiSquare(30000,1,0,30,30)
chisqSimN1S1C0 <- simulateChiSquare(30000,1,0,20,40)
chisqSimN2S1C0 <- simulateChiSquare(30000,1,0,40,20)

chisqSimEqualS1.25C0 <- simulateChiSquare(30000,1.25,0,30,30)
chisqSimN1S1.25C0 <- simulateChiSquare(30000,1.25,0,20,40)
chisqSimN2S1.25C0 <- simulateChiSquare(30000,1.25,0,40,20)

chisqSimEqualS1.5C0 <- simulateChiSquare(30000,1.5,0,30,30)
chisqSimN1S1.5C0 <- simulateChiSquare(30000,1.5,0,20,40)
chisqSimN2S1.5C0 <- simulateChiSquare(30000,1.5,0,40,20)

chisqSimEqualS2C0 <- simulateChiSquare(30000,2,0,30,30)
chisqSimN1S2C0 <- simulateChiSquare(30000,2,0,20,40)
chisqSimN2S2C0 <- simulateChiSquare(30000,2,0,40,20)

# Vary each level of c
# Center Shift is 0.5 base SD
chisqSimEqualS1C0.5 <- simulateChiSquare(30000,1,0.5,30,30)
chisqSimN1S1C0.5 <- simulateChiSquare(30000,1,0.5,20,40)
chisqSimN2S1C0.5 <- simulateChiSquare(30000,1,0.5,40,20)

chisqSimEqualS1.25C0.5 <- simulateChiSquare(30000,1.25,0.5,30,30)
chisqSimN1S1.25C0.5 <- simulateChiSquare(30000,1.25,0.5,20,40)
chisqSimN2S1.25C0.5 <- simulateChiSquare(30000,1.25,0.5,40,20)

chisqSimEqualS1.5C0.5 <- simulateChiSquare(30000,1.5,0.5,30,30)
chisqSimN1S1.5C0.5 <- simulateChiSquare(30000,1.5,0.5,20,40)
chisqSimN2S1.5C0.5 <- simulateChiSquare(30000,1.5,0.5,40,20)

chisqSimEqualS2C0.5 <- simulateChiSquare(30000,2,0.5,30,30)
chisqSimN1S2C0.5 <- simulateChiSquare(30000,2,0.5,20,40)
chisqSimN2S2C0.5 <- simulateChiSquare(30000,2,0.5,40,20)

# Center Shift is 1 base SD
chisqSimEqualS1C1 <- simulateChiSquare(30000,1,1,30,30)
chisqSimN1S1C1 <- simulateChiSquare(30000,1,1,20,40)
chisqSimN2S1C1 <- simulateChiSquare(30000,1,1,40,20)

chisqSimEqualS1.25C1 <- simulateChiSquare(30000,1.25,1,30,30)
chisqSimN1S1.25C1 <- simulateChiSquare(30000,1.25,1,20,40)
chisqSimN2S1.25C1 <- simulateChiSquare(30000,1.25,1,40,20)

chisqSimEqualS1.5C1 <- simulateChiSquare(30000,1.5,1,30,30)
chisqSimN1S1.5C1 <- simulateChiSquare(30000,1.5,1,20,40)
chisqSimN2S1.5C1 <- simulateChiSquare(30000,1.5,1,40,20)

chisqSimEqualS2C1 <- simulateChiSquare(30000,2,1,30,30)
chisqSimN1S2C1 <- simulateChiSquare(30000,2,1,20,40)
chisqSimN2S2C1 <- simulateChiSquare(30000,2,1,40,20)