ComputerData <- read.csv('Computer_Data.csv')
View(ComputerData)
summary(ComputerData)
dim(ComputerData) 

# Variance
var(ComputerData$X)
var(ComputerData$price)
var(ComputerData$speed)
var(ComputerData$hd)
var(ComputerData$ram)
var(ComputerData$screen)
var(ComputerData$ads)
var(ComputerData$trend)

# Standard Deviation
sd(ComputerData$X)
sd(ComputerData$price)
sd(ComputerData$speed)
sd(ComputerData$hd)
sd(ComputerData$ram)
sd(ComputerData$screen)
sd(ComputerData$ads)
sd(ComputerData$trend)

ComputerData$cd1 <- ifelse(ComputerData$cd=="yes",1,0)
ComputerData$multi1 <- ifelse(ComputerData$multi=='yes',1,0)
ComputerData$premium1 <- ifelse(ComputerData$premium=='yes',1,0)
View(ComputerData)

library(data.table)
pairs(ComputerData)
Computermodel <- lm(price ~ speed+hd+ram+screen+ads+trend+cd1+multi1+premium1, data = ComputerData)
summary(Computermodel)
influenceIndexPlot(Computermodel, id.n=5)
vif(Computermodel)
avPlots(Computermodel)
library(MASS)
stepAIC(Computermodel)
Finalmodel <- lm(price ~ speed+hd+ram+screen+ads+trend+cd1+multi1+premium1, data = ComputerData)
summary(Finalmodel)

# As all the p-values are significant, R-squared is also significant 
# therefore its a better model to predict the prices of computer 
# based on ram, storage size,etc..
