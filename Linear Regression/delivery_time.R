Delivery_time <- read.csv(file.choose())
View(Delivery_time)
summary(Delivery_time)
var(Delivery_time$Delivery.Time)
sd(Delivery_time$Delivery.Time)
var(Delivery_time$Sorting.Time)
sd(Delivery_time$Sorting.Time)

dotplot(Delivery_time$Delivery.Time, main="Dot Plot of Delivery Time")
dotplot(Delivery_time$Sorting.Time, main="Dot Plot of Sorting time")
boxplot(Delivery_time$Delivery.Time,col="blueviolet")
boxplot(Delivery_time$Sorting.Time,col="mediumorchid3", horizontal = F)
# from the boxplot the data is normally distributed and there are no outliers


cor(Delivery_time$Delivery.Time,Delivery_time$Sorting.Time)
plot(Delivery_time$Delivery.Time,Delivery_time$Sorting.Time,main="Calories Consumed v/s Weight Gained", col="violetred", 
     col.main="violetred", col.lab="violetred", xlab="Delivery time", 
     ylab="Sorting time", pch=20)
DeliveryTimeModel <- lm(Delivery.Time ~ Sorting.Time, data = Delivery_time)
summary(DeliveryTimeModel)

#Even though P-value is lesser than 0.05.
#The R^2 value is lower.Which states model us not good.

# To rise the value of  R^2, we need to find the influencing data points in data set.

library(mvinfluence)
infIndexPlot(DeliveryTimeModel)
DeliveryTimeModel <- lm(Delivery.Time ~ Sorting.Time, data = Delivery_time[c(-5,-9,-21),])
summary(DeliveryTimeModel)
plot(DeliveryTimeModel)

#Inference:
#1) The p-value is lesser than 0.05 which says model is significant
#2) The R^2 value has been increased 
#3) Since both p-value and R^2 value are good.This model predicts 0.826 or 83% output.
