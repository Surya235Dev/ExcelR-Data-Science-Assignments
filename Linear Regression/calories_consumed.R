Calories_consumed <- read.csv(file.choose())
View(Calories_consumed)
summary(Calories_consumed)
var(Calories_consumed$Weight.gained..grams.)
sd(Calories_consumed$Weight.gained..grams.)
var(Calories_consumed$Calories.Consumed)
sd(Calories_consumed$Calories.Consumed)

library(lattice)
dotplot(Calories_consumed$Calories.Consumed, main="Dot Plot of Calories Consumed")
dotplot(Calories_consumed$Weight.gained..grams., main="Dot Plot of Weight gained in grams")
boxplot(Calories_consumed$Calories.Consumed,col="blueviolet")
boxplot(Calories_consumed$Weight.gained..grams.,col="mediumorchid3", horizontal = T)
# From the boxplot of calories consumed and weight gained .
# We could say that they are no outliers and their are positive skew.


cor(Calories_consumed$Weight,Calories_consumed$Calories.Consumed)
plot(Calories_consumed$Weight,Calories_consumed$Calories.Consumed,main="Calories Consumed v/s Weight Gained", col="violetred", 
     col.main="violetred", col.lab="violetred", xlab="Weight.gained..grams", 
     ylab="Calories.Consumed", pch=20)
WeightGainModel <- lm(Weight.gained..grams. ~ Calories.Consumed, data = Calories_consumed)
summary(WeightGainModel)

# Inference: 
# 1) Since p-value is less than < 0.05 .We could say that this model is significant.
# 2) The R value of this model is 0.947. Hence, its is a positive strong correlation.
# 3) The value of R2 is 0.8968 
# 4) This is a strong positive correlation, which means that high X variable(weight gain) scores go with high (calories score) variable scores