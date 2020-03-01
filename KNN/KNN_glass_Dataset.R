install.packages('caTools')  #for train and test data split
install.packages('dplyr')    #for Data Manipulation
install.packages('ggplot2')  #for Data Visualization
install.packages('class')    #KNN 
install.packages('caret')    #Confusion Matrix
install.packages('corrplot') #Correlation Plot

library(caTools)
library(dplyr)
library(ggplot2)
library(caret)
library(class)
library(corrplot)
glass <-  read.csv(file.choose())

standard.features <- scale(glass[,1:9])

#Join the standardized data with the target column
data <- cbind(standard.features,glass[10])

head(data)

# Visulize the dataset
corrplot(cor(data))

# split dataset into train and test
sample <- sample.split(data$Type,SplitRatio = 0.70)


# Implementing KNN model
predicted.type <- knn(train[1:9],test[1:9],train$Type,k=1)
#Error in prediction
error <- mean(predicted.type!=test$Type)

#Confusion Matrix for train data
confusionMatrix(factor(predicted.type),factor(test$Type))


train <- subset(data,sample==TRUE)

test <- subset(data,sample==FALSE)


#Implement KNN for test data
predicted.type <- knn(train[1:9],test[1:9],train$Type,k=3)
#Error in prediction
error <- mean(predicted.type!=test$Type)
#Confusion Matrix for train data
confusionMatrix(factor(predicted.type),factor(test$Type))

# The overall Accuarcy for predicting is 0.7231 or 72%
