#install.packages(c("rpart","rpart.plot","party","tree"))
library("party")
library (tree)
library(rpart)
library(rpart.plot)
library(C50)

# Required Data ----
data("iris")
head(iris) #Here our target variable is Species

# Feature Selection
set.seed(1);split <- sample(nrow(iris),nrow(iris)*0.7,F)
df_iTrain <- iris[split,]
df_iTest <- iris[-split,]


#  rpart and rpart.plot
Treei3 <- rpart(formula = df_iTrain$Species ~.,data = df_iTrain[,-5])
predi3_ <- predict(Treei3,df_iTest);
predi3 = ifelse(predi3_[,2] > 0.5,"versicolor",ifelse(predi3_[,3]>0.5,"virginica","setosa"))


table(Predicted=predi3,Actual=df_iTest$Species)
mean(predi3==df_iTest$Species) # Accuracy is 91%
rpart.plot(Treei3,lwd=1,type = 4)



