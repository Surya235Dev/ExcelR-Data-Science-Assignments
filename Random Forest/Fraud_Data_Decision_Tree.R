#install.packages("randomForest")
library(randomForest)
normalize_dummy <- function(x){
  col <- ncol(x)
  row <- nrow(x)
  y <- 1:nrow(x)
  for (i in 1:col){
    if(class(x[,i])=="numeric" | class(x[,i])=="integer")
    {
      minx <- min(x[,i])
      maxx <- max(x[,i])
      for(j in 1:row)
      {
        x[j,i] <- ifelse((x[j,i] - minx) != 0,yes =((x[j,i] - minx) / (maxx - minx)),no = 0)
      }
    }
    
  }
  f <- c()
  for(i in 1:ncol(x)){
    if(class(x[,i])=="factor"){
      dummies <- data.frame(dummies::dummy(x[,i]))
      y <- data.frame(y,dummies)
      f <- c(f,i)
    }
    else{
      next
    }
  }
  if(is.null(f)){
    output <- x
  }
  else{output <- data.frame(x[,-f],y[,-1])}
  return(output)
}


df <- read.csv(file.choose())
head(df)
# Encoding Risky and Good
GR <- ifelse(df$Taxable.Income<30000,"Risky","Good")
Fraud <- data.frame(df[,-3],GR) # Removed the column Taxable income .
# Here we will remove the column Taxable income from our data frame, as If we consider the column 

#Visualize
boxplot(Fraud) #no sign of outliers in the data
barplot(table(Fraud$GR))
str(Fraud)
colSums(is.na(Fraud)) # No outliers in  data

# Train Test Splitting
set.seed(101);splitF <- sample(nrow(Fraud),nrow(Fraud)*.7,F)
Train_F <- Fraud[splitF,]
Test_F <- Fraud[-splitF,]

set.seed(101);ModelF1 <- randomForest(Train_F$GR~.,data = Train_F[,-6])
predF1 <- predict(ModelF1,Test_F[,-6])
mean(Test_F$GR==predF1) # accuracy is 80
table(Actual=Test_F$GR,Predicted=predF1)
plot(ModelF1)


set.seed(101);ModelF2 <- randomForest(Train_F$GR~.,ntree = 400,mtry = 5,data=Train_F)
predF2 <- predict(ModelF2,Test_F[,-6])
mean(Test_F$GR==predF2) # accuracy is  70
plot(ModelF2)

# Tuning of random forest for better outcome
set.seed(101);
tune <- tuneRF(Train_F[,-6], Train_F[,6], stepFactor = 0.5, plot = TRUE, ntreeTry = 300,
                             trace = TRUE, improve = 0.5)


set.seed(101);
ModelF3 <- randomForest(Train_F$GR~.,ntree = 400,mtry = 1,data=Train_F)
predF3 <- predict(ModelF3,Test_F[,-6])
mean(Test_F$GR==predF3) #0.8166667
table(Actual = Test_F$GR,Predicted=predF3)
plot(ModelF3)

#Finally We got a model with 81% accuracy