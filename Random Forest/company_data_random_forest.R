#install.packages("randomForest")
library(randomForest)


#Normalize function 
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
company <- read.csv(file.choose())
head(company)
nrow(company)
str(company)

boxplot(company)$out # 9 variable has  outliers
boxplot(scale(company[,c(1:6,8,9)])) # Sales, CompPrice and Price contains outlier.

# To treat outliers in dataset
boxplot(company)$out
outlie1 <- which(company$Sales >= min(boxplot(company$Sales)$out))#317,377
outlie2 <- which(company$CompPrice == max(boxplot(company$CompPrice)$out)
                 |company$CompPrice == min(boxplot(company$CompPrice)$out) )#43,311
outlie3 <- which(company$Price <= 53 | company$Price >= 185)
outlier <- c(outlie1,outlie2,outlie3)
company_o <- company[-outlier,]
boxplot(company_o)$out
boxplot(company_o$Price)$out
company_final <- company_o[company_o$Price >55,]
nrow(company_final)

SaleC <- ifelse(company_final$Sales > 8.5,"High","Low")
df_c <- data.frame(company_final[,-1],SaleC)

# Train Test Split of Data.
set.seed(101);splitC <- sample(nrow(company_final),nrow(company_final)*.7,F)
trainC <- df_c[splitC,]
testC <- df_c[-splitC,]

trainC_n <- normalize_dummy(trainC[,-11])
cn <- c("CompPrice","Income","Advertising","Population","Price","Age","Education","CBad","CGood","CMedium","CNo","CYes","CNo.1","CYes.1")
colnames(trainC_n) <- cn
testC_n <- normalize_dummy(testC[,-11])
colnames(testC_n) <- cn


# Model Building Model no 1
set.seed(101);modelC1 <- randomForest(trainC$SaleC~.,data = trainC[,-11])
summary(modelC1)
predC1 <- predict(modelC1,testC) 
table(Actual=testC$SaleC,Predicted=predC1)
mean(testC$SaleC==predC1) # 82.9% accuracy
plot(modelC1)
varImpPlot(modelC1, pch = 20, main = "Variables Importance")

# Model 2 Building with normalised values
set.seed(101);modelC2 <- randomForest(trainC$SaleC~.,data = trainC_n)
summary(modelC2)
predC2 <- predict(object = modelC2,newdata = testC_n) # If throughs Error Re run the whole code

table(Actual=testC$SaleC,Predicted=predC2)
mean(testC$SaleC==predC2) # 79.487% accuracy
plot(modelC2)


