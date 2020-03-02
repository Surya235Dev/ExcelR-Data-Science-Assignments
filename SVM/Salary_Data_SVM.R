library(kernlab)
library(caret)



#Train Data
train_salary <- read.csv(file.choose())

#Test Data
test_salary <- read.csv(file.choose())

head(train_salary)

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


salarytrain_norm <- normalize_dummy(train_salary)

modelS1 <- ksvm(Salary~.,data=train_salary)
predS1 <- predict(modelS1,test_salary)
table(test_salary$Salary,predS1)
mean(test_salary$Salary==predS1) # 0.8541


SVMinfo2 <- list()
SVMtable2 <- list()
SVMAccu2 <- list()
for(i in methodss){
  modelS <- ksvm(Salary~.,data=train_salary,kernel = i)
  predS <- predict(modelS,test_salary)
  SVMinfo2[[i]] <- predS
  SVMtable2[[i]] <- table(test_salary$Salary,predS)
  SVMAccu2[[i]] <- mean(test_salary$Salary==predS)
  
}

SVMAccu2 # Acurracy of  0.8562815
SVMinfo2
SVMtable2

SVMinfo2$vanilladot
SVMinfo2$vanilladot
SVMinfo2$vanilladot

SVMtable2$vanilladot
SVMtable2$rbfdot
SVMtable2$laplacedot

MLmetrics::F1_Score(y_pred = SVMinfo2$rbfdot,y_true = test_salary$Salary)
MLmetrics::F1_Score(y_pred = SVMinfo2$vanilladot,y_true = test_salary$Salary)
MLmetrics::F1_Score(y_pred = SVMinfo2$laplacedot,y_true = test_salary$Salary)
