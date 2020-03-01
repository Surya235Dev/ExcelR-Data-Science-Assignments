#Normilze function
#actual solution is below this function

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
denormalize <- function(x,max,min){
  denormalize <- ((x*(max-min)) + min)
  return(denormalize)
}

library(nnet);
library(neuralnet)
#install.packages(c("neuralnet","nnet"))


statups <- read.csv(file.choose())
head(statups)
str(statups)
# Target is Profit
plot(statups[,-4])
corrplot::corrplot(cor(statups[,-4]))
colSums(is.na(statups)) # No NA values are present in our data

df_statups <- data.frame(normalize_dummy(statups))#,profit=statups$Profit)
colnames(df_statups)[5:7] <- c("California","Florida","NewYork")
head(df_statups)
boxplot(statups)
# Train Test Splitting
set.seed(101)
Split_s <- sample(x = 1:nrow(df_statups),size = round(nrow(df_statups)*0.3),replace = F)
Train_St <- df_statups[-Split_s,];dim(Train_St)
Test_St <- df_statups[Split_s,];dim(Test_St)

# Model Building 
head(df_statups)
set.seed(4) #4
model_St1 <- neuralnet(Profit~.,data=Train_St)
plot(model_St1,rep = "best")
pred_St1 <- compute(model_St1,Test_St)
cor(pred_St1$net.result,Test_St$Profit) # 0.9585

pred_St_n1 <- denormalize(pred_St1$net.result,max(statups$Profit[Split_s]),min(statups$Profit[Split_s]))
cor(statups$Profit[Split_s],pred_St_n1) # 0.9585436
RMSE_S1 <- sqrt(mean((statups$Profit[Split_s]-pred_St_n1)^2)) # 14478.8
plot(statups$Profit[Split_s],pred_St_n1) 


model_St2 <- neuralnet(Profit~.,data=Train_St,hidden = 5)
plot(model_St2,rep = "best")
pred_St2 <- compute(model_St2,Test_St)
cor(pred_St2$net.result,Test_St$Profit) # 0.9600603

pred_St_n2 <- denormalize(pred_St2$net.result,max(statups$Profit[Split_s]),min(statups$Profit[Split_s]))
cor(statups$Profit[Split_s],pred_St_n2) # 0.9600603
RMSE_S2 <- sqrt(mean((statups$Profit[Split_s]-pred_St_n2)^2)) # 13479.83
plot(statups$Profit[Split_s],pred_St_n2) 
