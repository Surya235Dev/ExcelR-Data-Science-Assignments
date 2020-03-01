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

library(nnet)
library(neuralnet)
#install.packages(c("neuralnet","nnet"))


concret <- read.csv(file.choose())
# Final Value is strength
str(concret)
boxplot(concret)
boxplot(concret$age)
age_remove <- which(concret$age>150)
concret <- concret[-age_remove,]
boxplot(concret$age)$out

# As we can see after removing the outliers also we are getting another set of 
library(ggplot2)
# Visulize the distribution of cement slag with density plot
ggplot2::ggplot(data=concret,aes(x=cement,y=..density..))+geom_histogram(binwidth = 30,color="blue")+geom_density(aes(y=..density..),col="forestgreen")
ggplot2::ggplot(data=concret,aes(x=slag,y=..density..))+geom_histogram(binwidth = 30,color="blue")+geom_density(aes(y=..density..),col="forestgreen")

# Summary of the cement
summary(concret)
# Normalizing the data
df_norm_concret <- normalize_dummy(concret)
# Splitting the data
set.seed(101)
split_c <- sample(nrow(concret),0.3*nrow(concret))
train_concret <- df_norm_concret[-split_c,] 
test_concret <- df_norm_concret[split_c,] 

# Model Fitting
set.seed(101);con_model_1 <- neuralnet(strength~.,data = train_concret)
str(con_model_1)
plot(con_model_1, rep = "best")
summary(con_model_1)

NeuralNetTools::plotnet(con_model_1,alpha=0.3)

Pred_con_1 <- compute(con_model_1,test_concret)
cor(Pred_con_1$net.result,test_concret$strength) # 0.8153639

Actual_Strength <- concret$strength[split_c]
Act_Pred_c1 <- denormalize(Pred_con_1$net.result,max(concret$strength),min(concret$strength))
RMSE_cement1 <- sqrt(mean((Actual_Strength-Act_Pred_c1)^2)) # i.e 8.731773
cor(Act_Pred_c1,Actual_Strength) # 0.860514


