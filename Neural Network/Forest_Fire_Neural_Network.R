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

forest <- read.csv(file.choose())
head(forest)
dim(forest)
str(forest)
boxplot(forest)
table(forest$size_category)
boxplot(forest$area)$out
colSums(is.na(forest)) # No outliers 
length(boxplot(forest$area)$out)
dim(forest)
dum_fire <- normalize_dummy(forest)
head(dum_fire)
boxplot(dum_fire)



set.seed(101)
split_f <- sample(nrow(forest),0.3*nrow(forest))
train_forest <- dum_fire[-split_f,] 
test_forest <- dum_fire[split_f,] 

# Model Fitting
set.seed(101);fo_model_1 <- neuralnet(area~.,data = train_forest)
str(fo_model_1)
plot(fo_model_1, rep = "best")
summary(fo_model_1)

NeuralNetTools::plotnet(fo_model_1,alpha=0.3)

Pred_fo_1 <- compute(fo_model_1,test_forest)
cor(Pred_fo_1$net.result,test_forest$area) # 0.24

Act_Pred_f1 <- denormalize(Pred_fo_1$net.result,max(forest$area),min(forest$area))
table(Act_Pred_f1<0) # no negative values
Act_Pred_f1.1 <- ifelse(Act_Pred_f1<0,0,Act_Pred_f1)# removing the negative values

cor(Act_Pred_f1.1,test_forest$area) # 0.17

# Model 2 
set.seed(101);fo_model_2 <- neuralnet(area~.,data = train_forest,hidden = 5)
str(fo_model_2)
plot(fo_model_2, rep = "best")
summary(fo_model_2)
NeuralNetTools::plotnet(fo_model_2,alpha=0.3)
Pred_fo_2 <- compute(fo_model_2,test_forest)
cor(Pred_fo_2$net.result,test_forest$area) # 0.125
Act_Pred_f2 <- denormalize(Pred_fo_2$net.result,max(forest$area),min(forest$area))
cor(forest$area[split_f],Act_Pred_f2) # 0.125
data.frame(Predicted=Act_Pred_f2,Actual=test_forest$area) -> df_f_p
ggplot2::ggplot(data=df_f_p,aes(x=Actual,y=Predicted))+geom_smooth()+geom_point()


# Removing the records with burn area = 0  
head(forest)
table(forest[forest$area!=0,]$size_category) # burn area data where area is not 0
df_forest_2 <- forest[forest$area!=0,]
df_forest_2_norm <- normalize_dummy(df_forest_2)
split <- sample(nrow(df_forest_2_norm),nrow(df_forest_2_norm)*0.7,F)
train_forest <- df_forest_2_norm[-split,] 
test_forest <- df_forest_2_norm[split,] 

set.seed(101);fo_model_3 <- neuralnet(area~.,data = train_forest,hidden = 3)
str(fo_model_3)
plot(fo_model_3, rep = "best")
summary(fo_model_3)
NeuralNetTools::plotnet(fo_model_3,alpha=0.3)
Pred_fo_3 <- compute(fo_model_3,test_forest)
table(Pred_fo_3$net.result<0) # I am grtting 38 negative values in my prediction
cor(Pred_fo_3$net.result,test_forest$area) # 0.06976619
Act_Pred_f3 <- denormalize(Pred_fo_3$net.result,max(forest$area),min(forest$area))
table(Act_Pred_f3<0) # 49 negative values
cor(forest$area[forest$area!=0][split],Act_Pred_f3) # 0.0697
data.frame(Predicted=Act_Pred_f3,Actual=test_forest$area) -> df_f_p
ggplot2::ggplot(data=df_f_p,aes(x=Actual,y=Predicted))+geom_smooth()+geom_point()



