library(car)
library(ROCR)
#install.packages("MLmetrics")
#install.packages("caret")
library(caret)
library(MLmetrics)

Bank <- read.csv(file.choose())
table(Bank$y)
head(Bank)
colnames(Bank)
str(Bank)
summary(Bank)
boxplot(Bank)

#Visulize BOX plot for each variable in the dataset
boxplot(scale(Bank[,c(1,6,10,12:15)]),ylim = c(-10,20))
#Visulize Historgam plot for each variable in the dataset
hist(scale(Bank[,c(1,6,10,12:15)]),ylim = c(-10,20))

#write.csv(summary(Bank),"BankSummary.csv")
# Train and Test Split and Model Evaluation ----
set.seed(101)
Test_Spl <- as.integer(sample(x = rownames(Bank),size = round(nrow(Bank)*(30/100)),replace = F))
Train_B <- Bank[-c(Test_Spl),]
Test_B <- Bank[c(Test_Spl),]
nrow(Train_B);nrow(Test_B)


# Model Building
model_B1 <- glm(y~.,data = Train_B,family = binomial(link = "logit"))
summary(model_B1) 
#Test Efficiency
      Y_B1 <- predict(model_B1,Test_B)
      prob_B1 <-predict(model_B1,Test_B,type = "response")
      
      plot(prob_B1,Test_B$y,
           col=ifelse((prob_B1<0.5 & Test_B$y =="no")|(prob_B1>0.5 & Test_B$y =="yes"),"green","red")
           # ,pch = ifelse((prob_B1<0.5 & Test_B$y =="no")|(prob_B1>0.5 & Test_B$y =="yes"),1,4)
      )
      
#Visulize      
# In this plot the red circle represents wrong prediction and the gree circle represents correct prediction
plot(Y_B1,col=ifelse((prob_B1<0.5 & Test_B$y =="no")|(prob_B1>0.5 & Test_B$y =="yes"),"green","red"))
confu_B1 <- table(prob_B1>0.5,Test_B$y) ;
confu_B1
effi_B1 <- sum(diag(confu_B1))/sum(confu_B1);effi_B1 
      # F_1 Score
      pB1 <- ifelse(prob_B1>0.5,1,0)
      ytrueB1 <- ifelse(Test_B$y=="yes",1,0)
      F1_Score(y_true = ytrueB1,y_pred = pB1) # 0.945201
  #Train Efficiency
      Y_B1.1 <- predict(model_B1,Train_B)
      prob_B1.1 <-predict(model_B1,Train_B,type = "response")
      plot(prob_B1.1,Train_B$y,
           col=ifelse((prob_B1.1<0.5 & Train_B$y =="no") | (prob_B1.1>0.5 & Train_B$y =="yes"),"green","red")
           # ,pch = ifelse((prob_B1.1<0.5 & Train_B$y =="no")|(prob_B1.1>0.5 & Train_B$y =="yes"),1,4)
      )
      # In this plot the red circle represents wrong prediction and the gree circle represents correct prediction
      plot(Y_B1.1,col=ifelse((prob_B1.1<0.5 & Train_B$y =="no")|(prob_B1.1>0.5 & Train_B$y =="yes"),"green","red"))
      confu_B1.1 <- table(prob_B1.1>0.5,Train_B$y) ;confu_B1.1
      effi_B1.1 <- sum(diag(confu_B1.1))/sum(confu_B1.1);effi_B1.1 # Efficiency of my model is 0.9010996
  
  influenceIndexPlot(model_B1,id=list(col="red"))    
  influence_B1 <- as.integer(rownames(influencePlot(model_B1,id=list(n=5,col="red"))))
  length(influence_B1)
  
  # ROC Curve
  rocrpred1<-prediction(prob_B1,Test_B$y)
  rocrperf1<-performance(rocrpred1,'tpr','fpr')
  str(rocrperf1)
  rocrperf1@x.values
  plot(rocrperf1,colorize=T)
  
  rocr_cutoff <- data.frame(cut_off = rocrperf1@alpha.values[[1]],fpr=rocrperf1@x.values,tpr=rocrperf1@y.values)
  colnames(rocr_cutoff) <- c("cut_off","FPR","TPR")
  View(rocr_cutoff)
  
  library(dplyr)
  
  rocr_cutoff$cut_off <- round(rocr_cutoff$cut_off,6)
  # Sorting data frame with respect to tpr in decreasing order 
  rocr_cutoff <- arrange(rocr_cutoff,desc(TPR))
  View(rocr_cutoff)
  head(rocr_cutoff)

  #confusion matrix
  print(confu_B1)
 #accuracy is 90%