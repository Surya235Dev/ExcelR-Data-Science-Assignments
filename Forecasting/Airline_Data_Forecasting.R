#Airline Dataset

df = readxl::read_xlsx(file.choose())

#visualize
plot(df$Passengers,type = "b")
plot(log(df$Passengers))


table(df$Month)
str(df)
as.Date(df$Month)
df$Month
#Data Pre Processing
year <- rep(1995:2002,c(rep(12,length(1995:2002))))
month<- data.frame(outer(rep(month.abb,length = nrow(df)), month.abb,"==") + 0 )
colnames(month) <- c(month.abb)

df_1 <-data.frame(year,month,rn=1:nrow(df),Passengers=df$Passengers,day=seq(1,2880,by = 30)) 

# train and test splitting

df_train <-df_1[1:80,]
df_test <- df_1[81:96,]

#1 Linear Trend model

model_lt1 <- lm(df_train$Passengers~day,data = df_train[,-15])
summary(model_lt1) #  Rsquard value is 0.7829
pred_lt1 <- predict(model_lt1,df_test[,-15])
plot(df_test$Passengers,type = "b",col="blue")
lines(pred_lt1,type = "b",col="red")
cor(pred_lt1,df_test$Passengers) # 42% accuracy
rmse_lt1<-sqrt(mean((df_test$Passengers-pred_lt1)^2)) # 47.5462

#2 MODEL 1 Additive Seasonality
model1 <- lm(df_train$Passengers~.,data = df_train[,-15])
summary(model1) #  Rsquard value is 0.9533
pred1 <- predict(model1,df_test[,-15])
plot(df_test$Passengers,type = "b",col="blue")
lines(pred1,type = "b",col="red")
cor(pred1,df_test$Passengers) #  98% 
rmse_1<-sqrt(mean((df_test$Passengers-pred1)^2)) # 33.04

#3 Exponential Model
model_em1 <- lm(log(df_train$Passengers)~day,data = df_train[,-15])
summary(model_em1) # Rsquard value is 0.8181
pred_em1_ <- predict(model_em1,df_test[,-15])
pred_em1 <- exp(pred_em1_)
plot(df_test$Passengers,type = "b",col="blue")
lines(pred_em1,type = "b",col="red")
cor(pred_em1,df_test$Passengers) #  42% accuracy
rmse_em1<-sqrt(mean((df_test$Passengers-pred_em1)^2)) # 43.79374



#4 MODEL 2 Multiplicative Seasonality Linear trend

model2 <- lm(log(df_train$Passengers)~.,data = df_train[,-15])
summary(model2) #  Rsquare is 0.9743
pred2_ <- predict(model2,newdata = df_test[,-15])
pred2 <- exp(pred2_)
plot(df_test$Passengers,type = "b",col="blue")
lines(pred2,type = "b",col="red")
cor(pred2,df_test$Passengers) 
rmse_2<-sqrt(mean((df_test$Passengers-pred2)^2)) 

#Therefore model Multiplicative Seasonality Linear trend is best.Which gives 98% accuracy
