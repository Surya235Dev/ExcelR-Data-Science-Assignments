#Plastic Sales With respect to month

dfp=read.csv(file.choose())
str(dfp)
plot(dfp$Sales)
nrow(dfp) 
boxplot(dfp$Sales) # No outliers
sum(is.na(dfp$Sales)) # Check for missing values

# Pre processing my data

year <- rep(1949:1953,c(rep(12,length(1949:1953))))
month<- data.frame(outer(rep(month.abb,length = nrow(dfp)), month.abb,"==") + 0 )
colnames(month) <- c(month.abb)

df_plastic <-data.frame(year,month,rn=1:nrow(dfp),Sales=dfp$Sales) 
head(df_plastic)

# Train Test splitting
df_trainP <-df_plastic[1:40,]
df_testP <- df_plastic[41:60,]

#Linear Trend model

model_ltP1 <- lm(df_trainP$Sales~rn,data = df_trainP[,-15])
summary(model_ltP1) # Here my Rsquard value is 0.1211
pred_ltP1 <- predict(model_ltP1,df_testP[,-15])
plot(df_testP$Sales,type = "b",col="blue")
lines(pred_ltP1,type = "b",col="red")
cor(pred_ltP1,df_testP$Sales) # -0.1532 negative correlations 
rmse_ltP1<-sqrt(mean((df_testP$Sales-pred_ltP1)^2))

#Exponential Model
model_emP1 <- lm(log(df_trainP$Sales)~rn,data = df_trainP[,-15])
summary(model_emP1) # Rsquard value is 0.1269
pred_emP1_ <- predict(model_emP1,df_testP[,-15])
pred_emP1 <- exp(pred_emP1_)
plot(df_testP$Sales,type = "b",col="blue")
lines(pred_emP1,type = "b",col="red")
cor(pred_emP1,df_testP$Sales) #-0.1541 correlation
rmse_emP1<-sqrt(mean((df_testP$Sales-pred_emP1)^2)) 

#Seasonal Variation in Model
model_svP1 <- lm(df_trainP$Sales~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data = df_trainP[,-15])
summary(model_svP1) #  Rsquard value is 0.8405
pred_svP1 <- predict(model_svP1,df_testP[,-15])

plot(df_testP$Sales,type = "b",col="blue")
lines(pred_svP1,type = "b",col="red")
cor(pred_svP1,df_testP$Sales) # 0.9072 correlation
rmse_svP1<-sqrt(mean((df_testP$Sales-pred_svP1)^2))

#Additive Seasonality with linear trend
modelP1 <- lm(df_trainP$Sales~.,data = df_trainP[,-15])
summary(modelP1) # Rsquard value is 0.9791
predP1 <- predict(modelP1,df_testP[,-15])
plot(df_testP$Sales,type = "b",col="blue")
lines(predP1,type = "b",col="red")
cor(predP1,df_testP$Sales) #  0.88 correlation
rmse_P1<-sqrt(mean((df_testP$Sales-predP1)^2)) # 105.2468

#Multiplicative Seasonality
modelP2 <- lm(log(df_trainP$Sales)~.-rn,data = df_trainP[,-15])
summary(modelP2) #  Rsquard value is 0.9848
predP2_ <- predict(modelP2,df_testP[,-15])
predP2 <- exp(predP2_)
plot(predP2,type = "b",col="red")
lines(df_testP$Sales,type = "b",col="blue")
cor(predP2,df_testP$Sales) # 0.8763 correlation
rmse_P2<-sqrt(mean((df_testP$Sales-predP2)^2)) # 117.115

#Multiplicative Seasonality Linear Trend
modelP3 <- lm(log(df_trainP$Sales)~.,data = df_trainP[,-15])
summary(modelP3) #  Rsquard value is 0.9848
predP3_ <- predict(modelP3,df_testP[,-15])
predP3 <- exp(predP3_)
plot(predP3,type = "b",col="red")
lines(df_testP$Sales,type = "b",col="blue")
cor(predP3,df_testP$Sales) # 0.8763 correlation
rmse_P3<-sqrt(mean((df_testP$Sales-predP3)^2)) # 117.115

