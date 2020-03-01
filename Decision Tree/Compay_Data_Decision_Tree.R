Company_Data <- read.csv(file.choose())
head(Company_Data)
str(Company_Data)

summary(Company_Data)
Categorical_Company_Data <- ifelse(Company_Data$Sales>= 8, yes = 1,0)
head(Categorical_Company_Data)
Company_Data <- Company_Data[-1]

Company_Data <- cbind(Categorical_Company_Data,Company_Data)
print(Company_Data)

library(rpart)
#Visulize
#Plot A decision tree
model_cart1 <- rpart(Categorical_Company_Data~.,data=Company_Data,method="anova")
plot(model_cart1)
text(model_cart1)


pred_Categorical_Company_Data <- predict(model_cart1,Company_Data)
rmse_Categorical_Company_Data <- sqrt(mean((pred_Categorical_Company_Data-Company_Data$Categorical_Company_Data)^2))
rmse_Categorical_Company_Data
#since value is 0.33 we find R^2 value

Adjusted_RSqred <- function(pred, obs, formula = "corr", na.rm = FALSE) {
  n <- sum(complete.cases(pred))
  switch(formula,
         corr = cor(obs, pred, use = ifelse(na.rm, "complete.obs", "everything"))^2,
         traditional = 1 - (sum((obs-pred)^2, na.rm = na.rm)/((n-1)*var(obs, na.rm = na.rm))))
}

Adjusted_RSqred(pred_Categorical_Company_Data,Company_Data$Categorical_Company_Data)

cor(pred_Categorical_Company_Data,Company_Data$Categorical_Company_Data)
