Emp_data <- read.csv(file.choose())
View(Emp_data)
summary(Emp_data)
var(Emp_data$Salary_hike)
sd(Emp_data$Salary_hike.)
var(Emp_data$Churn_out_rate)
sd(Emp_data$Churn_out_rate)

library(lattice)
dotplot(Emp_data$Salary_hike, main="Dot Plot of Salary")
dotplot(Emp_data$Churn_out_rate, main="Dot Plot of Churn out rate")
boxplot(Emp_data$Salary_hike,col="blueviolet")
boxplot(Emp_data$Churn_out_rate,col="mediumorchid3", horizontal = T)
# From the BoxPlot we could say that, there are no outlier and are slightly Positive skew.

cor(Emp_data$Salary_hike,Emp_data$Churn_out_rate)
plot(Emp_data$Salary_hike,Emp_data$Churn_out_rate,main="Salary Hike vs  Churn Rate ", col="violetred", 
     col.main="violetred", col.lab="violetred", xlab="salary hike", 
     ylab="churn rate", pch=20)
Model <- lm(Salary_hike ~ Churn_out_rate, data = Emp_data)
summary(Model)

#  Inference:
# 1) The p-value is less than < 0.05 which is significant.
# 2) The value is R is -0.91 which is strongly negative correlation
# 3) The value of R^2 is 0.83 > 0.07 which suggest model is good.
# 4) We can predict 0.8312 or 83% right output

