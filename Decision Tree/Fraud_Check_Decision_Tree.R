library(party)
library(C50)

df = read.csv(file.choose())
head(df)
str(df)
pairs(df)
# taxable_income <= 30000 as "Risky" and others are "Good"
type <- ifelse(df$Taxable.Income <= 30000,"Risky","Good")
Fraud_df <- data.frame(df,type)
barplot(table(Fraud_df$type))
table(Fraud_df$type)
#  Good Risky 
# 476   124 

# since the risky and good data are biased we balance it
# balancing the data
balance <- as.integer(sample(rownames(Fraud_df[type=="Good",]),124,replace = F))
Fraud_df_ <- rbind(Fraud_df[type=="Risky",],Fraud_df[balance,])
barplot(table(Fraud_df_$type))

# Model with Imbalanced Data

Treef1 <- ctree(Fraud_df$type~Undergrad + Marital.Status + City.Population + 
                  Work.Experience,data = Fraud_df)
predf1 <- predict(Treef1,Fraud_df)
table(Predicted=predf1,Actual=Fraud_df$type)
mean(predf1==Fraud_df$type)

# Model with Balanced Data
Treef1.B <- ctree(Fraud_df_$type~Undergrad + Marital.Status + City.Population + 
                  Work.Experience,data = Fraud_df_)
predf1.B <- predict(Treef1.B,Fraud_df_)
table(Predicted=predf1.B,Actual=Fraud_df_$type)
mean(predf1.B==Fraud_df_$type)


#Feature selection

spl <- as.integer(sample(x = nrow(Fraud_df_),size = nrow(Fraud_df_)*.7,F));length(spl)
train_F <- Fraud_df_[spl,]; 
test_F <- Fraud_df_[-spl,];


Treef2 <- ctree(train_F$type~.,data = train_F[,-7])
predf2 <- predict(Treef2,test_F[,-7])

table(Actual =test_F$type ,Predicted = predf2)
mean(test_F$type==predf2) 
plot(Treef2)

# 0.989 i.e. 99%  accuracy we are getting