Startups_50 <- read.csv(file.choose())
View(Startups_50)
summary(Startups_50)
attach(Startups_50)

#Show row and columns of given dataset
dim(Startups_50)

# Variance
var(Startups_50$R.D.Spend)
var(Startups_50$Administration)
var(Startups_50$Marketing.Spend)
var(Startups_50$Profit)

# Standard Deviation
sd(Startups_50$R.D.Spend)
sd(Startups_50$Administration)
sd(Startups_50$Marketing.Spend)
sd(Startups_50$Profit)
class(Startups_50)

# Create Dummy Variable for categorical Data.
# New York, California, Florida
Dummyvariable <- data.frame(numbers = 1:3,
                            state  = c("New York", "California", "Florida"),
                            stringsAsFactors = FALSE)
View(Dummyvariable)
levels(State)
install.packages("fastDummies")
library(fastDummies)
knitr::kable(Dummyvariable)
results <- fastDummies::dummy_cols(Dummyvariable)
knitr::kable(results)
results <- fastDummies::dummy_cols(Dummyvariable, select_columns = "numbers")
knitr::kable(results)
StartupsState_50 <- cbind(Startups_50,ifelse(Startups_50$State=="New York",1,0), ifelse(Startups_50$State=="California",2,0),  ifelse(Startups_50$State=="Florida",3,0))
View(StartupsState_50)

# To check whether the data is normally distributed or not by using QQ plot
qqnorm(Profit)
qqline(Profit)
qqnorm(R.D.Spend)
qqline(R.D.Spend)
qqnorm(Marketing.Spend)
qqline(Marketing.Spend)
qqnorm(Administration)
qqline(Administration)

install.packages("data.table")
library(data.table)

# Plot graph of independent and dependent variables
plot(R.D.Spend,Profit) 
plot(Administration,Profit)
plot(Marketing.Spend,Profit)

## To check correlation of all possible variables pairs
pairs(Startups_50)   # Scatter plot for all pairs
cor(Startups_50[,-4])
cor(R.D.Spend,Profit) 
#we could say R&D spend and Profit has high correlation.
 
library(corpcor)
cor2pcor(cor(Startups_50[,-4]))

# Linear Model of interest
Profitmodel <- lm(Profit~R.D.Spend+Administration+Marketing.Spend+State) # lm(Y ~ X)
summary(Profitmodel)

# As p value is <0.05 only for R.D.Spend
Profitmodel1 <- lm(Profit~R.D.Spend)
summary(Profitmodel1)

Profitmodel2 <- lm(Profit~Administration)
summary(Profitmodel2)

Profitmodel3 <- lm(Profit~Marketing.Spend)
summary(Profitmodel3)

Profitmodel4 <- lm(Profit~State)
summary(Profitmodel4)

Profitmodel5 <- lm(Profit~R.D.Spend+Administration)
summary(Profitmodel5)

Profitmodel6 <- lm(Profit~R.D.Spend+Marketing.Spend)
summary(Profitmodel6)

Profitmodel7 <- lm(Profit~R.D.Spend+State)
summary(Profitmodel7)

Profitmodel8 <- lm(Profit~Administration+Marketing.Spend)
summary(Profitmodel8)

Profitmodel9 <- lm(Profit~Administration+State)
summary(Profitmodel9)

Profitmodel10 <- lm(Profit~Marketing.Spend+State)
summary(Profitmodel10)

Profitmodel11 <- lm(Profit~R.D.Spend+Administration+State)
summary(Profitmodel11)

Profitmodel12 <- lm(Profit~R.D.Spend+Administration+Marketing.Spend)
summary(Profitmodel12)

Profitmodel13 <- lm(Profit~Administration+Marketing.Spend+State)
summary(Profitmodel13)

influence.measures(Profitmodel)
install.packages("car")
library(car)
infIndexPlot(Profitmodel)
influencePlot(Profitmodel)

ProfitModelInfluence <- lm(Profit~R.D.Spend+Administration+Marketing.Spend, data = Startups_50[-c(50,49,47,46),])
summary(ProfitModelInfluence)

### Variance Inflation Factors
vif(Profitmodel)  # VIF is > 10 => collinearity

#### Added Variable Plots ######
avPlots(Profitmodel, col="red")

library("MASS")
stepAIC(Profitmodel) # backward
plot(Profitmodel)

# Final model depends on only R.D.spend and two pairs depending on p-value and R-squared value
Final <- lm(Profit ~ R.D.Spend+Administration+Marketing.Spend)
summary(Final)

Final1 <- lm(Profit~R.D.Spend+Marketing.Spend)
summary(Final1)

Final2 <- lm(Profit~R.D.Spend)
summary(Final2)

plot(Final2)
plot(R.D.Spend,Profit,main = "Profit with R.D.Spend")


# Inference
# Research and Development helps a company to gain more profit