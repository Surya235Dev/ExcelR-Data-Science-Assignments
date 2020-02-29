ToyotaCorolla <- read.csv(file.choose())
View(ToyotaCorolla)
summary(ToyotaCorolla)
dim(ToyotaCorolla)
levels(ToyotaCorolla$Model)
levels(ToyotaCorolla$Color)
levels(ToyotaCorolla$Fuel_Type)

# Inorder to predict the price, following variables Age,KM,HP,cc,Doors,Gears,Quarterly_Tax,Weight plays a important role.
var(ToyotaCorolla$Age_08_04)
var(ToyotaCorolla$KM)
var(ToyotaCorolla$HP)
var(ToyotaCorolla$cc)
var(ToyotaCorolla$Doors)
var(ToyotaCorolla$Gears)
var(ToyotaCorolla$Quarterly_Tax)
var(ToyotaCorolla$Weight)

sd(ToyotaCorolla$Age_08_04)
sd(ToyotaCorolla$KM)
sd(ToyotaCorolla$HP)
sd(ToyotaCorolla$cc)
sd(ToyotaCorolla$Doors)
sd(ToyotaCorolla$Gears)
sd(ToyotaCorolla$Quarterly_Tax)
sd(ToyotaCorolla$Weight)


Toyota <- ToyotaCorolla[c('Price','Age_08_04','KM','HP','cc','Doors','Gears','Quarterly_Tax','Weight')]
plot(Toyota)
cor(Toyota)
Toyotamodel <- lm(Price ~ Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight,data = Toyota)
summary(Toyotamodel)
Final_Model <- lm(Price ~ Age_08_04+KM+HP+log(cc)+Gears+Quarterly_Tax+Weight,data = Toyota)
summary(Final_Model)

# Inference
# This model hold good for estimating price of a Toyota Corolla Car.