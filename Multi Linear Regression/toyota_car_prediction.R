# Predicting Toyato Corolla cars price based on the Attributes such as 
#Color, Mileage,manufactured year
ToyatoCorolla <- read.csv(file.choose())
View(ToyatoCorolla)
summary(ToyatoCorolla)
dim(ToyatoCorolla)
levels(ToyatoCorolla$Model)
levels(ToyatoCorolla$Color)
levels(ToyatoCorolla$Fuel_Type)

# Inorder to predict the price, 
#Weight plays a important role.
var(ToyatoCorolla$Age_08_04)
var(ToyatoCorolla$KM)
var(ToyatoCorolla$HP)
var(ToyatoCorolla$cc)
var(ToyatoCorolla$Doors)
var(ToyatoCorolla$Gears)
var(ToyatoCorolla$Quarterly_Tax)
var(ToyatoCorolla$Weight)

sd(ToyatoCorolla$Age_08_04)
sd(ToyatoCorolla$KM)
sd(ToyatoCorolla$HP)
sd(ToyatoCorolla$cc)
sd(ToyatoCorolla$Doors)
sd(ToyatoCorolla$Gears)
sd(ToyatoCorolla$Quarterly_Tax)
sd(ToyatoCorolla$Weight)

plot(Toyato)
cor(Toyato)
Toyato <- ToyatoCorolla[c('Price','Age_08_04','KM','HP','cc','Doors','Gears','Quarterly_Tax','Weight')]
Toyatomodel <- lm(Price ~ Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight,data = Toyato)
summary(Toyatomodel)
influenceIndexPlot(Toyatomodel)
influencePlot(Toyatomodel)
vif(Toyatomodel)
avPlots(Toyatomodel)
stepAIC(Toyatomodel)
Final_Model <- lm(Price ~ Age_08_04+KM+HP+log(cc)+Gears+Quarterly_Tax+Weight,data = Toyato)
summary(Final_Model)

# Since all the P-value are significant we could say
#that this model hold good for predicting price
