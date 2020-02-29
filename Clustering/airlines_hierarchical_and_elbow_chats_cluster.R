#airlines_data
library(readr)
airlines_data <- read.csv(file.choose())
View(airlines_data)
summary(airlines_data)

# we check each and every variables of the datasets to find outliers and their normals distribution
attach(airlines_data)
qqnorm(Balance)
qqline(Balance)
boxplot(Balance)

qqnorm(Qual_miles)
qqline(Qual_miles)
boxplot(Qual_miles)

qqnorm(cc1_miles)
qqline(cc1_miles)
boxplot(Qual_miles)

qqnorm(Bonus_miles)
qqline(Bonus_miles)
boxplot(Bonus_miles)

qqnorm(Bonus_trans)
qqline(Bonus_trans)
boxplot(Bonus_trans)

qqnorm(Flight_miles_12mo)
qqline(Flight_miles_12mo)
boxplot(Flight_miles_12mo)


qqnorm(Flight_trans_12)
qqline(Flight_trans_12)
boxplot(Flight_trans_12)


qqnorm(Days_since_enroll)
qqline(Days_since_enroll)
boxplot(Days_since_enroll)


#Since many variables are not normally distributed and has outliers.
#We normalize the data.

normdata<-scale(airlines_data[,2:(ncol(airlines_data))]) 
View(normdata)
d<-dist(normdata,method="euclidean")
fit<-hclust(d,method="complete")
plot(fit)
plot(fit,hang=-1)
rect.hclust(fit,k=10,border="red")
groups<-cutree(fit,k=10)
membership<-as.matrix(groups)
final<-data.frame(airlines_data,membership)
View(final)

#using single linkage
fit1<-hclust(d,method="single")
plot(fit1)
plot(fit1,hang=-1)
rect.hclust(fit1,k=10,border="red")
groups1<-cutree(fit1,k=10)
membership1<-as.matrix(groups1)
final1<-data.frame(airline,membership1)
View(final1)

#Since there are 4000 obervation, its difficult to use hierarchical cluster
# Therefore we go with K-means cluster to group the data.
library(plyr)
wss = NULL

kmean1<- kmeans(normdata,10)
str(kmean1)

wss = (nrow(normdata)-1)*sum(apply(normdata, 2, var))
for (i in 2:10) wss[i] = sum(kmeans(normdata, centers=i)$withinss)
plot(1:10, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")   # Look for an "elbow" in the scree plot #
title(sub = "K-Means Clustering Scree-Plot")

twss <- NULL
for (i in 2:15){
  twss <- c(twss,kmeans(normdata,i)$tot.withinss)
}
twss
plot(2:15,twss,type="o")
kmean6 <- kmeans(normdata,6)
str(kmean6)
str(kmean6)
kmean11<-kmeans(normdata,11)
str(kmean11)
kmean13<-kmeans(normdata,13)
str(kmean13)
kmean8<-kmeans(normdata,8)
str(kmean8)
#using elbow curve to find N optimum no of Clusters
# There from the generated elbow chat we choose N = 13 clusters
final2<- data.frame(airlines_data, kmean13$cluster)
aggregate(airlines_data[,2:11],by=list(kmean13$cluster), FUN=mean)
final1<-final[,c(ncol(final2),1:(ncol(final2)-1))]
View(final1)
write.csv(final2,file="airlines_with_cluster_data.csv",row.names= F)
getwd()
view(final2)
# Therefore, we have grouped the airline's customer into 13 clusters.








