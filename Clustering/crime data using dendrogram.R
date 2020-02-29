library(readr)
crime_data <- read.csv(file.choose())
View(crime_data)

#checking for normality
summary(crime_data)
qqnorm(Murder)
qqline(Murder)
qqnorm(Assault)
qqline(Assault)
qqnorm(UrbanPop)
qqline(UrbanPop)
qqnorm(Rape)
qqline(Rape)

#Most of the data follows Normal Distribution
#boxplot to find for any outliers
boxplot(Murder)
boxplot(Assault)
boxplot(UrbanPop)
boxplot(Rape)
# There are few outliers in the dataset
# So we normalize them
normdata<-scale(crime_data[2:5])
normdata

#To genrate Dendrogramto 
d<-dist(normdata,method="euclidian")
fitvalue<-hclust(d,method="complete")
plot(fitvalue,hang=-1)


#To find best N no of clusters for K-means algorithm
clu<-rect.hclust(fitvalue,k=5,border="blue")
cluster<-cutree(fitvalue,k=5)
cluno<-as.matrix(cluster)
final<-data.frame(crime_data,cluno)

# From the generated Dendrogram we could choose 5 clusters

#Creating a  new Cluster Column and appending it. 
final1<-final[,c(ncol(final),1:(ncol(final)-1))]
print(final1)

#Create a new file with cluster information.
write.csv(final1,file="crime_data_with_cluster.csv",row.names= F)
getwd()







