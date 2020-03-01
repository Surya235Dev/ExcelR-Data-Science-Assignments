set.seed(1)
library(class)
d = read.csv(file.choose())
d = data.frame(d)

# renaming the columns of dataset 
names(d) <- c("animal", "hair", "feathers", "eggs", "milk", "airborne",
              "aquatic", "predator", "toothed", "backbone", "breathes", "venomous",
              "fins", "legs", "tail", "domestic", "size", "type")

types <- table(d$type)
d_target <- d[, 18]
d_key <- d[, 1]
d$animal <- NULL

#Classifying the animals data in Mammal, Bird,Fish etc..
names(types) <- c("mammal", "bird", "reptile", "fish", "amphibian", "insect", "crustacean")
types

summary(d)
str(d)

#Implementing KNN model 
k = sqrt(17) + 1
m1 <- knn.cv(d, d_target, k, prob = TRUE)
prediction <- m1

cmat <- table(d_target,prediction)
acc <- (sum(diag(cmat)) / length(d_target)) * 100
#Print accuracy for classification
print(acc)

# Frequency of mammal, bird,insects etc
data.frame(types)
#confusion_matrix
cmat

#Inference
# We could say that Accuracy Level of predection is 90%
# If there where more datasets the accuracy would be larger.


