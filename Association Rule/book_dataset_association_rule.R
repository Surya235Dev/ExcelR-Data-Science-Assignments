# Books Dataset
library(arules)
library(arulesViz)

book <- read.csv(file.choose())
summary(book)
str(book)

#Applying Aprori algoritham for datasets.
# Support value = 0.002 and confindnce = 0.05
rules <- apriori(as.matrix(book),parameter = list(support=0.002,confidence=0.05,minlen=3))
rules1 <- rules[is.redundant(rules)]
rules


#inspecting rules
inspect(rules1[1:100])
inspect(head(sort(rules1,by="lift")))
head(quality(rules1))


#Visualize rules
plot(rules1)
plot(rules1,method = "graph")
plot(rules1,method = "grouped")

#Applying different values for support=0.010 and confidence=0.6
rulesf <- apriori(as.matrix(book),parameter = list(support=0.010,confidence=0.6,minlen=3))
rulesf1 <- rulesf[is.redundant(rulesf)]
rulesf1

#rules no decresed to 3637 rules 
inspect(head(sort(rulesf1,by="lift")))
plot(rulesf1)

#Changing the minimum length in apriori algorithm 3 to 6
rulesl <- apriori(as.matrix(book),parameter = list(support=0.002,confidence=0.05,minlen=6))
rulesl
print(rules1)