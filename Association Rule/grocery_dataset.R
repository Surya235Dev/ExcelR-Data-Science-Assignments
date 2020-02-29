#Groceries Dataset
library(arules)
library(arulesViz)

grocery <- read.csv(file.choose())
summary(grocery)


#applying aprori algoritham.
rules <- apriori(grocery,parameter = list(support=0.002,confidence=0.05,minlen=3))
rules1 <- rules[is.redundant(rules)]
rules1
inspect(rules1[1:5])


#inspecting top 6 rules
inspect(head(sort(rules1,by="lift")))
head(quality(rules1))

#Visualize rules
plot(rules1)
plot(rules1,method = "graph")
plot(rules1,method = "grouped")

#Applying different values of support and confidence support=0.005,confidence=0.9
rulesf <- apriori(grocery,parameter = list(support=0.005,confidence=0.09,minlen=3))
rulesf1 <- rulesf[is.redundant(rulesf)]
rulesf1
#rules has been decreased after minlen of 3 and confidence of 0.09

inspect(head(sort(rulesf1,by="lift")))
plot(rulesf1)

#Changing the minimum length in apriori algorithm 3 to 2
rulesl <- apriori(grocery,parameter = list(support=0.002,confidence=0.05,minlen=2))
rulesl
# 956 rules after minlength=2, support of 0.002 and confidenc =0.05
