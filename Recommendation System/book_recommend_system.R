library("recommenderlab")
library(caTools)

books1 <- read.csv(file.choose())

#removing useless varaible such as Id, User iD
books <- books1[-c(1,2)]

str(books)

# Visulize Histogram for rating
hist(books$rating)

books_matrix <- as(books, 'realRatingMatrix')
movie_recomm_model1 <- Recommender(books_matrix, method="POPULAR")


# Predictions for two users 
recommended_items1 <- predict(movie_recomm_model1, books_matrix[413:414], n=5)
as(recommended_items1, "list")


#	Collaborative Filtering
movie_recomm_model2 <- Recommender(books_matrix, method="UBCF")
recommended_items2 <- predict(movie_recomm_model2, books_matrix[413:414], n=5)
as(recommended_items2, "list")

#	RANDOM recommendations
movie_recomm_model4 <- Recommender(books_matrix, method="RANDOM")
recommended_items4 <- predict(movie_recomm_model4, books_matrix[413:414], n=5)
as(recommended_items4, "list")


