#### Required packages ####
library(recommenderlab)
library(data.table)
library(ggplot2)
library(reshape2)

#### Data loading ####
movie_data <- read.csv('data/movies.csv', stringsAsFactors = FALSE)
rating_data <- read.csv('data/ratings.csv')

#### Data Prep ####
genre <- as.data.frame(movie_data$genres, stringsAsFactors = F)
genre2 <- as.data.frame(tstrsplit(genre[,1], '[|]',
                                  type.convert = TRUE),
                        stringsAsFactors = TRUE)
colnames(genre2) <- c(1:10)
colN <- ncol(genre2)
rowN <- nrow(genre2)

genre_list <- c("Action", "Adventure", "Animation", "Children", 
                "Comedy", "Crime","Documentary", "Drama", "Fantasy",
                "Film-Noir", "Horror", "Musical", "Mystery","Romance",
                "Sci-Fi", "Thriller", "War", "Western")
genre_mat <- matrix(0,10330,18)
colnames(genre_mat) <- genre_list

for (i in 1:rowN){
  for (j in 1:colN){
    gen_col = which(genre_list == genre2[i,j])
    genre_mat[i+1,gen_col] <- 1
  }
}

## construct search and rating matrix
search_matrix <- cbind(movie_data[,1:2], genre2)
rating_matrix <- dcast(rating_data, userId ~ movieId, 
                       value.var = 'rating', na.rm = FALSE)
rating_matrix <- as.matrix(rating_matrix[,-1])
rating_matrix <- as(rating_matrix, 'realRatingMatrix')


recommendation_model <- recommenderRegistry$get_entries(dataType = "realRatingMatrix")
names(recommendation_model)
lapply(recommendation_model, "[[", "description")
