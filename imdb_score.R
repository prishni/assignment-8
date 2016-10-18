library(dplyr)
library(ggplot2)
mv <- read.csv("C:/Users/acer/Downloads/assignment8/movie_metadata.csv", stringsAsFactors=FALSE)

training_data = mv[1:4000,-c(17,10,22)]

test_data = data.frame(mv[4001:nrow(mv),-c(17,10,22)]) 


formula1 = imdb_score ~ color + director_facebook_likes + actor_3_facebook_likes +  actor_1_facebook_likes + gross + num_voted_users +cast_total_facebook_likes  + budget + actor_2_facebook_likes +aspect_ratio + movie_facebook_likes + num_critic_for_reviews

#constructing a linear model 
model1  = lm(formula = formula1 , data = training_data )

p = predict(model1,newdata =  test_data)

#adding a column called "predicted_score" to the test_data data frame
test_data$predicted_score = p
q = test_data$imdb_score

sqrt(sum((q-p)*(q-p),na.rm = T)/nrow(test_data))


