library(dplyr)

mv <- read.csv("C:/Users/acer/Downloads/assignment8/movie_metadata.csv", stringsAsFactors=TRUE)

#mv = mv[!((mv$imbd_score == "") | mv$imdb_score == NULL)]

training_data = mv[1:4000,-c(17,10,22)]
test_data = data.frame(mv[4001:nrow(mv),-c(17,10,22)]) 

formula1 = training_data$imdb_score ~ training_data$color + training_data$director_facebook_likes + training_data$actor_3_facebook_likes + training_data$actor_1_facebook_likes + training_data$gross + training_data$num_voted_users + training_data$cast_total_facebook_likes  + training_data$budget + training_data$actor_2_facebook_likes + training_data$aspect_ratio + training_data$movie_facebook_likes
#+ training_data$genres + training_data$content_rating
#model1 = glm(formula = formula1 , data = training_data , family = binomial)
model1  = lm(formula = formula1 , data = training_data)

#p = predict(model,test_data , response = '')

predict(model1,newdata =  test_data)
