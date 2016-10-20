library(class)
library(gmodels)
mv <- read.csv("C:/Users/acer/Downloads/assignment8/movie_metadata.csv", stringsAsFactors=FALSE)

#selecting only numeric columns from data frame mv
nums = sapply(mv, is.numeric)
mvModified = mv[,nums]

#replace all Na in data frame with 0
mvModified[is.na(mvModified)] <- 0

#extract first genre from list of genres 
mv$genres = sapply(strsplit(mv$genres,"|",fixed =T), function(x) x[1])
g = mv$genres[1:4500]


for (i in 1:ncol(mvModified)) {
 if(is.numeric(mvModified[,i])) {
  mn = mean(mvModified[,i],na.rm = T)
 sd = sd(mvModified[,i], na.rm = T)
mvModified[,i] = (mvModified[,i] - mn)/sd
}
}

training_data = mvModified[1:4500,-c(6,11)]



test_data = data.frame(mvModified[4501:nrow(mvModified),-c(6,11)])



predicted_genres = knn(training_data,test_data,g, k =5)

#creating a truth vector which will contain only TRUE or FALSE 
# TRUE - when predicted genre = actual genre given
#FALSE - otherwise
truth = (predicted_genres == mv$genres[4501:nrow(mvModified)])

correct_predicton = 0
incorrect_prediction = 0

#finding number of correctly predicted genre (no. of TRUE entries in vector truth)
for(val in truth){
  if (val == TRUE)
    correct_predicton = correct_predicton+1
  else
    incorrect_prediction =incorrect_prediction +1
}

accuracy = correct_predicton/length(truth)
accuracy

cnfmatrix = CrossTable(predicted_genres, mv$genres[4501:nrow(mvModified)])
write.csv(cnfmatrix[[1]], file = "cnfmatrix.csv")