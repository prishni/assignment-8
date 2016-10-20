
mv <- read.csv("C:/Users/acer/Downloads/assignment8/movie_metadata.csv", stringsAsFactors=FALSE)
#input = readline(prompt = "Enter the imdb link: ")

input = "http://www.imdb.com/title/tt0398286/?ref_=fn_tt_tt_1"

#selecting only numeric columns from data frame mv
nums = sapply(mv, is.numeric)
mvModified = mv[,nums]

myVector = mvModified[mv$movie_imdb_link==input,]
#myVector = mvModified[mv$movie_title=="Iron Man 2Â ",]

# or
#data <- read.table(file.choose(),header = T,sep = ",")
#data = read.delim(file.choose(),header = T) for tab seperated file

#Computes cosine similarity of 2 vectors
computeCS = function(vect1,vect2){
  s = sum(vect1*vect2,na.rm=T)
  CS = s/(sqrt(sum(vect1*vect1,na.rm=T)) * sqrt(sum(vect2*vect2,na.rm=T)))
  return(CS)
}

matrixOfVectors = list()
cosineSimilarities = list()



for (i in 1:nrow(mv)) {

  matrixOfVectors[[i]] = as.numeric(unlist(mvModified[i,]))
  
}

matrixOfVector = do.call(rbind,matrixOfVectors)
myVector1 = as.numeric(unlist(myVector))
j=1
for(vect in matrixOfVectors){
  cosineSimilarities[[j]] = computeCS(vect,myVector1)
  j=j+1
}
ndx = order(unlist(cosineSimilarities),decreasing = T)[2:6]
maximumMatch = cosineSimilarities[ndx]

index = numeric()
for(i in 1:length(maximumMatch)){
  index[i] = which(unlist(cosineSimilarities) == maximumMatch[i]) 
  
}
mv[ndx,11]

####################################################################################3


computeEuclideanDistance = function(vect1,vect2){
  ED = sqrt(sum((vect1-vect2)*(vect1-vect2)))
  return(ED)
}

EuclideanDistance = list()
j=1
for(vect in matrixOfVectors){
  EuclideanDistance[[j]] = computeEuclideanDistance(vect,myVector1)
  j=j+1
}
ndx = order(unlist(EuclideanDistance),decreasing = T)[2:6]
maximumMatch = EuclideanDistance[ndx]
mv[ndx,11]

