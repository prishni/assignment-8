library(dplyr)

mv <- read.csv("C:/Users/acer/Downloads/assignment8/movie_metadata.csv", stringsAsFactors=FALSE)

#extracting col with numeric data
nums = sapply(mv, is.numeric)
mvModified = mv[,nums]

DirectorActorDuos1 = list()
DirectorActorDuos2 = list()
DirectorActorDuos3 = list()
DirectorActorDuos1 = mv[,c(2,7,12)]
DirectorActorDuos2 = mv[,c(2,11,12)]
DirectorActorDuos3 = mv[,c(2,15,12)]

names(DirectorActorDuos2) = names(DirectorActorDuos1)
names(DirectorActorDuos3) = names(DirectorActorDuos1)

DirectorActorDuos = unique(rbind(DirectorActorDuos1,DirectorActorDuos2,DirectorActorDuos3))

dsummary = DirectorActorDuos%>%group_by(director_name,actor_2_name)%>%summarise(n = n())%>%arrange(desc(n))
dsummary1 = dsummary[!((dsummary$director_name == "") | (dsummary$actor_2_name == "")),]

jaccardSim = list()

jaccardSimilarity = function(Dname,Aname,freq){
  dirmovielist = unique(DirectorActorDuos[DirectorActorDuos$director_name==Dname,3])
  actormovielist = unique(DirectorActorDuos[DirectorActorDuos$actor_2_name==Aname,3])
  u = length(union(dirmovielist, actormovielist))
  i = length(intersect(dirmovielist, actormovielist))
  
  #A = sum(as.numeric(unlist(dsummary1[dsummary1$director_name == Dname,3])),na.rm = T)
  #B = sum(as.numeric(unlist(dsummary1[dsummary1$actor_2_name == Aname,3])),na.rm = T)
  return(i/u)
}

for(i in 1:nrow(dsummary1)){
    jaccardSim[[i]] = jaccardSimilarity(dsummary1[[i,1]],dsummary1[[i,2]],dsummary1[[i,3]])
}

ndx1 = order(unlist(jaccardSim),decreasing = T)[1:5]
maximum = jaccardSim[ndx1]

dsummary1[ndx1,c(1,2,3)]
