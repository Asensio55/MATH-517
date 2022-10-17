movies_metadata <- movies_metadata[,!names(movies_metadata) %in% c("adult", "belongs_to_collection", "homepage", 'id', 'imdb_id', 'poster_path', 'spoken_languages', 'status', 'tagline', "title", 'video')]
h=c()
for (i in (1:length(movies_metadata$budget))){
  if (isTRUE(movies_metadata$budget[i]<60000)==TRUE || is.na(movies_metadata$budget[i])==TRUE || isTRUE(movies_metadata$revenue[i]<20000)==TRUE || isTRUE(movies_metadata$runtime[i]<10)==TRUE || isTRUE(movies_metadata$vote_average[i]<0.2)==TRUE || isTRUE(movies_metadata$vote_count[i]<10)==TRUE){
    h=c(h,i)
  }
}
movies_metadata=movies_metadata[-h,]
#length(movies_metadata$budget)


movies_metadata$cost=1:length(movies_metadata$budget)
for (i in 1:length(movies_metadata$budget)) {
  if (isTRUE(movies_metadata$budget[i]<=10000000)==TRUE){
  movies_metadata$cost[i]= 'LOW'  
  }
  else {
  movies_metadata$cost[i]= 'HIGH'  
  }
}
movies_metadata$english=1:length(movies_metadata$budget)
for (i in 1:length(movies_metadata$budget)) {
  if (isTRUE(movies_metadata$original_language[i]=='en')==TRUE){
    movies_metadata$english[i]= 'YES'  
  }
  else {
    movies_metadata$english[i]= 'NO'  
  }
}

movies_metadata$profit=1:length(movies_metadata$budget)
for (i in 1:length(movies_metadata$budget)) {
  movies_metadata$profit[i]=movies_metadata$revenue[i]-movies_metadata$budget[i]
}

movies_metadata$profitable=1:length(movies_metadata$budget)
for (i in 1:length(movies_metadata$budget)) {
  if (isTRUE(movies_metadata$profit[i]>=90000000)==TRUE){
    movies_metadata$profitable[i]= 'HUGE'  
  }
  else if (isTRUE(movies_metadata$profit[i]>0)==TRUE & isTRUE(movies_metadata$profit[i]<90000000)==TRUE){
    movies_metadata$profitable[i]= 'YES'  
  }
  else {
    movies_metadata$profitable[i]= 'NO'  
  }
}
#I don't know how to find informations from genres and production companies

