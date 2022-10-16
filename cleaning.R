movies_metadata <- movies_metadata[,!names(movies_metadata) %in% c("adult", "belongs_to_collection", "homepage", 'id', 'imdb_id', 'poster_path', 'spoken_languages', 'status', 'tagline', "title", 'video')]
h=c()
for (i in (1:length(movies_metadata$budget))){
  if (isTRUE(movies_metadata$budget[i]<60000)==TRUE || is.na(movies_metadata$budget[i])==TRUE || isTRUE(movies_metadata$revenue[i]<20000)==TRUE || isTRUE(movies_metadata$runtime[i]<10)==TRUE || isTRUE(movies_metadata$vote_average[i]<0.2)==TRUE || isTRUE(movies_metadata$vote_count[i]<10)==TRUE){
    h=c(h,i)
  }
}

movies_metadata=movies_metadata[-h,]
length(movies_metadata$budget)
