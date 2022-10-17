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
library(ggplot2)
ggplot(data = movies_metadata) + 
  geom_bar(mapping = aes(x = original_language))
# We can see that more or less all our movies are in english language
ggplot(data = movies_metadata) + 
  geom_bar(mapping = aes(x = original_language, fill = profitable))
#We can also see that the most of the profits come from movies in en language
ggplot(data = movies_metadata) + 
  geom_bar(mapping = aes(x = profitable)) +
  facet_wrap(~ english)
#also the proportions of huge profitable's movies is bigger
#for movies in en language
ggplot(data = movies_metadata) +
  geom_point(mapping = aes(x = vote_average, y = profit))
# we can't say too much, but of course there is a little positive correlation
ggplot(data = movies_metadata) +
  geom_point(mapping = aes(x = vote_count, y = profit))
# in this case we can see even less correlation
ggplot(data = movies_metadata) +
  geom_point(mapping = aes(x = popularity, y = profit))
# no correlation
ggplot(data = movies_metadata) +
  geom_point(mapping = aes(x = vote_average, y = popularity))
#no correlation
ggplot(data = movies_metadata) +
  geom_point(mapping = aes(x = vote_count, y = popularity))
#no correlation
