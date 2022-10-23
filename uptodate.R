movies_data_init <- read.csv('Z:/stat_comp_hw2/movies_metadata.csv')
movies_metadata <- read.csv('Z:/stat_comp_hw2/movies_metadata.csv') ### update path depending on how you stored it.
movies_metadata <- movies_metadata[,!names(movies_metadata) %in% c("adult", "belongs_to_collection", "homepage", 'id', 'imdb_id', 'poster_path', 'spoken_languages', 'status', 'tagline', "title", 'video')]
h=c()
for (i in (1:length(movies_metadata$budget))){
  if (isTRUE(movies_metadata$budget[i]<60000)==TRUE || is.na(movies_metadata$budget[i])==TRUE ||  isTRUE(movies_metadata$runtime[i]<10)==TRUE ||  isTRUE(movies_metadata$vote_count[i]<10)==TRUE){
    h=c(h,i)
  }### why remove low revenue or low vote average ?
}
movies_metadata=movies_metadata[-h,] ### retains only interesting variables.h contains some specific subset based on revenue ?
#length(movies_metadata$budget)

### Adjustment of the variable types:
movies_metadata$budget <- as.numeric(movies_metadata$budget)
movies_metadata$popularity <- as.numeric(movies_metadata$popularity)

### calculating and adding variables of interest
movies_metadata$cost=1:length(movies_metadata$budget)  ### creates a binary cost variable based on budget, threshold 10^7
for (i in 1:length(movies_metadata$budget)) {
  if (isTRUE(movies_metadata$budget[i]<=10000000)==TRUE){
  movies_metadata$cost[i]= 'LOW'  
  }
  else {
  movies_metadata$cost[i]= 'HIGH'  
  }
}
movies_metadata$english=1:length(movies_metadata$budget) ### creates a binary 'english-based' english variable
for (i in 1:length(movies_metadata$budget)) {
  if (isTRUE(movies_metadata$original_language[i]=='en')==TRUE){
    movies_metadata$english[i]= 'YES'  
  }
  else {
    movies_metadata$english[i]= 'NO'  
  }
}

movies_metadata$profit=1:length(movies_metadata$budget) ### adds a profit variable, dependent on budget and revenue
for (i in 1:length(movies_metadata$budget)) {
  movies_metadata$profit[i]=movies_metadata$revenue[i]-movies_metadata$budget[i]
}

movies_metadata$profitable=1:length(movies_metadata$budget) ### creates a ternary profitable variable with thresholds 9*10^7 and 0
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

#histogram of movies profitable with separation of english vs non english
ggplot(data = movies_metadata) + 
  geom_bar(mapping = aes(x = profitable)) +
  facet_wrap(~ english)
#also the proportions of huge profitable's movies is bigger
#for movies in en language

#vote average vs profit
ggplot(data = movies_metadata, mapping = aes(x = vote_average, y = profit, color = english, shape = english)) +
  geom_point() +
  stat_smooth(method=lm) +
  facet_wrap(~ english)
### There is a stronger correlation between average vote (consumer rating?) and profit for english films. 
### However, there are few non-english observations, so this (I guess) induces a higher variance in that matter.

#vote count vs profit
ggplot(data = movies_metadata, mapping = aes(x = vote_count, y = profit, color = english, shape = english)) +
  geom_point() +
  stat_smooth(method=lm) +
  facet_wrap(~ english)
### Same thing; few non-english observations.
### For english films, seemingly strong correlation between vote_count and profit, should not come as a surprise, as... 
### ... vote_count can be seen as an estimator for 'number of views', which is directly related to profit through advertisement.
### Maybe we could have a look at the outlier(s) ? 

#popularity vs profit
ggplot(data = movies_metadata, mapping = aes(x = popularity, y = profit, color = english, shape = english)) +
  geom_point() +
  stat_smooth(method=lm) +
  facet_wrap(~ english)
### Film around 400 popularity has most likely a HUGE Leverage. We should try plotting without it.
### anyone has insight on how the popularity index is generated ?

#to be commented
#vote average vs popularity
ggplot(data = movies_metadata, mapping = aes(x = vote_average, y = popularity, color = english, shape = english)) +
  geom_point() +
  stat_smooth(method=lm) +
  facet_wrap(~ english)
### no interesting correlation (maybe a small one). Either the voting subsample is not representative, or ...
### ... the popularity index is not primarily determined by votes
#to be commented

#vote count vs popularity
ggplot(data = movies_metadata, mapping = aes(x = vote_count, y = popularity, color = english, shape = english)) +
  geom_point() +
  stat_smooth(method=lm) +
  facet_wrap(~ english)
### Already a higher seeming correlation. Maybe the popularity index takes number of votes/watches into account.

#vote count vs vote average
ggplot(movies_metadata, mapping= aes(vote_count,vote_average, color = english, shape = english)) + 
  geom_point() + 
  stat_smooth(method = lm) + 
  facet_wrap(~ english)
### as expected, for lower counts, pretty even distribution of averages. For higher numbers (2000+), ...
### ... the two variables seem very much correlated. High average ~ High watch rate ~ high vote count ?  

### ideas : 1. runtime related to pop., profit, vote count,...
### 2. date of release related to ...
### 3. some companies (pixar, disney) amass more profit, views,...
### 4.add a variable dealing with profit as a percentage of the initial investment

### 4.
movies_metadata$percentprofit <- movies_metadata$profit/movies_metadata$budget
### biased by outliers ? need good representation



### Working on the genres
j <- which(grepl("Action",movies_metadata$genres)) #contains indices of films that are tagged 'Action'.

### relevant tags : Action, Adventure, Thriller, War, Comedy, Horror, Thriller, Crime, Drama, Animation, Fantasy, Science fiction

movies_metadata$Action=1:length(movies_metadata$genres) ### adds an Action variable for example
temp <- 1
#for (i in 1:length(movies_metadata$genres)) {
#  if (i==j[temp]) {
#    movies_metadata$Action[i] <- "TRUE"
#    temp <- temp + 1
#  }
#  else {
#    movies_metadata$Action[i] <- "FALSE"
#  }
#}
### easier way to do it. example with Adventure
movies_metadata$Action <- grepl("Action",movies_metadata$genres)
movies_metadata$Adventure <- grepl("Adventure",movies_metadata$genres)
movies_metadata$Comedy <- grepl("Comedy",movies_metadata$genres)
movies_metadata$Drama <- grepl("Drama",movies_metadata$genres)
movies_metadata$Thriller <- grepl("Thriller",movies_metadata$genres)

### How to do something relevant with genres ?

#vote_count vs vote average, separated by action
ggplot(movies_metadata, mapping= aes(vote_count,vote_average, color = Action, shape = Action)) + 
  geom_point() + 
  stat_smooth(method = lm) + 
  facet_wrap(~ Action)

# n. of action
count <- which(grepl("Science",movies_metadata$genres))
n <- length(count)
# 318 action, 649 non action

ggplot(data = movies_metadata) + 
  geom_bar(mapping = aes(x = Action, fill = profitable))


#histogram of movies profitable with separation of Action vs non-Action
ggplot(data = movies_metadata) + 
  geom_bar(mapping = aes(x = profitable)) +
  facet_wrap(~ Action)
### more huge profit, less no, less yes.

#plot of vote_count vs profit separated by genres
ggplot(movies_metadata, mapping= aes(vote_count,profit, color = Action, shape = Action)) + 
  geom_point() + 
  stat_smooth(method = lm) + 
  facet_wrap(~ Action)
### pretty much seemingly independent

ggplot(movies_metadata, mapping= aes(vote_count,profit, color = Adventure, shape = Adventure)) + 
  geom_point() + 
  stat_smooth(method = lm) + 
  facet_wrap(~ Adventure)
### more profit overall 

ggplot(movies_metadata, mapping= aes(vote_count,profit, color = Drama, shape = Drama)) + 
  geom_point() + 
  stat_smooth(method = lm) + 
  facet_wrap(~ Drama)
### less profit overall

ggplot(movies_metadata, mapping= aes(vote_count,profit, color =Comedy, shape = Comedy)) + 
  geom_point() + 
  stat_smooth(method = lm) + 
  facet_wrap(~ Comedy)
### less high view counts, so it seems Comedy attracts a niche audience. However, can still generate good profit

ggplot(movies_metadata, mapping= aes(vote_count,profit, color = Thriller, shape = Thriller)) + 
  geom_point() + 
  stat_smooth(method = lm) + 
  facet_wrap(~ Thriller)
### On the contrary, no high profit, but sometimes high audience

