
tmdb_5000_movies <- read_csv("tmdb-5000-movie-dataset/tmdb_5000_movies.csv")

tmdb_5000_credits <- read_csv("tmdb-5000-movie-dataset/tmdb_5000_credits.csv")

p <- tmdb_5000_credits[3978:3980,] 

p2 <- p %>% mutate(director = as.character(map(crew, .f = getDirector)))

getDirector <- function(x){
  
  director <- x %>% 
    fromJSON() %>%
    filter(job == 'Director') %>% 
    select(name) %>% 
    pull()
  
  if( length(director) > 1 ){
   director <- director[1]
  }
  
  return(director)
  
}



p2 <- tmdb_5000_credits %>% mutate(director = map(crew, .f = getDirector))
