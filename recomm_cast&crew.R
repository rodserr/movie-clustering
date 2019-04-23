# Libraries------
.apikey <- '444a1eafcf8399d3a09b9592ad9334b7'
.omdbkey <- '9e417600'
library(jsonlite)
library(rvest)
library(purrr)
library(tidyverse)
library(magrittr)
library(tm)
library(topicmodels)
library(tidytext)
library(factoextra)
library(FactoMineR)
library(wesanderson)

# Read data & Functions-----
tmdb_5000_movies <- read_csv("tmdb-5000-movie-dataset/tmdb_5000_movies.csv")

tmdb_5000_credits <- read_csv("tmdb-5000-movie-dataset/tmdb_5000_credits.csv")

getDirector <- function(x){
  
  if(x == '[]'){ return(NA_character_) }
  
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

getCast <- function(x){
  
  if(x == '[]'){ return(NA_character_) }
  
  cast <- x %>% 
    fromJSON() %>%
    select(name) %>% 
    head(5) %>% 
    pull()
  
  return(cast)
  
}

getGenres <- function(x){
  
  if(x == '[]'){ return(NA_character_) }
  
  genres <- x %>% 
    fromJSON() %>%
    select(name) %>%
    pull()
  
  return(genres)
  
}

cleanNames <- function(x){
  
  x %<>% str_remove_all('[:space:]') %>% 
    str_replace_all(',', ' ') %>%
    str_remove_all('^c') %>% 
    str_remove_all('[:punct:]')

  return(x)
}

# Cast, Genres & Director Only-----

cast_director <- tmdb_5000_credits %>% transmute(movie_id, 
                                                 cast = cleanNames(as.character(map(cast, .f = getCast))),
                                                 director = cleanNames(as.character(map(crew, .f = getDirector))))

genres <- tmdb_5000_movies %>% 
  transmute(movie_id = id, original_title, genres = cleanNames(as.character(map(genres, .f = getGenres))))

movie_cdg <- full_join(cast_director, genres, by = 'movie_id') %>%
  na.omit() %>% 
  distinct(original_title, .keep_all = TRUE) %>% 
  transmute(doc_id = original_title, text = paste(director, cast, genres)) %>% as.data.frame()

corpusPlots <- Corpus(DataframeSource(movie_cdg))

plotsDTM <- DocumentTermMatrix(corpusPlots, 
                               # control = list(weighting = function(x) weightBin(x)))
                               # control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE))) %>%
                               control = list(weighting = function(x) weightTf(x)))

system.time(
  dfMatrix <- plotsDTM %>%
    as.matrix() %>%
    # .[,1:1000] %>% 
    # t() %>%
    proxy::dist(method = "cosine") %>% 
    as.matrix()
)

movieNames <- row.names(dfMatrix)
excludeOwn <- which(movieNames == 'Mars Attacks!')
recom <- dfMatrix[excludeOwn,-excludeOwn] %>% order() %>% head(5)

row.names(dfMatrix[recom,])

# Content & Metadata----
cast_director <- tmdb_5000_credits %>% transmute(movie_id, 
                                                 cast = cleanNames(as.character(map(cast, .f = getCast))),
                                                 director = cleanNames(as.character(map(crew, .f = getDirector))))

genres_plot <- tmdb_5000_movies %>% 
  transmute(movie_id = id, original_title,
            genres = cleanNames(as.character(map(genres, .f = getGenres))),
            plot = overview)

moviedb <- full_join(cast_director, genres_plot, by = 'movie_id') %>%
  na.omit() %>% 
  distinct(original_title, .keep_all = TRUE) 

    # Content-----
content <- moviedb %>% select(doc_id = original_title, text = plot) %>% as.data.frame()

not_useful_words <- c("make", "find", 'set', '-', 'tell', 'becom', 'film', "–")

synonyms <- list(
  list(word = "kill", syns = c("kill", "killer", 'murder')),
  list(word = "dead", syns = c("dead", "death")),
  list(word = "life", syns = c("life", "live")),
  list(word = "crime", syns = c("crimin", "crime")),
  list(word = "love", syns = c("love", "lover"))
)

replaceSynonyms <- content_transformer(function(x, syn=NULL) { 
  Reduce(function(a,b) {
    gsub(paste0("\\b(", paste(b$syns, collapse="|"),")\\b"), b$word, a)}, syn, x)   
})

contentCorpus <- Corpus(DataframeSource(content)) %>% 
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>% 
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords("SMART")) %>%
  tm_map(stripWhitespace) %>% 
  tm_map(stemDocument) %>% 
  tm_map(removeWords, not_useful_words) %>% 
  tm_map(replaceSynonyms, synonyms)

contentDTM <- DocumentTermMatrix(contentCorpus, 
                               control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE))) %>%
  removeSparseTerms(0.999)

contentMatrix <- contentDTM %>%
    as.matrix()


    # Metadata-----
metadata <- moviedb %>% transmute(doc_id = original_title, text = paste(director, cast, genres)) %>% as.data.frame()

metadataCorpus <- Corpus(DataframeSource(metadata))

metadataDTM <- DocumentTermMatrix(metadataCorpus, 
                                  control = list(tolower = FALSE, weighting = function(x) weightTf(x)))

metadataMatrix <- metadataDTM %>%
  as.matrix()

    # Merge------
recomMatrix <- merge(metadataMatrix, contentMatrix, by = "row.names") 
recomMatrix2 <- as.matrix(recomMatrix[-1])
rownames(recomMatrix2) <- recomMatrix[,1]

system.time(
distanceMatrix <- recomMatrix2 %>% 
  proxy::dist(method = "cosine") %>% 
  as.matrix()
)

moviedb %>% filter(director == 'LillyWachowski') %>% select(original_title)

movieNames <- row.names(distanceMatrix)
excludeOwn <- which(movieNames == "Spectre")
recom <- distanceMatrix[excludeOwn,-excludeOwn] %>% order() %>% head(5)

row.names(distanceMatrix[recom,])

# Hybrid-----

hibryd <- moviedb %>% transmute(doc_id = original_title, text = paste(director, cast, genres, plot)) %>% as.data.frame()

hibrydCorpus <- Corpus(DataframeSource(hibryd)) %>% 
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>% 
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords("SMART")) %>%
  tm_map(stripWhitespace) %>% 
  tm_map(stemDocument) %>% 
  tm_map(removeWords, not_useful_words) %>% 
  tm_map(replaceSynonyms, synonyms)

hibrydDTM <- DocumentTermMatrix(hibrydCorpus, 
                                 control = list(weighting = function(x) weightTf(x))) %>%
  removeSparseTerms(0.999)

hibrydMatrix <- hibrydDTM %>%
  as.matrix() %>% 
  proxy::dist(method = "cosine") %>% 
  as.matrix()

moviedb %>% filter(director == 'LillyWachowski') %>% select(original_title)

movieNames <- row.names(hibrydMatrix)
excludeOwn <- which(movieNames == "Avatar")
recom <- distanceMatrix[excludeOwn,-excludeOwn] %>% order() %>% head(5)

row.names(hibrydMatrix[recom,])
