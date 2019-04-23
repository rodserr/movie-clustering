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

# Read data-----
tmdb_5000_movies <- read_csv("tmdb-5000-movie-dataset/tmdb_5000_movies.csv")

tmdb_5000_credits <- read_csv("tmdb-5000-movie-dataset/tmdb_5000_credits.csv")


plotdb <- tmdb_5000_movies %>% 
  select(doc_id = original_title, text = overview) %>%
  na.omit() %>% 
  distinct(doc_id, .keep_all = TRUE) %>% 
  as.data.frame()

not_useful_words <- c("make", "find", 'set', '-', 'tell', 'becom', 'film', "–")

# Synonyms
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

corpusPlots <- Corpus(DataframeSource(plotdb)) %>% 
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>% 
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords("SMART")) %>%
  tm_map(stripWhitespace) %>% 
  tm_map(stemDocument) %>% 
  tm_map(removeWords, not_useful_words) %>% 
  tm_map(replaceSynonyms, synonyms)

plotsDTM <- DocumentTermMatrix(corpusPlots, 
                                # control = list(weighting = function(x) weightBin(x)))
                                # control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE))) %>%
                                control = list(weighting = function(x) weightTf(x))) %>%
  removeSparseTerms(0.999999)

findFreqTerms(plotsDTM, lowfreq = 80) %>% sort()

lda <- topicmodels::LDA(plotsDTM, k = 5, 
                        control = list(estimate.alpha = FALSE, alpha = 5, 
                                       seed = 1234))

lda_topics <- tidy(lda, matrix = "beta")

lda_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta) %>% 
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() 
# + scale_fill_manual(values = wes_palette(n = 6, name = "GrandBudapest1", type = 'continuous'))

lda_documents <- tidy(lda, matrix = "gamma")

# TF-IDF----

plotsDTM <- DocumentTermMatrix(corpusPlots, 
                               # control = list(weighting = function(x) weightBin(x)))
                               control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE))) %>%
  # control = list(weighting = function(x) weightTf(x))) %>%
  removeSparseTerms(0.999)

system.time(
  tfidfMatrix <- plotsDTM %>%
    as.matrix() %>%
    # .[,1:1000] %>% 
    # t() %>%
    proxy::dist(method = "cosine") %>% 
    as.matrix()
)

movieNames <- row.names(tfidfMatrix)
excludeOwn <- which(movieNames == 'Batman')
recom <- tfidfMatrix[excludeOwn,-excludeOwn] %>% order() %>% head(5)

row.names(tfidfMatrix[recom,])
# distances----
fviz_nbclust(tfidfMatrix, cluster::pam, method = 'silhouette') 

pam_clust <- cluster::pam(tfidfMatrix, 3, diss = TRUE)


# Predict----
recommender <- function(movieTitle, movieRyear, lengthPlot = 'short', nRecomm = 3, showTerms = FALSE,
                        .remove_dir = remove_dir, .not_useful_words = not_useful_words,
                        .synonyms = synonyms, LDAmodel = lda, dtmMatrix = films_matrix){
  
  url <- paste0('http://www.omdbapi.com/?apikey=', .omdbkey, '&t=', movieTitle,
                '&y=', movieRyear, '&plot=', lengthPlot)
  
  movieTest <- fromJSON(url)
  
  PlotTest <- data_frame(doc_id = movieTitle, text = movieTest$Plot)
  
  dtmTest <- Corpus(DataframeSource(PlotTest)) %>% 
    tm_map(removePunctuation) %>%
    tm_map(removeNumbers) %>%
    tm_map(content_transformer(tolower)) %>% 
    tm_map(removeWords, stopwords("SMART")) %>% 
    # tm_map(removeWords, .remove_dir) %>% 
    tm_map(stripWhitespace) %>% 
    tm_map(stemDocument) %>% 
    tm_map(removeWords, .not_useful_words) %>% 
    tm_map(replaceSynonyms, .synonyms) %>% 
    DocumentTermMatrix(control = list(weighting = function(x) weightTf(x)))
  
  testModel <- posterior(LDAmodel, dtmTest)
  
  testMatrix <- dtmMatrix %>% rbind(testModel$topic) 
  
  distMatrix <- testMatrix %>% proxy::dist(method = "cosine") %>% as.matrix()
  
  len <- nrow(distMatrix)
  
  recommends <- distMatrix[len,-len] %>% order() %>% head(nRecomm)
  
  moviesRecomm <- row.names(distMatrix[recommends,])
  
  if(showTerms){
    terms <- tidy(dtmTest)$term
    
    return(list(recomms = moviesRecomm, movieTerms = terms))
    
  }
  
  return(moviesRecomm) 
  
}

recommender('Batman', '1989', nRecomm = 5, showTerms = TRUE)

tidy(dtmSparse) %>%
  filter(document == 'Batman') %>%
  arrange(desc(count))
