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

# Directors----
imdb_id <- c('nm0637615', # gaspar_noe
             'nm0004716', # darren_aronofsky
             'nm0000040', # stanley_kubrick
             'nm0898288', # denis_villeneuve
             'nm0230859', # xavier_dolan
             'nm0634240', # christopher_nolan
             # 'nm0000095', # woody_allen
             'nm0000233', # quentin_tarantino
             'nm0327944', # alejandro_gonzalez_inniaritu
             'nm0000217', # martin_scorsese
             # 'nm1053530', # tom_ford
             'nm0000399', # david_fincher
             'nm0001054', # joel_coen
             'nm0000186', # david_lynch
             'nm0000759', # paul_thomas_anderson
             'nm0001885') # lars_von_trier

director <- c('gaspar_noe', 
               'darren_aronofsky', 
               'stanley_kubrick',
               'denis_villeneuve',
               'xavier_dolan',
               'christopher_nolan',
               # 'woody_allen',
               'quentin_tarantino',
               'alejandro_gonzalez_inniaritu',
               'martin_scorsese',
               # 'tom_ford',
               'david_fincher',
               'joel_coen',
               'david_lynch',
               'paul_thomas_anderson',
               'lars_von_trier')

directors <- data_frame(director = director, id = imdb_id)

# API connection to get plots----
films_list <- list()

for(i in imdb_id){
  # Get director dbID from imdbID
  .url_director <- paste0('https://api.themoviedb.org/3/find/', 
                i, 
                '?api_key=',
                .apikey, 
                '&language=en-US&external_source=imdb_id')
  
  .director_id <- fromJSON(.url_director)
  .db_id <- .director_id$person_results$id
  
  #Get movies from dbID
  .url_films <- paste0('https://api.themoviedb.org/3/person/',
                      .db_id, 
                      '/movie_credits?api_key=', 
                      .apikey)
  
  .film <- fromJSON(.url_films)
  
  .film_descr <- .film$crew %>% 
    filter(job == 'Director') %>% 
    transmute(title, overview, id = i,
              vote_count)
  
  films_list %<>% rlist::list.append(.film_descr)
}

films_db <- films_list %>% map_dfr(as.data.frame) %>%
  distinct(title, .keep_all = TRUE) %>% 
  mutate(n_words = str_count(overview, '\\w+'),
         title = ifelse(str_count(title, '\\w+') > 6,
                        str_trunc(title, 25),
                        title)
         ) %>% 
  filter(n_words > 15, vote_count > 15) %>% 
  left_join(directors, by = 'id') %>% 
  filter(director != 'lars_von_trier') %>% 
  filter(director == 'martin_scorsese') %>% 
  filter(!(title %in% c('Mulholland Dr.', 'Twin Peaks', 'Twin Peaks: The Missing Pieces',
                        'Shine a Light', 'George Harrison: Livin...', 'No Direction Home: Bob Dylan')))
  
quant_dir <- films_db %>% group_by(director) %>%
  summarise(vote_count_q = quantile(vote_count, .15))

films_db %<>% left_join(quant_dir, by = 'director') %>% 
  filter(vote_count > vote_count_q)

films_db %>% ggplot(aes(x = director)) +
  geom_bar() + 
  coord_flip()

# Text Minning----
toCorpus <- films_db %>% setNames(c('doc_id', 'text'))

remove_dir <- strsplit(director, "_") %>% unlist()
not_useful_words <- c('film', 'find', 'one', 'two', 'take', 'make', '-',
                      'will', 'set', 'can', 'get', 'tell', 'come', 'becom')

# Synonyms
synonyms <- list(
  list(word = "kill", syns = c("kill", "killer", 'murder')),
  list(word = "life", syns = c("life", "live")),
  list(word = "crime", syns = c("crimin", "crime")),
  list(word = "love", syns = c("love", "lover"))
)

replaceSynonyms <- content_transformer(function(x, syn=NULL) { 
  Reduce(function(a,b) {
    gsub(paste0("\\b(", paste(b$syns, collapse="|"),")\\b"), b$word, a)}, syn, x)   
})
# Apply

films_corpus <- Corpus(DataframeSource(toCorpus)) %>% 
  tm_map(removePunctuation) %>%
  tm_map(content_transformer(tolower)) %>% 
  tm_map(removeNumbers) %>% 
  tm_map(removeWords, stopwords("english")) %>% 
  tm_map(removeWords, remove_dir) %>% 
  tm_map(stemDocument) %>% 
  tm_map(removeWords, not_useful_words) %>% 
  tm_map(replaceSynonyms, synonyms)
  
films_dtm <- DocumentTermMatrix(films_corpus, 
                                # control = list(weighting = function(x) weightBin(x)))
                                # control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE)))
                                control = list(weighting = function(x) weightTf(x)))

dtm_sparse <- removeSparseTerms(films_dtm, 0.999)
findFreqTerms(dtm_sparse, lowfreq = 5) %>% sort()


# films_df <- films_matrix %>% as.data.frame() %>% apply(2, FUN = function(x) as.factor(x)) %>% as.data.frame()

# LDA----
lda <- topicmodels::LDA(dtm_sparse, k = 3, 
                        control = list(estimate.alpha = FALSE, alpha = 2, 
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
  coord_flip() +
  scale_fill_manual(values = wes_palette(n = 6, name = "GrandBudapest1", 
                                          type = 'continuous'))
  
lda_documents <- tidy(lda, matrix = "gamma") %>% 
  left_join(films_db[,c('title', 'director')], by = c('document' = 'title'))

# Alpha
lda_documents %>% group_by(topic) %>%
  top_n(15, gamma) %>%
  ungroup() %>% 
  ggplot(aes(x = factor(topic), y = gamma, label = document, colour = director)) + 
  geom_text()

# All 
arrangePlot <- function(df){
  count_topic <- df %>% 
    count(cluster)
  
  aux_topic <- c()
  for(c in count_topic$n){
    aux_s <- seq(1, c, 1)
    
    aux_topic <- c(aux_topic, aux_s)
  }
  
  df %<>% arrange(cluster, director, title) %>% 
    cbind(aux_topic)
  
  return(df)
}

lda_documents %>% arrange(document, desc(gamma)) %>% 
  distinct(document, .keep_all = TRUE) %>% 
  arrangePlot() %>% 
  ggplot(aes(x = factor(topic), y = aux_topic, label = document, colour = director)) + 
  geom_text()

# Directors
lda_documents %>% ggplot(aes(x = director, fill = factor(topic))) + 
  geom_bar(position = 'fill') +
  coord_flip() 

# 2D
lda_documents %>% spread(topic, gamma) %>% 
  ggplot(aes(x = `2`, y = `4`, label = document)) +
  geom_text()

# CheckOut
films_db %>% filter(title == 'Kill Bill: Vol. 2') %>% select(overview)

tidy(dtm_sparse) %>%
  filter(document == 'Eraserhead') %>%
  arrange(desc(count))

d
# distances----

films_matrix <- lda_documents %>% 
  spread(topic, gamma)

rownames(films_matrix) <- films_matrix$document
films_matrix %<>% select(-one_of(c('document', 'director'))) %>% as.matrix()
dist.matrix <- proxy::dist(films_matrix, method = "cosine", pairwise = TRUE)

# K-means----

fviz_nbclust(films_matrix, kmeans, method = 'wss') 

km_clust <- kmeans(films_matrix, 3, 25)

aggregate(films_matrix, by = list(cluster = km_clust$cluster), mean)

cbind(films_matrix, cluster = km_clust$cluster)

km_clust$size

cluster_result <- data.frame(title = as.character(rownames(films_matrix)),
                             cluster = as.factor(km_clust$cluster)) %>% 
  left_join(films_db[,c('title', 'director')], by = 'title')

  

cluster_result %>% arrangePlot() %>% 
  ggplot(aes(x = cluster, y = aux_topic, label = title, colour = director)) + 
  geom_text(check_overlap = TRUE)

fviz_cluster(km_clust, films_matrix, repel = TRUE)


# PAM----
fviz_nbclust(films_matrix, cluster::pam, method = 'silhouette') 

dist.matrix <- proxy::dist(films_matrix, method = "cosine", pairwise = TRUE)

pam_clust <- cluster::pam(dist.matrix, 3, diss = TRUE)

clusters_pam <- data.frame(title = as.character(rownames(films_matrix)),
                           cluster = as.factor(pam_clust$clustering)) %>% 
  left_join(films_db[,c('title', 'director')], by = 'title')

clusters_pam %>% arrangePlot() %>% 
  ggplot(aes(x = cluster, y = aux_topic, label = title, colour = director)) + 
  geom_text()

fviz_cluster(pam_clust, films_matrix, repel = TRUE)

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
    tm_map(content_transformer(tolower)) %>% 
    tm_map(removeNumbers) %>% 
    tm_map(removeWords, stopwords("english")) %>% 
    tm_map(removeWords, .remove_dir) %>% 
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

recommender('Batman', '1989', showTerms = TRUE)

# PCA----

pca <- prcomp(dist.matrix, scale = FALSE)

p <- pca$x[,1]

pca_ind <- get_pca_ind(pca)
get_eigenvalue(pca)

fviz_eig(pca, addlabels = TRUE, choice = 'variance', main = '')
fviz_cos2(pca, choice = "var", axes = 1:2, top = 20)

fviz_pca_var(pca, 
             col.var = "contrib",
             # select.var = list(contrib = 20),
             # gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             # alpha.var = "contrib",
             repel = TRUE)


res.km <- kmeans(pca_ind$coord, centers = 5, nstart = 25)
grp <- as.factor(res.km$cluster)
fviz_pca_ind(pca, 
             col.ind = grp, #'contrib'
             geom = 'text',
             # gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             # palette = c("#0073C2FF", "#EFC000FF", "#868686FF"),
             legend.title = "Cluster",
             repel = TRUE)

# Contributions of variables to PC1
fviz_contrib(pca, choice = "var", axes = 1, top = 30)
