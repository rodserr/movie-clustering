apikey <- '444a1eafcf8399d3a09b9592ad9334b7'
library(jsonlite)
library(rvest)
library(purrr)
library(tidyverse)
library(magrittr)
library(tm)
library(tidytext)
library(factoextra)

imdb_id <- c('nm0637615', # gaspar_noe
             'nm0004716', # darren_aronofsky
             'nm0000040') # stanley_kubrick
             # 'nm0898288', # denis_villeneuve
             # 'nm0230859', # xavier_dolan
             # 'nm0634240', # christopher_nolan
             # 'nm0000095', # woody_allen
             # 'nm0000233', # quentin_tarantino
             # 'nm0327944', # alejandro_gonzalez_inniaritu
             # 'nm0000217', # martin_scorsese
             # 'nm1053530', # tom_ford
             # 'nm0000399', # david_fincher
             # 'nm0001054', # joel_coen
             # 'nm0000186', # david_lynch
             # 'nm0000759', # paul_thomas_anderson
             # 'nm0001885') # lars_von_trier

films_list <- list()

for(i in imdb_id){
  # Get director dbID from imdbID
  url_director <- paste0('https://api.themoviedb.org/3/find/', 
                i, 
                '?api_key=',
                apikey, 
                '&language=en-US&external_source=imdb_id')
  
  director_id <- fromJSON(url_director)
  db_id <- director_id$person_results$id
  
  #Get movies from dbID
  url_films <- paste0('https://api.themoviedb.org/3/person/',
                      db_id, 
                      '/movie_credits?api_key=', 
                      apikey)
  
  film <- fromJSON(url_films)
  
  film_descr <- film$crew %>% 
    filter(job == 'Director') %>% 
    transmute(title, overview, id = i)
  
  films_list %<>% rlist::list.append(film_descr)
}

films_df <- films_list %>% map_dfr(as.data.frame) %>%
  distinct(title, .keep_all = TRUE) %>% 
  mutate(n_words = str_count(overview, '\\w+')) %>% 
  filter(n_words > 15)

toCorpus <- films_df %>% setNames(c('doc_id', 'text'))

films_corpus <- Corpus(DataframeSource(toCorpus)) %>% 
  tm_map(content_transformer(tolower)) %>% 
  tm_map(removePunctuation) %>% 
  tm_map(removeNumbers) %>% 
  tm_map(removeWords, stopwords("english")) %>% 
  tm_map(stemDocument) 
  
films_dtm <- DocumentTermMatrix(films_corpus) 
                                # control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE))
                                # )

dtm_sparse <- removeSparseTerms(films_dtm, 0.975)

films_matrix <- as.matrix(dtm_sparse) 

clust <- kmeans(films_matrix, 4)

reduced_data <- pca$x[,1:20] %>% as.data.frame()
clust <- kmeans(reduced_data, 6)
cluster_result <- data.frame(film = rownames(films_matrix), cluster = clust$cluster)

hc_complete <- hclust(dist(reduced_data), method = "complete")
hc_complete <- hclust (dist(reduced_data), method = "average")
hc_complete <- hclust (dist(reduced_data), method = "single")
# hc_complete$labels[65] <- "Why Try to Escape..."
hc_complete$labels[33] <- "Dr. Strangelove or..."
plot(hc_complete, main = "Complete Linkage", xlab = "", sub = "", cex =.6)
d
# PCA----

pca <- prcomp(films_matrix, scale = FALSE)

p <- pca$x[,1]

pca_ind <- get_pca_ind(pca)

corrplot::corrplot(pca_ind$cos2[,1:7], is.corr=FALSE)

get_eigenvalue(pca)

fviz_eig(pca, addlabels = TRUE, choice = 'variance', main = '')
fviz_eig(pca, addlabels = TRUE, choice = 'eigenvalue')

fviz_cos2(pca, choice = "var", axes = 1:2, top = 20)

fviz_pca_var(pca, 
             col.var = "contrib",
             # select.var = list(contrib = 20),
             # gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             # alpha.var = "contrib",
             repel = TRUE)


res.km <- kmeans(pca_ind$coord, centers = 10, nstart = 25)
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

# Manual----
tokens <- films_df %>% unnest_tokens(word, overview) 

stopwords <- get_stopwords(language= "en", source = "smart") 

words <- anti_join(x = tokens, y = stopwords, by = 'word') %>%  
  filter(str_detect(word,'\\d') == FALSE)

words$stem <- SnowballC::wordStem(words$word, language = 'english')

#Create counter of words
words_count <- count(words, key, stem) %>% bind_tf_idf(stem, key, n) %>%
  left_join(select(BTC_news, key, timestamp), by = 'key')

words_count <- group_by(words_count, stem, timestamp) %>% summarise(tf_idf = mean(tf_idf))

#Diff of Price
var_df <- filter(raw_price, timestamp >= '2018-07-03 12:00:00', timestamp <= '2018-07-27 12:01:00')

var_vector <- var_df$close %>% diff()

#df for model
df <- matrix(ncol = length(var_df$timestamp)-1, nrow = length(stem_vector) + 1) %>% 
  data.frame() %>%
  setNames(var_df$timestamp[-length(var_df$timestamp)])

df[1,] <- var_vector

df[is.na(df)] <- 0

row.names(df) <- c('var',stem_vector)

for (i in 1:length(words_count$stems)){
  
  aux_row <- which(rownames(df) == as.character(words_count[i,'stems']))
  
  aux_col <- which(names(df) == as.character(words_count[i,'timestamp']))
  
  df[aux_row, aux_col] <- as.numeric(words_count[i,'n'])
  
}

#Audit
sum(words_count$n)
sum(df[-1,])
sum(df['arrest',])
sum(filter(words_count,stems=='arrest')$n)

