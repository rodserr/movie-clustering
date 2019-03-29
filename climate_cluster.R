# Libraries-----
library(tidyverse)
library(janitor)
library(magrittr)
library(caret)
library(factoextra)
library(gridExtra)
library(NbClust)
library(cluster)

# Data Prep----
data_sample <- read_csv('climate-data-sample.csv') %>% clean_names()

first_winter_median <- median(data_sample$date_first_winter_freeze)
last_winter_median <- median(data_sample$date_last_winter_freeze)

data_sample %<>% mutate(first_winter_diff = as.integer(first_winter_median - date_first_winter_freeze),
                        last_winter_diff = as.integer(last_winter_median - date_last_winter_freeze)) %>% 
  select(-one_of('date_first_winter_freeze', 'date_last_winter_freeze' ))

which(is.na(data_sample), arr.ind=TRUE)

data_sample[1330, 50] <- data_sample[1329, 50]

# PCA Analisys----

prec_var <- which(str_detect(colnames(data_sample), 'prec'))
rain_var <- which(str_detect(colnames(data_sample), 'rain'))
snow_var <- which(str_detect(colnames(data_sample), 'snow'))
temp_var <- which(str_detect(colnames(data_sample), 'temp'))


pca_prec <- prcomp(data_sample[prec_var], scale = TRUE)
pca_rain <- prcomp(data_sample[rain_var], scale = TRUE)
pca_snow <- prcomp(data_sample[snow_var], scale = FALSE)
pca_temp <- prcomp(data_sample[temp_var], scale = TRUE)

list_scree_plot <- list(fviz_eig(pca_prec, addlabels = TRUE, choice = 'variance', 
              main = 'Precipitation Effect'),
     fviz_eig(pca_rain, addlabels = TRUE, choice = 'variance', 
              main = 'Rain Effect'),
     fviz_eig(pca_snow, addlabels = TRUE, choice = 'variance', 
              main = 'Snow Effect'),
     fviz_eig(pca_temp, addlabels = TRUE, choice = 'variance', 
              main = 'Temp Effect')
)

grid.arrange(grobs = list_scree_plot)

reduced_data <- data.frame(prec_1 = pca_prec$x[,1], prec_2 = pca_prec$x[,2], prec_3 = pca_prec$x[,3],
                              rain_1 = pca_rain$x[,2], rain_2 = pca_rain$x[,2], rain_3 = pca_rain$x[,3],
                              snow = pca_snow$x[,1],
                              temp = pca_temp$x[,1]) %>% 
  cbind(data_sample[,c('growing_season_length', 'first_winter_diff', 'last_winter_diff')]) %>% 
  lapply(scale) %>% 
  as.data.frame()


# Clustering-----

stores_map <- data_sample[,c('longitude', 'latitude')]

ggplot() + geom_polygon(data = map_data("usa"), aes(x=long, y = lat, group = group), fill = NA, col = 'black') + 
  coord_fixed(1.3) +
  geom_point(data = data_sample, aes(x = longitude, y = latitude), color = "red", size = 1)

# Kmean

kmresult <- rep(0, 15)
for(i in 2:15){
  .km <- reduced_data %>% kmeans(centers = i, nstart = 25, iter.max = 25)
  
  kmresult[i] <- .km$betweenss/.km$totss
}
plot(kmresult, type = 'b', col = 'blue')

fviz_nbclust(reduced_data, kmeans, method = "wss", k.max = 12)
fviz_nbclust(reduced_data, kmeans, method = "silhouette", k.max = 12)
gap_stat <- clusGap(reduced_data, FUN = kmeans, nstart = 25, K.max = 18, B = 50, verbose = TRUE)
fviz_gap_stat(gap_stat)


# Hierarchical Clustering
hc_complete <- hclust (dist(reduced_data), method = "complete")
hc_complete <- hclust (dist(reduced_data), method = "average")
hc_complete <- hclust (dist(reduced_data), method = "single")

plot(hc_complete, main = "Complete Linkage", xlab = "", sub = "", cex =.9)


# Visualization 
kmeanfit <- reduced_data %>% kmeans(centers = 15, nstart = 25, iter.max = 25)
kmeanfit$centers %>% as.data.frame() %>% names()

barplot(t(kmeanfit$centers), beside = TRUE, col = heat.colors(11),legend=FALSE,
        args.legend=list(cex = 0.8))

clustered_data <- data_sample %>% cbind(cluster = as.character(kmeanfit$cluster))
ggplot() + 
  geom_polygon(data = map_data("usa"), aes(x=long, y = lat, group = group), fill = NA, col = 'black') + 
  coord_fixed(1.3) +
  geom_point(data = clustered_data, aes(x = longitude, y = latitude, col = cluster), size = 2)


# Another try w/ less PC
kmeanfit_PC <- reduced_data %>% select(-one_of('prec_2', 'prec_3', 'rain_2', 'rain_3')) %>%
  kmeans(centers = 6, nstart = 25, iter.max = 25)

kmeanfit_PC$centers %>% as.data.frame() %>% names()
barplot(t(kmeanfit_PC$centers), beside = TRUE, col = heat.colors(11),legend = TRUE,
        args.legend = list(cex = 0.3, horiz = TRUE))

clustered_data_PC <- data_sample %>% cbind(cluster = as.character(kmeanfit_PC$cluster))
ggplot() + 
  geom_polygon(data = map_data("usa"), aes(x=long, y = lat, group = group), fill = NA, col = 'black') + 
  coord_fixed(1.3) +
  geom_point(data = clustered_data_PC, aes(x = longitude, y = latitude, col = cluster), size = 2)
