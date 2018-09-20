# Defining optimal number of clusters
# Author: Mikhail Orlov

#clearence
rm(list = ls())

#loading data
load('data/matrix_2002.Rdata')

#exploring the data
str(dist_matrix_2002)
class(dist_matrix_2002)

#coercing to dist object
dist_2002 <- as.dist(dist_matrix_2002)
class(dist_2002)
str(dist_2002)
dim(dist_2002)

#heirarchical clusterization

hclusted <- hclust(dist_2002, method = 'ward.D2')
plot(hclusted, labels = F)
rect.hclust(hclusted, k = 7) #just an example

#optimal number of clusters
library(NbClust)
methods <- c('frey', 'mcclain', 'cindex', 'silhouette', 'dunn')
#list of methods that would take dist object without the initial data
library(NbClust)
suggested_ks <- c()
for(i in methods){
  tmp <- NbClust(diss = dist_2002, distance = NULL, min.nc = 2, max.nc = 50, method = 'ward.D2', index = i)
  suggested_ks <- c(suggested_ks, as.integer(tmp$Best.nc['Number_clusters']))
}

#enchanced dendrogram visualisation and comparison
library(dendextend)
hclusted %>% as.dendrogram %>% set('labels_cex', 1e-9) -> dend #the only way I know to make labels invisible here

#colors by clusters
dend %>% color_branches(k = 18) %>% plot

#coloring leaves accroding to some other feature
#random vector
colvec <- c(rep(1, 500), rep(2, 500), rep(3,500), rep(4, 500))
dend %>% set('branches_col', colvec) %>% plot

colvec <- sample(1:2500, size = 2500)
dend %>% set('branches_col', colvec) %>% plot

#comparing two different methods clusterization - are they robust?

hclusted <- hclust(dist_2002, method = 'ward.D2')

hclusted1 <- hclust(dist_2002, method = 'centroid')

hclusted %>% as.dendrogram %>% set('labels_cex', 1e-9) -> dend 
hclusted1 %>% as.dendrogram %>% set('labels_cex', 1e-9) -> dend1

#tanglegram - dendrograms are plotted one against other
tanglegram(dend, dend1)
#most common coefficient for dendrogram comparison
cor_cophenetic(dend, dend1)
