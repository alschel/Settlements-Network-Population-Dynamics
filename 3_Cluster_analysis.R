# The Impact of Settlements Network Structure on Population Dynamics
# Part 3. Cluster Analysis
# Author: Alexander Sheludkov
# Date: 23 August 2018

library(sp)
library(sf)
library(raster)
library(rgdal)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(igraph)
library(RColorBrewer)
library(tidyr)
library(scales)
library(ggdendro)   # визуализация дендрограмм
library(viridis)


# load the data
load("data/Part1_output.RData")

# ==========================
# 1. Hierarchical Clustering
# ==========================

# 1.1. Построение моделей (Ward Hierarchical Clustering). Анализ дендрограмм

# 1.1.1 Models
fit_2002 <- dist_matrix_2002 %>% 
  as.dist(diag = F) %>% 
  hclust(method="ward.D")

fit_2010 <- dist_matrix_2010 %>% 
  as.dist(diag = F) %>% 
  hclust(method="ward.D")

# 1.1.2. Display dendograms with ggdendro package

# Extract data
# dendro_1990 <- as.dendrogram(fit_1990) %>% dendro_data(type = "rectangle")
dendro_2002 <- as.dendrogram(fit_2002) %>% dendro_data(type = "rectangle")
dendro_2010 <- as.dendrogram(fit_2010) %>% dendro_data(type = "rectangle")

# Bind data and plot the results
dendro_p <-
  bind_rows(
    # segment(dendro_1990) %>% mutate(year = 1990),
                    segment(dendro_2002) %>% mutate(year = 2002),
                    segment(dendro_2010) %>% mutate(year = 2010)) %>% 
  ggplot()+
  geom_segment(aes(x=x, y=y, xend = xend, yend = yend))+
  scale_y_continuous(name = element_blank(), trans = "sqrt")+                                 # трансформируем шкалу y
  scale_x_continuous(name = element_blank())+
  # theme_dendro()+
  theme_classic()+
  # theme(axis.text.y = element_text(angle = 90, hjust = 0.5))+
  facet_grid(.~as.factor(year))

# Save the plot as jpeg file
ggsave(dendro_p, filename = "plots/Dendrograms.png", device = "png", 
       dpi = 1200, width = 7, height = 4)


# =============================
# 2. Optimal number of clusters
# =============================

# See Optimal_number_of_clusters.html
# frey: наверняка 18, 33. Менее точно: 2, 6, 25, 42
# mcclain: 5-6, 18-19. Но очень неточно
# cindex: 6, 18, 33
# silhouette: 9, но очень неточно
# dunn: 2, 19-20

# Итого, мы отбрасываем 33 и 42 как чрезмерно дробные и слабо интерпретируемые. 
# В качестве рабочих приняты варианты кластеризации с 6 и 18 группами.

# ===============================
# 3. Analysis of spatial clusters
# ===============================

# 4.2. Анализ территориальных кластеров

# 4.2.1. Межрайонный уровень

# Сut tree into 3 and 6 clusters
# clust_3_1990 <- cutree(fit_1990, k=3)
clust_3_2002 <- cutree(fit_2002, k=3)
clust_6_2002 <- cutree(fit_2002, k=6)

# Дополним точечный слой данными о принадлежности к кластерам
settlements_2002@data %>% 
  mutate(clust_3 = clust_3_2002, 
         clust_6 = clust_6_2002) -> settlements_2002@data

# Строим карту с 3 кластерами (2002)
subregions_3_2002_plot <- ggplot()+
  # geom_sf(data=st_as_sf(rayons), fill = "white", col = "grey")+
  geom_sf(data = st_as_sf(region), fill = "white")+
  geom_sf(data = st_as_sf(roads_fixed), col = "grey", lwd = 0.5, show.legend = F)+
  geom_sf(data = st_as_sf(settlements_2002), 
          aes(size = Rosstat1990, col = factor(clust_3)), show.legend = F)+
  scale_color_viridis_d(alpha = 0.7)+
  scale_size_continuous(trans = "sqrt", range = c(0.6, 6))+
  theme_minimal()

# Строим карту с 6 кластерами (2002) - черно-белый
clusters_6_2002_plot_grey <- 
  ggplot()+
  geom_sf(data = st_as_sf(region), fill = "white")+
  geom_sf(data = st_as_sf(river_test), col = "grey15", lwd = 0.3)+
  geom_sf(data = st_as_sf(settlements_2002), 
          aes(size = Census2002, shape = factor(clust_6)), alpha = 0.8, show.legend = F)+
  scale_size_continuous(trans = "sqrt", range = c(0.6, 6))+
  theme_minimal()

# сохраним рисунок
ggsave(clusters_6_2002_plot_grey, 
       filename = "plots/Clusters_6_2002_grey.jpeg", device = "jpeg")

# Строим карту с 6 кластерами (2002) - цветной

clusters_6_2002_plot_colored <- ggplot()+
  geom_sf(data = st_as_sf(region), fill = "white")+
  geom_sf(data = st_as_sf(hydr_lines), col = "steelblue4", lwd = 0.2, alpha = 0.4)+
  geom_sf(data = st_as_sf(hydr_polygons), fill = "steelblue4", alpha = 0.4, col = "steelblue4", lwd = 0.3)+
  geom_point(data = data_frame(x = coordinates(settlements_2002)[,1], 
                               y = coordinates(settlements_2002)[,2],
                               clust_6 = settlements_2002$clust_6,
                               Census2002 = settlements_2002$Census2002),
          aes(x = x, y = y, size = Census2002, col = factor(clust_6)), alpha = 0.7, show.legend = F)+
  scale_color_manual(values = inferno(8))+
  scale_size_continuous(trans = "sqrt", range = c(0.3, 3))+
  theme_minimal()

# сохраним рисунок
ggsave(clusters_6_2002_plot_colored, 
         filename = "plots/Clusters_6_2002_colored.jpeg", device = "jpeg")

# 4.2.1. Кластеры районного уровня
clust_18_2002 <- cutree(fit_2002, k = 18)

# Дополним точечный слой данными о принадлежности к кластерам
settlements_2002@data %>% 
  mutate(clust_18 = clust_18_2002) -> settlements_2002@data

# Строим карту с 18 кластерами (2002)
ggplot()+
  geom_sf(data=st_as_sf(rayons), fill = "white", col = "grey")+
  geom_sf(data = st_as_sf(region), fill = "white", alpha = 0.1)+
  # geom_sf(data = st_as_sf(roads_fixed), col = "grey", lwd = 0.3)+
  # geom_sf(data = st_as_sf(rivers), col = "steelblue", lwd = 1)+
  geom_sf(data = st_as_sf(settlements_2002), 
          aes(size = Census2002, col = factor(clust_18)), alpha = 0.6, show.legend = F)+
  # scale_color_viridis_d(alpha = 0.7)+
  scale_size_continuous(trans = "sqrt", range = c(0.4, 4))+
  theme_minimal()

# Чтобы добавить номер кластера на карту, вычислим расопложение географического центра 
# для каждого кластера
settlements_2002@data %>% 
  mutate(x = coordinates(settlements_2002)[,1], 
         y = coordinates(settlements_2002)[,2]) %>% 
  group_by(clust_18) %>% summarise(x_mean = mean(x), y_mean = mean(y)) -> cluster_notations 

# Построим обновленную карту
cluster_18_2002_plot <- ggplot()+
  geom_sf(data=st_as_sf(rayons), fill = "white", col = "grey")+
  geom_sf(data = st_as_sf(region), fill = "white", alpha = 0.1)+
  geom_sf(data = st_as_sf(settlements_2002), 
          aes(size = Census2002, col = factor(clust_18)), alpha = 0.6, show.legend = F)+
  scale_size_continuous(trans = "sqrt", range = c(0.6, 6))+
  geom_text(data = cluster_notations,
            aes(label = clust_18, x = x_mean, y = y_mean), show.legend = F)+
  theme_minimal()+
  theme(axis.title = element_blank())


ggsave(cluster_18_2002_plot, filename = "plots/cluster_18_2002.jpeg", device = "jpeg")

# На карте вылез Байкаловский район, которого не существует уже почти 70 лет (!)
# вот она инерция сети

# Save the results to Rdata file
save.image("data/Part2_output.RData")
