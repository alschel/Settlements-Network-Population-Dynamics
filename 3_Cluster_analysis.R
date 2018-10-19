# An Impact of Settlement Network Structure on Population Dynamics
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
library(leaflet)
library(RColorBrewer)
library(tidyr)
library(scales)
library(ggdendro)   # визуализация дендрограмм
library(viridis)


# load the data
load("data/Part1_output.RData")

# Для статьи мы пока будем использовать только данные за 2002 и 2010 гг.

# ==========================
# 1. Hierarchical Clustering
# ==========================

# 1.1. Построение моделей (Ward Hierarchical Clustering). Анализ дендрограмм

# 1.1.1 Models
# d_1990 <- as.dist(dist_matrix_1990, diag = F)
d_2002 <- as.dist(dist_matrix_2002, diag = F)
d_2010 <- as.dist(dist_matrix_2010, diag = F)

# fit_1990 <- hclust(d_1990, method="ward.D") 
fit_2002 <- hclust(d_2002, method="ward.D") 
fit_2010 <- hclust(d_2010, method="ward.D") 

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

# ==================================
# 3. Рассчет переменных по кластерам
# ==================================

# ==================
# 3.1. Предобработка

# Сохраним данные в новую переменную и очистим от лишних столбцов
df <- settlements_2002@data
df %>% 
  select(-cohort1981, -cohort1990, -cohort2002, 
         -trend_1981to1990, -trend_1990to2002, 
         -rel1981to1990, -rel1990to2002, -rel2002to2010) -> df

# =======================================================
# 3.2. Численность населения и ее динамика в 2002-2010 гг.

df %>% 
  group_by(clust_18) %>% 
  mutate(pop2002 = sum(Census2002), pop2010 = sum(Census2010),
         pop2010to2002_rel = pop2010/pop2002*100,         # отношение населения в 2010 году к населению в 2002
         max_pop2002 = max(Census2002),                       # величина крупнешего н.п.
         mean_pop2002 = mean(Census2002),                     # средний размер н.п.
         median_pop2002 = median(Census2002),                 # медианный размер н.п.
         sum_pop2002 = sum(Census2002)) %>%                   # сумма населения кластера
  select(clust_6, clust_18, pop2002, pop2010, pop2010to2002_rel, mean_pop2002, median_pop2002, max_pop2002, sum_pop2002) %>% 
  unique() -> clusters_18_metrics                         # Сохраним результат в новый data.frame

# ==================================================
# 3.2. Вариация в распределении населения между н.п

# Создадим переменные
clusters_18_metrics$variation_2002 <- NA_real_
clusters_18_metrics$variation_2010 <- NA_real_

for (i in 1:nrow(clusters_18_metrics)) {
  
  # Define logical vector to subset observations by the cluster
  select_condition <- clust_18_2002 == i
  settlements_temp <- df[select_condition,]

  # Calculate variation (standart deviation(x)/mean(x))
  # 2002
  clusters_18_metrics[clusters_18_metrics$clust_18 == i,]$variation_2002 <- 
    sd(settlements_temp$Census2002, na.rm = T)/mean(settlements_temp$Census2002, na.rm = T)
  # 2010
  clusters_18_metrics[clusters_18_metrics$clust_18 == i,]$variation_2010 <- 
    sd(settlements_temp$Census2010, na.rm = T)/mean(settlements_temp$Census2010, na.rm = T)
}

# Calculate the difference in variance between 2002 and 2010 (темпы сжатия расселения)
clusters_18_metrics %>%
  mutate(variation_dif = variation_2010/variation_2002*100) -> 
  clusters_18_metrics


# 3.2.1. Quick explorative analysis

# Темпы сжатия расселения vs общая динамика населения
clusters_18_metrics %>% 
  ggplot(aes(y=variation_dif, x=pop2010to2002_rel))+
  geom_point(aes(size = mean_pop))+
  # geom_smooth(method = "glm")+
  scale_size_continuous(name = "Ср. размер\nн.п. (чел.)",
                        breaks = c(0, 300, 500, 1000, 2000), trans = "sqrt", 
                        labels = c("<300", "300-499", "500-999", "1000-2000", ">8000"))+
  scale_y_continuous(name = "Динамика территориальной\nдифференциации расселения", breaks = seq(100, 115, 5),
                     limits = c(100,115))+
  scale_x_continuous(name = "Динамика населения (%)")

# Темпы сжатия расселения vs средний размер населенных пунктов
clusters_18_metrics %>% 
  ggplot()+
  geom_point(aes(y=mean_pop, x=variation_dif))+
  geom_smooth(aes(y=mean_pop, x=variation_dif), method = "glm")+
  scale_y_continuous(trans = "log")+
  scale_x_continuous(name = "Изменение вариации")

# Сжатие расселения наблюдается везде, но его траектория разная: есть две группы районов: 
# 1 группа: низкие темпы сжатия на фоне роста или незначительного сокращения населения
# 2 группа: высокие темпы сжатия на фоне общего значительного сокращения населения


# ============================================
# 3.3. Функция для выборки subgraphs из графа, созданного методом shp2graph

# У нас нестандартная структура графа и некоторые функции igraph с ним не работают
# В частности из-за несовпадения числа вершин и числа реальных н.п. induced_subgraph()
# возвращает набор несвязанных точек: все перекрестки, дорожные развязки "опускаются", и
# граф теряет связность.

# Создадим собственную функцию, которая будет принимать на вход граф (@graph), 
# вектор индексов вершин-н.п. (@nodes) и возвращать subgraph без потерь

my_subgraph_function <- function(graph, nodes) {
  
  # 1) сохраним в отдельный вектор номера всех вершин, лежащих между н.п.
  # shortest_paths() возвращает именованный list длины @to, 
  # который содержит индексы всех вершин и ребер каждого пути
  all_the_verticies <- 
    shortest_paths(graph = graph,        # igraph object
                   from = nodes,         # vertex ids from
                   to = nodes) %>%       # vertex ids to
    .$vpath %>%                          # extract list of returned vertex ids                               
    unlist()                             # unlist
  
  # 2) выборка из графа
  induced_subgraph(graph = graph,                        # igraph object
                   vids = all_the_verticies) %>%         # vertex ids 
    simplify() ->                                        # remove loop and multiple edges
    sub_graph
  
  return(sub_graph)
}

# =================================
# 3.4. Централизация/связность сети

# Для оценки связности сети в сетевой анализе используется понятие "connectivity". Оно показывает,
# сколько вершин или ребер нужно удалить, чтобы разбить граф на части. Однако при условии, что
# наши кластеры слабо связаны, эта метрика имеет мало смысла - мы получим везде 1.

#  "It is also possible to examine the extent to which a whole graph has a centralized structure. 
# The concepts of density and centralization refer to differing aspects of the overall 'compactness' of a graph. 
# Density describes the general level of cohesion in a graph; centralization describes the extent 
# to which this cohesion is organized around particular focal points. Centralization and density, 
# therefore, are important complementary measures".
# "The general procedure involved in any measure of graph centralization is 
# to look at the differences between the centrality scores of the most 
# central point and those of all other points. Centralization, then, 
# is the ratio of the actual sum of differences to the maximum possible sum of differences". 
# Source: http://www.analytictech.com/mb119/chapter5.htm

# 3.4.1. Density
# The density of a graph is the ratio of the number of edges and the number of possible edges.

# Создадим переменную
clusters_18_metrics$density <- NA_real_
# Calculate
for (i in 1:nrow(clusters_18_metrics)) {
  # Define logical vector to subset settlements by the cluster
  select_condition <- clust_18_2002 == i
  # Create subgraph
  temp_graph <- my_subgraph_function(res_graph_2002, settl_index_2002[select_condition])
  # Calculate edge_density
  clusters_18_metrics[clusters_18_metrics$clust_18 == i,]$density <- edge_density(temp_graph)
}

# Quick explorative analysis:
# PopDynamics vs Density
clusters_18_metrics %>%
  ggplot(aes(x = density, y = pop2010to2002_rel))+
  geom_point(aes(size = mean_pop2002))+
  # geom_smooth()+
  geom_text(aes(x = density + 0.001, y = pop2010to2002_rel - 0.5, label = clust_18))

# Var_dif vs Density
clusters_18_metrics %>%
  ggplot(aes(x = density, y = variation_dif))+
  # geom_smooth()+
  geom_point(aes(size = mean_pop2002))+
  geom_text(aes(x = density+0.001, y = variation_dif + 0.5,  label = clust_18))


# 3.4.1. Centralization

# Централизация по посредничеству
# Создадим переменную
clusters_18_metrics$centr_betw <- NA_real_
# Calculate
for (i in 1:nrow(clusters_18_metrics)) {
  # Define logical vector to subset settlements by the cluster
  select_condition <- clust_18_2002 == i
  # Create subgraph
  temp_graph <- my_subgraph_function(res_graph_2002, settl_index_2002[select_condition])
  # Calculate edge_density
  clusters_18_metrics[clusters_18_metrics$clust_18 == i,]$centr_betw <- centr_betw(temp_graph, normalized = T)$centralization
}

# Централизацию по близости
# Создадим переменную
clusters_18_metrics$centr_clo<- NA_real_
# Calculate
for (i in 1:nrow(clusters_18_metrics)) {
  # Define logical vector to subset settlements by the cluster
  select_condition <- clust_18_2002 == i
  # Create subgraph
  temp_graph <- my_subgraph_function(res_graph_2002, settl_index_2002[select_condition])
  # Calculate edge_density
  clusters_18_metrics[clusters_18_metrics$clust_18 == i,]$centr_clo <- centr_clo(temp_graph, normalized = T)$centralization
}

# Quick explorative analysis:
# Var_dif vs centr_betw
clusters_18_metrics %>%
  ggplot(aes(x = centr_betw, y = variation_dif))+
  # geom_smooth()+
  geom_point(aes(size = mean_pop2002))+
  geom_text(aes(x = centr_betw+0.001, y = variation_dif + 0.5, label = clust_18))

# Var_dif vs centr_betw
clusters_18_metrics %>%
  ggplot(aes(x = centr_clo, y = variation_dif))+
  # geom_smooth()+
  geom_point(aes(size = mean_pop2002))+
  geom_text(aes(x = centr_clo+0.001, y = variation_dif + 0.5, label = clust_18))


# 3.4.3. Проверка результатов
# Расчеты igraph включают в себя все узлы, поэтому могут быть смещения 
# Для проверки рассчитаем централизацию по близости вручную по матрице расстояний
# и сравним с результатами igraph

# Создаем пустую переменную
clusters_18_metrics$centr_close_man <- NA_real_
for (i in 1:18) {
  # Define logical vector to subset observations by the cluster
  select_condition <- clust_18_2002 == i
  # Subset distance matrix
  temp_dist <- dist_matrix_2002[select_condition, select_condition]
  # Calculate vector of closeness centrality ids
  res <- apply(temp_dist, MARGIN = 1, FUN = function(x) return(1/sum(x, na.rm = T)))
  # Calculate centralisation index
  clusters_18_metrics[clusters_18_metrics$clust_18 == i,]$centr_close_man <- sum(max(res)-res)/centr_clo_tmax(nodes = length(res))
}

# Compare with igraph results
clusters_18_metrics %>% 
  ggplot(aes(x = centr_close_man, y = centr_clo, col = density))+
  geom_point()
# Higher density, higher bias
# However, the values quite highly correlate
cor(clusters_18_metrics$centr_close_man, clusters_18_metrics$centr_clo) # 0.83


# ========================================
# 3.5. Удаленность от регионального центра

clusters_18_metrics$dist2Tyumen <- NA_real_
for (i in 1:nrow(clusters_18_metrics)) {
  # Define logical vector to subset observations by the cluster
  select_condition <- clust_18_2002 == i
  settlements_temp <- df[select_condition,]
  # Subset distance matrix: by row - cluster members, by column - Tyumen
  distances_to_Tyumen <- dist_matrix_2002[select_condition, df$ShortName == "г. Тюмень"]
  # Weight by population proportion
  res <- sum(distances_to_Tyumen * settlements_temp$Census2002/sum(settlements_temp$Census2002))
  # Save to res cell
  clusters_18_metrics[clusters_18_metrics$clust_18 == i,]$dist2Tyumen <- res
}

# 3.5.1. Quick explorative analysis
# Удаленность от Тюмени vs динамика населения
clusters_18_metrics %>% 
  ggplot(aes(x=dist2Tyumen/1000, y=pop2010to2002_rel, col = centr_betw))+
  geom_point(aes(size = mean_pop2002))+
  geom_smooth(method = "glm")+
  scale_color_viridis_c()+
  scale_size_continuous(name = "Ср. размер\nн.п. (чел.)",
                        breaks = c(0, 300, 500, 1000, 2000),
                        labels = c("<300", "300-499", "500-999", "1000-2000", ">8000"))+
  scale_y_continuous(name = "Динамика населения (%)")+
  scale_x_continuous(name = "Расстояние от центра кластера (км)")



# ==========================
# 4. Рассчет метрик для н.п.
# ==========================

# 4.1. Closeness Centrality



# # Мы будем мерить по 3, 6, и 23 кластерам
# # Создадим отдельный дата фрейм, куда будем складывать метрики
# df %>% select(-Rosstat1981, - Rosstat1990, - trend_2002to2010) -> np_metrics
# 
# # ===============================================
# # 4.1. Динамика численности населения в 2002-2010
# 
# np_metrics %>% 
#   mutate(pop2010to2002rel = Census2010/Census2002*100) -> np_metrics
# 
# # ======================
# # 4.2. Degree Centrality
# np_metrics$degree <- degree(graph = res_graph_2002, v = settl_index_2002)
# 
# # ===========================
# # 4.2. Betweenness Centrality
# 
# # Для рассчета BC нужно задать порог поиска (cutoff). 
# # Рассчитаем его на основе средних расстояний между н.п. внутри 23 кластеров
# 
clusters_18_metrics$centr_close_man <- NA_real_
for (i in 1:18) {
  # Define logical vector to subset observations by the cluster
  select_condition <- clust_18_2002 == i
  # Subset distance matrix: by row - cluster members, by column - Tyumen
  temp_dist <- dist_matrix_2002[select_condition, select_condition]
  res <- apply(temp_dist, MARGIN = 1, FUN = function(x) return(1/sum(x, na.rm = T)))
  clusters_18_metrics[clusters_18_metrics$clust_18 == i,]$centr_close_man <- sum(max(res)-res)/centr_clo_tmax(nodes = length(res))
}

# # Вариации от 30 до 90 км. Возьмем медианное значение
# median(clusters_23_metrics$mean_dist) # 44813.96 м
# 
# np_metrics$betw_23 <- estimate_betweenness(graph = res_graph_2002, vids = settl_index_2002, 
#                                                  cutoff = median(clusters_23_metrics$mean_dist))
# 
# # =========================
# # 4.2. Closeness Centrality
# 
# # По 23 кластерам
# np_metrics$closeness_23 <- NA_real_   # чистая
# np_metrics$closeness_23_w <- NA_real_ # взвешенная (индуцированный потенциал поля расселения)
# for (i in 1:23) {
#   # Define logical vector to subset observations by the cluster
#   select_condition <- clust_23_2002 == i
#   settlements_temp <- settlements_2002[select_condition,]@data
#   # Subset distance matrix
#   dist_matrix_temp <- dist_matrix_2002[select_condition,select_condition]
#   # Calculte closeness centrality 
#   np_metrics[np_metrics$clust_23 == i,]$closeness_23 <- dist_matrix_temp %>% apply(1, function(x) 1/sum(x))
#   
#   # Population coefficient matrix
#   pop_coef_temp <- rep.row(settlements_temp$Census2002,
#                            nrow(dist_matrix_temp))
#   dist_matrix_temp_w <- pop_coef_temp/dist_matrix_temp
#   
#   np_metrics[np_metrics$clust_23 == i,]$closeness_23_w <- apply(dist_matrix_temp_w, MARGIN = 1, FUN = function(x) x[!is.infinite(x)] %>% sum())
# }
# 
# # По 6 кластерам
# 
# np_metrics$closeness_6 <- NA_real_   # чистая
# np_metrics$closeness_6_w <- NA_real_ # взвешенная (индуцированный потенциал поля расселения)
# for (i in 1:6) {
#   # Define logical vector to subset observations by the cluster
#   select_condition <- clust_6_2002 == i
#   settlements_temp <- settlements_2002[select_condition,]@data
#   # Subset distance matrix
#   dist_matrix_temp <- dist_matrix_2002[select_condition,select_condition]
#   # Calculte closeness centrality 
#   np_metrics[np_metrics$clust_6 == i,]$closeness_6 <- dist_matrix_temp %>% apply(1, function(x) 1/sum(x))
#   
#   # Population coefficient matrix
#   pop_coef_temp <- rep.row(settlements_temp$Census2002,
#                            nrow(dist_matrix_temp))
#   dist_matrix_temp_w <- pop_coef_temp/dist_matrix_temp
#   
#   np_metrics[np_metrics$clust_6 == i,]$closeness_6_w <- apply(dist_matrix_temp_w, MARGIN = 1, FUN = function(x) x[!is.infinite(x)] %>% sum())
# }