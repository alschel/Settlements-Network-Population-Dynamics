# An Impact of Settlement Network Structure on Population Dynamics
# Part 4. Network topology analysis
# Author: Alexander Sheludkov
# Date: 19 October 2018

library(sp)
library(sf)
library(raster)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(igraph)
library(RColorBrewer)
library(tidyr)
library(viridis)


# load the data
load("data/Part2_output.RData")


# ================
# 1. Preprocessing
# ================

# 1.1. Сохраним данные в новую переменную и очистим от лишних столбцов
df <- settlements_2002@data
df %>% 
  select(-cohort1981, -cohort1990, -cohort2002, 
         -trend_1981to1990, -trend_1990to2002, 
         -rel1981to1990, -rel1990to2002, -rel2002to2010) -> df


# ============================================
# 1.2. Функция для выборки subgraphs из графа, созданного методом shp2graph

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
# 2. Рассчет метрик для 6 кластеров
# =================================

# ======================================
# 2.1. Descriptive metrics on population
df %>% 
  group_by(clust_6) %>% 
  mutate(CL6_pop2002 = sum(Census2002),                        # 2002 population
         CL6_pop2010 = sum(Census2010),                        # 2010 population общая численность населения
         CL6_pop2010to2002_rel = CL6_pop2010/CL6_pop2002*100,  # percentage of 2010-population to 2002-population
         CL6_max_pop2002 = max(Census2002),                    # the largest settlement's size
         CL6_mean_pop2002 = mean(Census2002),                  # mean settlement's size
         CL6_median_pop2002 = median(Census2002)) %>%          # median settlement's size
  # select the columns we need
  select(clust_6, CL6_pop2002, CL6_pop2010, CL6_pop2010to2002_rel, 
         CL6_max_pop2002, CL6_mean_pop2002, CL6_median_pop2002) %>%
  unique() -> clusters_6_metrics    # Save the results into new data.frame

# ==============================================================
# 2.2. Variance in population distribution among the settlements

# Create new columns
clusters_6_metrics$CL6_variance_2002 <- NA_real_
clusters_6_metrics$CL6_variance_2010 <- NA_real_

for (i in 1:nrow(clusters_6_metrics)) {
  # Define logical vector to subset observations by the cluster
  select_condition <- clust_6_2002 == i
  settlements_temp <- df[select_condition,]
  # Calculate variation (standart deviation(x)/mean(x))
  # 2002
  clusters_6_metrics[clusters_6_metrics$clust_6 == i,]$CL6_variance_2002 <- 
    sd(settlements_temp$Census2002, na.rm = T)/mean(settlements_temp$Census2002, na.rm = T)
  # 2010
  clusters_6_metrics[clusters_6_metrics$clust_6 == i,]$CL6_variance_2010 <- 
    sd(settlements_temp$Census2010, na.rm = T)/mean(settlements_temp$Census2010, na.rm = T)
}

# Calculate the difference in variance between 2002 and 2010 (темпы сжатия расселения)
clusters_6_metrics %>%
  mutate(CL6_variance_dif = CL6_variance_2010/CL6_variance_2002*100) -> 
  clusters_6_metrics


# 3.2.1. Quick explorative analysis
# Темпы сжатия расселения vs общая динамика населения
clusters_6_metrics %>% 
  ggplot(aes(y=CL6_variance_dif, x=CL6_pop2010to2002_rel))+
  geom_point(aes(size = CL6_mean_pop2002))+
  geom_smooth(method = "glm")+
  scale_size_continuous(name = "Ср. размер\nн.п. (чел.)", breaks = c(0, 500, 2000))+
  scale_y_continuous(name = "Динамика территориальной\nдифференциации расселения") +
  scale_x_continuous(name = "Население в 2010 году к населению в 2002, %")



# =================================
# 2.3. Централизация/связность сети

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

# 2.3.1. Density
# The density of a graph is the ratio of the number of edges and the number of possible edges.

# Создадим переменную
clusters_6_metrics$CL6_density <- NA_real_
# Calculate
for (i in 1:nrow(clusters_6_metrics)) {
  # Define logical vector to subset settlements by the cluster
  select_condition <- clust_6_2002 == i
  # Create subgraph
  temp_graph <- my_subgraph_function(res_graph_2002, settl_index_2002[select_condition])
  # Calculate edge_density
  clusters_6_metrics[clusters_6_metrics$clust_6 == i,]$CL6_density <- edge_density(temp_graph)
}

# Quick explorative analysis:
# PopDynamics vs Density
clusters_6_metrics %>%
  ggplot(aes(x = CL6_density, y = CL6_pop2010to2002_rel))+
  geom_point(aes(size = CL6_mean_pop2002))+
  # geom_smooth(col = "grey")+
  geom_text(aes(x = CL6_density, y = CL6_pop2010to2002_rel - 1, label = clust_6))

# Var_dif vs Density
clusters_6_metrics %>%
  ggplot(aes(x = CL6_density, y = CL6_variance_dif))+
  # geom_smooth()+
  geom_point(aes(size = CL6_mean_pop2002))+
  geom_text(aes(x = CL6_density, y = CL6_variance_dif + 0.5,  label = clust_6))


# 2.3.2. Centralization

# Betweenness Centralisation
# Create column
clusters_6_metrics$CL6_centr_betw <- NA_real_
# Calculate
for (i in 1:nrow(clusters_6_metrics)) {
  # Define logical vector to subset settlements by the cluster
  select_condition <- clust_6_2002 == i
  # Create subgraph
  temp_graph <- my_subgraph_function(res_graph_2002, settl_index_2002[select_condition])
  # Calculate edge_density
  clusters_6_metrics[clusters_6_metrics$clust_6 == i,]$CL6_centr_betw <- centr_betw(temp_graph, normalized = T)$centralization
}

# Централизацию по близости
# Создадим переменную
clusters_6_metrics$CL6_centr_clo<- NA_real_
# Calculate
for (i in 1:nrow(clusters_6_metrics)) {
  # Define logical vector to subset settlements by the cluster
  select_condition <- clust_6_2002 == i
  # Create subgraph
  temp_graph <- my_subgraph_function(res_graph_2002, settl_index_2002[select_condition])
  # Calculate edge_density
  clusters_6_metrics[clusters_6_metrics$clust_6 == i,]$CL6_centr_clo <- centr_clo(temp_graph, normalized = T)$centralization
}

# Quick explorative analysis:
# Var_dif vs centr_betw
clusters_6_metrics %>%
  ggplot(aes(x = CL6_centr_betw, y = CL6_variance_dif))+
  # geom_smooth()+
  geom_point(aes(size = CL6_mean_pop2002))+
  geom_text(aes(x = CL6_centr_betw+0.001, y = CL6_variance_dif + 0.5, label = clust_6))

# Var_dif vs centr_betw
clusters_6_metrics %>%
  ggplot(aes(x = CL6_centr_clo, y = CL6_variance_dif))+
  # geom_smooth()+
  geom_point(aes(size = CL6_mean_pop2002))+
  geom_text(aes(x = CL6_centr_clo+0.001, y = CL6_variance_dif + 0.5, label = clust_6))


# ========================================
# 2.4. Удаленность от регионального центра

clusters_6_metrics$CL6_dist2Tyumen <- NA_real_
for (i in 1:nrow(clusters_6_metrics)) {
  # Define logical vector to subset observations by the cluster
  select_condition <- clust_6_2002 == i
  settlements_temp <- df[select_condition,]
  # Subset distance matrix: by row - cluster members, by column - Tyumen
  distances_to_Tyumen <- dist_matrix_2002[select_condition, df$ShortName == "г. Тюмень"]
  # Weight by population proportion
  res <- sum(distances_to_Tyumen * settlements_temp$Census2002/sum(settlements_temp$Census2002))
  # Save to res cell
  clusters_6_metrics[clusters_6_metrics$clust_6 == i,]$CL6_dist2Tyumen <- res
}

# Quick explorative analysis
# Удаленность от Тюмени vs динамика населения
clusters_6_metrics %>% 
  ggplot(aes(x=CL6_dist2Tyumen/1000, y=CL6_pop2010to2002_rel))+
  geom_point(aes())+
  geom_smooth(method = "glm")+
  scale_color_viridis_c()














# =======================================================
# 3.2. Численность населения и ее динамика в 2002-2010 гг.

df %>% 
  group_by(clust_18) %>% 
  mutate(pop2002 = sum(Census2002), pop2010 = sum(Census2010),
         pop2010to2002_rel = pop2010/pop2002*100,             # отношение населения в 2010 году к населению в 2002
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