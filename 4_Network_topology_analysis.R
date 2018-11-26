# The Impact of Settlements Network Structure on Population Dynamics
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
  dplyr::select(-Rosstat1981, -cohort1981, -cohort1990, -cohort2002, 
         -trend_1981to1990, -trend_1990to2002, -trend_2002to2010,
         -rel1981to1990, -rel1990to2002, -rel2002to2010) -> df

# Add coordinates of the settlements as new columns
df %>% 
  mutate(lon = coordinates(settlements_2002)[,1], 
         lat = coordinates(settlements_2002)[,2]) %>% 
  dplyr::select(id, lon, lat, ShortName, MunicipalDistrict, 
         Rosstat1990, Census2002, Census2010, clust_3, clust_6, clust_18) -> df

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

# 1.3. Functions for creating matrix by repeating rows and columns 
# (we will need them for calculating weighted cetnrality measures)

rep.row <-function(x,n){
  matrix(rep(x,each=n),nrow=n)
}

rep.col <-function(x,n){
  matrix(rep(x,each=n),ncol=n)
}

# 1.4. Define function for normalizing data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# =================================
# 2. Рассчет метрик для 6 кластеров
# =================================

# ======================================
# 2.1. Descriptive metrics on population
df %>% 
  group_by(clust_6) %>% 
  mutate(CL6_n = n(),                                          # Number of settlements
         CL6_pop2002 = sum(Census2002),                        # 2002 population
         CL6_pop2010 = sum(Census2010),                        # 2010 population общая численность населения
         CL6_pop2010to2002_rel = CL6_pop2010/CL6_pop2002*100,  # percentage of 2010-population to 2002-population
         CL6_max_pop2002 = max(Census2002),                    # the largest settlement's size
         CL6_mean_pop2002 = mean(Census2002),                  # mean settlement's size
         CL6_median_pop2002 = median(Census2002)) %>%          # median settlement's size
  # select the columns we need
  dplyr::select(clust_6, CL6_n, CL6_pop2002, CL6_pop2010, CL6_pop2010to2002_rel, 
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
  # Calculate variance (standard deviation(x)/mean(x))
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

# "It is also possible to examine the extent to which a whole graph has a centralized structure. 
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

# Betweenness Centralisation (централизация по посредничеству)
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

# Closeness Centralisation (централизация по близости)
# Create column
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

# PopDyn vs centr_betw
clusters_6_metrics %>%
  ggplot(aes(x = CL6_centr_betw, y = CL6_pop2010to2002_rel))+
  # geom_smooth()+
  geom_point(aes(size = CL6_mean_pop2002))+
  geom_text(aes(x = CL6_centr_betw+0.01, y = CL6_pop2010to2002_rel - 0.5, label = clust_6))

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


# ==================================
# 3. Рассчет метрик для 18 кластеров
# ==================================

# ======================================
# 3.1. Descriptive metrics on population
df %>% 
  group_by(clust_18) %>% 
  mutate(CL18_n = n(),                                          # Number of settlements
         CL18_pop2002 = sum(Census2002),                        # 2002 population
         CL18_pop2010 = sum(Census2010),                        # 2010 population общая численность населения
         CL18_pop2010to2002_rel = CL18_pop2010/CL18_pop2002*100,  # percentage of 2010-population to 2002-population
         CL18_max_pop2002 = max(Census2002),                    # the largest settlement's size
         CL18_mean_pop2002 = mean(Census2002),                  # mean settlement's size
         CL18_median_pop2002 = median(Census2002)) %>%          # median settlement's size
  # select the columns we need
  dplyr::select(clust_6, clust_18, CL18_pop2002, CL18_pop2010, CL18_pop2010to2002_rel, 
         CL18_max_pop2002, CL18_mean_pop2002, CL18_median_pop2002) %>%
  unique() -> clusters_18_metrics    # Save the results into new data.frame

# ==============================================================
# 3.2. Variance in population distribution among the settlements

# Create new columns
clusters_18_metrics$CL18_variance_2002 <- NA_real_
clusters_18_metrics$CL18_variance_2010 <- NA_real_

for (i in 1:nrow(clusters_18_metrics)) {
  # Define logical vector to subset observations by the cluster
  select_condition <- clust_18_2002 == i
  settlements_temp <- df[select_condition,]
  # Calculate variation (standart deviation(x)/mean(x))
  # 2002
  clusters_18_metrics[clusters_18_metrics$clust_18 == i,]$CL18_variance_2002 <- 
    sd(settlements_temp$Census2002, na.rm = T)/mean(settlements_temp$Census2002, na.rm = T)
  # 2010
  clusters_18_metrics[clusters_18_metrics$clust_18 == i,]$CL18_variance_2010 <- 
    sd(settlements_temp$Census2010, na.rm = T)/mean(settlements_temp$Census2010, na.rm = T)
}

# Calculate the difference in variance between 2002 and 2010 (темпы сжатия расселения)
clusters_18_metrics %>%
  mutate(CL18_variance_dif = CL18_variance_2010/CL18_variance_2002*100) -> 
  clusters_18_metrics

# 3.2.1. Quick explorative analysis

# Темпы сжатия расселения vs общая динамика населения
clusters_18_metrics %>% 
  ggplot(aes(y=CL18_variance_dif, x=CL18_pop2010to2002_rel))+
  geom_point(aes(size = CL18_mean_pop2002))+
  # geom_smooth(method = "glm")+
  scale_size_continuous(name = "Ср. размер\nн.п. (чел.)",
                        breaks = c(0, 300, 500, 1000, 2000), trans = "sqrt", 
                        labels = c("<300", "300-499", "500-999", "1000-2000", ">8000"))+
  scale_y_continuous(name = "Динамика территориальной\nдифференциации расселения", breaks = seq(100, 115, 5),
                     limits = c(100,115))+
  scale_x_continuous(name = "Динамика населения (%)")

# Темпы сжатия расселения vs средний размер населенных пунктов
clusters_18_metrics %>% 
  ggplot(aes(x=CL18_mean_pop2002, y=CL18_variance_dif))+
  geom_point()+
  # geom_smooth(method = "glm")+
  scale_x_continuous(trans = "log")+
  scale_y_continuous(name = "Изменение вариации")

# Сжатие расселения наблюдается везде, но его траектория разная: есть две группы районов: 
# 1 группа: низкие темпы сжатия на фоне роста или незначительного сокращения населения
# 2 группа: высокие темпы сжатия на фоне общего значительного сокращения населения


# =================================
# 3.3. Централизация/связность сети

# 3.3.1. Density
# The density of a graph is the ratio of the number of edges 
# and the number of possible edges.

# Create column
clusters_18_metrics$CL18_density <- NA_real_
# Calculate
for (i in 1:nrow(clusters_18_metrics)) {
  # Define logical vector to subset settlements by the cluster
  select_condition <- clust_18_2002 == i
  # Create subgraph
  temp_graph <- my_subgraph_function(res_graph_2002, settl_index_2002[select_condition])
  # Calculate edge_density
  clusters_18_metrics[clusters_18_metrics$clust_18 == i,]$CL18_density <- edge_density(temp_graph)
}

# Quick explorative analysis:
# PopDynamics vs Density
clusters_18_metrics %>%
  ggplot(aes(x = CL18_density, y = CL18_pop2010to2002_rel))+
  geom_point(aes(size = CL18_mean_pop2002))+
  # geom_smooth(col = "grey")+
  geom_text(aes(x = CL18_density+0.001, y = CL18_pop2010to2002_rel - 0.5, label = clust_18))

# Var_dif vs Density
clusters_18_metrics %>%
  ggplot(aes(x = CL18_density, y = CL18_variance_dif))+
  # geom_smooth()+
  geom_point(aes(size = CL18_mean_pop2002))+
  geom_text(aes(x = CL18_density, y = CL18_variance_dif + 0.5,  label = clust_6))


# 3.3.2. Centralization

# Betweenness Centralisation (централизация по посредничеству)
# Create column
clusters_18_metrics$CL18_centr_betw <- NA_real_
# Calculate
for (i in 1:nrow(clusters_18_metrics)) {
  # Define logical vector to subset settlements by the cluster
  select_condition <- clust_18_2002 == i
  # Create subgraph
  temp_graph <- my_subgraph_function(res_graph_2002, settl_index_2002[select_condition])
  # Calculate edge_density
  clusters_18_metrics[clusters_18_metrics$clust_18 == i,]$CL18_centr_betw <- centr_betw(temp_graph, normalized = T)$centralization
}

# Closeness Centralisation (централизация по близости)
# Create column
clusters_18_metrics$CL18_centr_clo <- NA_real_
# Calculate
for (i in 1:nrow(clusters_18_metrics)) {
  # Define logical vector to subset settlements by the cluster
  select_condition <- clust_18_2002 == i
  # Create subgraph
  temp_graph <- my_subgraph_function(res_graph_2002, settl_index_2002[select_condition])
  # Calculate edge_density
  clusters_18_metrics[clusters_18_metrics$clust_18 == i,]$CL18_centr_clo <- centr_clo(temp_graph, normalized = T)$centralization
}

# Quick explorative analysis:
# Var_dif vs centr_betw
clusters_18_metrics %>%
  ggplot(aes(x = CL18_centr_betw, y = CL18_variance_dif))+
  # geom_smooth()+
  geom_point(aes(size = CL18_mean_pop2002))+
  geom_text(aes(x = CL18_centr_betw+0.001, y = CL18_variance_dif + 0.5, label = clust_6))

# Var_dif vs centr_clo
clusters_18_metrics %>%
  ggplot(aes(x = CL18_centr_clo, y = CL18_variance_dif))+
  # geom_smooth()+
  geom_point(aes(size = CL18_mean_pop2002))+
  geom_text(aes(x = CL18_centr_clo+0.001, y = CL18_variance_dif + 0.5, label = clust_6))


# # 3.3.3. Check the calculations
# 
# # Расчеты igraph включают в себя все узлы, поэтому могут быть смещения 
# # Для проверки рассчитаем централизацию по близости вручную по матрице расстояний
# # и сравним с результатами igraph
# 
# # Создаем пустую переменную
# clusters_18_metrics$CL18_centr_clo_m <- NA_real_
# for (i in 1:18) {
#   # Define logical vector to subset observations by the cluster
#   select_condition <- clust_18_2002 == i
#   # Subset distance matrix
#   temp_dist <- dist_matrix_2002[select_condition, select_condition]
#   # Calculate vector of closeness centrality ids
#   res <- apply(temp_dist, MARGIN = 1, FUN = function(x) return(1/sum(x, na.rm = T)))
#   # Calculate centralisation index
#   clusters_18_metrics[clusters_18_metrics$clust_18 == i,]$CL18_centr_clo_m <- sum(max(res)-res)/centr_clo_tmax(nodes = length(res))
# }
# 
# # Compare with igraph results
# clusters_18_metrics %>% 
#   ggplot(aes(x = CL18_centr_clo_m, y = CL18_centr_clo, col = CL18_density))+
#   geom_point()
# # Higher density, higher bias
# # However, the values quite highly correlate
# cor(clusters_18_metrics$CL18_centr_clo_m, clusters_18_metrics$CL18_centr_clo) # 0.83


# ========================================
# 3.4. Удаленность от регионального центра

clusters_18_metrics$CL18_dist2Tyumen <- NA_real_
for (i in 1:nrow(clusters_18_metrics)) {
  # Define logical vector to subset observations by the cluster
  select_condition <- clust_18_2002 == i
  settlements_temp <- df[select_condition,]
  # Subset distance matrix: by row - cluster members, by column - Tyumen
  distances_to_Tyumen <- dist_matrix_2002[select_condition, df$ShortName == "г. Тюмень"]
  # Weight by population proportion
  res <- sum(distances_to_Tyumen * settlements_temp$Census2002/sum(settlements_temp$Census2002))
  # Save to res cell
  clusters_18_metrics[clusters_18_metrics$clust_18 == i,]$CL18_dist2Tyumen <- res
}

# ==========================
# 4. Рассчет метрик для н.п.
# ==========================

# =====================================
# 4.1. Distance to the regional capital
df$dist2Tyumen <- dist_matrix_2002[,which(settlements_2002$ShortName == "г. Тюмень")]

# ==========================
# 4.1.1 Populationn dynamics
df %>% 
  mutate(pop2010to2002_rel = Census2010/Census2002*100) -> df


# ==========================================================
# 4.2. Closeness Centrality (in a scope of the whole region)

# Closeness centrality описывает способность актора достичь максимальное количество других 
# акторов, затратив наименьшее число сил - насколько "близок" ко всем. 
# В самом простом варианте считается как 1, деленная на сумму
# всех кратчайших расстояний до всех других узлов

# 4.2.1. Closeness centrality 
df$clo <- 1/(dist_matrix_2002 %>% apply(1, sum))

# 4.2.2. Weighted closeness centrality

# However, for a settlement the position in relation to all the other vertices may not so
# important, as the position to the main (largest) ones. Let's calculate centrality measures 
# in accordance to the size of settlements. In order to take the size into account, we 
# multiply distance matrix to normalized population by the destination nodes

# Create matrix of population sizes in 2002
pop_2002_matrix <- rep.row(normalize(settlements_2002$Census2002), 
                           nrow(dist_matrix_2002))

# Calculate distance matrices, weighted by population (_w)
dist_matrix_2002_w <- dist_matrix_2002 * pop_2002_matrix

# Calculate centrality closeness, weightened by population
df$clo_w <- 1/(dist_matrix_2002_w %>% apply(1, sum))

# How relate 'pure' closeness centrality to the weighted one?
df %>% 
  ggplot(aes(x = clo, y = clo_w, col= as.factor(clust_6)))+
  geom_point()

# ====================================================
# 4.3. Closeness Centrality (in a scope of 6 clusters)

# 4.3.1. Closeness centrality

df$clo_CL6 <- NA_real_
# Calculate
for (i in 1:nrow(clusters_6_metrics)) {
  # Define logical vector to subset settlements by the cluster
  select_condition <- clust_6_2002 == i
  # Subset distance matrix
  temp_matrix <- dist_matrix_2002[select_condition, select_condition]
  # Calculate edge_density
  df[df$clust_6 == i,]$clo_CL6 <- 1/(temp_matrix %>% apply(1, sum))
}

# 4.3.1. Weighted closeness centrality

df$clo_CL6_w <- NA_real_
# Calculate
for (i in 1:nrow(clusters_6_metrics)) {
  # Define logical vector to subset settlements by the cluster
  select_condition <- clust_6_2002 == i
  # Subset distance matrix
  temp_matrix <- dist_matrix_2002[select_condition, select_condition]
  
  # Create matrix of population sizes in 2002
  temp_pop_matrix <- rep.row(normalize(settlements_2002[select_condition,]$Census2002), 
                             nrow(temp_matrix))
  
  # Calculate distance matrices, weighted by population (_w)
  temp_matrix_w <- temp_matrix * temp_pop_matrix
  
  # Calculate edge_density
  df[df$clust_6 == i,]$clo_CL6_w <- 1/(temp_matrix_w %>% apply(1, sum, na.rm = T))
}


# ====================================================
# 4.4. Closeness Centrality (in a scope of 18 clusters)

# 4.4.1. Closeness centrality

df$clo_CL18 <- NA_real_
# Calculate
for (i in 1:nrow(clusters_18_metrics)) {
  # Define logical vector to subset settlements by the cluster
  select_condition <- clust_18_2002 == i
  # Subset distance matrix
  temp_matrix <- dist_matrix_2002[select_condition, select_condition]
  # Calculate edge_density
  df[df$clust_18 == i,]$clo_CL18 <- 1/(temp_matrix %>% apply(1, sum))
}

# 4.4.1. Weighted closeness centrality

df$clo_CL18_w <- NA_real_
# Calculate
for (i in 1:nrow(clusters_18_metrics)) {
  # Define logical vector to subset settlements by the cluster
  select_condition <- clust_18_2002 == i
  # Subset distance matrix
  temp_matrix <- dist_matrix_2002[select_condition, select_condition]
  
  # Create matrix of population sizes in 2002
  temp_pop_matrix <- rep.row(normalize(settlements_2002[select_condition,]$Census2002), 
                             nrow(temp_matrix))
  
  # Calculate distance matrices, weighted by population (_w)
  temp_matrix_w <- temp_matrix * temp_pop_matrix
  
  # Calculate edge_density
  df[df$clust_18 == i,]$clo_CL18_w <- 1/(temp_matrix_w %>% apply(1, sum, na.rm = T))
}

# ===========================
# 4.5. Betweenness Centrality

# Чтобы выделить населенные пункты, связывающие кластеры между собой, 
# мы рассчитали центральность по посредничеству 
# с ограничением максимальной длины пути, учитываемой в вычислениях.
# Первоначально идея была ограничить путь средним диаметром кластеров. Диаметр в 
# сетевом анализе - это расстояние между двумя самыми удаленными точками графа.
# Однако оказалось, что это слишком большие величины. Средний диаметр 6 кластеров - 
# 385522.3. Вetweenness Centrality на его основе на 0.97 коррелирует 
# с обычной центральностью по всему графу. Средний диаметр по 18 кластерам - 194501.3 -
# тоже достаточно большой. В итоге, в качестве ограничения мы взяли медианный путь внутри
# кластеров (52021.79 м)

# 4.5.1. Calculate median path

# 6 clusters
clusters_6_metrics$CL6_median_path <- NA_real_
# Calculate
for (i in 1:nrow(clusters_6_metrics)) {
  # Define logical vector to subset settlements by the cluster
  select_condition <- clust_6_2002 == i
  # Subset distance matrix
  temp_matrix <- dist_matrix_2002[select_condition, select_condition]
  # Calculate edge_density
  clusters_6_metrics[clusters_6_metrics$clust_6 == i,]$CL6_median_path <- median(temp_matrix)
}

# 18 clusters
clusters_18_metrics$CL18_median_path <- NA_real_
# Calculate
for (i in 1:nrow(clusters_18_metrics)) {
  # Define logical vector to subset settlements by the cluster
  select_condition <- clust_18_2002 == i
  # Subset distance matrix
  temp_matrix <- dist_matrix_2002[select_condition, select_condition]
  # Calculate edge_density
  clusters_18_metrics[clusters_18_metrics$clust_18 == i,]$CL18_median_path <- median(temp_matrix)
}


# 4.5.2. Betweenness centrality (limited by clusters median path)

df$betw_CL6 <- estimate_betweenness(graph = res_graph_2002, 
                                     vids = settl_index_2002,
                                     cutoff = median(clusters_6_metrics$CL6_median_path))

df$betw_CL18 <- estimate_betweenness(graph = res_graph_2002, 
                                     vids = settl_index_2002,
                                     cutoff = median(clusters_18_metrics$CL18_median_path))


# 4.5.3. Explore betweenness centrality

# Distribution of values
df %>% 
  ggplot(aes(x = betw_CL18))+
  geom_density()
df %>% 
  ggplot(aes(x = betw_CL6))+
  geom_density()
# Distributions are similiar. Let's compare the values? 
df %>% 
  ggplot(aes(x = betw_CL18, y = betw_CL6))+
  geom_point()
cor(df$betw_CL18, df$betw_CL6) # 0.86 - the values are highly correlated.
# Conclusion: may be, it makes sence to use in the model just one of the variables

# Betweenness Centrality vs Population Dynamics
df %>% 
  filter(pop2010to2002_rel < 200) %>% 
  ggplot(aes(x = betw_CL6, y = pop2010to2002_rel))+
  geom_point(aes(col = betw_CL6), alpha = 0.4)+
  geom_smooth(method = "glm")+
  scale_colour_gradientn(colours = viridis(7), trans = "sqrt")


# ==================================
# 5. Compiling the resulting dataset
# ==================================

# Combine all the metrics into a single dataset
df %>% 
  left_join(clusters_6_metrics, by = "clust_6") %>%
  left_join(clusters_18_metrics %>% dplyr::select(-clust_6), by = "clust_18") -> df

# Save datasets into Rdatafile
save(df, clusters_6_metrics, clusters_18_metrics, file = "data/Part3_res_dataset.Rdata")
