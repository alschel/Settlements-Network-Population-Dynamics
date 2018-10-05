# An Impact of Settlement Network Structure on Population Dynamics
# Part 2. Explorative analysis
# Author: Alexander Sheludkov
# Date: 14 August 2018

library(sp)
library(sf)
library(raster)
library(rgdal)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(shp2graph)
library(igraph)
library(leaflet)
library(RColorBrewer)
library(tidyr)
library(scales)
library(ggdendro)   # визуализация дендрограмм (кластерный анализ)

load("data/Part1_output.RData")

# ==========================================
# 1. Nearest neighbour distance distribution
# ==========================================

# Extract minimal distances from the matrices, combine into data.frame 
# and plot the distribution

distance_list <- list(dist_matrix_1990, dist_matrix_2002, dist_matrix_2010)
# Create empty df to keep the results
nearest_neighbours <- data_frame()
for(i in 1:length(distance_list)){
  distance_list[[i]] %>% 
    apply(MARGIN = 1, FUN = function(x) x[x != 0] %>% min()) %>% # вытащим минимальные расстояния
    data_frame() %>%                                             # преобразуем в df
    mutate(year = c(1990,2002,2010)[i]) %>%                      # добавим колонку с годом
    bind_rows(nearest_neighbours) -> nearest_neighbours          # прикрепляем к df
}

# Histogram
nearest_neighbours %>% 
  filter(.<50000) %>%                                          # есть пара выбросов, уберем их
  ggplot()+
  geom_histogram(aes(x =., fill = factor(year)), binwidth = 1000)+
  scale_x_continuous(breaks = seq(0, 50000, 5000))+
  facet_wrap(~year, nrow = 3)

# Violin
nearest_neighbours %>% 
  # filter(.<50000) %>%                                         
  ggplot()+
  geom_violin(aes(y =., x = factor(year)))+
  scale_y_continuous(trans = "sqrt")

# ВЫВОДЫ: абсолютное большинство н.п. имеет соседа в пределах 10 км, 
# и ситуация сильно не поменялась

# ==========================
# 2. Centrality: closeness 1
# ==========================

# Closeness centrality описывает способность актора достичь максимальное количество других 
# акторов, затратив наименьшее число сил - насколько "близок" ко всем. В самом простом варианте
# считается как сумма всех кратчайших расстояний до всех других узлов

closeness_1990 <- 1/(dist_matrix_1990 %>% apply(2, sum))
closeness_2002 <- 1/(dist_matrix_2002 %>% apply(2, sum))
closeness_2010 <- 1/(dist_matrix_2010 %>% apply(2, sum))

# =====================================================
# 2.1. Менялось ли распределение сloseness со временем?

bind_rows(data_frame(val = closeness_1990, year = 1990),
          data_frame(val = closeness_2002, year = 2002),
          data_frame(val = closeness_2010, year = 2010)) -> closeness_df

closeness_df %>%  
  ggplot()+
  geom_histogram(aes(x = val, fill = as.factor(year)), 
                 position = "identity", alpha = 0.7, show.legend = F)+
  geom_vline(data = filter(closeness_df, year == 1990), aes(xintercept = mean(val)),
             size = 0.7, color = "grey3", linetype = "dashed")+
  geom_vline(data = filter(closeness_df, year == 2002), aes(xintercept = mean(val)),
             size = 0.7, color = "grey3", linetype = "dashed")+
  geom_vline(data = filter(closeness_df, year == 2010), aes(xintercept = mean(val)),
             size = 0.7, color = "grey3", linetype = "dashed")+
  scale_x_continuous(name = "Closeness centrality")+
  scale_y_continuous(name = "Число наблюдений")+
  facet_grid(year ~ .)
  
# ВЫВОД: сеть стала более плотной, но просто за счет того, что исчезли периферийные узлы


# ===========================================================
# 2.2. Где находится географический центр поселенческой сети?

# 1990
geo_center_1990 <- data_frame(Longitude = coordinates(settlements_1990)[,1],
          Latitude = coordinates(settlements_1990)[,2],
                   Population = settlements_1990$Rosstat1990,
                   closeness = closeness_1990) %>% 
  ggplot()+
  geom_sf(data = st_as_sf(region), fill = "white")+
  geom_sf(data = st_as_sf(roads_fixed), col = "grey", lwd = 0.4, show.legend = F)+
  geom_point(aes(x = Longitude, y = Latitude, col = closeness, size = Population), alpha = 0.8)+
  # stat_contour(data = test, aes(x = Longitude, y = Latitude, z = closeness))+
  scale_color_viridis_c()+
  scale_size_continuous(range = c(0.7, 7), guide = F)+
  scale_x_continuous(name = element_blank())+
  scale_y_continuous(name = element_blank())+
  theme_minimal()

ggsave(plot = geo_center_1990, path = "plots/", filename = "geo_center_1990.png", 
       device = "png", dpi = 1200)

# 2002
geo_center_2002 <- data_frame(Longitude = coordinates(settlements_2002)[,1],
                              Latitude = coordinates(settlements_2002)[,2],
                              Population = settlements_2002$Census2002,
                              closeness = closeness_2002) %>% 
  ggplot()+
  geom_sf(data = st_as_sf(region), fill = "white")+
  geom_sf(data = st_as_sf(roads_fixed), col = "grey", lwd = 0.4, show.legend = F)+
  geom_point(aes(x = Longitude, y = Latitude, col = closeness, size = Population), alpha = 0.8)+
  # stat_contour(data = test, aes(x = Longitude, y = Latitude, z = closeness))+
  scale_color_viridis_c()+
  scale_size_continuous(range = c(0.7, 7), guide = F)+
  scale_x_continuous(name = element_blank())+
  scale_y_continuous(name = element_blank())+
  theme_minimal()

ggsave(plot = geo_center_2002, path = "plots/", filename = "geo_center_2002.png", 
       device = "png", dpi = 1200)

# 2010
geo_center_2010 <- data_frame(Longitude = coordinates(settlements_2010)[,1],
                              Latitude = coordinates(settlements_2010)[,2],
                              Population = settlements_2010$Census2010,
                              closeness = closeness_2010) %>% 
  ggplot()+
  geom_sf(data = st_as_sf(region), fill = "white")+
  geom_sf(data = st_as_sf(roads_fixed), col = "grey", lwd = 0.4, show.legend = F)+
  geom_point(aes(x = Longitude, y = Latitude, col = closeness, size = Population), alpha = 0.8)+
  # stat_contour(geom='tile', aes(x = Longitude, y = Latitude, size = Population, z = closeness, fill=..level..), binwidth=10)
  scale_color_viridis_c()+
  scale_size_continuous(range = c(0.7, 7), guide = F)+
  scale_x_continuous(name = element_blank())+
  scale_y_continuous(name = element_blank())+
  theme_minimal()

ggsave(plot = geo_center_2010, path = "plots/", filename = "geo_center_2010.png", 
       device = "png", dpi = 1200)


# ===============================================
# 2.3. Какие поселения ближе всего к центру сети?

settlements_1990@data %>% 
  mutate(Closeness = closeness_1990) %>% 
  select(ShortName, MunicipalDistrict, Closeness) %>% 
  arrange(-Closeness) %>% .[1:10,]

settlements_2002@data %>% 
  mutate(Closeness = closeness_2002) %>% 
  select(ShortName, MunicipalDistrict, Closeness) %>% 
  arrange(-Closeness) %>% .[1:10,]

settlements_2010@data %>% 
  mutate(Closeness = closeness_2010) %>% 
  select(ShortName, MunicipalDistrict, Closeness) %>% 
  arrange(-Closeness) %>% .[1:10,]


# ===============================================================
# 2.4. Как соотносятся динамика населения и показатели closeness?
cor(settlements_1990$rel1990to2002, closeness_1990) # -0.02192481
cor(settlements_2002$rel2002to2010, closeness_2002) # 0.08622524

# Размер поселения и показатель центральности в сети
cor(settlements_2010$Census2010, closeness_2010) # 0.01469046


# ==========================
# 3. Centrality: closeness 2
# ==========================

# Важно не столько положение н.п. относительно всех других узлов, 
# сколько относительно немногих, но наиболее важных (крупных)

# Чтобы проверить эту гипотезу мы скорректируем матрицу расстояний,
# помножив значения ячеек на 1/численность населения входящего узла

# 3.1. Calculate distance matrices, weightened by population

# The idea is costruct matrix of population sizes by replicating vector of population variable
# And then to multiply distance matrix by 1/matrix of population sizes

# Define function to repeat rows
rep.row <-function(x,n){
  matrix(rep(x,each=n),nrow=n)
}

rep.col <-function(x,n){
  matrix(rep(x,each=n),ncol=n)
}


# Create matrices of population sizes
pop_1990_matrix <- rep.row(settlements_1990$Rosstat1990/max(settlements_1990$Rosstat1990), nrow(dist_matrix_1990))
pop_2002_matrix <- rep.row(settlements_2002$Census2002/max(settlements_2002$Census2002), nrow(dist_matrix_2002))
pop_2010_matrix <- rep.row(settlements_2010$Census2010/max(settlements_2010$Census2010), nrow(dist_matrix_2010))

# Calculate distance matrices, weightened by population (_w)
dist_matrix_1990_w <- dist_matrix_1990 * pop_1990_matrix
dist_matrix_2002_w <- dist_matrix_2002 * pop_2002_matrix
dist_matrix_2010_w <- dist_matrix_2010 * pop_2010_matrix

# 3.2. Calculate centrality closeness, weightened by population

closeness_1990_w <- 1/(dist_matrix_1990_w %>% apply(1, sum))
closeness_2002_w <- 1/(dist_matrix_2002_w %>% apply(1, sum))
closeness_2010_w <- 1/(dist_matrix_2010_w %>% apply(1, sum))

# =====================================================
# 3.3. Менялось ли распределение сloseness со временем?

bind_rows(data_frame(val = closeness_1990_w, year = 1990),
          data_frame(val = closeness_2002_w, year = 2002),
          data_frame(val = closeness_2010_w, year = 2010)) -> closeness_df_w

closeness_df_w %>%  
  ggplot()+
  geom_histogram(aes(x = val, fill = as.factor(year)), 
                 position = "identity", alpha = 0.7, show.legend = F)+
  geom_vline(data = filter(closeness_df_w, year == 1990), aes(xintercept = mean(val)),
             size = 0.7, color = "grey3", linetype = "dashed")+
  geom_vline(data = filter(closeness_df_w, year == 2002), aes(xintercept = mean(val)),
             size = 0.7, color = "grey3", linetype = "dashed")+
  geom_vline(data = filter(closeness_df_w, year == 2010), aes(xintercept = mean(val)),
             size = 0.7, color = "grey3", linetype = "dashed")+
  scale_x_continuous(name = "Closeness centrality (взвешенная)")+
  scale_y_continuous(name = "Число наблюдений")+
  facet_grid(year ~ .)

# ====================================================
# 3.4. Где находится центр тяжести поселенческой сети?

# 1990
data_frame(Longitude = coordinates(settlements_1990)[,1],
           Latitude = coordinates(settlements_1990)[,2],
           Population = settlements_1990$Rosstat1990,
           closeness = closeness_1990_w) %>% 
  ggplot()+
  geom_sf(data = st_as_sf(region), fill = "white")+
  geom_sf(data = st_as_sf(roads_fixed), col = "grey", lwd = 0.4, show.legend = F)+
  geom_point(aes(x = Longitude, y = Latitude, col = closeness, size = Population), alpha = 0.8)+
  # stat_contour(data = test, aes(x = Longitude, y = Latitude, z = closeness))+
  scale_color_viridis_c()+
  scale_size_continuous(range = c(0.7, 7), guide = F)+
  scale_x_continuous(name = element_blank())+
  scale_y_continuous(name = element_blank())+
  theme_minimal()


# 2010
geo_center_2010_w <- data_frame(Longitude = coordinates(settlements_2010)[,1],
           Latitude = coordinates(settlements_2010)[,2],
           Population = settlements_2010$Census2010,
           closeness = closeness_2010_w) %>% 
  ggplot()+
  geom_sf(data = st_as_sf(region), fill = "white")+
  geom_sf(data = st_as_sf(roads_fixed), col = "grey", lwd = 0.4, show.legend = F)+
  geom_point(aes(x = Longitude, y = Latitude, col = closeness, size = Population), alpha = 0.8)+
  # stat_contour(geom='tile', aes(x = Longitude, y = Latitude, size = Population, z = closeness, fill=..level..), binwidth=10)
  scale_color_viridis_c()+
  scale_size_continuous(range = c(0.7, 7), guide = F)+
  scale_x_continuous(name = element_blank())+
  scale_y_continuous(name = element_blank())+
  theme_minimal()

# Save the plot
ggsave(plot = geo_center_2010_w, path = "plots/", filename = "geo_center_2010_w.svg", 
       device = "svg", dpi = 1200)

# ===============================================================
# 3.4. Как соотносятся динамика населения и показатели closeness?
cor(settlements_1990$rel1990to2002, closeness_1990_w) # 0.05435004
cor(settlements_2002$rel2002to2010, closeness_2002_w) # 0.2391195

# А если убрать н.п., которые исчезли (есть явный bias из-за пригородных, включенных в состав городов)?
cor(settlements_2002$rel2002to2010[settlements_2002$rel2002to2010 > -100], 
    closeness_2002_w[settlements_2002$rel2002to2010 > -100]) # 0.263081 - лучше, но не особо

# Размер поселения и показатель центральности в сети
cor(settlements_2010$Census2010, closeness_2010_w) # 0.095627

# "pos" and "neg" trends in 1990s VS weightened closeness
ggplot()+
  geom_boxplot(data = data_frame(x = settlements_1990$trend_1990to2002, y = closeness_1990_w),
               aes(x = x, y = y))

# "pos" and "neg" trends in 2000s VS weightened closeness
pos_neg_closeness_w_boxplot_2000s <- ggplot()+
  geom_boxplot(data = data_frame(x = settlements_2002$trend_2002to2010, y = closeness_2002_w),
               aes(x = x, y = y))+
  scale_x_discrete(name = "тренд")+
  scale_y_continuous(name = "Взвешенная по численности населения Closeness Centrality")+
  theme(axis.text.x = element_text(size = 14))

ggsave(plot = pos_neg_closeness_w_boxplot_2000s, path = "plots/", 
       filename = "pos_neg_closeness_w_boxplot_2000s.png", 
       device = "png", dpi = 1200)
# То же самое, но в виде гистограммы
ggplot()+
  geom_histogram(data = data_frame(fill = settlements_2002$trend_2002to2010, x = closeness_2002_w),
                 aes(x = x, fill = fill), position = "identity", alpha = 0.7)

# В виде scatter plot - есть явный линейный тренд (!): 
# чем ближе поселение к центру тяжести сети,
# тем выше его динамика населения
ggplot(data = data_frame(y = settlements_2002$rel2002to2010, x = closeness_2002_w, 
                         size = settlements_2002$Census2002),
       aes(x = x, y = y))+
  geom_point(aes(size = size), alpha = 0.6)+
  geom_smooth(method = "glm")

# Это надо проверить, вообще уместно ли проводить подобные тесты??? 
# У нас ведь ненормальное распределение
z.test(x = data_frame(x = settlements_2002$trend_2002to2010, y = closeness_2002_w) %>% 
         filter(x == "pos") %>% pull(y),
       y = data_frame(x = settlements_2002$trend_2002to2010, y = closeness_2002_w) %>% 
         filter(x == "neg") %>% pull(y))

t.test(x = data_frame(x = settlements_1990$trend_1990to2002, y = closeness_1990_w) %>% 
         filter(x == "pos") %>% pull(y),
       y = data_frame(x = settlements_1990$trend_1990to2002, y = closeness_1990_w) %>% 
         filter(x == "neg") %>% pull(y))

# ======================================================================================
# 3.5. Динамика численности населения по удалению от центра тяжести сети (ритмы и волны)

# Динамика численности населения в 1990-2002 гг.
data_frame(farness = dist_matrix_1990_w[,which(settlements_1990$ShortName == "г. Тюмень")], 
           size = settlements_1990$Rosstat1990, dif_02_10_rel = settlements_1990$rel1990to2002) %>% 
  ggplot()+
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey3")+
  geom_point(aes(x = farness/1000, y = dif_02_10_rel, size = size, col = dif_02_10_rel), alpha = 0.6, show.legend = F)+
  geom_smooth(aes(x = farness/1000, y = dif_02_10_rel), lwd = 0.5, color = "red", se = F)+
  scale_x_continuous(name = "Расстояние до г. Тюмени (км)", breaks = seq(0, 650, 50), limits = c(0, 550))+
  scale_y_continuous(name = "Динамика численности населения (%)", limits = c(-100, 250))+
  scale_size_continuous(range = c(1.5,15))+
  scale_color_viridis_c()+
  theme_minimal()

# Динамика численности населения в 2002-2010 гг.
data_frame(farness = dist_matrix_2002_w[,which(settlements_2002$ShortName == "г. Тюмень")], 
           size = settlements_2002$Census2002, dif_02_10_rel = settlements_2002$rel2002to2010) %>% 
  ggplot()+
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey3")+
  geom_point(aes(x = farness/1000, y = dif_02_10_rel, size = size, col = dif_02_10_rel), alpha = 0.6, show.legend = F)+
  geom_smooth(aes(x = farness/1000, y = dif_02_10_rel), lwd = 0.5, color = "red", se = F)+
  scale_x_continuous(name = "Расстояние до г. Тюмени (км)", breaks = seq(0, 650, 50), limits = c(0, 650))+
  scale_y_continuous(name = "Динамика численности населения (%)", limits = c(-100, 200))+
  scale_size_continuous(range = c(1.5,15))+
  scale_color_viridis_c()+
  theme_minimal(base_size = 14)

# 3.5.1. Для начала построим гистограмму распределения населения по удалению от Тюмени (2010)
data_frame(farness = dist_matrix_2010_w[,which(settlements_2010$ShortName == "г. Тюмень")], 
           Pop_2010 = settlements_2010$Census2010/1000) %>% 
  ggplot()+
  geom_histogram(aes(x = farness/1000, y = ..count.., weight = Pop_2010), binwidth = 10)+
  scale_x_continuous(name = "Расстояние, км", breaks = seq(0, 650, 50))+
  scale_y_continuous(name = "Численность населения, тыс. чел.", trans = "sqrt",
                     breaks = c(seq(0, 250, 50), seq(300, 600, 100)))

# 3.5.2. Теперь посмотрим, как изменилось население в абсолютных цифрах c 2002 по 2010

# Чтобы легче читать график, используем функцию трансформации оси с негативными значениями
# спасибо: https://andrewpwheeler.wordpress.com/2015/07/31/custom-square-root-scale-with-negative-values-in-ggplot2-r/
S_sqrt <- function(x){sign(x)*sqrt(abs(x))}
IS_sqrt <- function(x){x^2*sign(x)}
S_sqrt_trans <- function() trans_new("S_sqrt",S_sqrt, IS_sqrt)

# График
data_frame(farness = dist_matrix_2002_w[,which(settlements_2002$ShortName == "г. Тюмень")], 
           abs_dif_02_10 = settlements_2002$Census2010-settlements_2002$Census2002) %>%
  ggplot()+
  geom_histogram(aes(x = farness/1000, y = ..count../1000, weight = abs_dif_02_10), binwidth = 10)+
  scale_x_continuous(name = "Расстояние, км", breaks = seq(0, 650, 50))+
  scale_y_continuous(name = "Прирост населения, тыс. чел.",
                     # trans = "S_sqrt",
                     # limits = c(-5, 30),
                     breaks = seq(-30,30,5))


# 3.5.3. Динамика населения в относительных цифрах (2002-2010)

# 1990-2002
data_frame(farness = dist_matrix_1990_w[,which(settlements_1990$ShortName == "г. Тюмень")]/1000,
           Pop_1990 = settlements_1990$Rosstat1990,
           Pop_2002 = settlements_1990$Census2002) %>%
  group_by(tens = cut(farness, breaks = seq(0, 650, 10), include.lowest = TRUE)) %>% 
  summarise(Pop_2002_sum = sum(Pop_2002),
            Pop_1990_sum = sum(Pop_1990)) %>%
  mutate(dif_rel = (Pop_2002_sum-Pop_1990_sum)/Pop_1990_sum*100) %>% 
  ggplot()+
  geom_col(aes(x = tens, y = dif_rel))

# 2002-2010
data_frame(farness = dist_matrix_2002_w[,which(settlements_2002$ShortName == "г. Тюмень")]/1000,
           Pop_2002 = settlements_2002$Census2002,
           Pop_2010 = settlements_2002$Census2010) %>%
  group_by(tens = cut(farness, breaks = seq(0, 650, 10), include.lowest = TRUE)) %>% 
  summarise(Pop_2002_sum = sum(Pop_2002),
            Pop_2010_sum = sum(Pop_2010)) %>%
  mutate(dif_rel = (Pop_2010_sum-Pop_2002_sum)/Pop_2002_sum*100) %>% 
  ggplot()+
  geom_col(aes(x = tens, y = dif_rel))



# =========================
# 5. Betweenness Centrality
# =========================

# Рассчитаем betweenness
betw_1990 <- betweenness(res_graph_1990, v = settl_index_1990)
betw_2002 <- betweenness(res_graph_2002, v = settl_index_2002)
betw_2010 <- betweenness(res_graph_2010, v = settl_index_2010)


adjacent_vertices(res_graph_2002, settl_index_2002)

betw_2010_w

settlements_2010@data %>% 
  mutate(betweenness = betw_2010) -> settlements_2010@data

ggplot()+
  geom_sf(data = st_as_sf(region), fill = "white")+
  geom_point(data = 
               data_frame(x = coordinates(settlements_2010)[,1],
                          y = coordinates(settlements_2010)[,2],
                          betweenness = betw_2010),
             aes(x=x, y=y, col = betweenness))+
  scale_color_viridis_c()


settlements_2010@data %>% 
  ggplot()+
  geom_point(aes(x=rel2002to2010, y = Census2002))+
  scale_y_continuous(trans = "log")


settlements_2010@data %>%
  mutate(pop_difference = Census2010/Census2002*100) %>% 
  filter(!is.infinite(pop_difference)) -> test
  filter(Census2002 < 25000) %>%
  ggplot()+
  geom_point(aes(x = pop_difference, y = Census2002))+
  scale_y_continuous(trans = "log")


settlements_2010@data %>% filter(Census2002 < 20000) -> test

cor.test(test$rel2002to2010, test$Census2002)

settlements_2010@data %>% View()

test$Census2002 %>% max()
