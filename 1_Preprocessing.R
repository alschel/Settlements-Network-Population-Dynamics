# An Impact of Settlement Network Structure on Population Dynamics
# Part 1. Preprocessing
# Author: Alexander Sheludkov
# Date: 11 August 2018

library(sp)
library(sf)
library(raster)
library(rgdal)
library(dplyr)
library(ggplot2)
library(shp2graph)
library(igraph)
library(visNetwork)

# =============================
# 1. Чтение и подготовка данных
# =============================

# 1.1. Settlements layers

# Lad the data
load("data/settlements.Rdata")
# Создадим три набора точек по трем временным срезам, 
# отфильтровав поселения: существовавшие на данный момент времени 
np[np@data$Rosstat1990 > 0, ] -> settlements_1990
np[np@data$Census2002 > 0, ] -> settlements_2002
np[np@data$Census2010 > 0, ] -> settlements_2010

# 1.2 Basemap

# Define the CRS 
# EPSG:28412
pulkovo1942.GK12 <- "+proj=tmerc +lat_0=0 +lon_0=69 +k=1 +x_0=12500000 +y_0=0 +ellps=krass +units=m +no_defs"
# Load rayouns borders from OSM and remove attributes except the names
rayons <- readOGR("data/MR_polygons.shp") %>% spTransform(CRS(pulkovo1942.GK12))
rayons@data %>% select(name) -> rayons@data
# Take a look
ggplot()+
  geom_sf(data=st_as_sf(rayons), fill = "white", col = "grey")+
  theme_minimal()

# Rivers

# # Main rivers only
# main_rivers <- readOGR("data/Gidrology/Main_rivers.shp") %>% spTransform(CRS(pulkovo1942.GK12))
# 
# # All the rivers (from OSM data, 16.09.2018)
# all_rivers <- readOGR("data/Gidrology/OSM_rivers.geojson") %>% 
#   geometry() %>%                                                # keep the geometry only
#   spTransform(CRS(pulkovo1942.GK12)) %>%
#   raster::intersect(region) -> all_rivers

# TO_shapes
hydr_lines <- readOGR("data/Gidrology/Гидрография_линии.shp") %>% 
  geometry() %>%                                                # keep the geometry only
  spTransform(CRS(pulkovo1942.GK12)) %>%
  raster::intersect(region) -> hydr_lines
hydr_polygons <- readOGR("data/Gidrology/River_pols.shp") %>% 
  geometry() %>%                                                # keep the geometry only
  spTransform(CRS(pulkovo1942.GK12)) %>%
  raster::intersect(region) -> hydr_polygons

# 1.3. OSM roads 

# Дороги из OSM, выгрузка 11 августа 2018 г.
roads <- readOGR("data/roads_OSM_7August2018.geojson")

# Избавимся от лишних переменных в таблице атрибутов
roads@data %>% 
  select(id, highway, surface, tracktype, winter_road, ice_road) -> roads@data

# Перепроецруем слой и обрежем по контуру региона
roads %>%
  spTransform(CRS(pulkovo1942.GK12)) %>%
  raster::intersect(region) -> roads

plot(roads)
plot(settlements, pch = 1, add = T)

# 1.3.1. Проверка топологии

# Существует ли в нашей сети изолированные участки?
road_topology_test <- nt.connect(roads) # 2808 self-connected parts 

# Для устранения проблем топологии выгрузим слой и поправим топологию в QGIS 
# с помощью инструментов grass
writeOGR(roads, dsn = "data/roads.GeoJSON", layer = "roads_initial.GeoJSON", driver = "GeoJSON")

# Загрузим исправленный слой
roads_fixed <- readOGR("data/roads_fixed.shp")
nt.connect(roads_fixed) # остался 1 self-connected parts 
# Удалим лишние столбцы
roads_fixed@data %>% select(-cat) -> roads_fixed@data

# 1.3.2. Присвоение коэффициентов проходимости

#  Все дороги типа trunk, primary и secondary, а также дороги с асфальтовым либо 
# бетонным покрытием получили коэффициент 1. Все остальные дороги 
# (гравийные, грунтовые, зимники и пр.) - коэффициент 0.5.
# Зададим коэффициенты проходимости
# roads_fixed@data %>% 
#   mutate(pass_koef = case_when(highway == "trunk" | highway == "trunk_link" |
#                                  highway == "primary" | highway == "primary_link" |
#                                  highway == "secondary" | highway == "secondary_link" |
#                                  surface == "asphalt" | surface == "concrete" |
#                                  surface == "concrete:plates" | surface == "paved" ~ 1,
#                                TRUE ~ 0.5)) -> roads_fixed@data

# =============================================
# 2. Создание графов и расчет матриц расстояний
# =============================================

# Добавим к сети дорог слои с населенными пунктами
# Метод интеграции - nearest point (2), т.е. для каждой точки в слое поселений
# будет найдена ближайшая точка на графе дорог и в этом месте создан узел (node)

# 2.1. 1990 год
# Integrate settlements to graph
roads_points_temp <- points2network(ntdata = roads_fixed,            # roads layer
                          pointsxy = coordinates(settlements_1990),  # points ccordinates
                          ELComputed = T,                            # calculate and return the length of each edge
                          approach = 2,
                          ea.prop = c(0,1,1,1,1,1))                  # keep edge attributes      

# View the result
ptsinnt.view(ntdata=roads_fixed, nodelist=roads_points_temp[[1]], 
             pointsxy=coordinates(settlements_1990), CoorespondIDs=roads_points_temp[[3]]) 

# Convert to igraph object
res_graph_1990 <- nel2igraph(roads_points_temp[[1]],      # nodelist
                        roads_points_temp[[2]],           # edgelist
                        weight = roads_points_temp[[8]])  # the length of edges (from @ea.prop)
summary(res_graph_1990)

# Save node indexes, accotiated with settlements (we need them later)
settl_index_1990 <- roads_points_temp[[3]]


# # 2.1.1. Visualize igraph with ggraph
# library(tidygraph)
# library(ggraph)
# 
# # Convert igraph object to tbl_graph object
# res_graph_tidy <- as_tbl_graph(res_graph_1990)
# 
# # Make a plot
# ggraph(graph = res_graph_tidy) + 
#   geom_edge_link() + 
#   geom_node_point() +
#   theme_graph()

# 2.2. 2002 год
# Integrate settlements to graph
roads_points_temp <- points2network(ntdata = roads_fixed,                      # roads layer
                                    pointsxy = coordinates(settlements_2002),  # points ccordinates
                                    ELComputed = T,                            # calculate and return the length of each edge
                                    approach = 2,
                                    ea.prop = c(0,1,1,1,1,1))                  # keep edge attributes
# Convert to igraph object
res_graph_2002 <- nel2igraph(roads_points_temp[[1]],           # nodelist
                             roads_points_temp[[2]],           # edgelist
                             weight = roads_points_temp[[8]])  # the length of edges (from @ea.prop)

# Save node indexes, accotiated with settlements
settl_index_2002 <- roads_points_temp[[3]]

# 2.3. 2010 год
# Integrate settlements to graph
roads_points_temp <- points2network(ntdata = roads_fixed,                      # roads layer
                                    pointsxy = coordinates(settlements_2010),  # points ccordinates
                                    ELComputed = T,                            # calculate and return the length of each edge
                                    approach = 2,
                                    ea.prop = c(0,1,1,1,1,1))                  # keep edge attributes
# Convert to igraph object
res_graph_2010 <- nel2igraph(roads_points_temp[[1]],           # nodelist
                             roads_points_temp[[2]],           # edgelist
                             weight = roads_points_temp[[8]])  # the length of edges (from @ea.prop)
# Save node indexes, accotiated with settlements
settl_index_2010 <- roads_points_temp[[3]]


# 2.4. Calculate distance matrices

# 2.4.1. 1990

dist_matrix_1990 <- 
  shortest.paths(res_graph_1990,                                        # igraph object
                 v = settl_index_1990,                                  # from
                 # to: use the same indexes, but ordered
                 to = as.numeric(levels(as.factor(settl_index_1990))))
# Приведем матрицу в соответствие стандартному виду (диагональная ось = 0)
dist_matrix_1990 <- dist_matrix_1990[, match(settl_index_1990, 
                                             as.numeric(levels(as.factor(settl_index_1990))))]
# Check
dist_matrix_1990[1:10, 1:10]

# 2.4.2. 2002
dist_matrix_2002 <- 
  shortest.paths(res_graph_2002,                                        # igraph object
                 v = settl_index_2002,                                  # from
                 # to: use the same indexes, but ordered
                 to = as.numeric(levels(as.factor(settl_index_2002))))
# Приведем матрицу в соответствие стандартному виду (диагональная ось = 0)
dist_matrix_2002 <- dist_matrix_2002[, match(settl_index_2002, 
                                             as.numeric(levels(as.factor(settl_index_2002))))]

# 2.4.3. 2010
dist_matrix_2010 <- 
  shortest.paths(res_graph_2010,                                        # igraph object
                 v = settl_index_2010,                                  # from
                 # to: use the same indexes, but ordered
                 to = as.numeric(levels(as.factor(settl_index_2010))))
# Приведем матрицу в соответствие стандартному виду (диагональная ось = 0)
dist_matrix_2010 <- dist_matrix_2010[, match(settl_index_2010, 
                                             as.numeric(levels(as.factor(settl_index_2010))))]


# ==================================================================================
# Итак, на выходе у нас есть 3 графа с интегрированными в них точечными слоями, 
# 3 вектора с ключами (интексами) к этим графам, 3 матрицы кратчайших расстояний,
# а также набор слоем для basemap - границы региона (region), районов (rayons), 
# дороги (roads_fixed), и 2 слоz с гидрологией: hydr_lines и hydr_pols, 
# based on TO-shapes data. При необходимости можно подгрузить реки из OSM
# ==================================================================================

# Удалим временные файлы и сохраним результат
rm(np, roads_points_temp, roads, road_topology_test)
save.image("data/Part1_output.RData")
