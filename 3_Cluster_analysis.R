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
library(RColorBrewer)
library(ggdendro)

# ================
# 0. Preprocessing
# ================

# load the data
load("data/Part1_output.RData")

# Define helper function to extract legend from ggplot object
# Source: https://stackoverflow.com/questions/12041042/how-to-plot-just-the-legends-in-ggplot2
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  legend
}

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
  bind_rows(segment(dendro_2002) %>% mutate(year = 2002),
            segment(dendro_2010) %>% mutate(year = 2010)) %>% 
  ggplot()+
  geom_segment(aes(x=x, y=y, xend = xend, yend = yend))+
  scale_y_continuous(name = element_blank(), trans = "sqrt")+   # трансформируем шкалу y
  scale_x_continuous(name = element_blank())+
  # theme_dendro()+
  theme_classic(base_family = "Times New Roman", base_size = 14)+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())+
  facet_grid(.~as.factor(year))

# Save the plot as jpeg file
ggsave(dendro_p, filename = "plots/Dendrograms.jpeg", device = "jpeg", 
       dpi = 300, width = 7, height = 4)

# =============================
# 2. Оптимальное число кластеров
# =============================

# See Optimal_number_of_clusters.html
# frey: наверняка 18, 33. Менее точно: 2, 6, 25, 42
# mcclain: 5-6, 18-19. Но очень неточно
# cindex: 6, 18, 33
# silhouette: 9, но очень неточно
# dunn: 2, 19-20

# Итого, мы отбрасываем 33 и 42 как чрезмерно дробные и слабо интерпретируемые. 
# В качестве рабочих приняты варианты кластеризации с 6 и 18 группами.

# Обрежем дерево 2002 года и извлечем номера кластеров
clust_3_2002 <- cutree(fit_2002, k=3)
clust_6_2002 <- cutree(fit_2002, k=6)
clust_18_2002 <- cutree(fit_2002, k = 18)

# Дополним точечный слой данными о принадлежности к кластерам
# settlements_2002@data %>% 
#   mutate(clust_3 = clust_3_2002, 
#          clust_6 = clust_6_2002,
#          clust_18 = clust_18_2002) -> settlements_2002@data


# =========================
# 3. Визуализация кластеров
# =========================

# Создадим таблицу с данными по н.п. для визуализации кластеров
settlements <- data_frame(id = settlements_2002@data$id,
                          Population = settlements_2002@data$Census2002,
                          clust_3 = clust_3_2002,
                          clust_6 = clust_6_2002,
                          clust_18 = clust_18_2002,
                          Lon = coordinates(settlements_2002)[,1],
                          Lat = coordinates(settlements_2002)[,2])

# ==========
# 3 кластера

cities_labels_ru <- data_frame(lon = c(12300298, 12450121, 12549841), 
                               lat = c(6332398, 6440103, 6224085), 
                               label = c("Тюмень", "Тобольск", "Ишим"))

clust_3_plot <- 
  ggplot()+
  geom_sf(data = st_as_sf(roads_fixed), col = "grey", lwd = 0.5, show.legend = F)+
  geom_point(data = settlements, aes(x = Lon, y = Lat, 
                                     size = Population/1000, 
                                     col = factor(clust_3)), alpha = 0.6, show.legend = F)+
  scale_colour_manual(values = brewer.pal(n = 3, name = "Dark2"))+
  geom_text(data = cities_labels_ru, 
            aes(x=lon-5000, y=lat+22000, label=label),
            family = "Times New Roman",
            color = "black", fontface = "bold", 
            size=4, hjust="topleft", alpha = 1, show.legend = F)+
  annotate("text", x = 12780736, y = 6640000, label = "a", size = 5, family = "Times New Roman")+
  scale_size_continuous(breaks = c(0.1, 1, 5, 20, 50, 100, 500),
                        range = c(0.3, 13), 
                        labels = c("<= 0.1", "1", "5", "20", "50", "100", ">= 500"))+
  coord_sf(crs = pulkovo1942.GK12, datum = NA)+
  theme_minimal(base_size = 12, base_family = "Times New Roman")+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank())

# Сохраним рисунок
ggsave(clust_3_plot,
       filename = "plots/clust_3_plot.jpeg", device = "jpeg", dpi = 300, width = 7, height = 7)

# ===========
# 6 кластеров

clust_6_plot <-
  ggplot()+
  geom_sf(data = st_as_sf(hydr_lines), col = "grey", lwd = 0.3, alpha = 0.8)+
  geom_sf(data = st_as_sf(hydr_polygons), fill = "grey", alpha = 0.8, col = "grey", lwd = 0.4)+
  geom_point(data = settlements, aes(x = Lon, y = Lat, size = Population/1000, 
                                     col = factor(clust_6)), alpha = 0.6, show.legend = F)+
  annotate("text", x = 12780736, y = 6640000, label = "б", size = 5, family = "Times New Roman")+
  scale_colour_manual(values = brewer.pal(n = 6, name = "Dark2"))+
  scale_size_continuous(name = "Население,\nтыс. чел.", breaks = c(0.1, 1, 5, 20, 50, 100, 500),
                          range = c(0.3, 13), labels = c("<= 0.1", "1", "5", "20", "50", "100", ">= 500"))+
  coord_sf(crs = pulkovo1942.GK12, datum = NA)+
  theme_minimal()+
  theme(axis.title = element_blank(),
          axis.text = element_blank(),
          panel.grid = element_blank())

# Cохраним рисунок
ggsave(clust_6_plot,
         filename = "plots/clust_6_plot.jpeg", device = "jpeg", dpi = 300, width = 7, height = 7)

# ============
# 18 кластеров

# Чтобы добавить номер кластера на карту, вычислим расположение географического центра 
# для каждого кластера
settlements %>% 
  group_by(clust_18) %>% summarise(x_mean = mean(Lon), y_mean = mean(Lat)) -> cluster_notations 

clust_18_plot <- ggplot()+
  geom_sf(data=st_as_sf(rayons), fill = "white", col = "grey")+
  geom_point(data = settlements, aes(x = Lon, y = Lat, size = Population/1000, 
                                     col = factor(clust_18)), show.legend = F, alpha = 0.6)+
  geom_text(data = cluster_notations,
            aes(label = clust_18, x = x_mean, y = y_mean), 
            family = "Times New Roman",
            color = "black", fontface = "bold")+
  annotate("text", x = 12780736, y = 6640000, label = "в", size = 5, family = "Times New Roman")+
  scale_colour_manual(values = c(brewer.pal(n = 8, name = "Dark2"), brewer.pal(10, "Paired")))+
  scale_size_continuous(name = "Население,\nтыс. чел.", breaks = c(0.1, 1, 5, 20, 50, 100, 500),
                        labels = c("<= 0.1", "1", "5", "20", "50", "100", ">= 500"),  
                        range = c(0.3, 13))+
  coord_sf(crs = pulkovo1942.GK12, datum = NA)+
  theme_minimal(base_size = 12, base_family = "Times New Roman")+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank())
# На карте вылез Байкаловский район (16 кластер), которого не существует уже почти 70 лет (!)
# вот она инерция сети

# Сохраним рисунок
ggsave(clust_18_plot,
       filename = "plots/clust_18_plot.jpeg", device = "jpeg", dpi = 300, width = 7, height = 7)

# =================
# Совместим рисунки
par(mar=c(0,0,0,0))

fig_5 <- ggplot()+
  coord_equal(xlim = c(1, 28), ylim = c(0, 10))+
  annotation_custom(ggplotGrob(clust_3_plot),
                    xmin = 0, xmax = 10, ymin = 0.35, ymax = 9.55)+
  annotation_custom(ggplotGrob(clust_6_plot),
                    xmin = 9, xmax = 19, ymin = 0, ymax = 10)+
  annotation_custom(ggplotGrob(clust_18_plot),
                    xmin = 18, xmax = 28, ymin = 0, ymax = 10)+
  labs(x = NULL, y = NULL)+
  theme_void()

# Сохраним рисунок
ggsave(fig_5, filename = "plots/Fig5.jpeg", device = "jpeg", dpi = 300, width = 13.5, height = 5)



# Save the results to Rdata file
save.image("data/Part2_output.RData")
