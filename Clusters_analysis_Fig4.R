# Топология сети населенных пунктов как фактор динамики сельского расселения 
# (на примере Тюменской области)
# Александр Шелудков, 2019

# Группы (кластеры) более плотно связанных населенных пунктов

library(sp)
library(sf)
library(raster)
library(rgdal)
library(dplyr)
library(ggplot2)
library(ggrepel)
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

# ====================
# 1. Кластерный анализ
# ====================

# Модель (Ward Hierarchical Clustering)
fit_2002 <- dist_matrix_2002 %>% 
  as.dist(diag = F) %>% 
  hclust(method="ward.D")

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

# ===============
# 3. Визуализация
# ===============

# ======================
# 3.1. Кластерное дерево

# Для визуализации дендрограмы мы используем библиотеку ggdendro

# Извлечем данные из модели и преобразуем в формат ggdendro
dendro_2002 <- as.dendrogram(fit_2002) %>% dendro_data(type = "rectangle")

# Извлечем информацию о принадлежности отдельных наблюдений к кластерам и создадим ключи, 
# по которым будем красить сегменты дерева в ggplot
dendro_2002$label %>% 
  mutate(label = as.character(label) %>% as.integer()) %>% 
  arrange(label) %>% 
  mutate(clust_6 = clust_6_2002, 
         clust_18 = clust_18_2002) %>% arrange(x) -> key
key %>% 
  group_by(clust_18) %>% 
  summarise(min(x), max(x), mean(x)) -> keys_18
key %>% 
  group_by(clust_6) %>% 
  summarise(min(x), max(x), mean(x)) -> keys_6

# Создадим переменную с номером одного из 18 кластеров
segment(dendro_2002) %>%
  mutate(clust_18 = NA_integer_) -> my_dendro
for(i in 1:18) {
  my_dendro[my_dendro$y < 2550000 & my_dendro$x >= keys_18$`min(x)`[i] & my_dendro$x <= keys_18$`max(x)`[i], 5] <- i
}

# Лейблы 3 кластеров
labels <- data_frame(label = c("Ишим", "Тобольск", "Тюмень"), 
                     x = c(188, 675, 1015), 
                     y = rep(35000000, 3))  

# Строим дендрограмму 
dendrogram <- 
  my_dendro %>% 
  ggplot()+
  geom_segment(aes(x=x, y=y , xend = xend, yend = yend, col = as.factor(clust_18)), show.legend = F)+
  geom_rect(data = keys_6, aes(xmin=`min(x)`, xmax=`max(x)`, ymin=0, ymax=11000000), 
            col = "grey30", alpha = 0, lty = "dashed")+
  geom_text(data = keys_6,
            aes(label = clust_6, x = `max(x)` - 50, y = 9000000), 
            family = "Arial",
            color = "grey30", fontface = "bold")+
  geom_text_repel(data = labels, aes(label = label, x = x, y= y), angle = 90, direction = "x")+
  annotate("text", x = 1300, y = 100000000, label = "(a)", size = 5, family = "Arial")+
  scale_y_continuous(name = element_blank(), trans = "sqrt")+  # трансформируем шкалу y
  scale_x_continuous(name = element_blank(), labels = 1:18, breaks = keys_18$`mean(x)`)+
  scale_colour_manual(values = c(brewer.pal(n = 8, name = "Dark2"), brewer.pal(10, "Paired")), na.value = "black")+
  theme_minimal(base_family = "Arial", base_size = 12)+
  theme(panel.grid = element_blank(),
        axis.ticks.x = element_line(),
        # axis.line.x = element_line(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        plot.margin=unit(c(0.1,0.1,0.1,0.1),"cm"))

# ====================
# 3.2. Карта кластеров

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
  geom_sf(data = st_as_sf(region), col = "grey30", lwd = 0.4, alpha = 0)+
  geom_sf(data = st_as_sf(roads_fixed), col = "grey40", lwd = 0.5, show.legend = F)+
  geom_point(data = settlements, aes(x = Lon, y = Lat, 
                                     size = Population/1000, 
                                     col = factor(clust_3)), alpha = 0.6, show.legend = F)+
  scale_colour_manual(values = brewer.pal(n = 3, name = "Dark2"))+
  geom_text_repel(data = cities_labels_ru, aes(label = label, x = lon, y= lat),family = "Arial",
                            color = "black", direction = "x")+
  annotate("text", x = 12900000, y = 6640000, label = "(б)", size = 5, family = "Arial")+
  scale_size_continuous(breaks = c(0.1, 1, 5, 20, 50, 100, 500),
                        range = c(0.2, 10), 
                        labels = c("<= 0.1", "1", "5", "20", "50", "100", ">= 500"))+
  coord_sf(crs = pulkovo1942.GK12, datum = NA)+
  theme_void(base_size = 12, base_family = "Arial")

# # Сохраним рисунок
# ggsave(clust_3_plot,
#        filename = "plots/clust_3_plot.jpeg", device = "jpeg", dpi = 300, width = 7, height = 7)

# ===========
# 6 кластеров
settlements %>% 
  group_by(clust_6) %>% summarise(x_mean = mean(Lon), y_mean = mean(Lat)) -> cluster_6_notations 

clust_6_plot <-
  ggplot()+
  geom_sf(data = st_as_sf(hydr_lines), col = "grey40", lwd = 0.3, alpha = 0.8)+
  geom_sf(data = st_as_sf(hydr_polygons), fill = "grey40", alpha = 0.8, col = "grey", lwd = 0.4)+
  geom_sf(data = st_as_sf(region), col = "grey30", lwd = 0.4, alpha = 0)+
  geom_point(data = settlements, aes(x = Lon, y = Lat, size = Population/1000, 
                                     col = factor(clust_6)), alpha = 0.6, show.legend = F)+
  geom_text(data = cluster_6_notations,
            aes(label = clust_6, x = x_mean, y = y_mean), 
            family = "Arial",
            color = "black")+
  annotate("text", x = 12900000, y = 6640000, label = "(в)", size = 5, family = "Arial")+
  scale_colour_manual(values = brewer.pal(n = 6, name = "Dark2"))+
  scale_size_continuous(name = "Население,\nтыс. чел.", breaks = c(0.1, 1, 5, 20, 50, 100, 500),
                        range = c(0.2, 10), 
                        labels = c("<= 0.1", "1", "5", "20", "50", "100", ">= 500"))+
  coord_sf(crs = pulkovo1942.GK12, datum = NA)+
  theme_void(base_size = 12, base_family = "Arial")

# # Cохраним рисунок
# ggsave(clust_6_plot,
#          filename = "plots/clust_6_plot.jpeg", device = "jpeg", dpi = 300, width = 7, height = 7)

# ============
# 18 кластеров

# Чтобы добавить номер кластера на карту, вычислим расположение географического центра 
# для каждого кластера
settlements %>% 
  group_by(clust_18) %>% summarise(x_mean = mean(Lon), y_mean = mean(Lat)) -> cluster_18_notations 

clust_18_plot <- ggplot()+
  geom_sf(data=st_as_sf(rayons), fill = "white", col = "grey40")+
  geom_sf(data = st_as_sf(region), col = "grey30", lwd = 0.4, alpha = 0)+
  geom_point(data = settlements, aes(x = Lon, y = Lat, size = Population/1000, 
                                     col = factor(clust_18)), show.legend = F, alpha = 0.6)+
  geom_text(data = cluster_18_notations,
            aes(label = clust_18, x = x_mean, y = y_mean), 
            family = "Arial",
            color = "black")+
  annotate("text", x = 12900000, y = 6640000, label = "(г)", size = 5, family = "Arial")+
  scale_colour_manual(values = c(brewer.pal(n = 8, name = "Dark2"), brewer.pal(10, "Paired")))+
  scale_size_continuous(name = "Население,\nтыс. чел.", breaks = c(0.1, 1, 5, 20, 50, 100, 500),
                        labels = c("<= 0.1", "1", "5", "20", "50", "100", ">= 500"),  
                        range = c(0.2, 10))+
  coord_sf(crs = pulkovo1942.GK12, datum = NA)+
  theme_void(base_size = 12, base_family = "Arial")

# На карте вылез Байкаловский район (16 кластер), которого не существует уже почти 70 лет (!)
# вот она инерция сети

# # Сохраним рисунок
# ggsave(clust_18_plot,
#        filename = "plots/clust_18_plot.jpeg", device = "jpeg", dpi = 300, width = 7, height = 7)

# =================
# Совместим рисунки
par(mar=c(0,0,0,0))

fig_4 <- ggplot()+
  coord_equal(xlim = c(0, 24.2), ylim = c(0, 18), expand = c(0, 0))+
  annotation_custom(ggplotGrob(dendrogram),
                    xmin = 0, xmax = 13, ymin = 9, ymax = 18)+
  annotation_custom(ggplotGrob(clust_3_plot),
                    xmin = 12.2, xmax = 24.2, ymin = 9, ymax = 18)+
  annotation_custom(ggplotGrob(clust_6_plot),
                    xmin = 0, xmax = 13, ymin = 0, ymax = 9)+
  annotation_custom(ggplotGrob(clust_18_plot),
                    xmin = 12.2, xmax = 24.2, ymin = 0, ymax = 9)+
  labs(x = NULL, y = NULL)+
  theme_void()

# Сохраним рисунок
ggsave(plot = fig_4, filename = "Fig4.jpeg", path = "plots/Иллюстрации для статьи/",
       dpi = 200, device = "jpeg", width = 24.2, height = 18, units = "cm")

cowplot::ggsave(plot = fig_4, filename = "Fig4.eps", path = "plots/Иллюстрации для статьи/", 
                width = 24.2, height = 18, units = "cm", device = cairo_ps)

# Save the results to Rdata file
save.image("data/Part2_output.RData")
