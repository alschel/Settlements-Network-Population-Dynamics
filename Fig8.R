# Топология сети населенных пунктов как фактор динамики сельского расселения 
# (на примере Тюменской области)
# Александр Шелудков, 2019

# Рис. 8. Населенные пункты юго-востока Тюменской области (кластер 2) по межрайонной центральности

library(sp)
library(sf)
library(rgdal)
library(dplyr)
library(tidyr)
library(ggplot2)
# library(ggforce)
library(ggrepel)
library(grid)
library(RColorBrewer)

load("data/Part3_res_dataset.Rdata")
load("data/Part1_output.RData")
load("data/settlements.Rdata")
load("data/crs.Rdata")

# ================
# 1. Preprocessing

# Filter rayons of cluster 2
rayons[rayons@data$name %in% c("Ишимский район", "Абатский район", "Казанский район", "городской округ Ишим",
                               "Бердюжский район", "Сладковский район", "Голышмановский район"), ] ->
  rayons_cl2

# Read spatial data and clip by rayons border
# Main landscape and infrastructure features
rivers <- raster::shapefile("data/Case region/Gidrology/Main_rivers.shp") %>% 
  spTransform(x = ., CRSobj = pulkovo1942.GK12) %>% raster::intersect(rayons_cl2)
roads <- raster::shapefile("data/Case region/MainRoads/Main_roads.shp") %>% 
  spTransform(x = ., CRSobj = pulkovo1942.GK12) %>% raster::intersect(rayons_cl2)
railRoads <- raster::shapefile("data/Case region/RailWays2/RailWays_lines.shp") %>% 
  spTransform(x = ., CRSobj = pulkovo1942.GK12) %>% raster::intersect(rayons_cl2)

# Crop other spatial layers (roads and rivers)
hydr_lines %>% 
  raster::intersect(rayons_cl2) -> hydr_lines_cl2
roads_fixed %>% 
  raster::intersect(rayons_cl2) -> roads_fixed_cl2

# =======
# 2. Plot

# Rayon names
rayon_names <- c("Ишимский район", "Абатский район", "Казанский район", "городской округ Ишим",
  "Бердюжский район", "Сладковский район", "Голышмановский район")
# Rayon centres names
centres_names <- 
  data_frame(name = c("Абатское", "Бердюжье","Голышманово", "Ишим", "Казанское", "Сладково"),
            lon = c(12589736 + 15000, 12456851 - 14000, 12461530 - 18000, 12529841 + 9000, 12513971 - 16000, 12585024 + 16000),
            lat = c(6241510 - 1000, 6187171 + 5000, 6252255 - 4500, 6221085 - 7500, 6168704 - 2000, 6157291 - 2000))

transsib_label <-  data_frame(name = c("Транссиб"),
                               lon = c(12625024 - 8000),
                               lat = c(6196291 - 9000))

fig_8 <-
  ggplot()+
  geom_sf(data = st_as_sf(rayons_cl2), col = "white", lwd = 0.5, fill = "grey90")+
  geom_sf(data = st_as_sf(hydr_lines_cl2), col = "steelblue3", lwd = 0.4)+
  geom_sf(data = st_as_sf(roads_fixed_cl2), col = "grey10", lwd = 0.4)+
  geom_sf(data = st_as_sf(roads, col = "grey10"), col = "grey10", lwd = 0.7)+
  geom_sf(data = st_as_sf(railRoads), col = "red", lwd = 0.4)+
  geom_point(data = df %>% filter(MunicipalDistrict %in% rayon_names,
                                  clust_6 != 2),
             aes(x = lon, y = lat, size = Census2002/1000,
             col = pop2010to2002_rel > 92.67),
             fill = "grey90",
             pch = 21, stroke = 1)+
  geom_point(data = df %>% filter(clust_6 == 2) %>% mutate(clo_CL6 = scale(clo_CL6)),
             aes(x = lon, y = lat, size = Census2002/1000,
                 col = pop2010to2002_rel > 92.67,
                 fill = clo_CL6),
             pch = 21, stroke = 1)+
  geom_text(data = centres_names,
            aes(x = lon, y = lat, label = name), fontface = "bold", check_overlap = T)+
  geom_text(data = transsib_label,
            aes(x = lon, y = lat, label = name), fontface = "italic", check_overlap = T)+
  scale_size_continuous(name = "Население,\nтыс. чел. (2002)", breaks = c(0.1, 0.5, 1, 5, 10, 50),
                        range = c(2, 12),
                        labels = c("0.1", "0.5", "1", "5", "10", "50"))+
  scale_fill_viridis_c(name = "Центральность\nпо близости", option = "D")+
  scale_color_manual(values = c("white", "black"))+
  coord_sf(crs = pulkovo1942.GK12, datum = NA)+
  ggsn::scalebar(st_as_sf(rayons_cl2), dist = 20, 
                 location = "bottomleft", dd2km = FALSE, 
                 height = 0.01, st.size = 3)+
  theme_minimal(base_family = "Arial", base_size = 12)+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank(),
        plot.margin=unit(c(0,0,0,0),"cm"),
        legend.position = "bottom",
        legend.box.margin = unit(c(0,0.2,0,0),"cm"),
        legend.margin=margin(-15,0,0,0))+
  guides(color = FALSE, size = guide_legend(title.position = "top", label.position = "bottom", nrow = 1),
         fill = guide_colorbar(title.position = "top"))

# Сохраним рисунок
ggsave(plot = fig_8, filename = "Fig8.jpeg", path = "plots/Иллюстрации для статьи/",
       dpi = 300, device = "jpeg", width = 18, height = 20, units = "cm")

cowplot::ggsave(plot = fig_8, filename = "Fig8.eps", path = "plots/Иллюстрации для статьи/",
                width = 18, height = 20, units = "cm", device = cairo_ps)
