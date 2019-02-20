library(sf)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggforce)

load(file = "data/Part3_res_dataset.Rdata")
load("data/settlements.Rdata")
load("data/crs.Rdata")


# Рис. XX. Связь динамики (2010 к 2002) расселения в кластерах районного уровня 
# с положением в региональной системе расселения.
# а) динамика населения кластеров; б) динамика вариации людности населенных пунктов

y_ticks <- 80:105
y_ticks[-seq(1, 26, 5)] <- ""

x_ticks <- seq(0, 420, 20)
x_ticks[-seq(1, 22, 5)] <- ""

p1 <-
  clusters_18_metrics %>% 
  ggplot(aes(y = CL18_pop2010to2002_rel, x = CL18_dist2Tyumen/1000))+
  # geom_point()+
  geom_point(aes(size = CL18_pop2002/1000), pch = 1, stroke = 1)+
  geom_smooth(method = "glm", color = "red", se = F)+
  annotate("text", x = 420, y = 105, label = "a", size = 5, family = "Times New Roman")+
  scale_x_continuous(name = "Расстояние до регионального центра, км",
                     breaks = seq(0, 420, 20), labels = x_ticks)+
  scale_y_continuous(name = "Динамика населения (2010 к 2002), %",
                     breaks =  80:105, labels = y_ticks)+
  scale_size_continuous(name = "Численность\nнаселения\n(2002), тыс. чел.", 
                          breaks = c(20, 50, 100, 200, 500), range = c(0.5, 10))+
  theme_bw(base_size = 12, base_family = "Times New Roman")+
  theme(panel.grid = element_blank(),
        axis.ticks = element_line(),
        legend.position = "bottom")+
  guides(size=guide_legend(
    keywidth=0.1,
    keyheight=0.1,
    default.unit="inch")
  )

p2 <-
  ggplot()+
  geom_sf(data = st_as_sf(region), fill = "white")+
  geom_point(data = df, aes(x = lon, y = lat, col = CL18_variance_dif), size = 0.8)+
  annotate("text", x = 12850736, y = 6640000, label = "б", size = 5, family = "Times New Roman")+
  scale_color_viridis_c(name = "Динамика вариации людности\nнаселенных пунктов (2010 к 2002), %")+
  coord_sf(crs = pulkovo1942.GK12, datum = NA)+
  theme_bw(base_family = "Times New Roman", base_size = 12)+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank(),
        plot.margin=unit(c(0.1,0.1,0.1,0.1),"cm"),
        legend.position = "bottom")

fig_6 <- gridExtra::grid.arrange(p1,p2, nrow = 1)


ggsave(filename = "Fig6.jpeg", path = "plots/", dpi = 200,
       plot = fig_6, device = "jpeg", width = 8.5, height = 4)


# Рис. XX. Соотношение динамики  населения и динамики вариации 
# людности населенных пунктов в кластерах районного уровня (2010 к 2002)

x_ticks <- 80:105
x_ticks[-seq(1, 26, 5)] <- ""

y_ticks <- 100:117
y_ticks[-seq(1,17,5)] <- ""

fig_7 <- clusters_18_metrics %>%
  ggplot(aes(x = CL18_pop2010to2002_rel, y = CL18_variance_dif))+
  geom_point(aes(size = CL18_pop2002/1000), pch = 1, stroke = 1)+
  # geom_point(aes())+
  geom_circle(data = data_frame(x = c(90, 98), y = c(112, 104)), 
              mapping = aes(x0 = x, y0 = y, r = 5.6), 
              color = "grey70",linetype = "dotted",  alpha = 1,
              inherit.aes = F)+
  annotate("text", x = 99, y = 108.5, label = "a", size = 5, family = "Times New Roman")+
  annotate("text", x = 91, y = 116, label = "б", size = 5,family = "Times New Roman")+
  scale_x_continuous(name = "Динамика населения, %", 
                     breaks =  80:105, labels = x_ticks)+
  scale_y_continuous(name = "Динамики вариации людности\nнаселенных пунктов, %",
                     breaks =  100:117, labels = y_ticks)+
  scale_size_continuous(name = "Численность\nнаселения\n(2002), тыс. чел.", 
                        breaks = c(20, 50, 100, 200, 500), range = c(0.5, 10))+
  coord_equal()+
  theme_bw(base_size = 12, base_family = "Times New Roman")+
  theme(panel.grid = element_blank(),
        axis.ticks = element_line())

# Сохраним графики
ggsave(filename = "Fig7.jpeg", path = "plots/", dpi = 200,
       plot = fig_7, device = "jpeg", width = 6, height = 4)
