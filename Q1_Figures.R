# Топология сети населенных пунктов как фактор динамики сельского расселения 
# (на примере Тюменской области)
# Александр Шелудков, 2019

# Исследовательский вопрос 1: 
# каково влияние топологических свойств сети населенных пунктов на динамику расселения?

library(sf)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggforce)
library(grid)

load(file = "data/Part3_res_dataset.Rdata")
load("data/settlements.Rdata")
load("data/crs.Rdata")


# Рис. 5. Связь динамики (2010 к 2002) расселения в кластерах районного уровня 
# с положением в региональной системе расселения.
# а) динамика населения кластеров; б) динамика вариации людности населенных пунктов

y_ticks <- 80:105
y_ticks[-seq(1, 26, 5)] <- ""

x_ticks <- seq(0, 420, 20)
x_ticks[-seq(1, 22, 5)] <- ""

p1 <-
  clusters_18_metrics %>% 
  ggplot(aes(y = CL18_pop2010to2002_rel, x = CL18_dist2Tyumen/1000))+
  geom_point(aes(size = CL18_pop2002/1000), pch = 1, stroke = 1)+
  geom_smooth(method = "glm", color = "red", se = F)+
  annotate("text", label = "(a)", x = 420, y = 107.2, vjust = 0, size = 5, family = "Times New Roman")+
  geom_label(aes(x = 270, y = 103, label = "Нас. пункты\nУватского района"), 
             hjust = 0, 
             vjust = 0.5, 
             lineheight = 0.8,
             label.size = NA, size = 3)+
  geom_segment(aes(x = 370, y = 101.5, xend = 405, yend = 100.55),
               size=0.1)+
  scale_x_continuous(name = "Расстояние до регионального центра, км",
                     breaks = seq(0, 420, 20), labels = x_ticks)+
  scale_y_continuous(name = "Динамика числ. нас-я (2010 к 2002),%",
                     breaks =  80:105, labels = y_ticks)+
  scale_size_continuous(name = "Численность\nнаселения\n(2002), тыс. чел.", 
                          breaks = c(20, 50, 100, 200, 500), range = c(0.5, 10))+
  coord_cartesian(ylim = c(80, 105), clip = 'off')+
  theme_bw(base_size = 11, base_family = "Arial")+
  theme(panel.grid = element_blank(),
        axis.ticks = element_line(),
        legend.position = "bottom",
        plot.margin=unit(c(1,0.1,0.1,0.1),"cm"))+
  guides(size=guide_legend(
    keywidth=0.1,
    keyheight=0.1,
    default.unit="cm")
  )


p2 <-
  ggplot()+
  geom_sf(data = st_as_sf(region), fill = "white")+
  geom_point(data = df, aes(x = lon, y = lat, col = CL18_variance_dif), size = 0.8)+
  annotate("text", label = "(б)", x = 12850736, y = 6680000, vjust = 0, size = 5, family = "Times New Roman")+
  scale_color_viridis_c(name = "Динамика вариации людности\nнас-ных пунктов (2010 к 2002),%")+
  coord_sf(crs = pulkovo1942.GK12, datum = NA, ylim = c(6135000, 6640000), clip = 'off')+
  theme_bw(base_family = "Arial", base_size = 11)+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank(),
        plot.margin=unit(c(1,0.1,0.1,0.1),"cm"),
        legend.position = "bottom", legend.box.margin = unit(c(0,0,0,0),"cm"))

fig_5 <- gridExtra::grid.arrange(p1,p2, nrow = 1)

#  Экспорт
ggsave(plot = fig_5, filename = "Fig5.jpeg", path = "plots/Иллюстрации для статьи/", 
       dpi = 200, device = "jpeg", width = 21, height = 10.5, units = "cm")

cowplot::ggsave(plot = fig_5, filename = "Fig5.eps", path = "plots/Иллюстрации для статьи/", 
                width = 21, height = 10.5, units = "cm", device = cairo_ps)


# Рис. 6. Соотношение динамики  населения и динамики вариации 
# людности населенных пунктов в кластерах районного уровня (2010 к 2002)

x_ticks <- 80:105
x_ticks[-seq(1, 26, 5)] <- ""

y_ticks <- 100:117
y_ticks[-seq(1,17,5)] <- ""

fig_6 <- clusters_18_metrics %>%
  ggplot(aes(x = CL18_pop2010to2002_rel, y = CL18_variance_dif))+
  geom_point(aes(size = CL18_pop2002/1000), pch = 1, stroke = 1)+
  # geom_point(aes())+
  geom_circle(data = data_frame(x = c(90, 98), y = c(112, 104)), 
              mapping = aes(x0 = x, y0 = y, r = 5.6), 
              color = "grey70",linetype = "dotted",  alpha = 1,
              inherit.aes = F)+
  annotate("text", x = 99, y = 108.5, label = "a", size = 5, family = "Times New Roman")+
  annotate("text", x = 91, y = 116, label = "б", size = 5,family = "Times New Roman")+
  scale_x_continuous(name = "Динамика численности населения, %", 
                     breaks =  80:105, labels = x_ticks)+
  scale_y_continuous(name = "Динамики вариации людности\nнаселенных пунктов, %",
                     breaks =  100:117, labels = y_ticks)+
  scale_size_continuous(name = "Численность\nнаселения\n(2002), тыс. чел.", 
                        breaks = c(20, 50, 100, 200, 500), range = c(0.5, 10))+
  coord_equal()+
  theme_bw(base_size = 12, base_family = "Times New Roman")+
  theme(panel.grid = element_blank(),
        axis.ticks = element_line(),
        plot.margin=unit(c(0.1,0.1,0.1,0.1),"cm"))

# Экспорт
ggsave(plot = fig_6, filename = "Fig6.jpeg", path = "plots/Иллюстрации для статьи/", 
       dpi = 200, device = "jpeg", width = 18, height = 10, units = "cm")

cowplot::ggsave(plot = fig_6, filename = "Fig6.eps", path = "plots/Иллюстрации для статьи/", 
                width = 18, height = 11, units = "cm", device = cairo_ps)
