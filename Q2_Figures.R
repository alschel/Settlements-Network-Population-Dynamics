# Топология сети населенных пунктов как фактор динамики сельского расселения 
# (на примере Тюменской области)
# Александр Шелудков, 2019

# Исследовательский вопрос 2: 
# как структурное положение н.п. в сети населенных пунктов влияло на динамику его населения?

library(dplyr)
library(tidyr)
library(corrplot)
library(lme4)
library(stargazer)
library(ggplot2)
library(gridExtra)
library(RColorBrewer)

# ================
# 1. Preprocessing
# ================

# Загрузка данных
load(file = "data/Part3_res_dataset.Rdata")

# Define helper fucntion to extract legend from ggplot object
# Source: https://stackoverflow.com/questions/12041042/how-to-plot-just-the-legends-in-ggplot2
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  legend
}

# 1.1. Чистка данных

# Удалим населенные пункты, которые исчезли в результате поглощения городами
df %>% 
  filter(!(pop2010to2002_rel == 0 & Census2002 > 1000 & MunicipalDistrict == "Тобольский район")) %>%
  filter(!(pop2010to2002_rel == 0 & MunicipalDistrict == "Тюменский район")) ->
  df_cleaned

# Удалим выбросы за пределами 3 медианных абсолютных отклонений
df_cleaned %>% 
  mutate(dev = pop2010to2002_rel - median(pop2010to2002_rel)) %>% 
  filter(dev <= 3*mad(pop2010to2002_rel)) %>% 
  dplyr::select(-dev) -> df_cleaned

## 1.2 Отбор предикторов и их трансформация
  
# Создадим новую переменную: логарифм численности (2002), 
# которая послужит в модели в качестве контрольной
df_cleaned %>% 
  mutate(logPop = log(Census2002)) ->
  df_cleaned

# Приведем метрики центральности к стандартизированному виду
df_cleaned %>% 
  mutate_at(.vars = vars(clo_CL6, clo_CL18, betw_CL6, betw_CL18), scale) -> df_cleaned

# Проверка предикторов на корреляцию
corr <- cor(df_cleaned %>% dplyr::select(pop2010to2002_rel, logPop, clo_CL6, clo_CL18, betw_CL6, betw_CL18))
# colnames(corr) <- c("Динамика числ. нас-я\n(2010 к 2002),%","log(числ. нас-я, 2002)", "ЦБ(6)", "ЦБ(18)", "ЦП(98)", "ЦП(52)")
# rownames(corr) <- c("Динамика числ. нас-я\n(2010 к 2002),%","log(числ. нас-я, 2002)", "Ц0Б(6)", "ЦБ(18)", "ЦП(98)", "ЦП(52)")
corrplot(corr, method = "number", type = "full", insig = "p-value", col = viridis::inferno(12))
# Коррелируют метрики центральности по посредничеству, поэтому строим две модели

# =========
# 2. Модель
# =========

model1 <- lmer(pop2010to2002_rel ~ logPop + clo_CL6 + clo_CL18 + betw_CL18 + (1|clust_6/clust_18) + (0 + clo_CL6|clust_6) + (0 + clo_CL18|clust_18), REML = F, data = df_cleaned)
model2 <- lmer(pop2010to2002_rel ~ logPop + clo_CL6 + clo_CL18 + betw_CL6 + (1|clust_6/clust_18) + (0 + clo_CL6|clust_6) + (0 + clo_CL18|clust_18), REML = F, data = df_cleaned)

# Compare the results  
stargazer::stargazer(model1, model2,
                     type = "text")

# Доля дисперсии, объясняемая сетевыми метриками
MuMIn::r.squaredGLMM(model1)
MuMIn::r.squaredGLMM(model2)

# Диагностика
# Проверка на гомоскедастичность остатков
resid(model1) %>% plot()
resid(model2) %>% plot()
# Проверка нормальности распределения остатков
resid(model1) %>% qqnorm()
resid(model2) %>% qqnorm()

# ===========================
# 3. Визуализация результатов
# ===========================

pop.labels <- seq(0, 140, 10)
pop.labels[-seq(1, 15, 2)] <- ""

# Центральность по близости (6) vs Динамика населения
clo_CL6_vs_popDyn <- df_cleaned %>%
  ggplot(aes(y = pop2010to2002_rel, x = clo_CL6, fill = as.factor(clust_6), col = as.factor(clust_6)))+
  geom_point(shape = 21, stroke = 0, alpha = 0.4, size = 1.2)+
  geom_smooth(method = "glm", se = F)+
  geom_hline(aes(yintercept = 100), linetype = "dashed", col = "grey3")+
  scale_y_continuous(name = "Динамика числ-и населения (2010 к 2002), %",
                     limits = c(0, 150),
                     breaks = seq(0, 140, 10), labels = pop.labels)+
  scale_x_continuous(name = "Центральность по близости (6 кластеров)", 
                     breaks = seq(-2, 4, 1))+
  scale_fill_manual(name = "Кластер", values = brewer.pal(n = 6, name = "Dark2"))+
  scale_colour_manual(name = "Кластер", values = brewer.pal(n = 6, name = "Dark2"))+
  theme_bw(base_size = 12, base_family = "Arial")+
  theme(panel.grid = element_blank(),
        axis.ticks = element_line(),
        # legend.position = "bottom",
        # legend.direction = "horizontal",
        legend.title = element_text(size = 12),
        legend.box = "horizontal",
        plot.margin=unit(c(0.1,0.1,0.3,0.1),"cm"))+
  guides(fill = FALSE)

# Центральность по посредничеству (6) vs Динамика населения
betw_CL6_vs_popDyn <- df_cleaned %>%
  ggplot(aes(y = pop2010to2002_rel, x = betw_CL6, col = as.factor(clust_6)))+
  geom_point(alpha = 0.4, stroke = 0, size = 1.2)+
  geom_smooth(method = "glm", se = F)+
  geom_hline(aes(yintercept = 100), linetype = "dashed", col = "grey3")+
  scale_y_continuous(name = "Динамика числ-и населения (2010 к 2002), %",
                     limits = c(0, 150),
                     breaks = seq(0, 140, 10), labels = pop.labels)+
  scale_x_continuous(name = "Центральность по посредничеству", 
                     breaks = seq(-2, 6, 1))+
  scale_fill_manual(name = "Кластер", values = brewer.pal(n = 6, name = "Dark2"))+
  scale_colour_manual(name = "Кластер", values = brewer.pal(n = 6, name = "Dark2"))+
  theme_bw(base_size = 12, base_family = "Arial")+
  theme(panel.grid = element_blank(),
        axis.ticks = element_line(),
        legend.position = "bottom",
        # legend.direction = "horizontal",
        legend.title = element_text(size = 12),
        plot.margin=unit(c(0.1,0.1,0.1,0.1),"cm"))+
  guides(colour = guide_legend(title.position = "top", nrow = 1))

# Извлечем легенду
color_legend <- g_legend(betw_CL6_vs_popDyn)

# Совместим графики
par(mar=c(0,0,0,0))
fig_7 <- ggplot()+
  coord_equal(xlim = c(0, 10), ylim = c(0, 13), expand = c(0.1,0.1))+
  annotation_custom(ggplotGrob(clo_CL6_vs_popDyn + 
                                 guides(colour = FALSE)),
                    xmin = 0, xmax = 10, ymin = 7, ymax = 13)+
  annotation_custom(ggplotGrob(betw_CL6_vs_popDyn + guides(colour = FALSE)),
                    xmin = 0, xmax = 10, ymin = 1, ymax = 7)+
  annotation_custom(color_legend,
                    xmin = 2, xmax = 5, ymin = 0, ymax = 1)+
  labs(x = NULL, y = NULL)+
  theme_void()+
  theme(plot.margin=unit(c(0.1,0.1,0.1,0.1),"cm"))

# Сохраним графики
ggsave(plot = fig_7, filename = "Fig7.jpeg", path = "plots/Иллюстрации для статьи/", 
       dpi = 200, device = "jpeg", width = 18, height = 22, units = "cm")

cowplot::ggsave(plot = fig_7, filename = "Fig7.eps", path = "plots/Иллюстрации для статьи/", 
                width = 18, height = 22, units = "cm", device = cairo_ps)


# ======================================================
# 4. Как показатели центральности распределены на карте?
# ======================================================

load("data/Part2_output.RData")

# Центральность по близости (18)
clo_CL18_map <- df %>% 
  group_by(clust_18) %>% 
  mutate(clo_CL18 = scale(clo_CL18)) %>%
  ungroup() %>% 
  ggplot()+
  geom_sf(data = st_as_sf(region), col = "grey30", lwd = 0.4, alpha = 0)+
  geom_sf(data = st_as_sf(roads_fixed), col = "grey40", lwd = 0.5, show.legend = F)+
  geom_point(aes(x = lon, y = lat, 
                 size = Census2002/1000, 
                 col = clo_CL18), alpha = 1, show.legend = T)+
  scale_color_viridis_c(name = "Центральность\nпо близости (18)")+
  # scale_color_brewer()+
  scale_size_continuous(breaks = c(0.1, 1, 5, 20, 50, 100, 500),
                        range = c(0.2, 10), 
                        labels = c("<= 0.1", "1", "5", "20", "50", "100", ">= 500"))+
  coord_sf(crs = pulkovo1942.GK12, datum = NA)+
  theme_void(base_size = 12, base_family = "Arial")+
  theme(legend.position = "bottom")+
  guides(size = FALSE)

# Сохраним рисунок
ggsave(plot = clo_CL18_map, filename = "clo_CL18_map.jpeg", path = "plots/",
       dpi = 300, device = "jpeg", width = 15, height = 13, units = "cm")

# Центральность по близости (6)
clo_CL6_map <- df %>% 
  group_by(clust_6) %>% 
  mutate(clo_CL6 = scale(clo_CL6)) %>%
  ungroup() %>% 
  ggplot()+
  geom_sf(data = st_as_sf(region), col = "grey30", lwd = 0.4, alpha = 0)+
  geom_sf(data = st_as_sf(roads_fixed), col = "grey40", lwd = 0.5, show.legend = F)+
  geom_point(aes(x = lon, y = lat, 
                 size = Census2002/1000, 
                 col = clo_CL6), alpha = 1, show.legend = T)+
  scale_color_viridis_c(name = "Центральность\nпо близости (6)")+
  scale_size_continuous(breaks = c(0.1, 1, 5, 20, 50, 100, 500),
                        range = c(0.2, 10), 
                        labels = c("<= 0.1", "1", "5", "20", "50", "100", ">= 500"))+
  coord_sf(crs = pulkovo1942.GK12, datum = NA)+
  theme_void(base_size = 12, base_family = "Arial")+
  theme(legend.position = "bottom")+
  guides(size = FALSE)

# Сохраним рисунок
ggsave(plot = clo_CL6_map, filename = "clo_CL6_map.jpeg", path = "plots/",
       dpi = 300, device = "jpeg", width = 15, height = 13, units = "cm")



# Центральность по посредническу (52)
betw_CL18_map <- 
  df %>% 
  # group_by(clust_18) %>%
  mutate(betw_CL18 = scale(betw_CL18)) %>%
  # ungroup() %>%
  ggplot()+
  geom_sf(data = st_as_sf(region), col = "grey30", lwd = 0.4, alpha = 0)+
  geom_sf(data = st_as_sf(roads_fixed), col = "grey40", lwd = 0.5, show.legend = F)+
  geom_point(aes(x = lon, y = lat, 
                 size = Census2002/1000, 
                 col = betw_CL18), alpha = 1, show.legend = T)+
  scale_color_viridis_c(name = "Центральность\nпо посредничеству (52 км)")+
  scale_size_continuous(breaks = c(0.1, 1, 5, 20, 50, 100, 500),
                        range = c(0.2, 10), 
                        labels = c("<= 0.1", "1", "5", "20", "50", "100", ">= 500"))+
  coord_sf(crs = pulkovo1942.GK12, datum = NA)+
  theme_void(base_size = 12, base_family = "Arial")+
  theme(legend.position = "bottom")+
  guides(size = FALSE)

# Сохраним рисунок
ggsave(plot = betw_CL18_map, filename = "betw_CL18_map.jpeg", path = "plots/",
       dpi = 300, device = "jpeg", width = 15, height = 13, units = "cm")

# Центральность по посредническу (52)
betw_52_map <- 
  df %>% 
  # group_by(clust_18) %>%
  mutate(betw_CL18 = scale(betw_CL18)) %>%
  # ungroup() %>%
  ggplot()+
  geom_sf(data = st_as_sf(region), col = "grey30", lwd = 0.4, alpha = 0)+
  geom_sf(data = st_as_sf(roads_fixed), col = "grey40", lwd = 0.5, show.legend = F)+
  geom_point(aes(x = lon, y = lat, 
                 size = Census2002/1000, 
                 col = betw_CL18), alpha = 1, show.legend = T)+
  scale_color_viridis_c(name = "Центральность\nпо посредничеству (52 км)")+
  scale_size_continuous(breaks = c(0.1, 1, 5, 20, 50, 100, 500),
                        range = c(0.2, 10), 
                        labels = c("<= 0.1", "1", "5", "20", "50", "100", ">= 500"))+
  coord_sf(crs = pulkovo1942.GK12, datum = NA)+
  theme_void(base_size = 12, base_family = "Arial")+
  theme(legend.position = "bottom")+
  guides(size = FALSE)

# Сохраним рисунок
ggsave(plot = betw_52_map, filename = "betw_52_map.jpeg", path = "plots/",
       dpi = 300, device = "jpeg", width = 15, height = 13, units = "cm")

# Центральность по посредническу (98)
betw_98_map <- 
  df %>% 
  # group_by(clust_18) %>%
  mutate(betw_CL6 = scale(betw_CL6)) %>%
  # ungroup() %>%
  ggplot()+
  geom_sf(data = st_as_sf(region), col = "grey30", lwd = 0.4, alpha = 0)+
  geom_sf(data = st_as_sf(roads_fixed), col = "grey40", lwd = 0.5, show.legend = F)+
  geom_point(aes(x = lon, y = lat, 
                 size = Census2002/1000, 
                 col = betw_CL6), alpha = 1, show.legend = T)+
  scale_color_viridis_c(name = "Центральность\nпо посредничеству (98 км)")+
  scale_size_continuous(breaks = c(0.1, 1, 5, 20, 50, 100, 500),
                        range = c(0.2, 10), 
                        labels = c("<= 0.1", "1", "5", "20", "50", "100", ">= 500"))+
  coord_sf(crs = pulkovo1942.GK12, datum = NA)+
  theme_void(base_size = 12, base_family = "Arial")+
  theme(legend.position = "bottom")+
  guides(size = FALSE)

# Сохраним рисунок
ggsave(plot = betw_98_map, filename = "betw_98_map.jpeg", path = "plots/",
       dpi = 300, device = "jpeg", width = 15, height = 13, units = "cm")


# ==================================
# 5. Real values vs predicted values
# ==================================

# Создадим data frame с предсказанными значениями
df_cleaned %>% 
  mutate(preds1 = predict(model1),
         preds2 = predict(model2)) -> preds

# Y-axis labels
pop.labels <- seq(0, 200, 25)
pop.labels[-seq(1, 9, 2)] <- ''

# Text annotation
cities_labels <- data_frame(x = c(df %>% 
                                        filter(ShortName == "г. Тюмень") %>% 
                                        pull(dist2Tyumen)/1000,
                                      df %>% 
                                        filter(ShortName == "г. Тобольск") %>% 
                                        pull(dist2Tyumen)/1000,
                                      df %>% 
                                        filter(ShortName == "г. Ишим") %>% 
                                        pull(dist2Tyumen)/1000),
                            y = c(160, 145, 145),
                            label = c('Тюмень','Тобольск','Ишим'))

rayon_centres <- data_frame(Rayon = unique(df$MunicipalDistrict),
                            Centre = c("с. Абатское", "с. Армизонское", "с. Аромашево",
                                       "с. Бердюжье", "с. Вагай", "с. Викулово",
                                       "с. Голышманово", "г. Заводоуковск", "с. Исетское",
                                       "г. Ишим", "с. Казанское", "с. Нижняя Тавда", 
                                       "с. Омутинское", "с. Сладково", "с. Большое Сорокино",
                                       "г. Тобольск", "г. Тюмень", "с. Уват", 
                                       "с. Упорово", "с. Юргинское", "г. Ялуторовск",
                                       "с. Ярково"))


# Постановка проблемы

problematization_plot <- df %>%
  filter(pop2010to2002_rel < 200) %>%                                # remove outliers
  filter(dist2Tyumen < 510000) %>%                                     # remove outliers
  filter(!(pop2010to2002_rel == 0 & MunicipalDistrict == "Тюменский район")) %>%
  filter(!(pop2010to2002_rel == 0 & MunicipalDistrict == "Тобольский район")) %>%
  ggplot(aes(x = dist2Tyumen/1000, y = pop2010to2002_rel))+
  geom_point(aes(size = Census2002), 
             color = "black", alpha = 0.4)+
  geom_point(data = df %>% 
               filter(ShortName %in% rayon_centres$Centre), 
             mapping = aes(size = Census2002), alpha = 1, fill = "white",
             shape = 21, stroke = 0.5, show.legend = T)+
  geom_point(data = df %>% 
               filter(pop2010to2002_rel == 0) %>% filter(dist2Tyumen < 510000),
             aes(size = Census2002), 
             color = "black", alpha = 0.4, show.legend = F)+
  stat_smooth(method = "loess", col = "red",
              lwd = 0.6, alpha = 1, se = F, span = 0.2)+
  geom_text(data = cities_labels, 
            aes(x=x,y=y,label=label),
            color = "black", fontface = "italic",
            size=3, hjust=0.5, vjust=0)+
  geom_curve(data = data.frame(x=cities_labels$x,
                               xend=cities_labels$x,
                               y=cities_labels$y - 5, 
                               yend=cities_labels$y - 30),
             aes(x=x,y=y,xend=xend,yend=yend),
             color='black', size=.15, curvature = 0,
             arrow = arrow(type="closed", length = unit(0.1,"cm")))+
  geom_hline(aes(yintercept = 100), linetype = "dashed", col = "grey3")+
  scale_size_continuous(name = "Население (2002), тыс. чел.",
                        breaks = rev(c(0, 100, 1000, 5000, 20000, 100000)), 
                        labels = rev(c("< 0.1", "0.1-1", "1-5", "5-20", "20-100", ">100")), 
                        range = c(0.9, 10), 
                        guide = guide_legend(title.position = "top"))+
  scale_x_continuous(name = "Расстояние до Тюмени, км", 
                     breaks = seq(0, 500, 50),
                     minor_breaks = seq(0 , 500, 50))+
  scale_y_continuous(name = "Динамика числ. нас-я (2010 к 2002), %",
                     breaks = seq(0, 200, 25), labels = pop.labels)+
  theme_bw(base_size = 12, base_family = "Helvetica")+
  theme(panel.grid = element_blank(),
        axis.ticks = element_line(),
        plot.margin=unit(c(0.5,0.5,2,0.5),"cm"),
        legend.position = c(0.45, -0.22),
        legend.direction = "horizontal",
        legend.title = element_text(size = 12),
        legend.box = "horizontal")+
  guides(fill = FALSE)

ggsave(plot = problematization_plot, filename = "problematization_plot.jpeg", 
       device = "jpeg", path = "plots/", 
       dpi = 1200, width = 18, height = 13, units = "cm")


# raw
problematization_plot2 <- df %>%
  filter(pop2010to2002_rel < 200) %>%                                # remove outliers
  filter(dist2Tyumen < 510000) %>%                                     # remove outliers
  filter(!(pop2010to2002_rel == 0 & MunicipalDistrict == "Тюменский район")) %>%
  filter(!(pop2010to2002_rel == 0 & MunicipalDistrict == "Тобольский район")) %>%
  ggplot(aes(x = dist2Tyumen/1000, y = pop2010to2002_rel))+
  geom_point(aes(size = Census2002), 
             color = "black", alpha = 0.4)+
  geom_point(data = df %>% 
               filter(ShortName %in% rayon_centres$Centre), 
             mapping = aes(size = Census2002), alpha = 1, fill = "white",
             shape = 21, stroke = 0.5, show.legend = T)+
  geom_point(data = df %>% 
               filter(pop2010to2002_rel == 0) %>% filter(dist2Tyumen < 510000),
             aes(size = Census2002), 
             color = "black", alpha = 0.4, show.legend = F)+
  geom_hline(aes(yintercept = 100), linetype = "dashed", col = "grey3")+
  scale_size_continuous(name = "Население (2002), тыс. чел.",
                        breaks = rev(c(0, 100, 1000, 5000, 20000, 100000)), 
                        labels = rev(c("< 0.1", "0.1-1", "1-5", "5-20", "20-100", ">100")), 
                        range = c(0.9, 10), 
                        guide = guide_legend(title.position = "top"))+
  scale_x_continuous(name = "Расстояние до Тюмени, км", 
                     breaks = seq(0, 500, 50),
                     minor_breaks = seq(0 , 500, 50))+
  scale_y_continuous(name = "Динамика числ. нас-я (2010 к 2002), %",
                     breaks = seq(0, 200, 25), labels = pop.labels)+
  theme_bw(base_size = 12, base_family = "Helvetica")+
  theme(panel.grid = element_blank(),
        axis.ticks = element_line(),
        plot.margin=unit(c(0.5,0.5,2,0.5),"cm"),
        legend.position = c(0.45, -0.22),
        legend.direction = "horizontal",
        legend.title = element_text(size = 12),
        legend.box = "horizontal")+
  guides(fill = FALSE)

ggsave(plot = problematization_plot2, filename = "problematization_plot2.jpeg", 
       device = "jpeg", path = "plots/", 
       dpi = 1200, width = 18, height = 13, units = "cm")


# Predicted values
FigA3 <- df %>%
  filter(pop2010to2002_rel < 200) %>%                                # remove outliers
  filter(dist2Tyumen < 510000) %>%                                     # remove outliers
  filter(!(pop2010to2002_rel == 0 & MunicipalDistrict == "Тюменский район")) %>%
  filter(!(pop2010to2002_rel == 0 & MunicipalDistrict == "Тобольский район")) %>%
  ggplot(aes(x = dist2Tyumen/1000, y = pop2010to2002_rel))+
  geom_point(aes(size = Census2002), 
             color = "black", alpha = 0.4)+
  geom_point(data = df %>% 
               filter(ShortName %in% rayon_centres$Centre), 
             mapping = aes(size = Census2002), alpha = 1, fill = "white",
             shape = 21, stroke = 0.5, show.legend = T)+
  geom_point(data = df %>% 
               filter(pop2010to2002_rel == 0) %>% filter(dist2Tyumen < 510000),
             aes(size = Census2002), 
             color = "black", alpha = 0.4, show.legend = F)+
  geom_point(data = preds %>% filter(dist2Tyumen < 510000), aes(x = dist2Tyumen/1000, y = preds2, size = Census2002), 
             color = "red", alpha = 0.4)+
  geom_hline(aes(yintercept = 100), linetype = "dashed", col = "grey3")+
  scale_size_continuous(name = "Население (2002), тыс. чел.",
                        breaks = rev(c(0, 100, 1000, 5000, 20000, 100000)), 
                        labels = rev(c("< 0.1", "0.1-1", "1-5", "5-20", "20-100", ">100")), 
                        range = c(0.9, 10), 
                        guide = guide_legend(title.position = "top"))+
  scale_x_continuous(name = "Расстояние до Тюмени, км", 
                     breaks = seq(0, 500, 50),
                     minor_breaks = seq(0 , 500, 50))+
  scale_y_continuous(name = "Динамика числ. нас-я (2010 к 2002), %",
                     breaks = seq(0, 200, 25), labels = pop.labels)+
  theme_bw(base_size = 12, base_family = "Helvetica")+
  theme(panel.grid = element_blank(),
        axis.ticks = element_line(),
        plot.margin=unit(c(0.5,0.5,2,0.5),"cm"),
        legend.position = c(0.45, -0.22),
        legend.direction = "horizontal",
        legend.title = element_text(size = 12),
        legend.box = "horizontal")+
  guides(fill = FALSE)

# Сохраним рисунок
ggsave(plot = FigA3, filename = "FigA3.jpeg", 
       device = "jpeg", path = "plots/Иллюстрации для статьи/", 
       dpi = 1200, width = 18, height = 13, units = "cm")

cowplot::ggsave(plot = FigA3, filename = "FigA3.eps", path = "plots/Иллюстрации для статьи/", 
                width = 18, height = 22, units = "cm", device = cairo_ps)