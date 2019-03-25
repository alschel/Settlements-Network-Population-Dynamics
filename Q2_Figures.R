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
  filter(!(pop2010to2002_rel == 0 & MunicipalDistrict == "Тюменский район")) %>%
  # Выделим из набора данных интересующие нас предикторы  
  dplyr::select(clust_6, clust_18, Census2002, pop2010to2002_rel, starts_with("clo"), starts_with("betw"), -ends_with("w"), - clo) ->
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
corr <- cor(df_cleaned %>% select(pop2010to2002_rel, logPop, clo_CL6, clo_CL18, betw_CL6, betw_CL18))
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
fig_8 <- ggplot()+
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
ggsave(plot = fig_8, filename = "Fig8.jpeg", path = "plots/Иллюстрации для статьи/", 
       dpi = 200, device = "jpeg", width = 18, height = 22, units = "cm")

cowplot::ggsave(plot = fig_8, filename = "Fig8.eps", path = "plots/Иллюстрации для статьи/", 
                width = 18, height = 22, units = "cm", device = cairo_ps)
