# Топология сети населенных пунктов как фактор динамики сельского расселения 
# (на примере Тюменской области)
# Александр Шелудков, 2019

# Регион исследования 
# Рис. 2. Население Тюменской области в 1990–2018 гг., тыс. чел.

library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)

# Description:
# '000 people
# 1990-2002: retrospective re-assessment, based on 2002 population census data (Tyumenstat)
# 2004-2015: Rosstat (federal data base), includes retrospective re-assessment for 2004-2010 data 
# and current account for 2011-2018 
# (source:http://www.gks.ru/wps/wcm/connect/rosstat_main/rosstat/ru/statistics/publications/catalog/afc8ea004d56a39ab251f2bafc3a6fce)

regional_population <- read_csv("data/Regional_population.csv") %>% 
  filter(Year > 1989) # we need only data from 1990

# Plot

year.labels <- 1990:2018
year.labels[-seq(1, 28, 5)] <- ''

fig_2 <-
  regional_population %>% 
  gather(Type, Population, Urban:Rural) %>% 
  ggplot(aes(x = Year, y = Population, linetype = Type)) +
  geom_rect(data = NULL, aes(xmin = 2002, 
                             xmax = 2010,
                             ymin = -Inf,
                             ymax = +Inf),
            fill="grey90", inherit.aes = F)+
  geom_line(lwd = 0.8, show.legend = T)+
  scale_x_continuous(breaks = 1990:2018, labels=year.labels, limits = c(1990, 2018))+
  scale_y_continuous(limits = c(300, 1000), breaks = seq(300, 1000, 100))+
  scale_linetype_discrete(labels = c("сельское", "городское"))+
  theme_bw(base_size = 12, base_family = "Arial")+
  theme(panel.grid = element_blank(),
        axis.ticks = element_line(),
        axis.title = element_blank(),
        legend.title = element_blank(),
        legend.background = element_rect(fill="white", colour ="white"),
        legend.position = c(.11,.15))

# Экспорт
ggsave(plot = fig_2, filename = "Fig2.jpeg", path = "plots/Иллюстрации для статьи/", 
       dpi = 200, device = "jpeg", width = 18, height = 10, units = "cm")

cowplot::ggsave(plot = fig_2, filename = "Fig2.eps", path = "plots/Иллюстрации для статьи/", 
                width = 18, height = 10, units = "cm", device = cairo_ps)
