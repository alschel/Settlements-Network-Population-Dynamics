# The Impact of Settlements Network Structure on Population Dynamics
# Part 5. Model
# Author: Alexander Sheludkov
# Date: 26 November 2018

library(sp)
library(sf)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)

# ========================================
# 1. Сжатие расселения на уровне кластеров
# ========================================

# Как изменилась территориальная дифференциация расселения с 2002 по 2010 гг.?
clusters_18_metrics %>% 
  select(clust_18, CL18_variance_2002, CL18_variance_2010) %>% 
  gather(Year, Variance, CL18_variance_2002:CL18_variance_2010) %>% 
  mutate(Year = as.integer(str_extract(Year, "\\d{4}"))) %>% 
  ggplot(aes(x = clust_18, y = Variance, fill = as.factor(Year)))+
  geom_col(position = position_dodge())+
  scale_x_continuous(breaks = 1:18)
# Вывод 1: во всех кластерах территориальная дифференциация расселения увеличилась
# (то есть расселение сжималось)

# Существуют ли пространственные закономерности в распределении показателя вариации?
ggplot()+
  geom_sf(data = st_as_sf(region), fill = "white")+
  geom_point(data = df, aes(x = lon, y = lat, col = CL18_variance_2010))+
  # geom_raster(data = df, aes(x = lon, y = lat, fill = CL18_variance_2010))+
  scale_color_viridis_c()
# Самая высокая дифференциация расселения у кластеров вокруг городов (Тюмень, Тобольск, Ишим),
# далее идут 11 кластер (Ялуторовск, Заводоуковск, Упорово), затем Голышманово и Викулово+Сорокино.
# Самые низкие показатели вариации - в Исетском районе, а также на ЮВ - в Казанском, Бердюжском,
# Сладковском и Абатском районах + в бывшем Байкаловском районе

# Существуют ли пространственные закономерности в темпах сжатия расселения?
ggplot()+
  geom_sf(data = st_as_sf(region), fill = "white")+
  geom_point(data = df, aes(x = lon, y = lat, col = CL18_variance_dif))+
  # geom_raster(data = df, aes(x = lon, y = lat, fill = CL18_variance_2010))+
  scale_color_viridis_c()
# Да, более восточные (за исключением Ишимског района) и северные (за исключением Уватского 
# и Вагайского районов) сжимаются быстрее

# Как сжатие расселения соотносится с изменением численности населения кластеров?
# Изменение численности населения формирует обратный сжатию расселения пространственный градиент
ggplot()+
  geom_sf(data = st_as_sf(region), fill = "white")+
  geom_point(data = df, aes(x = lon, y = lat, col = CL18_pop2010to2002_rel))+
  # geom_raster(data = df, aes(x = lon, y = lat, fill = CL18_pop2010to2002_rel))+
  scale_color_viridis_c()
# В целом, действует правило: сокращение населения приводит к его сжатию - более крупные населенные
# становятся крупнее, менее крупные - меньше. # По интенсивности депопуляции исжатия расселения 
# кластеры разибваются на две примерно равные группы.
clusters_18_metrics %>%
  ggplot(aes(x = CL18_pop2010to2002_rel, y = CL18_variance_dif))+
  geom_point()+
  geom_circle(data = data_frame(x = c(90, 98), y = c(112, 104)), 
              mapping = aes(x0 = x, y0 = y, r = 5.5), 
              color = "grey70",linetype = "dotted",  alpha = 1,
              inherit.aes = F)
