# Fig 2. Динамика населения Тюменской области в 1990-2018 гг.

library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(readr)
# library(wesanderson)

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

study_period <-
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
  theme_bw(base_size = 12, base_family = "Times New Roman")+
  theme(panel.grid = element_blank(),
        axis.ticks = element_line(),
        axis.title = element_blank(),
        legend.title = element_blank(),
        legend.background = element_rect(fill="white", colour ="white"),
        legend.position = c(.13,.15))

ggsave(study_period, 
       filename = "Fig2.jpeg", path = "plots/", 
       device = "jpeg", dpi = 300, height = 2.7, width = 5)
