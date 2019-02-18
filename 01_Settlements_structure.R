# Fig. XX. Settlement structure

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(wesanderson)
library(colorblindr)

# ================
# 1. Preprocessing
# ================

rosstat_pop_tidy <- read_delim("data/rosstat_pop_tidy.csv", delim = ",")
rosstat_pop_tidy %>% 
  filter(Year >= 2002, !is.na(Population), Population > 0) %>%
  select(-Adm_1, -Adm_2)-> data

# =====================
# 2. General statistics
# =====================

# Общая численность населения
data %>% 
  group_by(Year) %>% 
  summarise(sum(Population))

# Year `sum(Population)`
# 2002           1347264
# 2010           1326877

# Число населенных пунктов
data %>% 
  group_by(Year) %>% 
  summarise(n())

# Сколько людей жило в городах?
data %>% 
  filter(ShortName %in% c("г. Тюмень", "г. Тобольск", "г. Ишим", "г. Ялуторовск", "г. Заводоуковск")) %>% 
  group_by(Year) %>% 
  summarise(sum(Population))

# =========================
# 3. Settlements' structure
# =========================

# Определим классы наблюдений
data %>%
  mutate(Cohort = factor(case_when(
    Population > 500000 ~ "более 500000",
    Population > 100000 ~ "100001 - 500000",
    Population > 50000 ~ "50001 - 100000",
    Population > 20000 ~ "20001 - 50000",
    Population > 5000 ~ "5001 - 20000",
    Population > 3000 ~ "3001-5000",
    Population > 1000 ~ "1001-3000",
    Population > 500 ~ "501-1000",
    Population > 200 ~ "201-500",
    Population > 100 ~ "101-200",
    Population > 50 ~ "51-100",
    Population > 10 ~ "11-50",
    Population > 0 ~ "1-10"),
                         levels = c("1-10", "11-50", "51-100", "101-200", "201-500", "501-1000",
                                    "1001-3000", "3001-5000", "5001 - 20000", "20001 - 50000", "50001 - 100000", 
                                    "100001 - 500000", "более 500000"))) -> data

# Структура населеных пунктов по людности и общая численность населения

data %>% 
  group_by(Year, Cohort) %>% 
  summarise(Amount = n()) %>% View()
data %>% 
  group_by(Year, Cohort) %>% 
  summarise(Population = sum(Population)) %>% View()


# Visualize

# Число н.п. по категориям

n.labels <- seq(0, 300, 25)
n.labels[-seq(1, 13, 2)] <- ''

data %>%
  filter(Population < 20000) %>% 
  group_by(Year, Cohort) %>% 
  summarise(Number = n()) %>% 
  ggplot(aes(fill = as.factor(Year), y = Number, x = Cohort))+
  geom_col(position = position_dodge(0.6), width = 0.6)+
  geom_text(aes(label = Number),
            position = position_dodge(0.6), vjust = -0.5, size = 3.5)+
  scale_fill_manual(values = wes_palette(n = 2, name = "Chevalier1") %>% alpha(0.9))+
  scale_y_continuous(breaks = seq(0, 300, 25), labels = n.labels)+
  theme_bw(base_size = 14)+
  # coord_flip()+
  theme(panel.grid = element_blank(),
        axis.ticks = element_line(),
        axis.title = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.9,0.9),
        legend.background = element_rect(fill="white", size=0.5, colour ="grey50"))

# Число жителей по категориям

data %>%
  # filter(Population < 20000) %>% 
  group_by(Year, Cohort) %>% 
  summarise(Population = sum(Population)) %>% 
  ggplot(aes(fill = as.factor(Year), y = Population, x = Cohort))+
  geom_col(position = position_dodge(0.6), width = 0.6)+
  geom_text(aes(label = round(Population/1000, 1)),
            position = position_dodge(0.6), vjust =-0.7, size = 3.5)+
  scale_fill_manual(values = wes_palette(n = 2, name = "Chevalier1"))+
  # scale_y_continuous(trans = "log")+
  # coord_flip()+
  theme_bw(base_size = 14)+
  theme(panel.grid = element_blank(),
        axis.ticks = element_line(),
        axis.title = element_blank(),
        legend.title = element_blank(),
        # legend.position = c(0.9,0.9),
        legend.background = element_rect(fill="white", size=0.5, colour ="grey50"))


# А что если попробовать проследить судьбу каждого? 
# Динамика структуры населенных пунктов с населением до 3000 чел. по людности, 2002-2010
data %>% 
  filter(Population < 3000) %>%
  ggplot(aes(x = as.factor(Year), y = Population, color = Cohort))+
  geom_point(pch = 1)+
  geom_line(aes(group = id), lwd = 0.06)+
  geom_hline(yintercept = c(10, 50, 100, 200, 500, 1000), lwd = 0.2, lty = 2)+
  scale_y_continuous(name = "Население, чел.",
                     trans = "log", breaks = c(0, 10, 50, 100, 200, 500, 1000, 3000))+
  scale_x_discrete(name = element_blank())+
  theme_bw(base_family = "Times New Roman", base_size = 12)+
  scale_color_OkabeIto(name = "Категория н.п.\nпо людности")+
  theme(panel.grid = element_blank(),
        axis.ticks = element_line(),
        # legend.title = element_blank(),
        # legend.position = c(0.9,0.2),
        legend.background = element_rect(fill="white", size=0.5, colour ="grey50"))


# # График с накоплением
# rosstat_pop_tidy %>%
#   filter(Population < 20000) %>%
#   group_by(Year) %>% 
#   arrange(Population) %>% 
#   mutate(Cumsum = cumsum(Population/1000)) %>%
#   ggplot(aes(x = Population, y = Cumsum, color = factor(Year)))+
#   geom_step()+
#   scale_y_continuous(breaks = seq(0, 1500, 100))+
#   scale_x_continuous(
#     breaks = c(0, 100, 1000, 3000, 5000, 10000, 15000, 20000),
#     trans = "sqrt"
#   )+
#   labs(x = "Population, '000", y = "Cumulative sum, '000")+
#   theme_bw(base_size = 14)+
#   theme(panel.grid = element_blank(),
#         axis.ticks = element_line(),
#         legend.title = element_blank(),
#         legend.position = c(0.9,0.2),
#         legend.background = element_rect(fill="white", size=0.5, colour ="grey50"))
