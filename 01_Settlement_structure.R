library(dplyr)
library(ggplot2)
library(tidyr)



load("data/res_dataset.Rdata")


population_dynamics_trans %>% 
  filter(Period == "1990-2002", init_pop > 0) %>%
  mutate(Cohort = factor(case_when(
    init_pop > 500000 ~ "более 500000",
    init_pop > 100000 ~ "100001 - 500000",
    init_pop > 50000 ~ "50001 - 100000",
    init_pop > 20000 ~ "20001 - 50000",
    init_pop > 5000 ~ "5001 - 20000",
    init_pop > 3000 ~ "3001-5000",
    init_pop > 1000 ~ "1001-3000",
    init_pop > 500 ~ "501-1000",
    init_pop > 200 ~ "201-500",
    init_pop > 100 ~ "101-200",
    init_pop > 50 ~ "51-100",
    init_pop > 10 ~ "11-50",
    init_pop > 0 ~ "1-10"),
    levels = c("1-10", "11-50", "51-100", "101-200", "201-500", "501-1000",
               "1001-3000", "3001-5000", "5001 - 20000", "20001 - 50000", "50001 - 100000", 
               "100001 - 500000", "более 500000"))) -> data_1990

table(data_1990[data_1990$init_pop < 20000,]$Cohort)

population_dynamics_trans %>% 
  filter(Period == "2002-2010", res_pop > 0) %>%
  mutate(Cohort = factor(case_when(
    res_pop > 500000 ~ "более 500000",
    res_pop > 100000 ~ "100001 - 500000",
    res_pop > 50000 ~ "50001 - 100000",
    res_pop > 20000 ~ "20001 - 50000",
    res_pop > 5000 ~ "5001 - 20000",
    res_pop > 3000 ~ "3001-5000",
    res_pop > 1000 ~ "1001-3000",
    res_pop > 500 ~ "501-1000",
    res_pop > 200 ~ "201-500",
    res_pop > 100 ~ "101-200",
    res_pop > 50 ~ "51-100",
    res_pop > 10 ~ "11-50",
    res_pop > 0 ~ "1-10"),
    levels = c("1-10", "11-50", "51-100", "101-200", "201-500", "501-1000",
               "1001-3000", "3001-5000", "5001 - 20000", "20001 - 50000", "50001 - 100000", 
               "100001 - 500000", "более 500000"))) -> data_2010

data_2010 %>% nrow()
