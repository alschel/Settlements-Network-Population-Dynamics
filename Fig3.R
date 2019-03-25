# Топология сети населенных пунктов как фактор динамики сельского расселения 
# (на примере Тюменской области)
# Александр Шелудков, 2019

# Методы 
# Рис. 3. Значения центральности по близости (Б) и по посредничеству (П) на примере гипотетической сети

library(readr)
library(igraph)
library(tidyr)
library(dplyr)
library(visNetwork)
library(ggplot2)
library(network)
library(ggnetwork)

# Load adjacency_matrix
m <- read_csv("data/Sample graph.csv") %>% .[,-1] %>% as.matrix()

# Create network and igraph objects
g <- graph_from_adjacency_matrix(m, mode = "undirected")
network_g <- network(m)

# Calculate centrality measures, scale them and assign to nodes of network
network_g %v% "П" <- igraph::betweenness(g) %>% scale() %>% .[,1]
network_g %v% "Б" <- igraph::closeness(g) %>% scale() %>% .[,1]

par(margin(b = 1))

# Plot the graph and compare centrality measures
fig_3 <- fortify(network_g) %>% 
  gather(measure, value, `П`:`Б`) %>%
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(color = "grey50") +
  geom_nodes(aes(size = value, color = value))+
  # geom_nodes(aes(color = value), size = 5, show.legend = F)+
  geom_text(aes(x = x + 0.07, y = y + 0.07, label = round(value, 1)))+
  scale_size_continuous(range = c(1,8))+
  scale_y_continuous(expand = c(0.1,0.1,0.1,0.1))+
  scale_color_viridis_c(name = "Центральность")+
  facet_grid(.~measure)+
  theme_void(base_size = 12, base_family = "Arial")+
  theme(legend.position = c(0.1,0.1), strip.text = element_text(size = 14))+
  guides(size = FALSE, colour = guide_colorbar(direction = "horizontal", title.position = "top"))

# Экспорт
ggsave(plot = fig_3, filename = "Fig3.jpeg", path = "plots/Иллюстрации для статьи/", 
       dpi = 200, device = "jpeg", width = 18, height = 9, units = "cm")

cowplot::ggsave(plot = fig_3, filename = "Fig3.eps", path = "plots/Иллюстрации для статьи/", 
                width = 18, height = 9, units = "cm", device = cairo_ps)
