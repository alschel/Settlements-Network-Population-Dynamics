library(dplyr)
library(ggplot2)

# Fig. Appendix. Результаты статистических тестов

load("data/Optimal_Cluster_Test2002.Rdata")

frey <- data_frame(n = 2:50, val = NbClust_outputs_2002[[1]][[1]]) 
cindex <- data_frame(n = 2:50, val = NbClust_outputs_2002[[3]][[1]]) 
dunn <- data_frame(n = 2:50, val = NbClust_outputs_2002[[5]][[1]])

# plot

x_ticks <- seq(1, 50, 1)
x_ticks[-c(1, seq(0, 51, 5))] <- ""


frey_plot <- frey %>% 
  ggplot(aes(x = n, y = val))+
  geom_col(fill = "grey30", width = 0.8)+
  ggtitle(label = "frey")+
  scale_y_continuous(name = "Значение индекса",
                     breaks = seq(0, 1.2, 0.2), 
                     limits = c(0, 1.2))+
  scale_x_continuous(name = "Число кластеров", 
                     breaks = seq(1, 50, 1), labels = x_ticks)+
  theme_bw(base_size = 12, base_family = "Times New Roman")+
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank())

cindex_plot <- cindex %>% 
  ggplot(aes(x = n, y = val))+
  geom_col(fill = "grey30", width = 0.8)+
  ggtitle(label = "cindex")+
  scale_y_continuous(name = "Значение индекса",
                     breaks = seq(0, 0.3, 0.05), limits = c(0, 0.3))+
  scale_x_continuous(name = "Число кластеров", 
                     breaks = seq(1, 50, 1), labels = x_ticks)+
  theme_bw(base_size = 12, base_family = "Times New Roman")+
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        title = element_text())

dunn_plot <- dunn %>% 
  ggplot(aes(x = n, y = val))+
  geom_col(fill = "grey30", width = 0.8)+
  ggtitle(label = "dunn")+
  scale_y_continuous(name = "Значение индекса",
                     breaks = seq(0, 0.05, 0.01), limits = c(0, 0.05))+
  scale_x_continuous(name = "Число кластеров", 
                     breaks = seq(1, 50, 1), labels = x_ticks)+
  theme_bw(base_size = 12, base_family = "Times New Roman")+
  theme(panel.grid = element_blank())

par(mar=c(0,0,0,0))

Appendix_fig_1 <- ggplot()+
  coord_equal(xlim = c(0, 10), ylim = c(0, 9), expand = c(0.1,0.1))+
  annotation_custom(ggplotGrob(frey_plot),
                    xmin = 0, xmax = 10, ymin = 6, ymax = 9)+
  annotation_custom(ggplotGrob(cindex_plot),
                    xmin = 0, xmax = 10, ymin = 3, ymax = 6)+
  annotation_custom(ggplotGrob(dunn_plot),
                    xmin = 0, xmax = 10, ymin = 0, ymax = 3)+
  theme_void()

# Save the plot
ggsave(filename = "Appendix_fig_1.jpeg", path = "plots/", dpi = 300,
       plot = Appendix_fig_1, device = "jpeg", width = 7, height = 6)
