# Fig. XX. Case region

library(sp)
library(raster)
library(dplyr)
library(tidyr)
library(sf)
library(ggplot2)
library(ggthemes)
library(gridExtra)
library(RColorBrewer)

# =================================
# 1. Reading data and preprocessing
# =================================

load("data/crs.Rdata")
load("data/settlements.Rdata")
load("data/Part3_res_dataset.Rdata")

# Regional borders in Russia
russia <- shapefile("data/Case region/Russia_AdmDivision/Russia.shp")

# Elevation (ASTER GDEM 2011) 
r <- raster("data/Case region/ASTER_GDEM_TyumenProvince.tif")
r_reproj <- projectRaster(from = r, crs = pulkovo1942.GK12)

# Landscape
rivers <- shapefile("data/Case region/Gidrology/Main_rivers.shp") %>% 
  spTransform(x = ., CRSobj = pulkovo1942.GK12)
roads <- shapefile("data/Case region/MainRoads/Main_roads.shp") %>% 
  spTransform(x = ., CRSobj = pulkovo1942.GK12)
railRoads <- shapefile("data/Case region/RailWays2/RailWays_lines.shp") %>% 
  spTransform(x = ., CRSobj = pulkovo1942.GK12)


# Define helper fucntion to extract legend from ggplot object
# Source: https://stackoverflow.com/questions/12041042/how-to-plot-just-the-legends-in-ggplot2
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  legend
}

# ================
# 2. Separate maps
# ================

# 2.1. Context map (Tyumen oblast aming Russian regions)

# Extract names of the regions
region_names <- russia@data$name_en

# Simplify polygons geometry
russia %>% rgeos::gSimplify(tol = 1000, topologyPreserve = T) -> russia



# Transform CRS
AlbersSiberia <- "+proj=aea +lat_1=52 +lat_2=64 +lat_0=0 +lon_0=105 +x_0=18500000 +y_0=0 +ellps=krass +units=m +towgs84=28,-130,-95,0,0,0,0 +no_defs"
russia %>% spTransform(AlbersSiberia) -> russia

# Plot
russia_plot <- ggplot()+
  geom_sf(data = st_as_sf(russia), fill = "white", col = "black", lwd = 0.5)+
  geom_sf(data = st_as_sf(russia[which(region_names == "Tyumen Oblast")]), 
          col = "black", fill = "red", lwd = 0.5)+
  coord_sf(crs = AlbersSiberia, datum = NA)+
  theme_void()

# 2.2. Main map

# Convert Raster object into data-frame
elevation_spdf <- as(r_reproj, "SpatialPixelsDataFrame") %>% as.data.frame()
colnames(elevation_spdf) <- c("value", "x", "y")

# Labels
cities_labels_ru <- data_frame(lon = c(12260298, 12446121, 12520841), 
                            lat = c(6332398, 6430103, 6200085), 
                            label = c("Тюмень", "Тобольск", "Ишим"))

directions_labels_ru <- data_frame(x = c(12690000, 12300000, 12250298, 12680000, 12680000, 12529841), 
                                   y = c(6657000, 6240000, 6330000, 6260000, 6193000, 6120000), 
                                   label = c("в автономные округа", 
                                             "на Курган", 
                                             "на Урал",
                                             "на Омск",
                                             "Транссиб",
                                             "в Казахстан"))

# Plot
case_region_plot <- ggplot()+
  geom_sf(data = st_as_sf(region), fill = "white", lwd = 0)+
  geom_tile(data=elevation_spdf, aes(x=x, y=y, fill=value), alpha=1)+
  scale_fill_gradientn(name = "Высота, м",
                       colours = rev(brewer.pal(5, 'RdYlGn'))[1:4],
                       limits = c(0, 160),
                       breaks = c(0, 40, 80, 120, 160),
                       space = "Lab", na.value = NA, guide = guide_legend(title.position = "top"))+
  geom_sf(data = st_as_sf(rivers), col = "steelblue3", lwd = 0.7)+
  geom_sf(data = st_as_sf(roads), col = "black", lwd = 0.7)+
  geom_sf(data = st_as_sf(railRoads), col = "black", lwd = 0.7, lty = 4)+
  geom_point(data = df, aes(x = lon, y = lat, size = Census2002/1000),
             pch = 21, col = "black", fill = "white", stroke = 0.7)+
  geom_text(data = cities_labels_ru, 
            aes(x=lon-5000, y=lat+22000, label=label),
            family = "Times New Roman",
            color = "black", fontface = "bold", 
            size=4, hjust="right", fill = "white", alpha = 1)+
  geom_text(data = directions_labels_ru, 
             aes(x=x, y=y, label=label),
             family = "Times New Roman", hjust = "right",
             color = "black", fontface = "bold.italic", 
             size=3.5)+
  scale_size_continuous(name = "Население,\nтыс. чел. (2010)", breaks = c(0.1, 1, 5, 20, 50, 100, 500),
                        range = c(0.3, 13), labels = c("< 0.1", "1", "5", "20", "50", "100", "> 500"))+
  scale_x_continuous(expand = c(.1, .1))+
  coord_sf(crs = pulkovo1942.GK12, datum = NA)+
  ggsn::scalebar(st_as_sf(region), dist = 50, 
                 location = "bottomleft", dd2km = FALSE, 
                 height = 0.01, st.size = 3)+
  theme_minimal(base_family = "Times New Roman", base_size = 12)+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank(),
        plot.margin=unit(c(0.1,0.1,0.1,0.1),"cm"))

# Extract legends
fill_legend <- g_legend(case_region_plot + 
                          guides(fill = guide_legend(direction = "horizontal",
                                                    title.position = "top", 
                                                    label.position = "bottom"), 
                                 size = FALSE)+
                          theme(legend.position = 'bottom',
                                legend.spacing.x = unit(0, "lines"),
                                legend.key.size = unit(1, 'lines')))

size_legend <- g_legend(case_region_plot + 
                          guides(fill = FALSE,
                                 legend.spacing.y = unit(0, "lines")))

# ===============
# 3. Combine maps
# ===============

par(mar=c(0,0,0,0))

gg <- ggplot()+
  coord_equal(xlim = c(0, 10), ylim = c(0, 10), expand = c(0.1,0.1))+
  annotation_custom(ggplotGrob(case_region_plot + 
                                 guides(fill = FALSE, size = FALSE)),
                    xmin = 0, xmax = 10, ymin = 0, ymax = 10)+
  annotation_custom(ggplotGrob(russia_plot),
                    xmin = 0, xmax = 3.5, ymin = 7, ymax = 9)+
  annotation_custom(fill_legend,
                    xmin = 7.1, xmax = 9.1, ymin = 0.5, ymax = 3)+
  annotation_custom(size_legend,
                    xmin = 7.5, xmax = 9, ymin = 2.2, ymax = 6.2)+
  labs(x = NULL, y = NULL)+
  theme_void()

# Save the plot
ggsave(filename = "case_region.jpeg", path = "plots/",
       plot = gg, device = "jpeg", width = 7, height = 7)