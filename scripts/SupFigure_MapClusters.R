#  Figure Supplementary
#  Spatial distribution of clusters in the Contiguous US

rm(list = ls())
Sys.setenv(TZ='UTC')

if (!require("pacman"))install.packages("pacman")
pacman::p_load(tidyverse, sf, rgdal)

# Read data
data <- read.csv('data/Code_data/all_groups.csv')

data <- data %>% 
  mutate(cluster = case_when(
    Group_num %in%  1 ~ "Group 1: No change over time",
    Group_num %in%  2 ~ "Group 2: Substantial increase and then maintain",
    Group_num %in%  3 ~ "Group 3: Steady increase over time",
    Group_num %in%  4 ~ "Group 4: Steady decrease over time",
    Group_num %in%  5 ~ "Group 5: Peaks",
    Group_num %in%  6 ~ "Group 6: Troughs",
    Group_num %in%  7 ~ "Group 7: Outliers"
  ))


color <- c("#56B4E9","#009E73","#F0E442","#0072B2","#E69F00","#D55E00","#999999")


shp <- readOGR(dsn = file.path(paste0(getwd(),'/data/s_22mr22.shp')), stringsAsFactors = F)

data <- na.omit(data)

# create map 
data_sf <- st_as_sf(data, coords = c("long", "lat"),crs = 4326)

theme_clusters_map <- function(text_size) {
    
    # scale_y_continuous(breaks = c(20, 30, 40, 50))+
    theme_light()+
    theme(text = element_text(size = text_size))+
    theme(legend.position = 'none')+
    theme(axis.title = element_blank())+
    theme(text = element_text(size = text_size + 3))
  
}
  


# ----------------------  Maps ---------------------------------------------------
p1 <- ggplot() +
  geom_polygon(shp, mapping = aes(x = long, y = lat, group = group),
               colour = "black",fill = 'white')+
  geom_sf(filter(data_sf, Group_num == '1'), mapping = aes(col = cluster), size = 0.5)+
  coord_sf(expand = FALSE, ylim = c(20, 50), xlim=c(-130, -60))+
  scale_y_continuous(breaks = c(20, 30, 40, 50))+
  theme_clusters_map(text_size = 15)+
  scale_color_manual(values = color[1])+
  annotate(geom="text", x = -128, y = 48, label = "a)", size = 10)


p2 <- ggplot() +
  geom_polygon(shp, mapping = aes(x = long, y = lat, group = group),
               colour = "black",fill = 'white')+
  geom_sf(filter(data_sf, Group_num == '2'), mapping = aes(col = cluster), size = 0.5)+
  coord_sf(expand = FALSE, ylim = c(20, 50), xlim = c(-130, -60))+
  scale_y_continuous(breaks = c(20, 30, 40, 50))+
  theme_clusters_map(text_size = 15)+
  scale_color_manual(values = color[2])+
  annotate(geom = "text", x = -128, y = 48, label = "b)", size = 10)

p3 <- ggplot() +
  geom_polygon(shp, mapping = aes(x = long, y = lat, group = group),
               colour = "black",fill = 'white')+
  geom_sf(filter(data_sf, Group_num == '3'), mapping = aes(col = cluster),size = 0.5)+
  coord_sf(expand = FALSE, ylim = c(20, 50), xlim=c(-130, -60))+
  scale_y_continuous(breaks = c(20, 30, 40, 50))+
  theme_clusters_map(text_size = 15)+
  scale_color_manual(values = color[3])+
  annotate(geom = "text", x = -128, y = 48, label= "c)", size = 10)

p4 <- ggplot() +
  geom_polygon(shp, mapping = aes(x = long, y = lat, group = group),
               colour = "black",fill = 'white')+
  geom_sf(filter(data_sf, Group_num == '4'), mapping = aes(col = cluster), size = 0.5)+
  coord_sf(expand = FALSE, ylim = c(20, 50),xlim = c(-130, -60))+
  scale_y_continuous(breaks = c(20, 30, 40, 50))+
  theme_clusters_map(text_size = 15)+
  scale_color_manual(values = color[4])+
  annotate(geom = "text", x = -128, y = 48, label = "d)",size = 10)


p5 <- ggplot() +
  geom_polygon(shp, mapping = aes(x = long, y = lat, group = group),
               colour = "black",fill = 'white')+
  geom_sf(filter(data_sf, Group_num == '5'), mapping = aes(col = cluster), size = 0.5)+
  coord_sf(expand = FALSE, ylim = c(20, 50),xlim = c(-130, -60))+
  scale_y_continuous(breaks = c(20, 30, 40, 50))+
  theme_clusters_map(text_size = 15)+
  scale_color_manual(values = color[5])+
  annotate(geom="text", x = -128, y = 48, label = "e)", size = 10)

p6 <- ggplot() +
  geom_polygon(shp, mapping = aes(x = long, y = lat, group = group),
               colour = "black", fill = 'white')+
  geom_sf(filter(data_sf, Group_num == '6'), mapping = aes(col = cluster), size = 0.5)+
  coord_sf(expand = FALSE, ylim = c(20, 50), xlim = c(-130, -60))+
  scale_y_continuous(breaks = c(20, 30, 40, 50))+
  theme_clusters_map(text_size = 15)+
  scale_color_manual(values = color[6])+
  annotate(geom ="text", x = -128, y = 48, label = "f)", size = 10)

p7 <- ggplot() +
  geom_polygon(shp, mapping = aes(x = long, y = lat, group = group),
               colour = "black",fill = 'white')+
  geom_sf(filter(data_sf, Group_num == '7'), mapping = aes(col = cluster), size = 0.5)+
  coord_sf(expand = FALSE, ylim = c(20, 50), xlim=c(-130, -60))+
  scale_y_continuous(breaks = c(20, 30, 40, 50))+
  theme_clusters_map(text_size = 15)+
  scale_color_manual(values = color[7])+
  annotate(geom = "text", x = -128, y = 48, label = "g)", size = 10)

layout = rbind(
  c(1, 2),
  c(3, 4),
  c(5, 6),
  c(7, NA)
)

fig_maps_clusters <- grid.arrange(p1, p2, p3, p4, p5, p6, p7, layout_matrix=layout)

ggsave(fig_maps_clusters,
       file = "figures/SupFigure_ClustersCONUS.png",
       units = "in",
       width = 12, height = 20)






