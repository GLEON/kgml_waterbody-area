#  Spatial distribution of clusters by latitude and longitude

rm(list = ls())
Sys.setenv(TZ='UTC')

if (!require("pacman"))install.packages("pacman")
pacman::p_load(rgdal, sf, tidyverse)

#  Get density plot by latitude

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


color <- c("#56B4E9", "#009E73", "#F0E442", "#0072B2", "#E69F00", "#D55E00", "#999999")

theme_figure <- function(text_size = 14){
  
  theme_bw()+
    #  text
    theme(text = element_text(size = text_size + 2))+
    theme(axis.text = element_text(size = text_size),
          axis.title=element_text(size = text_size))+
    #  panel
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    # legend
    theme(legend.position = 'none')
}

p1 <- ggplot(data, mapping=aes(x=long,color=cluster))+ 
  geom_density()+
  scale_color_manual(values = color)+
  labs(y = expression("Density"), x = expression("Longitude"), colour = " ")+
  theme_figure()

p2 <- ggplot(data, mapping = aes(x = lat, color = cluster))+ 
  geom_density()+
  scale_color_manual(values = color)+
  labs(y = expression("Density"),x = expression("Latitude"), colour = " ")+
  scale_x_continuous(limits = c(20, 50),breaks = c(20, 30, 40, 50))+
  coord_flip()+
  theme_figure()

#  read shape file for map
shp <- readOGR(dsn = file.path(paste0(getwd(),'/data/Code_data/s_22mr22.shp')), stringsAsFactors = F)

data <- na.omit(data)

# create map of lakes
data_sf <- st_as_sf(data, coords = c("long", "lat"),crs = 4326)

# map 
p3 <- ggplot() +
  geom_polygon(shp, mapping= aes(x = long, y = lat, group = group),
               colour = "black",fill = 'white')+
  geom_sf(data_sf, mapping = aes(col = cluster), size = 1)+
  coord_sf(expand = FALSE, ylim = c(20, 50),xlim = c(-130, -60))+
  scale_y_continuous(breaks = c(20, 30, 40, 50))+
  theme_light()+
  theme(legend.position = 'bottom', legend.direction = 'horizontal')+
  theme(text = element_text(size = 15))+
  theme(axis.title = element_blank())+
  theme(text = element_text(size = 18))+
  scale_color_manual(values = color)

empty <- ggplot() + 
  geom_point(aes(1, 1), colour = "white") +
  theme(                              
    plot.background = element_blank(), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    panel.border = element_blank(), 
    panel.background = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank()
  )

figure_cluster_dist <- ggarrange(p1, empty, p3, p2, 
                                 ncol = 2, nrow = 2, widths = c(4, 1), heights = c(1, 4))

ggsave(figure_cluster_dist,
       file = "figures/Figure_DistributionLonLatClusters.png",
       units = "in",
       width = 12, height = 8)

