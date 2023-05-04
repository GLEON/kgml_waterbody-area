#Multivariate analyses of lake area change using LSTM cluster/group output
# 05Jun2022 - PCA w/ lat, long, mean precip, temp, and elevation as drivers 

#set working directory
lake_directory <- here::here()

# loading the vegan library in order to calculate distance matrices
if(!require("pacman")) install.packages("pacman")
pacman::p_load(vegan,ggplot2, gridExtra, MASS, nortest, factoextra, 
               tidyverse, remotes, rcompanion,multcomp,multcompView)

#install pairwise adonis package for post hoc test
#install_github("pmartinezarbizu/pairwiseAdonis/pairwiseAdonis")
library(pairwiseAdonis)

#read in LSTM output
LSTM_df <- read.csv(file.path(lake_directory,"data/all_groups.csv")) %>% select(-c(X.1,X))

#read in driver data
driver_df <- read.csv(file.path(lake_directory,"data/pr_temp_elev_us_1985_2015.csv")) %>% select(-c(X))

#subset df so can manipulate relevant columns for PCA
LSTM_areas <- LSTM_df[,c(1,2,5,8,9,60,61)]

#drop NA rows
LSTM_areas <- LSTM_areas[!is.na(LSTM_areas$AREA),]
driver_df <- driver_df[!is.na(driver_df$Lat),]

#add precip (cm/yr), temp (C), and elev (m) to df and remove lakes that do not have precip/temp data (1643 waterbodies)
LSTM_areas <- merge(LSTM_areas, driver_df, by = c('ID'))
LSTM_areas <- LSTM_areas[,-c(8,9)]
LSTM_areas <- na.omit(LSTM_areas)
colnames(LSTM_areas) <- c("ID", "RESERVOIR", "AREA", "Cluster_50", "Group_num", "long", "lat", "mean_temp", "mean_precip", "mean_elev")

#rename column names
names(LSTM_areas) <- c("ID", "Reservoir","Area","Cluster_50","Group_num",
                       "Longitude","Latitude","Mean Air Temperature",
                       "Mean Precipitation","Mean Elevation")

remove(driver_df, LSTM_df)
gc()

#select only numeric columns
LSTM_areas_noids <- LSTM_areas[,c(3,6:10)]
groups <- LSTM_areas[,c(3,5,6:10)]

#transform driver data (z-score normalization)
LSTM_areas_noids$Area <- (LSTM_areas_noids$Area - mean(LSTM_areas_noids$Area)) / sd(LSTM_areas_noids$Area)
LSTM_areas_noids$Longitude <- (LSTM_areas_noids$Longitude - mean(LSTM_areas_noids$Longitude)) / sd(LSTM_areas_noids$Longitude)
LSTM_areas_noids$Latitude <- (LSTM_areas_noids$Latitude - mean(LSTM_areas_noids$Latitude)) / sd(LSTM_areas_noids$Latitude)
LSTM_areas_noids$`Mean Precipitation` <- (LSTM_areas_noids$`Mean Precipitation` - mean(LSTM_areas_noids$`Mean Precipitation`)) / sd(LSTM_areas_noids$`Mean Precipitation`)
LSTM_areas_noids$`Mean Air Temperature` <- (LSTM_areas_noids$`Mean Air Temperature` - mean(LSTM_areas_noids$`Mean Air Temperature`)) / sd(LSTM_areas_noids$`Mean Air Temperature`)
LSTM_areas_noids$`Mean Elevation` <- (LSTM_areas_noids$`Mean Elevation` - mean(LSTM_areas_noids$`Mean Elevation`)) / sd(LSTM_areas_noids$`Mean Elevation`)

#now pca - spectral decomposition that examines the covariances/correlations between variables
#we have more samples/lakes than features/drivers so princomp is preferred over prcomp
pca <- princomp(LSTM_areas_noids, cor=TRUE, scores=TRUE)

fviz_eig(pca)
#ggsave(file.path(lake_directory,"figures/pca/PCA_screeplot.jpg"),
#       units="in", width=5, height=4, dpi=300, device="jpeg")


#add groups to df
LSTM_areas_noids$Group_num <- groups$Group_num

#manuscript color palatte
ms_colors <- c("#56B4E9","#009E73","#F0E442","#0072B2","#E69F00","#D55E00","#999999")

fviz_pca_biplot(pca,
                addEllipses=TRUE, ellipse.level=0.95,
                palette = ms_colors, pointshape=19,
                geom.ind = "none",
                mean.point = TRUE,
                col.var = "black",
                legend.title = "",
                title = "",
                repel = TRUE,
                #select.ind = list(contrib=2000),
                axes = c(1,2),
                col.ind = as.factor(LSTM_areas_noids$Group_num)) +
    labs(x = "PC1", y = "PC2") + guides(fill=FALSE,
    color=guide_legend(ncol=4)) +
  scale_color_manual(name = "", labels = c(
    "Cluster 1: No change over time",
    "Cluster 2: Substantial increase\n\ and then maintain",
    "Cluster 3: Steady increase\n\ over time", 
    "Cluster 4: Steady decrease\n\ over time",
    "Cluster 5: Peaks", "Cluster 6: Troughs",
    "Cluster 7: Outliers"), values= ms_colors) +
  theme(legend.position = "bottom", legend.direction = "horizontal", 
        legend.key.size = unit(0.5,"cm"),
        plot.margin = unit(c(-0.5,0,0,0), "cm"),
        legend.text=element_text(size=6),
        legend.margin=margin(t = 0, l=-1, unit='cm'))
#ggsave(file.path(lake_directory,"figures/pca/PCA_biplot_dim12_final.jpg"),
#                 units="in", width=5, height=4, dpi=300, device="jpeg")

#-------------------------------------------------------------------------------#
# PERMANOVA (nonparmaetric test; permutational multivariate anova)
#Null = centroid/spread is the same between clusters

#summary stats for lat, long, and area
LSTM_areas %>% group_by(Group_num) %>%  summarise(n = n(), mean_area = mean(Area), mean_lat = mean(Latitude), mean_long = mean(Longitude), mean_precip = mean('Mean Precipitation', na.rm=T),
                                                  sd_area = sd(Area), sd_lat = sd(Latitude), sd_long = sd(Longitude), sd_precip = sd('Mean Precipitation', na.rm=T))


#visualize data
p1 <- ggplot(LSTM_areas, aes(x = as.factor(Group_num), y = Area, fill = Group_num)) + geom_jitter(width = 0.2) + geom_boxplot(outlier.shape = NA) + theme(legend.position="none")
p2 <- ggplot(LSTM_areas, aes(x = as.factor(Group_num), y = Latitude, fill = Group_num)) + geom_jitter(width = 0.2) + geom_boxplot(outlier.shape = NA) + theme(legend.position="none")
p3 <- ggplot(LSTM_areas, aes(x = as.factor(Group_num), y = Longitude, fill = Group_num)) + geom_jitter(width = 0.2) + geom_boxplot(outlier.shape = NA) + theme(legend.position="none")
p4 <- ggplot(LSTM_areas, aes(x = as.factor(Group_num), y = 'Mean Precipitation', fill = Group_num)) + geom_jitter(width = 0.2) + geom_boxplot(outlier.shape = NA) + theme(legend.position="none")
p5 <- ggplot(LSTM_areas, aes(x = as.factor(Group_num), y = 'Mean Temperature', fill = Group_num)) + geom_jitter(width = 0.2) + geom_boxplot(outlier.shape = NA) + theme(legend.position="none")
p6 <- ggplot(LSTM_areas, aes(x = as.factor(Group_num), y = 'Mean Elevation', fill = Group_num)) + geom_jitter(width = 0.2) + geom_boxplot(outlier.shape = NA) + theme(legend.position="none")
g <- grid.arrange(p1, p2, p3, p4, p5, p6, ncol=3)
#ggsave(file.path(lake_directory,"figures/pca/drivers_by_group.jpg"),
#       units="in", width=5, height=4, dpi=300, device="jpeg", g)

#------------------------------------------------------------------------------------#
#PERMANOVA on transformed driver data

#randomly select 2000 from each cluster
area_subset <-  LSTM_areas %>% group_by(Group_num) %>% slice_sample(n = 2000)

#create distance matrix using only numeric columns
groups_dist_subset <- vegdist(area_subset[], method = "euclidean") #specify these!

#PERMANOVA
adonis2(groups_dist_subset~Group_num, data=area_subset, method="euclidean")

mod <- betadisper(groups_dist_subset, area_subset$Group_num)
permutest(mod)

plot(mod)
boxplot(mod)

mod.HSD <- TukeyHSD(mod)
plot(mod.HSD)

tukey_p <- mod.HSD[[1]][,4]
tukey_letters <- data.frame(multcompLetters(tukey_p)['Letters'])
#1a; 2a; 3bc; 4abc; 5b; 6b; 7ac 

#export table with p-values
tukey_table <- data.frame("clusters" = as.character(rownames(tukey_letters)) ,"p-value"= mod.HSD[[1]][,4])

write.csv(tukey_table, file.path(lake_directory,"data/tukey_p-values.csv"), row.names = FALSE)
